{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Args
  ( Option
  , Argument
  , parseArgument
  , parseOption
  , runArgs
  ) where

import           Control.Monad (msum, guard)
import           Control.Monad.State (gets, modify)
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import qualified Data.Array as A
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import           Text.Read (readMaybe)
import           Text.Regex.Posix.String (Regex, compExtended)
import qualified Text.Regex.Base as RE
import qualified System.Console.GetOpt as Opt

import Placeholder
import JSON
import Time
import Query

data Context = Context
  { contextTime :: UTCTime
  }

type ArgM = QueryM Context
type ArgQuery = QueryEval Context

argParseJSON :: (a -> J.Parser b) -> a -> ArgM b
argParseJSON p = either fail return . J.parseEither p

runArgs :: [ArgQuery] -> IO (Either [String] ParamQuery)
runArgs args = do
  contextTime <- getCurrentTime
  return $ evaluateQuery args Context{..}

type Argument = String -> ArgQuery
type Option = Opt.OptDescr ArgQuery

data ArgType
  = TypeString
  | TypeDate
  | TypeNumber

instance J.FromJSON ArgType where
  parseJSON = J.withText "arg type" pt where
    pt "string"    = return TypeString
    pt "text"      = return TypeString
    pt "date"      = return TypeDate
    pt "time"      = return TypeDate
    pt "datetime"  = return TypeDate
    pt "timestamp" = return TypeDate
    pt "number"    = return TypeNumber
    pt t = fail $ "unknown arg type: " ++ show t

parseDateArg :: String -> ArgM UTCTime
parseDateArg s = do
  t0 <- gets contextTime
  maybe (fail $ "could not parse date: " ++ s)
    (\t -> do
      modify (\c -> c{ contextTime = t })
      return t)
    $ parseDatetime' t0 s

jsonDate :: UTCTime -> J.Value
jsonDate = (J.toJSON :: Integer -> J.Value) . truncate . (1e3 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

data ArgCase = ArgCase
  { argMatch :: Maybe (T.Text, Regex)
  , argSplit :: Maybe T.Text
  , argType :: ArgType
  , argQuery :: J.Object -- remaining fields, with placeholders
  }

data ArgHandler = ArgHandler
  { argCases :: [ArgCase]
  , argLabel :: String
  }

instance FromObject ArgCase where
  parseObject = do
    argMatch <- explicitParseFieldMaybe 
      (J.withText "regex" $ \m -> (,) m <$> RE.makeRegexOptsM compExtended RE.blankExecOpt (T.unpack m))
      "match"
    argSplit <- parseFieldMaybe "split"
    argType  <- parseFieldMaybe "type"   .!= TypeString
    argQuery <- parseRawQueryToken
    return ArgCase{..}

instance J.FromJSON ArgCase where
  parseJSON = parseJSONObject "argument case"

instance FromObject ArgHandler where
  parseObject = do
    argLabel <- parseFieldMaybe "arg" .!= "ARG"
    argCases <- parseField "switch" `objectPlus` return <$> parseObject
    return ArgHandler{..}

matchMaybe :: Maybe (T.Text, Regex) -> String -> Maybe (A.Array Int String)
matchMaybe Nothing s = Just (A.listArray (0,0) [s])
matchMaybe (Just (_, r)) s
  | Just ("", groups, "") <- RE.matchOnceText r s = Just (fmap fst groups)
  | otherwise = Nothing

evaluateCase :: ArgCase -> Argument
evaluateCase ArgCase{..} a
  | Just groups <- matchMaybe argMatch a =
    build groups =<< maybe (format at)
      (\d -> J.toJSON <$> mapM format (T.splitOn d at))
      argSplit
  | otherwise = fail $ "argument " ++ show a ++ " does not match " ++ foldMap (show . fst) argMatch
  where
  format s = case argType of
    TypeString -> return $ J.String s
    TypeDate   -> jsonDate <$> parseDateArg (T.unpack s)
    TypeNumber -> J.Number <$> maybe (fail $ "invalid numeric value: " ++ show s) return (readMaybe (T.unpack s))
  build groups s0 = argParseJSON (runObjectParser parseObject) 
    $ substitutePlaceholdersObject subst argQuery where
    subst "" = Just s0
    subst (TR.decimal -> Right (i, ""))
      | i == 0 = Just $ J.String at
      | A.inRange (A.bounds groups) i = Just $ J.String $ T.pack $ groups A.! i
    subst _ = Nothing
  at = T.pack a

evaluateCases :: [ArgCase] -> Argument
evaluateCases cases s = msum $ map (`evaluateCase` s) cases

parseArgument :: Macros -> J.Value -> J.Parser (Argument, String, String)
parseArgument macros = withObjectParser "argument" $ do
  expandMacros macros
  help <- parseFieldMaybe "help" .!= ""
  h <- parseObject
  return (evaluateCases $ argCases h, argLabel h, help)

parseOption :: Macros -> J.Value -> J.Parser Option
parseOption macros = withObjectParser "option" $ do
  expandMacros macros
  flags <- parseFieldMaybe "flags" .!= V.empty
  let (short, long) = V.partitionWith shortlong flags
  help <- parseFieldMaybe "help" .!= ""
  o <- parseNoArg `objectPlus` parseArg
  return $ Opt.Option
    (V.toList short)
    (V.toList long)
    o
    help
  where
  shortlong [c] = Left c
  shortlong s = Right s
  parseNoArg = do
    -- make sure there are no argument placeholders
    guard . not . any T.null =<< gets collectPlaceholdersObject
    q <- parseObject
    guard =<< gets JM.null -- checkUnparsedFields
    return $ Opt.NoArg $ return q
  parseArg :: ObjectParser (Opt.ArgDescr ArgQuery)
  parseArg = do
    h <- parseObject
    return $ Opt.ReqArg (evaluateCases $ argCases h) (argLabel h)
