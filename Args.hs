{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Args
  ( Query(..)
  , Option
  , Argument
  , foldM'
  , runArg
  ) where

import           Control.Monad (msum, foldM)
import           Control.Monad.Except (Except, throwError, runExcept)
import           Control.Monad.State (StateT, gets, modify, evalStateT)
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import qualified Data.Array as A
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import           Text.Regex.Posix.String (Regex, compExtended)
import qualified Text.Regex.Base as RE
import qualified System.Console.GetOpt as Opt

import Placeholder
import Time

{-
 global args:
   -n NUM number of results
   \( ... \) grouping
   \| -o or
 -}

data Query = Query
  { querySort :: [J.Value]
  , queryFilters :: [J.Value]
  }

-- |which field this sort term sorts on (invalid for invalid sort terms)
querySortKey :: J.Value -> T.Text
querySortKey (J.String s) = s
querySortKey (J.Object m) = foldMap JK.toText $ JM.keys m
querySortKey _ = T.empty

instance Semigroup Query where
  Query s1 f1 <> Query s2 f2 = Query
    (s1 <> filter (\s -> querySortKey s `notElem` map querySortKey s1) s2)
    (f1 <> f2)

instance Monoid Query where
  mempty = Query [J.String "_doc"] []

data Context = Context
  { contextTime :: UTCTime
  }

type ArgM = StateT Context (Except String)
type ArgQuery = ArgM Query

instance {-# OVERLAPPING #-} MonadFail ArgM where
  fail = throwError

foldM' :: (Monad m, Foldable t, Monoid a) => t (m a) -> m a
foldM' = foldM (\m f -> (m <>) <$> f) mempty

evaluateArg :: ArgM a -> Context -> Either String a
evaluateArg arg = runExcept . evalStateT arg

runArg :: ArgM a -> IO a
runArg arg = do
  t <- getCurrentTime
  either fail return $ evaluateArg arg (Context t)

type Argument = String -> ArgQuery
type Option = Opt.OptDescr ArgQuery

data ArgType
  = TypeString
  | TypeDate

instance J.FromJSON ArgType where
  parseJSON = J.withText "arg type" pt where
    pt "string" = return TypeString
    pt "text" = return TypeString
    pt "date" = return TypeDate
    pt "time" = return TypeDate
    pt "datetime" = return TypeDate
    pt "timestamp" = return TypeDate
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
jsonDate = J.toJSON . (1e3 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

data ArgCase = ArgCase
  { argMatch :: Maybe (String, Regex)
  , argSplit :: Maybe T.Text
  , argType :: Maybe ArgType
  , argFilter :: Maybe J.Value -- with placeholders
  , argSort :: [J.Value]
  }

newtype ArgHandler = ArgHandler
  { argCases :: [ArgCase]
  }

parseCase :: J.Object -> J.Parser ArgCase
parseCase a = do
  match <- a J..:? "match"
  argMatch <- mapM (\m -> (,) m <$> RE.makeRegexOptsM compExtended RE.blankExecOpt m) match
  argSplit <- a J..:? "split"
  argType <- a J..:? "type"
  argFilter <- a J..:? "filter"
  argSort <- a J..:? "sort" J..!= []
  return ArgCase{..}

caseKeys :: [JK.Key]
caseKeys = ["match", "split", "type", "filter", "sort"]

instance J.FromJSON ArgCase where
  parseJSON = J.withObject "argument case" $ parseCase

parseHandler :: J.Object -> J.Parser ArgHandler
parseHandler a = do
  argBase <- if any (`JM.member` a) caseKeys
    then (++) . return <$> parseCase a
    else return id
  argCases <- argBase <$> a J..:! "switch" J..!= []
  return ArgHandler{..}

instance J.FromJSON ArgHandler where
  parseJSON = J.withObject "argument handler" $ parseHandler

noArg :: ArgHandler -> Maybe ArgQuery
noArg (ArgHandler [ArgCase Nothing Nothing Nothing f s])
  | hasp f = Nothing
  | otherwise = Just $ return $ Query s (maybeToList f)
  where
  hasp = any (any T.null . collectPlaceholdersJSON)
noArg _ = Nothing

matchMaybe :: Maybe (String, Regex) -> String -> Maybe (A.Array Int String)
matchMaybe Nothing s = Just (A.listArray (0,0) [s])
matchMaybe (Just (_, r)) s
  | Just ("", groups, "") <- RE.matchOnceText r s = Just (fmap fst groups)
  | otherwise = Nothing

evaluateCases :: [ArgCase] -> Argument
evaluateCases cases s = msum $ map (`evaluateCase` s) cases

evaluateCase :: ArgCase -> Argument
evaluateCase ArgCase{..} a
  | Just groups <- matchMaybe argMatch a =
    query groups <$> maybe (format at)
      (\d -> J.toJSON <$> mapM format (T.splitOn d at))
      argSplit
  | otherwise = fail $ "argument " ++ show a ++ " does not match " ++ foldMap (show . fst) argMatch
  where
  format s = case fromMaybe TypeString argType of
    TypeString -> return $ J.String s
    TypeDate -> jsonDate <$> parseDateArg (T.unpack s)
  query groups s0 = Query argSort $ maybe [] (return . substitutePlaceholdersJSON (subst groups s0)) argFilter
  subst _ s0 "" = Just s0
  subst groups _ (TR.decimal -> Right (i, ""))
    | i == 0 = Just aj
    | A.inRange (A.bounds groups) i = Just $ J.String $ T.pack $ groups A.! i
  subst _ _ _ = Nothing
  at = T.pack a
  aj = J.String at

evaluateHandler :: ArgHandler -> Argument
evaluateHandler = evaluateCases . argCases

instance J.FromJSON Argument where
  parseJSON j =
    evaluateHandler <$> J.parseJSON j

instance J.FromJSON Option where
  parseJSON = J.withObject "option" $ \a -> do
    flags <- a J..:? "flags" J..!= V.empty
    let (short, long) = V.partitionWith shortlong flags
    h <- parseHandler a
    arg <- a J..:? "arg" J..!= "STRING"
    help <- a J..:? "help" J..!= ""
    return $ Opt.Option
      (V.toList short)
      (V.toList long)
      (maybe
        (Opt.ReqArg (evaluateHandler h) arg)
        Opt.NoArg $ noArg h)
      help
    where
    shortlong [c] = Left c
    shortlong s = Right s
