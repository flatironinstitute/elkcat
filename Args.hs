{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Args
  ( Query(..)
  , Option
  , Argument
  , parseArgument
  , parseOption
  , runArgs
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (msum, guard, foldM)
import           Control.Monad.Except (Except, throwError, runExcept)
import           Control.Monad.State (StateT, get, gets, modify, evalStateT)
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import qualified Data.Array as A
import           Data.Default (Default(..))
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import           Text.Read (readMaybe)
import           Text.Regex.Posix.String (Regex, compExtended)
import qualified Text.Regex.Base as RE
import qualified System.Console.GetOpt as Opt

import Placeholder
import JSON
import Time

{-
 TODO:
   \( ... \) grouping
   \| -o or
 -}

newtype Terms = Terms{ termsList :: [J.Value] }
  deriving (Semigroup, Monoid, J.ToJSON)

jsonTerms :: J.Value -> Terms
jsonTerms J.Null = Terms []
jsonTerms (J.Array v) = Terms $ V.toList v
jsonTerms j = Terms [j]

instance J.FromJSON Terms where
  parseJSON = return . jsonTerms

data Query = Query
  { queryCount :: Maybe Word
  , querySort :: Terms
  , queryFilter :: Terms
  }

-- |which field this sort term sorts on (invalid for invalid sort terms)
querySortKey :: J.Value -> T.Text
querySortKey (J.String s) = s
querySortKey (J.Object m) = foldMap JK.toText $ JM.keys m
querySortKey _ = T.empty

instance Semigroup Query where
  Query n1 s1 f1 <> Query n2 s2 f2 = Query
    (n1 <|> n2)
    (s1 <> Terms (filter (\s -> querySortKey s `notElem` map querySortKey (termsList s1)) (termsList s2)))
    (f1 <> f2)

instance Monoid Query where
  mempty = Query Nothing mempty mempty

instance Default Query where
  def = mempty{ querySort = Terms [J.String "_doc"] }

data Context = Context
  { contextTime :: UTCTime
  }

type ArgM = StateT Context (Except [String])
type ArgQuery = ArgM Query

instance {-# OVERLAPPING #-} MonadFail ArgM where
  fail = throwError . return

foldArgs :: (Monad m, Monoid a) => [m a] -> m a
foldArgs = foldM (\b f -> (b <>) <$> f) mempty

evaluateArg :: ArgM a -> Context -> Either [String] a
evaluateArg arg = runExcept . evalStateT arg

runArg :: ArgM a -> IO (Either [String] a)
runArg arg = do
  t <- getCurrentTime
  return $ evaluateArg arg (Context t)

runArgs :: [ArgM Query] -> IO (Either [String] Query)
runArgs = runArg . foldArgs

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
jsonDate = J.Number . (1e3 *) . realToFrac . utcTimeToPOSIXSeconds

data ArgCase = ArgCase
  { argMatch :: Maybe (T.Text, Regex)
  , argSplit :: Maybe T.Text
  , argType :: ArgType
  , argFilter
  , argSort
  , argCount :: J.Value -- with placeholders
  }

data ArgHandler = ArgHandler
  { argCases :: [ArgCase]
  , argLabel :: String
  }

parseQuery :: ObjectParser Query
parseQuery = do
  queryCount  <- parseFieldMaybe "count"
  querySort   <- parseFieldMaybe "sort"   .!= Terms []
  queryFilter <- parseFieldMaybe "filter" .!= Terms []
  return Query{..}

instance J.FromJSON Query where
  parseJSON = withObjectParser "query" parseQuery

parseCase :: ObjectParser ArgCase
parseCase = do
  argMatch <- explicitParseFieldMaybe 
    (J.withText "regex" $ \m -> (,) m <$> RE.makeRegexOptsM compExtended RE.blankExecOpt (T.unpack m))
    "match"
  argSplit  <- parseFieldMaybe "split"
  argType   <- parseFieldMaybe "type"   .!= TypeString
  argFilter <- parseFieldMaybe "filter" .!= J.Null
  argSort   <- parseFieldMaybe "sort"   .!= J.Null
  argCount  <- parseFieldMaybe "count"  .!= J.Null
  return ArgCase{..}

instance J.FromJSON ArgCase where
  parseJSON = withObjectParser "argument case" parseCase

parseHandler :: ObjectParser ArgHandler
parseHandler = do
  argCases <- parseField "switch" <|> return <$> parseCase
  argLabel <- parseFieldMaybe "arg" .!= "ARG"
  return ArgHandler{..}

matchMaybe :: Maybe (T.Text, Regex) -> String -> Maybe (A.Array Int String)
matchMaybe Nothing s = Just (A.listArray (0,0) [s])
matchMaybe (Just (_, r)) s
  | Just ("", groups, "") <- RE.matchOnceText r s = Just (fmap fst groups)
  | otherwise = Nothing

evaluateCases :: [ArgCase] -> Argument
evaluateCases cases s = msum $ map (`evaluateCase` s) cases

evaluateCase :: ArgCase -> Argument
evaluateCase ArgCase{..} a
  | Just groups <- matchMaybe argMatch a =
    query groups =<< maybe (format at)
      (\d -> J.toJSON <$> mapM format (T.splitOn d at))
      argSplit
  | otherwise = fail $ "argument " ++ show a ++ " does not match " ++ foldMap (show . fst) argMatch
  where
  format s = case argType of
    TypeString -> return $ J.String s
    TypeDate   -> jsonDate <$> parseDateArg (T.unpack s)
    TypeNumber -> J.Number <$> maybe (fail $ "invalid numeric value: " ++ show s) return (readMaybe (T.unpack s))
  query groups s0 = do
    queryCount <- parsecount $ subph argCount
    let querySort = terms argSort
        queryFilter = terms argFilter
    return Query{..}
    where
    terms = jsonTerms . subph
    subph = substitutePlaceholdersJSON subst
    subst "" = Just s0
    subst (TR.decimal -> Right (i, ""))
      | i == 0 = Just aj
      | A.inRange (A.bounds groups) i = Just $ J.String $ T.pack $ groups A.! i
    subst _ = Nothing
  at = T.pack a
  aj = J.String at
  parsecount (J.String "") = return Nothing
  parsecount (J.String (TR.decimal -> Right (n, ""))) = return $ Just n
  parsecount j = either fail return $ J.parseEither J.parseJSON j

evaluateHandler :: ArgHandler -> Argument
evaluateHandler = evaluateCases . argCases

parseArgument :: Macros -> J.Value -> J.Parser Argument
parseArgument macros = withObjectParser "argument" $ do
  expandMacros macros
  evaluateHandler <$> parseHandler

parseOption :: Macros -> J.Value -> J.Parser Option
parseOption macros = withObjectParser "option" $ do
  expandMacros macros
  flags <- parseFieldMaybe "flags" .!= V.empty
  let (short, long) = V.partitionWith shortlong flags
  help <- parseFieldMaybe "help" .!= ""
  o <- parseNoArg <|> parseArg
  return $ Opt.Option
    (V.toList short)
    (V.toList long)
    o
    help
  where
  shortlong [c] = Left c
  shortlong s = Right s
  parseNoArg = do
    q@Query{..} <- parseQuery
    guard . JM.null =<< get -- checkUnparsedFields
    noph queryFilter
    noph querySort
    return $ Opt.NoArg $ return q
    where
    noph = guard . not . any (any T.null . collectPlaceholdersJSON) . termsList
  parseArg :: ObjectParser (Opt.ArgDescr ArgQuery)
  parseArg = do
    h <- parseHandler
    return $ Opt.ReqArg (evaluateHandler h) (argLabel h)

