{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Args
  ( Query(..)
  , Option
  , Argument
  ) where

import qualified Data.Aeson.Types as J
import qualified Data.Array as A
import           Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import           Text.Regex.Posix.String (Regex, compExtended)
import qualified Text.Regex.Base as RE
import qualified System.Console.GetOpt as Opt

import Placeholder
import Time

data Query = Query
  { queryFilters :: [J.Value]
  }

instance Semigroup Query where
  Query f1 <> Query f2 = Query (f1 <> f2)

instance Monoid Query where
  mempty = Query []

type Argument = String -> Query
type Option = Opt.OptDescr Query

data ArgCase = ArgCase
  { argMatch :: Maybe Regex
  , argSplit :: Maybe T.Text
  , argFilter :: Maybe J.Value -- with placeholders
  }

newtype ArgHandler = ArgHandler
  { argCases :: [ArgCase]
  }

parseCase :: J.Object -> J.Parser ArgCase
parseCase a = do
  match <- a J..:? "match"
  argMatch <- mapM (RE.makeRegexOptsM compExtended RE.blankExecOpt) (match :: Maybe String)
  argSplit <- a J..:? "split"
  argFilter <- a J..:? "filter"
  return ArgCase{..}

instance J.FromJSON ArgCase where
  parseJSON = J.withObject "argument case" $ parseCase

parseHandler :: J.Object -> J.Parser ArgHandler
parseHandler a = do
  argBase <- parseCase a
  argCases <- (++ [argBase]) <$> a J..:! "switch" J..!= []
  return ArgHandler{..}

instance J.FromJSON ArgHandler where
  parseJSON = J.withObject "argument handler" $ parseHandler

noArg :: ArgHandler -> Maybe Query
noArg (ArgHandler [ArgCase Nothing Nothing f])
  | any (any T.null . collectPlaceholdersJSON) f = Nothing
  | otherwise = Just $ Query (maybeToList f)
noArg _ = Nothing

matchMaybe :: Maybe Regex -> String -> Maybe (A.Array Int String)
matchMaybe Nothing s = Just (A.listArray (0,0) [s])
matchMaybe (Just r) s
  | Just ("", groups, "") <- RE.matchOnceText r s = Just (fmap fst groups)
  | otherwise = Nothing

evaluateCases :: [ArgCase] -> Argument
evaluateCases [] _ = mempty
evaluateCases (ArgCase{..}:r) a
  | Just groups <- matchMaybe argMatch a =
    query groups $ maybe aj
      (\d -> J.toJSON $ T.splitOn d at)
      argSplit
  | otherwise = evaluateCases r a
  where
  query groups s0 = Query $ maybe [] (return . substitutePlaceholdersJSON (subst groups s0)) argFilter
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
    help <- a J..:? "help" J..!= "filter"
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
