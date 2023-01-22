{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Query
  ( Query(..)
  , queryKeys
  , parseQuery
  ) where

import           Control.Applicative ((<|>))
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import           Data.Default (Default(..))
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import JSON

type Terms = [J.Value]

jsonTerms :: J.Value -> Terms
jsonTerms J.Null = []
jsonTerms (J.Array v) = V.toList v
jsonTerms j = [j]

data QueryOp
  = OpNot
  | OpOpen
  | OpClose
  | OpOr

instance J.FromJSON QueryOp where
  parseJSON = J.withText "query op" po where
    po "!"     = return OpNot
    po "not"   = return OpNot
    po "("     = return OpOpen
    po "open"  = return OpOpen
    po ")"     = return OpClose
    po "close" = return OpClose
    po "|"     = return OpOr
    po "or"    = return OpOr
    po o = fail $ "unknown query op: " ++ show o

instance J.ToJSON QueryOp where
  toJSON = J.String . so where
    so OpNot   = "!"    
    so OpOpen  = "("    
    so OpClose = ")"    
    so OpOr    = "|"    

data Query = Query
  { queryCount :: Maybe Word
  , querySort :: Terms
  , queryFilter :: Terms
  , queryMustNot :: Terms
  , queryOp :: Maybe QueryOp
  }

instance Semigroup Query where
  Query c1 s1 f1 n1 o1 <> Query c2 s2 f2 n2 o2 = Query
    (c1 <|> c2)
    (s1 <> s2)
    (f1 <> f2)
    (n1 <> n2)
    (o1 <|> o2)

instance Monoid Query where
  mempty = Query Nothing mempty mempty mempty Nothing

instance Default Query where
  def = mempty{ querySort = [J.String "_doc"] }

parseQuery :: ObjectParser Query
parseQuery = do
  queryOp      <- parseFieldMaybe "op"
  queryCount   <- explicitParseFieldMaybe (\j -> J.parseJSON j <|> parseReader TR.decimal j) "count"
  querySort    <- terms "sort"
  queryFilter  <- terms "filter"
  queryMustNot <- terms "must_not"
  return Query{..}
  where
  terms k = foldMap jsonTerms <$> parseFieldMaybe k

instance J.FromJSON Query where
  parseJSON = withObjectParser "query" parseQuery

queryJSON :: Query -> J.Object
queryJSON Query{..} = JM.fromList
  [ "op" J..= queryOp
  , "count" J..= queryCount
  , "sort" J..= querySort
  , "filter" J..= queryFilter
  , "must_not" J..= queryMustNot
  ]

instance J.ToJSON Query where
  toJSON = J.Object . queryJSON

queryKeys :: J.Object
queryKeys = queryJSON mempty
