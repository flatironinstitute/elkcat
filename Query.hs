{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Query
  ( Query(..)
  , queryKeys
  , parseQueryToken
  , parseRawQueryToken
  , QueryM
  , QueryEval
  , evaluateQuery
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Control.Monad.Except (Except, throwError, runExcept)
import           Control.Monad.State (StateT(..), evalStateT, state)
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import           Data.Default (Default(..))
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import JSON

type Terms = [J.Value]

jsonTerms :: J.Value -> Terms
jsonTerms J.Null = []
jsonTerms (J.Array v) = V.toList v
jsonTerms j = [j]

data Query = Query
  { queryCount :: Maybe Word
  , querySort :: Terms
  , queryFilter :: Terms
  , queryMustNot :: Terms
  , queryShould :: Terms
  }

instance Semigroup Query where
  -- this is essentially an AND, but is not used to combine ORs
  Query c1 s1 f1 n1 o1 <> Query c2 s2 f2 n2 o2 = Query
    (c1 <|> c2)
    (s1 <> s2)
    (f1 <> f2)
    (n1 <> n2)
    (o1 <> o2)

instance Monoid Query where
  mempty = Query Nothing mempty mempty mempty mempty

instance Default Query where
  def = mempty{ querySort = [J.String "_doc"] }

parseQuery :: ObjectParser Query
parseQuery = do
  queryCount   <- explicitParseFieldMaybe (\j -> J.parseJSON j `jsonPlus` parseReader TR.decimal j) "count"
  querySort    <- terms "sort"
  queryFilter  <- terms "filter"
  queryMustNot <- terms "must_not"
  queryShould  <- terms "should"
  return Query{..}
  where
  terms k = foldMap jsonTerms <$> parseFieldMaybe k

instance J.FromJSON Query where
  parseJSON = withObjectParser "query" parseQuery

parseRawQueryOp :: ObjectParser J.Object
parseRawQueryOp = JM.singleton "op" <$> parseField "op"

queryKeys :: JM.KeyMap ()
queryKeys = JM.fromList $ map (, ())
  [ "count", "sort", "filter", "must_not", "should" ]

parseRawQuery :: ObjectParser J.Object
parseRawQuery = state (\o -> (JM.intersection o queryKeys, JM.difference o queryKeys))

parseRawQueryToken :: ObjectParser J.Object
parseRawQueryToken = parseRawQueryOp <|> parseRawQuery

instance J.ToJSON Query where
  toJSON Query{ queryFilter = [t], queryMustNot = [], queryShould = [] } = t -- simplication
  toJSON Query{..} = J.object
    [ "bool" J..= J.object (catMaybes
      [ term "filter"   queryFilter
      , term "must_not" queryMustNot
      , term "should"   queryShould
      , "minimum_should_match" J..= J.Number 1 <$ guard (not $ null queryShould)
      ])
    ]
    where
    term _ [] = Nothing
    term k [t] = Just $ k J..= t
    term k t = Just $ k J..= t

-- query as OR terms
queryShoulds :: Query -> Terms
queryShoulds Query{ queryFilter = [], queryMustNot = [], queryShould = s } = s
queryShoulds q = [J.toJSON q]

negateQuery :: Query -> Query
negateQuery q = q
  { queryFilter = []
  , queryMustNot = queryShoulds q
  , queryShould = []
  }

orQuery :: Query -> Query -> Query
orQuery q1 q2 = (q1 <> q2)
  { queryFilter = []
  , queryMustNot = []
  , queryShould = queryShoulds q1 ++ queryShoulds q2
  }

data QueryOp
  = OpOpen
  | OpClose
  | OpNot
  | OpOr

instance J.FromJSON QueryOp where
  parseJSON = J.withText "query op" (po . T.toLower) where
    po "!"     = return OpNot
    po "not"   = return OpNot
    po "("     = return OpOpen
    po "open"  = return OpOpen
    po ")"     = return OpClose
    po "close" = return OpClose
    po "|"     = return OpOr
    po "or"    = return OpOr
    po o = fail $ "unknown query op: " ++ show o

data QueryToken
  = TokenQuery !Query
  | TokenOp !QueryOp

parseQueryToken :: ObjectParser QueryToken
parseQueryToken =
  TokenOp <$> parseField "op" `objectPlus` TokenQuery <$> parseQuery

instance J.FromJSON QueryToken where
  parseJSON = withObjectParser "query token" parseQueryToken

instance {-# OVERLAPPING #-} MonadFail (Except [String]) where
  fail = throwError . return

type QueryM c = StateT c (Except [String])
type QueryEval c = QueryM c QueryToken

data ParseState c = ParseState [QueryEval c] c

type QueryParser c = QueryM (ParseState c)

-- get and evaluate the next arg
popToken :: QueryParser c (Maybe QueryToken)
popToken = StateT pop where
  pop s@(ParseState [] _) = return (Nothing, s)
  pop (ParseState (e:r) c) = do
    (q, c') <- runStateT e c
    return (Just q, ParseState r c')

-- handle open groups, treat as single argument
popGroup :: QueryParser c (Maybe QueryToken)
popGroup = do
  mapM got =<< popToken
  where
  got (TokenOp OpOpen) = TokenQuery <$> parseGroup False mempty
  got t = return t

-- ensure next arg exists
popGroup' :: String -> QueryParser c QueryToken
popGroup' err = do
  q <- maybe (fail $ "Missing " ++ err) return =<< popGroup
  case q of
    TokenQuery (Query{ queryCount = Just _ }) -> fail "Count specifications can only be at top-level"
    TokenQuery (Query{ querySort = _:_ }) -> fail "Sort specifications can only be at top-level"
    _ -> return q

-- handle the rest of a group
parseGroup :: Bool -> Query -> QueryParser c Query
parseGroup top q0 =
  if top
    then maybe (return q0) got =<< popGroup
    else got =<< popGroup' "close group"
  where
  got (TokenOp OpOpen) = fail "Unexpected open group" -- handled in popGroup
  got (TokenOp OpClose)
    | top = fail "Unmatched close group"
    | otherwise = return q0
  got (TokenOp OpNot) = do
    t <- popGroup' "NOT argument"
    case t of
      TokenQuery q -> next $ negateQuery q
      TokenOp _ -> fail "NOT needs a query argument"
  got (TokenOp OpOr) = do
    q <- parseGroup top mempty
    return $ orQuery q0 q
  got (TokenQuery q) = next q
  next = parseGroup top . (q0 <>)

evaluateQuery :: [QueryEval c] -> c -> Either [String] Query
evaluateQuery args = runExcept
  . evalStateT (parseGroup True mempty) . ParseState args
