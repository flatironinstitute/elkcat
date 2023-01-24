{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Query
  ( Param(..)
  , Query(..)
  , ParamQuery(..)
  , queryKeys
  , parseRawQueryToken
  , QueryM
  , QueryEval
  , evaluateQuery
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow (second)
import           Control.Monad (guard, msum)
import           Control.Monad.Except (Except, throwError, runExcept)
import           Control.Monad.State (StateT(..), evalStateT, gets, modify)
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

parseTermsField :: J.Key -> ObjectParser Terms
parseTermsField k = foldMap jsonTerms <$> parseFieldMaybe k


-- |top-level parameters for the overall search
data Param = Param
  { paramCount :: Maybe Word
  , paramSort :: Terms
  } deriving (Show)

instance Semigroup Param where
  Param c1 s1 <> Param c2 s2 = Param
    (c1 <|> c2)
    (s1 <> s2)

instance Monoid Param where
  mempty = Param Nothing mempty

instance Default Param where
  def = mempty{ paramSort = [J.String "_doc"] }

instance FromObject Param where
  parseObject = do
    paramCount   <- explicitParseFieldMaybe (\j -> J.parseJSON j `jsonPlus` parseReader TR.decimal j) "count"
    paramSort    <- parseTermsField "sort"
    return Param{..}

paramKeys :: JM.KeyMap ()
paramKeys = JM.fromList $ map (, ())
  [ "count", "sort" ]

instance J.FromJSON Param where
  parseJSON = parseJSONObject "query params"


-- |A query in ES bool form, representing a conjuction of the three sets of terms
data Query = Query
  { queryFilter :: Terms -- ^ ANDed terms (ALL)
  , queryMustNot :: Terms -- ^ NORed terms (NONE)
  , queryShould :: Terms -- ^ ORed terms (ANY) except that [] is TRUE
  }

instance FromObject Query where
  parseObject = do
    queryFilter  <- parseTermsField "filter"
    queryMustNot <- parseTermsField "must_not"
    queryShould  <- parseTermsField "should"
    return Query{..}

queryKeys :: JM.KeyMap ()
queryKeys = JM.fromList $ map (, ())
  [ "filter", "must_not", "should" ]

instance J.FromJSON Query where
  parseJSON = parseJSONObject "query"

instance J.ToJSON Query where
  -- |render as an ES query term
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

instance Semigroup Query where
  -- |AND two queries
  Query f1 n1 o1 <> Query f2 n2 o2 = Query
    (f1 <> f2 <> ands)
    (n1 <> n2)
    ors
    where
    -- (A OR B) AND (X OR Y) needs additional nesting
    (ands, ors)
      | null o1 || null o2 = ([], o1 <> o2)
      | otherwise = (map (J.toJSON . Query [] []) [o1, o2], [])

instance Monoid Query where
  mempty = Query mempty mempty mempty

-- query as OR (should) terms
queryShoulds :: Query -> Terms
queryShoulds Query{ queryFilter = [], queryMustNot = [], queryShould = s } = s
queryShoulds q = [J.toJSON q]

-- NOT(OR(should)) becomes NOR(should); NOT(AND(filter)) becomes nested
negateQuery :: Query -> Query
negateQuery q = Query
  { queryFilter = []
  , queryMustNot = queryShoulds q
  , queryShould = []
  }

orQuery :: Query -> Query -> Query
orQuery q1 q2 = Query
  { queryFilter = []
  , queryMustNot = []
  , queryShould = queryShoulds q1 ++ queryShoulds q2
  }


-- |combining operators passed as arguments to build an expression
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

instance FromObject QueryOp where
  parseObject = parseField "op"

opKeys :: JM.KeyMap ()
opKeys = JM.singleton "op" ()


data QueryToken
  = TokenParam Param
  | TokenQuery Query
  | TokenOp QueryOp

instance FromObject QueryToken where
  parseObject =
                 TokenOp    <$> parseObject
    `objectPlus` TokenParam <$> nonEmptyParser parseObject
    `objectPlus` TokenQuery <$> parseObject

instance J.FromJSON QueryToken where
  parseJSON = parseJSONObject "query argument"

parseRawQueryToken :: ObjectParser J.Object
parseRawQueryToken = msum $ map takeSome [opKeys, paramKeys, queryKeys] where
  takeSome q = do
    o <- takeObjectKeys q
    guard $ not $ JM.null o
    return o


data ParamQuery = ParamQuery !Param !Query

instance Semigroup ParamQuery where
  ParamQuery p1 q1 <> ParamQuery p2 q2 =
    ParamQuery (p1 <> p2) (q1 <> q2)

instance Monoid ParamQuery where
  mempty = ParamQuery mempty mempty

instance Default ParamQuery where
  def = ParamQuery def mempty

instance FromObject ParamQuery where
  parseObject = ParamQuery <$> parseObject <*> parseObject

instance J.FromJSON ParamQuery where
  parseJSON = parseJSONObject "query params"

instance {-# OVERLAPPING #-} MonadFail (Except [String]) where
  fail = throwError . return

type QueryM c = StateT c (Except [String])
type QueryEval c = QueryM c QueryToken
type ParseState c = ([QueryEval c], c)
type QueryParser c = QueryM (ParseState c)

-- get and evaluate the next arg
popToken :: QueryParser c (Maybe QueryToken)
popToken = StateT pop where
  pop s@([], _) = return (Nothing, s)
  pop (e:r, c) = do
    (q, c') <- runStateT e c
    return (Just q, (r, c'))

-- handle open groups, treat as single argument
popGroup :: QueryParser c (Maybe QueryToken)
popGroup = do
  mapM got =<< popToken
  where
  got (TokenOp OpOpen) = do
    -- context state is local
    c <- gets snd
    q <- parseGroup mempty
    modify (second $ const c)
    return $ TokenQuery q
  got (TokenOp OpNot) = do
    t <- popGroup' "NOT argument"
    case t of
      TokenQuery q -> return $ TokenQuery $ negateQuery q
      _ -> fail "NOT needs a query argument"
  got t = return t

-- ensure next arg exists
popGroup' :: String -> QueryParser c QueryToken
popGroup' err = do
  q <- maybe (fail $ "Missing " ++ err) return =<< popGroup
  case q of
    TokenParam p -> fail $ "Count and sort specifications can only be at top-level: " ++ show p
    _ -> return q

-- handle the rest of a group
parseGroup :: Query -> QueryParser c Query
parseGroup q0 = do
  t <- popGroup' "close group"
  case t of
    TokenOp OpClose -> return q0
    TokenOp OpOr -> do
      q <- parseGroup mempty
      return $ orQuery q0 q
    TokenQuery q -> parseGroup (q0 <> q)

-- handle the rest of a group
parseTopGroup :: ParamQuery -> QueryParser c ParamQuery
parseTopGroup pq0@(ParamQuery p0 q0) = do
  t <- popGroup
  case t of
    Nothing -> return pq0
    Just (TokenOp OpClose) -> fail "Unmatched close group"
    Just (TokenOp OpOr) -> do
      ParamQuery p q <- parseTopGroup (ParamQuery p0 mempty)
      return $ ParamQuery p (orQuery q0 q)
    Just (TokenParam p) -> parseTopGroup (ParamQuery (p0 <> p) q0)
    Just (TokenQuery q) -> parseTopGroup (ParamQuery p0 (q0 <> q))

evaluateQuery :: [QueryEval c] -> c -> Either [String] ParamQuery
evaluateQuery args = runExcept
  . evalStateT (parseTopGroup mempty) . (,) args
