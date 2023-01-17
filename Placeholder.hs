{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Placeholder
  ( Placeholder(..)
  , Placeholders
  , collectPlaceholders
  , parsePlaceholders
  , jsonToText
  , substitutePlaceholders
  , substitutePlaceholdersBuilder
  , substitutePlaceholdersJSON
  , collectPlaceholdersJSON
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Text as JT
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

data Placeholder a
  = Literal !T.Text
  | Placeholder !a
  deriving Show

instance Functor Placeholder where
  fmap f (Placeholder a) = Placeholder (f a)
  fmap _ (Literal t) = Literal t

instance Foldable Placeholder where
  foldr f b (Placeholder a) = f a b
  foldr _ b _ = b

instance Traversable Placeholder where
  traverse f (Placeholder a) = Placeholder <$> f a
  traverse _ (Literal t) = pure $ Literal t

type Placeholders a = [Placeholder a]

collectPlaceholders :: Placeholders a -> [a]
collectPlaceholders [] = []
collectPlaceholders (Placeholder a:r) = a:collectPlaceholders r
collectPlaceholders (_:r) = collectPlaceholders r

parsePlaceholders :: T.Text -> Placeholders T.Text
parsePlaceholders = parse1 . T.splitOn "{" where
  parse1 [] = []
  parse1 (p:r)
    | T.null p = parse r
    | otherwise = Literal p : parse r
  parse [] = []
  parse (a:r)
    | T.null s = Literal ('{' `T.cons` a) : parse r -- unbalanced { error
    | otherwise = Placeholder p : parse1 (T.tail s : r)
    where (p,s) = T.breakOn "}" a

instance J.FromJSON1 Placeholder where
  liftParseJSON _ _ = J.withText "placeholder" $ return . Literal -- not used
  liftParseJSONList p _ = J.withText "placeholders" $ mapM (mapM (p . J.String)) . parsePlaceholders

instance J.FromJSON a => J.FromJSON (Placeholder a) where
  parseJSON = J.parseJSON1
  parseJSONList = J.liftParseJSONList J.parseJSON J.parseJSONList

substitutePlaceholdersBuilder :: (a -> TB.Builder) -> Placeholders a -> TB.Builder
substitutePlaceholdersBuilder f = foldMap sp where
  sp (Literal t) = TB.fromText t
  sp (Placeholder x) = f x

substitutePlaceholders :: (a -> T.Text) -> Placeholders a -> T.Text
substitutePlaceholders f = foldMap sp where
  sp (Literal t) = t
  sp (Placeholder x) = f x

jsonToText :: J.Value -> TB.Builder
jsonToText (J.String s) = TB.fromText s
jsonToText J.Null = mempty
jsonToText j = JT.encodeToTextBuilder j

substitutePlaceholdersValues :: (T.Text -> Maybe J.Value) -> Placeholders T.Text -> T.Text
substitutePlaceholdersValues f = TL.toStrict . TB.toLazyText .
  substitutePlaceholdersBuilder (\s -> maybe (TB.singleton '{' <> TB.fromText s <> TB.singleton '}') jsonToText $ f s)

substitutePlaceholdersJSON :: (T.Text -> Maybe J.Value) -> J.Value -> J.Value
substitutePlaceholdersJSON sub j@(J.String s) = case parsePlaceholders s of
  [Literal _] -> j
  [Placeholder p] -> fromMaybe j $ sub p
  m -> J.String $ substitutePlaceholdersValues sub m
substitutePlaceholdersJSON sub (J.Object j) = J.Object $ JM.mapKeyVal
  (JK.fromText . substitutePlaceholdersValues sub . parsePlaceholders . JK.toText)
  (substitutePlaceholdersJSON sub) j
substitutePlaceholdersJSON sub (J.Array j) = J.Array $
  fmap (substitutePlaceholdersJSON sub) j
substitutePlaceholdersJSON _ j = j

collectPlaceholdersText :: T.Text -> [T.Text]
collectPlaceholdersText = collectPlaceholders . parsePlaceholders

collectPlaceholdersJSON :: J.Value -> [T.Text]
collectPlaceholdersJSON (J.String s) = collectPlaceholdersText s
collectPlaceholdersJSON (J.Object j) = JM.foldMapWithKey
  (\k v -> collectPlaceholdersText (JK.toText k) <> collectPlaceholdersJSON v) j
collectPlaceholdersJSON (J.Array j) = foldMap collectPlaceholdersJSON j
collectPlaceholdersJSON _ = mempty
