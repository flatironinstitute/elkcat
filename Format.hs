{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Format
  ( Format
  , formatFields
  , Doc(..)
  , formatMessage
  ) where

import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Text as JT
import qualified Data.Aeson.Types as J
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Time.LocalTime (utcToLocalZonedTime)
import qualified Data.Vector as V
import qualified System.IO.Unsafe as Unsafe (unsafeDupablePerformIO)

import Placeholder

data Formatter
  = FormatNone
  | FormatES T.Text -- ^ES format, e.g., date format like "strict_date_optional_time"
  | FormatDate String -- ^formatTime percent format (implies FormatES date)
  | FormatWidth Int -- ^space pad left (positive) or right (negative) to given width

data FieldFormat = FieldFormat !T.Text !Formatter

type Format = Placeholders FieldFormat

parseFieldFormat :: T.Text -> FieldFormat
parseFieldFormat a
  | T.null o = f FormatNone
  | T.isPrefixOf "%" o' = f $ FormatDate (T.unpack o')
  | Right (w, "") <- TR.signed TR.decimal o' = f $ FormatWidth w
  | otherwise = f $ FormatES o'
  where
  f = FieldFormat n
  (n,o) = T.breakOn ":" a
  o' = T.tail o

instance J.FromJSON FieldFormat where
  parseJSON = J.withText "field format" $ return . parseFieldFormat

data Doc = Doc
  { docId :: T.Text
  , docFields :: J.Object
  , docSort :: J.Array
  } deriving (Show)

instance J.FromJSON Doc where
  parseJSON = J.withObject "document" $ \d -> Doc
    <$> d J..: "_id"
    <*> d J..:! "fields" J..!= JM.empty
    <*> d J..: "sort"

instance J.ToJSON Doc where
  toJSON Doc{..} = J.object
    [ "_id" J..= docId
    , "fields" J..= docFields
    , "sort" J..= docSort
    ]
  toEncoding Doc{..} = J.pairs
    $ "_id" J..= docId
    <> "fields" J..= docFields
    <> "sort" J..= docSort

formatFields :: Format -> [JE.Encoding]
formatFields = map pf . collectPlaceholders where
  pf (FieldFormat n f) = maybe
    (JE.text n)
    (JE.pairs . ("field" J..= n <>) . ("format" J..=))
    $ case f of
      FormatES s -> Just s
      FormatDate _ -> Just "epoch_millis"
      _ -> Nothing

fieldFormat :: Doc -> FieldFormat -> T.Text
fieldFormat d (FieldFormat n fw) = justify fw $ getf n where
  getf "_id" = docId d
  getf f = foldMap (fmt fw) $ JM.lookup (JK.fromText f) (docFields d)
  fmt (FormatDate fd) (J.Number e) = date fd (realToFrac e)
  fmt (FormatDate fd) (J.String (TR.rational -> Right (e, _))) = date fd e
  fmt _ (J.String s) = s
  fmt _ J.Null = T.empty
  fmt _ (J.Array a) = T.intercalate ";" $ map (fmt fw) $ V.toList a
  fmt _ j = TL.toStrict $ JT.encodeToLazyText j
  date fd e = T.pack $ formatTime defaultTimeLocale fd
    $ Unsafe.unsafeDupablePerformIO . utcToLocalZonedTime
    $ posixSecondsToUTCTime $ secondsToNominalDiffTime (e / 1e3)
  justify (FormatWidth w)
    | w < 0 = T.justifyRight (negate w) ' '
    | otherwise = T.justifyLeft w ' '
  justify _ = id

formatMessage :: Format -> Doc -> T.Text
formatMessage fmt doc = substitutePlaceholders (fieldFormat doc) fmt
