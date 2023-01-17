{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , Field(..)
  , FieldFormat(..)
  , Format
  ) where

import           Control.Arrow ((***))
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import           Network.HTTP.Types.Header (hContentType)

import Placeholder
import Args

newtype Field = Field{ fieldName :: T.Text }
  deriving Show

instance J.FromJSON Field where
  parseJSON = J.withText "field name" $ return . Field

data FieldFormat = FieldFormat
  { formatField :: !Field
  , formatFieldFormat :: Maybe T.Text -- ES format, e.g., date format like "strict_date_optional_time"
  , formatFieldWidth :: Int -- space pad left (positive) or right (negative) to given width
  }
  deriving Show

type Format = Placeholders FieldFormat

parseFieldFormat :: T.Text -> FieldFormat
parseFieldFormat a
  | T.null o = f
  | Right (w, "") <- TR.signed TR.decimal o' = f{ formatFieldWidth = w }
  | otherwise = f{ formatFieldFormat = Just o' }
  where
  f = FieldFormat (Field n) Nothing 0
  (n,o) = T.breakOn ":" a
  o' = T.tail o

instance J.FromJSON FieldFormat where
  parseJSON = J.withText "field format" $ return . parseFieldFormat

data Config = Config
  { confRequest :: HTTP.Request
  , confIndex :: String
  , confTimestamp :: Field
  , confSize :: Word
  , confFormat :: Format
  , confOpts :: [Option]
  , confArgs :: Argument
  }

data AuthData = AuthData
  { authUsername, authPassword :: T.Text
  }

instance J.FromJSON AuthData where
  parseJSON = J.withObject "auth data" $ \a ->
    AuthData <$> a J..: "username" <*> a J..: "password"

applyAuth :: (BS.ByteString -> BS.ByteString -> a) -> AuthData -> a
applyAuth f a = f (TE.encodeUtf8 $ authUsername a) (TE.encodeUtf8 $ authPassword a)

instance J.FromJSON Config where
  parseJSON = J.withObject "elkcat config" $ \c -> do
    es <- c J..:? "elasticsearch" J..!= JM.empty
    url <- es J..: "url"
    req <- either (J.parserThrowError [J.Key "url"] . show) return $ HTTP.parseRequestThrow url
    basic <- es J..:? "basic-auth"
    proxy <- es J..:? "proxy-auth"
    bearer <- es J..:? "bearer-auth"
    headers <- es J..:? "headers" J..!= []
    let confRequest =
            maybe id (HTTP.applyBearerAuth . TE.encodeUtf8) bearer
          $ maybe id (applyAuth HTTP.applyBasicProxyAuth) proxy
          $ maybe id (applyAuth HTTP.applyBasicAuth) basic
          $ req
            { HTTP.requestHeaders =
                [ (hContentType, "application/json") 
                ] ++ map (CI.mk . TE.encodeUtf8 *** TE.encodeUtf8) headers
            , HTTP.responseTimeout = HTTP.responseTimeoutNone
            }
    confIndex <- c J..: "index"
    confTimestamp <- c J..: "timestamp"
    confSize <- c J..:! "size" J..!= 1000
    confFormat <- c J..: "format"
    confOpts <- c J..: "opts"
    confArgs <- c J..: "args"
    return Config{..}

