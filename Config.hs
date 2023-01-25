{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , Formatter(..)
  , FieldFormat(..)
  , Format
  ) where

import           Control.Arrow ((***))
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import           Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import           Network.HTTP.Types.Header (hContentType)

import Placeholder
import JSON
import Query
import Args

data Formatter
  = FormatNone
  | FormatES T.Text -- ^ES format, e.g., date format like "strict_date_optional_time"
  | FormatDate String -- ^formatTime percent format (implies FormatES date)
  | FormatWidth Int -- ^space pad left (positive) or right (negative) to given width

data FieldFormat = FieldFormat
  { formatField :: !T.Text
  , fieldFormatter :: !Formatter
  }

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

data Config = Config
  { confRequest :: HTTP.Request
  , confIndex :: String
  , confSize :: Word
  , confFormat :: Format
  , confDefault :: ParamQuery
  , confOpts :: [Option]
  , confArgs :: Argument
  , confArgLabel :: String
  , confArgHelp :: String
  , confHelp :: String
  , confDebug :: Bool
  }

data AuthData = AuthData
  { authUsername, authPassword :: T.Text
  }

instance J.FromJSON AuthData where
  parseJSON = withObjectParser "auth data" $
    AuthData <$> parseField "username" <*> parseField "password"

applyAuth :: (BS.ByteString -> BS.ByteString -> a) -> AuthData -> a
applyAuth f a = f (TE.encodeUtf8 $ authUsername a) (TE.encodeUtf8 $ authPassword a)

instance J.FromJSON Config where
  parseJSON = withObjectParser "elkcat config" $ do
    confRequest <- parseSubObject "elasticsearch" $ do
      req <- explicitParseField (J.withText "url" $ 
        either (fail . show) return . HTTP.parseRequest . T.unpack) "url"
      basic <- parseFieldMaybe "basic-auth"
      proxy <- parseFieldMaybe "proxy-auth"
      bearer <- parseFieldMaybe "bearer-auth"
      headers <- parseFieldMaybe "headers" .!= []
      return $
          maybe id (HTTP.applyBearerAuth . TE.encodeUtf8) bearer
        $ maybe id (applyAuth HTTP.applyBasicProxyAuth) proxy
        $ maybe id (applyAuth HTTP.applyBasicAuth) basic
        $ req
          { HTTP.requestHeaders =
              [ (hContentType, "application/json") 
              ] ++ map (CI.mk . TE.encodeUtf8 *** TE.encodeUtf8) headers
          , HTTP.responseTimeout = HTTP.responseTimeoutNone
          }
    confIndex <- parseField "index"
    confSize <- parseFieldMaybe' "size" .!= 1000
    confFormat <- parseField "format"
    macros <- parseFieldMaybe "macros" .!= JM.empty
    confDefault <- (<> def) <$> parseFieldMaybe "default" .!= mempty
    confOpts <- explicitParseField (J.listParser $ parseOption macros) "opts"
    (confArgs, confArgLabel, confArgHelp) <- explicitParseField (parseArgument macros) "args"
    confHelp <- parseFieldMaybe "help" .!= ""
    confDebug <- parseFieldMaybe "debug" .!= False
    return Config{..}

