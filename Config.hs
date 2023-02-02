{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  ) where

import           Control.Arrow ((***))
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import           Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client.Conduit as HTTP
import           Network.HTTP.Types.Header (hContentType)

import JSON
import Query
import Args

data Config = Config
  { confRequest :: HTTP.Request
  , confSize :: Word
  , confDefault :: ParamQuery
  , confOpts :: [Option]
  , confArgs :: Argument
  , confArgLabel :: String
  , confArgHelp :: String
  , confHelp :: String
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
    confSize <- parseFieldMaybe' "size" .!= 1000
    macros <- parseFieldMaybe "macros" .!= JM.empty
    d0 <- parseObject
    d1 <- parseSubObjectMaybe "default" parseObject .!= mempty -- backwards compatibility
    let confDefault = d1 <> d0 <> def
    confOpts <- explicitParseField (J.listParser $ parseOption macros) "opts"
    (confArgs, confArgLabel, confArgHelp) <- explicitParseField (parseArgument macros) "args"
    confHelp <- parseFieldMaybe "help" .!= ""
    return Config{..}

