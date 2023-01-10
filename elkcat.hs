{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow ((***))
import           Control.Monad (unless)
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import           Network.HTTP.Types.Header (hContentType)
import qualified Network.URI as URI
import qualified System.Console.GetOpt as Opt
import           System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist, copyFile, setPermissions, setOwnerReadable, setOwnerWritable, emptyPermissions)
import           System.Environment (getProgName, getArgs, lookupEnv)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import Paths_elkcat (getDataFileName)

newtype Field = Field{ fieldName :: T.Text }
  deriving Show

instance J.FromJSON Field where
  parseJSON = J.withText "field name" $ return . Field

data Formatter
  = FormatLiteral T.Text
  | FormatField
    { formatField :: Field
    , formatFieldFormat :: Maybe T.Text -- ES format, e.g., date format like "strict_date_optional_time"
    , formatFieldWidth :: Int -- space pad left (positive) or right (negative) to given width
    }
  deriving Show

type Format = [Formatter]

parseFormat :: T.Text -> Format
parseFormat = parse1 . T.splitOn "{" where
  parse1 [] = []
  parse1 (p:r)
    | T.null p = parse r
    | otherwise = FormatLiteral p : parse r
  parse [] = []
  parse (a:r)
    | T.null s = FormatLiteral ('{' `T.cons` a) : parse r -- unbalanced { error
    | otherwise = parsefield p : parse1 (T.tail s : r)
    where (p,s) = T.breakOn "}" a
  parsefield a
    | T.null o = f
    | Right (w, "") <- TR.signed TR.decimal o' = f{ formatFieldWidth = w }
    | otherwise = f{ formatFieldFormat = Just o' }
    where
    f = FormatField (Field n) Nothing 0
    (n,o) = T.breakOn ":" a
    o' = T.tail o

instance J.FromJSON Formatter where
  parseJSON = J.withText "format string" $ return . FormatLiteral -- not used
  parseJSONList = J.withText "format string" $ return . parseFormat

data Config = Config
  { confRequest :: HTTP.Request
  , confIndex :: String
  , confTimestamp :: Field
  , confSize :: Word
  , confFormat :: Format
  } deriving Show

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
    return Config{..}

data Options = Options
  { 
  }

defOptions :: Options
defOptions = Options
  { 
  }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ {- Opt.Option "h" ["hostname"]
      (Opt.ReqArg (\u o -> o{ optEndpoint = u }) "URL")
      ("connect to elasticsearch at URL [" ++ show (optEndpoint defOptions) ++ "]") -}
  ]

formatFields :: Format -> [JE.Encoding]
formatFields [] = []
formatFields (FormatField (Field n) o _:r) = maybe
  (JE.text n)
  (JE.pairs . ("field" J..= n <>) . ("format" J..=)) o
  : formatFields r
formatFields (_:r) = formatFields r

searchRequest :: Config -> J.Encoding -> HTTP.Request
searchRequest Config{..} body =
  confRequest
    { HTTP.path = HTTP.path confRequest <> BS.intercalate "/" (map (BSC.pack . URI.escapeURIString URI.isUnescapedInURIComponent)
      [confIndex, "_search"])
    , HTTP.requestBody = HTTP.RequestBodyLBS $ JE.encodingToLazyByteString body
    }

data Doc = Doc
  { docId :: T.Text
  , docFields :: J.Object
  , docSort :: J.Array
  }

instance J.FromJSON Doc where
  parseJSON = J.withObject "document" $ \d -> Doc
    <$> d J..: "_id"
    <*> d J..: "fields"
    <*> d J..: "sort"

data Response = Response
  { responseHits :: V.Vector Doc
  }

instance J.FromJSON Response where
  parseJSON = J.withObject "response" $ \r -> Response
    <$> (r J..: "hits" >>= (J..: "hits"))

justify :: Int -> T.Text -> T.Text
justify w = case compare w 0 of
  EQ -> id
  GT -> T.justifyLeft w ' '
  LT -> T.justifyRight (negate w) ' '

formatter :: Formatter -> Doc -> T.Text
formatter (FormatLiteral s) = const s
formatter (FormatField (Field n) _ w) = justify w . getf n where
  getf "_id" d = docId d
  getf f d = foldMap fmt $ JM.lookup (JK.fromText f) (docFields d)
  fmt (J.String s) = s
  fmt (J.Number x) = T.pack $ show x
  fmt (J.Array a) = T.intercalate ";" $ map fmt $ V.toList a
  fmt J.Null = T.empty

formatMessage :: Format -> Doc -> T.Text
formatMessage fmt doc = foldMap (flip formatter doc) fmt

main :: IO ()
main = do
  conffile <- maybe (getXdgDirectory XdgConfig "elkcat.yaml") return =<< lookupEnv "ELKCAT"
  isconf <- doesFileExist conffile
  unless isconf $ do
    hPutStrLn stderr $ "Copying default config file to " ++ conffile
    src <- getDataFileName "elkcat.yaml"
    copyFile src conffile
    setPermissions conffile (setOwnerReadable True $ setOwnerWritable True emptyPermissions)
  config@Config{..} <- Y.decodeFileThrow conffile

  prog <- getProgName
  args' <- getArgs
  (Options{}, args) <- case Opt.getOpt Opt.RequireOrder options args' of
    (o, a, []) -> return (foldl (flip ($)) defOptions o, a)
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTIONS]\n") options
      exitFailure

  let fmt = formatMessage confFormat
  r <- HTTP.httpJSON $ searchRequest config $ JE.pairs 
    $  "track_total_hits" J..= False
    <> "size" J..= confSize
    <> "sort" J..= ["@timestamp", "_doc" :: T.Text]
    <> "_source" J..= False
    <> "fields" `JE.pair` JE.list id (formatFields confFormat)
  mapM_ (BSC.hPutStrLn stderr) $ HTTP.getResponseHeader "warning" r
  let Response{..} = HTTP.getResponseBody r
  V.mapM_ (TIO.putStrLn . fmt) responseHits

  return ()
