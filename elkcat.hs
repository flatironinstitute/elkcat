{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Monad (when, unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Function (on)
import           Data.List (nubBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Simple as HTTP
import           Network.HTTP.Types (statusCode, statusMessage, statusIsSuccessful)
import qualified Network.URI as URI
import qualified System.Console.GetOpt as Opt
import           System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesDirectoryExist, doesFileExist, copyFile, setPermissions, setOwnerReadable, setOwnerWritable, emptyPermissions)
import           System.Environment (getProgName, getArgs, lookupEnv)
import           System.Exit (exitFailure)
import           System.FilePath ((</>), (<.>), takeBaseName)
import           System.IO (hPutStrLn, stderr)

import Paths_elkcat (getDataFileName)
import Format
import Query
import Args
import Config

data QueryResponse = QueryResponse
  { responseHits :: V.Vector Doc
  } deriving (Show)

instance J.FromJSON QueryResponse where
  parseJSON = J.withObject "query response" $ \r -> QueryResponse
    <$> (r J..: "hits" >>= (J..: "hits"))

data CountResponse = CountResponse
  { responseCount :: Integer
  } deriving (Show)

instance J.FromJSON CountResponse where
  parseJSON = J.withObject "count response" $ \r -> CountResponse
    <$> (r J..: "count")

-- |which field this sort term sorts on (invalid for invalid sort terms)
querySortKey :: J.Value -> T.Text
querySortKey (J.String s) = s
querySortKey (J.Object m) = foldMap JK.toText $ JM.keys m
querySortKey _ = T.empty

main :: IO ()
main = do
  prog <- getProgName

  confpath <- maybe (getXdgDirectory XdgConfig "") return =<< lookupEnv "ELKCAT"
  isdir <- doesDirectoryExist confpath
  let conffile
        | isdir = confpath </> (if null base then "elkcat" else base) <.> "yaml"
        | otherwise = confpath
        where base = takeBaseName prog
  isconf <- doesFileExist conffile
  unless isconf $ do
    hPutStrLn stderr $ "Copying default config file to " ++ conffile
    src <- getDataFileName "elkcat.yaml"
    copyFile src conffile
    setPermissions conffile (setOwnerReadable True $ setOwnerWritable True emptyPermissions)
  Config{..} <- Y.decodeFileThrow conffile
  HTTPS.setGlobalManager =<< confManager

  args' <- getArgs

  let (opts, ~[], errs) = Opt.getOpt (Opt.ReturnInOrder confArgs) confOpts args'
  qore <- runArgs opts
  ParamQuery Param{..} q <- case (errs, qore) of
    ([], Right q) -> return (q <> confDefault)
    (err, qerr) -> do
      mapM_ (hPutStrLn stderr) (either (++) (\_ -> id) qerr err)
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " OPTION|" ++ confArgLabel ++ " ...") confOpts
        ++ "  " ++ confArgLabel ++ replicate (14-length confArgLabel) ' ' ++ confArgHelp
        ++ confHelp
      exitFailure

  index <- maybe (fail "index must be set in config") return paramIndex
  format <- maybe (fail "format must be set in config") return paramFormat

  let
    es api args = do
      let body = JE.encodingToLazyByteString $ JE.pairs args
      when paramDebug $ BSLC.hPutStrLn stderr body
      res <- HTTP.httpJSON $ confRequest
        { HTTP.path = HTTP.path confRequest <> BS.intercalate "/" (map (BSC.pack . URI.escapeURIString URI.isUnescapedInURIComponent)
          [index, api])
        , HTTP.requestBody = HTTP.RequestBodyLBS body
        }
      mapM_ (BSC.hPutStrLn stderr) $ HTTP.getResponseHeader "warning" res
      let s = HTTP.responseStatus res
          ok = statusIsSuccessful s
          j = HTTP.responseBody res
          p = J.fromJSON j
      case p of
        J.Success r | statusIsSuccessful s -> do
          when paramDebug $ BSLC.hPutStrLn stderr $ J.encode j
          return r
        _ -> do
          if ok
            then hPutStrLn stderr $ "Parse error: " <> show p
            else BSC.hPutStrLn stderr  $ "Error: " <> BSC.pack (show (statusCode s)) <> " " <> statusMessage s
          BSLC.hPutStrLn stderr $ "Request: " <> body
          BSLC.hPutStrLn stderr $ "Response: " <> J.encode j
          exitFailure

    fmt = formatMessage format

    stream (Just 0) _ = return ()
    stream count sa = do
      QueryResponse{..} <- es "_search" $
           "track_total_hits" J..= False
        <> "sort" J..= nubBy ((==) `on` querySortKey) paramSort
        <> "_source" J..= False
        <> "fields" `JE.pair` JE.list id (formatFields format)
        <> "query" J..= q
        <> "size" J..= size
        -- <> "collapse" `JE.pair` (JE.pairs $ "field" J..= J.String "hostname" <> "inner_hits" `JE.pair` (J.pairs $ "name" J..= J.String "hostname" <> "size" J..= J.Number 0))
        <> foldMap ("search_after" J..=) sa
      let n = fromIntegral $ V.length responseHits
      V.mapM_ (TIO.putStrLn . fmt) responseHits
      unless (n < size) $
        stream (subtract n <$> count) $ Just $ docSort $ V.last responseHits
      where
      size = maybe id min count confSize

  case paramCount of
    CountOnly -> do
      CountResponse{..} <- es "_count" $ "query" J..= q
      print $ responseCount
    CountUnlimited -> stream Nothing Nothing
    CountLimit n   -> stream (Just n) Nothing
