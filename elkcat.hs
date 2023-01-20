{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (when, unless)
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Text as JT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.URI as URI
import qualified System.Console.GetOpt as Opt
import           System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist, copyFile, setPermissions, setOwnerReadable, setOwnerWritable, emptyPermissions)
import           System.Environment (getProgName, getArgs, lookupEnv)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import Paths_elkcat (getDataFileName)
import Placeholder
import Args
import Config

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

formatFields :: Format -> [JE.Encoding]
formatFields = map pf . collectPlaceholders where
  pf (FieldFormat n o _) = maybe
    (JE.text n)
    (JE.pairs . ("field" J..= n <>) . ("format" J..=)) o

justify :: Int -> T.Text -> T.Text
justify w = case compare w 0 of
  EQ -> id
  GT -> T.justifyLeft w ' '
  LT -> T.justifyRight (negate w) ' '

fieldFormat :: Doc -> FieldFormat -> T.Text
fieldFormat d (FieldFormat n _ w) = justify w $ getf n where
  getf "_id" = docId d
  getf f = foldMap fmt $ JM.lookup (JK.fromText f) (docFields d)
  fmt (J.String s) = s
  fmt J.Null = T.empty
  fmt (J.Array a) = T.intercalate ";" $ map fmt $ V.toList a
  fmt j = TL.toStrict $ JT.encodeToLazyText j

formatMessage :: Format -> Doc -> T.Text
formatMessage fmt doc = substitutePlaceholders (fieldFormat doc) fmt

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

  let (opts, _, errs) = Opt.getOpt (Opt.ReturnInOrder confArgs) confOpts args'
  qore <- runArgs opts
  Query{..} <- case (errs, qore) of
    ([], Right q) -> return (q <> confDefault)
    (err, qerr) -> do
      mapM_ (hPutStrLn stderr) (either (++) (\_ -> id) qerr err)
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTIONS]\n") confOpts
      exitFailure

  let fmt = formatMessage confFormat
      req =
           "track_total_hits" J..= False
        <> "sort" J..= querySort
        <> "_source" J..= False
        <> "fields" `JE.pair` JE.list id (formatFields confFormat)
        <> "query" `JE.pair` (JE.pairs
          $  "bool" `JE.pair` (JE.pairs
            $  "filter" J..= queryFilter))
  when confDebug $ BSLC.putStrLn $ BSB.toLazyByteString $ J.fromEncoding $ JE.pairs req
  -- exitFailure

  let loop :: Maybe Word -> Maybe J.Array -> IO ()
      loop (Just 0) _ = return ()
      loop count sa = do
        let size = maybe id min count confSize
        r <- HTTP.httpJSON $ searchRequest config (JE.pairs $ req
          <> "size" J..= size
          <> foldMap ("search_after" J..=) sa)
        mapM_ (BSC.hPutStrLn stderr) $ HTTP.getResponseHeader "warning" r
        let Response{..} = HTTP.getResponseBody r
            n = fromIntegral $ V.length responseHits
        V.mapM_ (TIO.putStrLn . fmt) responseHits
        unless (n < size) $
          loop (subtract n <$> count) $ Just $ docSort $ V.last responseHits

  loop queryCount Nothing
