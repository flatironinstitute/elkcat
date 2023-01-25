{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Monad (when, unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Text as JT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Function (on)
import           Data.List (nubBy)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Time.LocalTime (utcToLocalZonedTime)
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import           Network.HTTP.Types (statusCode, statusMessage, statusIsSuccessful)
import qualified Network.URI as URI
import qualified System.Console.GetOpt as Opt
import           System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist, copyFile, setPermissions, setOwnerReadable, setOwnerWritable, emptyPermissions)
import           System.Environment (getProgName, getArgs, lookupEnv)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import qualified System.IO.Unsafe as Unsafe (unsafeDupablePerformIO)

import Paths_elkcat (getDataFileName)
import Placeholder
import Query
import Args
import Config

data Doc = Doc
  { docId :: T.Text
  , docFields :: J.Object
  , docSort :: J.Array
  } deriving (Show)

instance J.FromJSON Doc where
  parseJSON = J.withObject "document" $ \d -> Doc
    <$> d J..: "_id"
    <*> d J..: "fields"
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

-- |which field this sort term sorts on (invalid for invalid sort terms)
querySortKey :: J.Value -> T.Text
querySortKey (J.String s) = s
querySortKey (J.Object m) = foldMap JK.toText $ JM.keys m
querySortKey _ = T.empty

main :: IO ()
main = do
  conffile <- maybe (getXdgDirectory XdgConfig "elkcat.yaml") return =<< lookupEnv "ELKCAT"
  isconf <- doesFileExist conffile
  unless isconf $ do
    hPutStrLn stderr $ "Copying default config file to " ++ conffile
    src <- getDataFileName "elkcat.yaml"
    copyFile src conffile
    setPermissions conffile (setOwnerReadable True $ setOwnerWritable True emptyPermissions)
  Config{..} <- Y.decodeFileThrow conffile

  prog <- getProgName
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

  let es api args = do
        let body = JE.encodingToLazyByteString $ JE.pairs args
        when confDebug $ BSLC.hPutStrLn stderr body
        res <- HTTP.httpJSON $ confRequest
          { HTTP.path = HTTP.path confRequest <> BS.intercalate "/" (map (BSC.pack . URI.escapeURIString URI.isUnescapedInURIComponent)
            [confIndex, api])
          , HTTP.requestBody = HTTP.RequestBodyLBS body
          }
        mapM_ (BSC.hPutStrLn stderr) $ HTTP.getResponseHeader "warning" res
        let s = HTTP.responseStatus res
            ok = statusIsSuccessful s
            j = HTTP.responseBody res
            p = J.fromJSON j
        case p of
          J.Success r | statusIsSuccessful s -> do
            when confDebug $ BSLC.hPutStrLn stderr $ J.encode j
            return r
          _ -> do
            if ok
              then hPutStrLn stderr $ "Parse error: " <> show p
              else BSC.hPutStrLn stderr  $ "Error: " <> BSC.pack (show (statusCode s)) <> " " <> statusMessage s
            BSLC.hPutStrLn stderr $ "Request: " <> body
            BSLC.hPutStrLn stderr $ "Response: " <> J.encode j
            exitFailure

      fmt = formatMessage confFormat

      stream (Just 0) _ = return ()
      stream count sa = do
        QueryResponse{..} <- es "_search" $
             "track_total_hits" J..= False
          <> "sort" J..= nubBy ((==) `on` querySortKey) paramSort
          <> "_source" J..= False
          <> "fields" `JE.pair` JE.list id (formatFields confFormat)
          <> "query" J..= q
          <> "size" J..= size
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
