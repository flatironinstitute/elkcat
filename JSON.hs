module JSON where

import           Control.Monad ((<=<), unless)
import           Control.Monad.State (StateT(..), evalStateT, get)
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as J
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

parseReader :: TR.Reader a -> J.Value -> J.Parser a
parseReader r = J.withText "reader" $ either fail return . (check <=< r) where
  check (x, t)
    | T.null t = Right x
    | otherwise = Left $ "unhandled input: " ++ show t

-- |A 'J.Parser' that keeps track of the object being parsed
type ObjectParser = StateT J.Object J.Parser

-- |Fail if there are any remaining fields.
checkUnparsedFields :: String -> ObjectParser ()
checkUnparsedFields n = do
  o <- get
  unless (JM.null o) $ fail $ "Unexpected fields in " ++ n ++ ": " ++ show (JM.keys o)

parseObject :: ObjectParser a -> J.Object -> J.Parser a
parseObject = evalStateT

-- |Apply 'J.withObject' to an 'ObjectParser', ignoring unsparsed fields.
withObjectParser_ :: String -> ObjectParser a -> J.Value -> J.Parser a
withObjectParser_ n = J.withObject n . parseObject

-- |Apply 'J.withObject' to an 'ObjectParser', failing if there are any remaining fields at the end.
withObjectParser :: String -> ObjectParser a -> J.Value -> J.Parser a
withObjectParser n p = withObjectParser_ n $ do
  r <- p
  checkUnparsedFields n
  return r

-- |Apply a field parser (like 'J..:' or 'J..:?') to a field of the current object, and remove that field.
parseFieldWith :: (J.Object -> J.Key -> J.Parser a) -> J.Key -> ObjectParser a
parseFieldWith op k = StateT $ \o -> do
  a <- o `op` k
  return (a, JM.delete k o)

-- |Consuming version of 'J.parseField' or 'J..:'
parseField :: J.FromJSON a => J.Key -> ObjectParser a
parseField = parseFieldWith (J..:)

-- |Consuming version of 'J.parseFieldMaybe' or 'J..:?'
parseFieldMaybe :: J.FromJSON a => J.Key -> ObjectParser (Maybe a)
parseFieldMaybe = parseFieldWith (J..:?)

-- |Consuming version of 'J.parseFieldMaybe'' or 'J..:!'
parseFieldMaybe' :: J.FromJSON a => J.Key -> ObjectParser (Maybe a)
parseFieldMaybe' = parseFieldWith (J..:!)

-- |Consuming version of 'J.explicitParseField'
explicitParseField :: (J.Value -> J.Parser a) -> J.Key -> ObjectParser a
explicitParseField p = parseFieldWith (J.explicitParseField p)

-- |Consuming version of 'J.explicitParseFieldMaybe'
explicitParseFieldMaybe :: (J.Value -> J.Parser a) -> J.Key -> ObjectParser (Maybe a)
explicitParseFieldMaybe p = parseFieldWith (J.explicitParseFieldMaybe p)

-- |Consuming version of 'J.explicitParseFieldMaybe''
explicitParseFieldMaybe' :: (J.Value -> J.Parser a) -> J.Key -> ObjectParser (Maybe a)
explicitParseFieldMaybe' p = parseFieldWith (J.explicitParseFieldMaybe' p)

-- |Generic version of 'J..!='.
(.!=) :: Functor p => p (Maybe a) -> a -> p a
pmval .!= val = fromMaybe val <$> pmval

-- |Use 'withObjectParser' as an 'explicitParseField'.
parseSubObject :: J.Key -> ObjectParser a -> ObjectParser a
parseSubObject k p = explicitParseField (withObjectParser (JK.toString k) p) k
