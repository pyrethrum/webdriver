module WebDriverPreCore.Internal.AesonUtils
  ( aesonTypeError,
    aesonTypeErrorMessage,
    asText,
    bodyText',
    emptyObj,
    lookup,
    lookupTxt,
    nonEmpty,
    opt,
    empty,
    subtractProps,
    jsonPrettyString,
    jsonToText,
    objectOrThrow,
    parseObject,
    parseObjectMaybe,
    parseOpt,
    prettyPrintJson,
    parseJson,
  )
where

-- \| Utility functions for working with Aeson (JSON) values.
import Control.Exception (Exception (displayException), SomeException, try)
import Data.Aeson
  ( FromJSON (..),
    Key,
    KeyValue ((.=)),
    Result (..),
    ToJSON (),
    Value (..),
    eitherDecodeStrict, (.:?),
    Object
  )
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (Either, either)
import Data.Function (($), (&), (.))
import Data.Functor (Functor, (<$>))
import Data.String (String)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as T
import GHC.Base (IO)
import GHC.Show (Show (..))
import System.IO (print)
import Prelude
  ( Maybe (..),
    maybe,
    pure,
    (<>),
    (>>=),
    Bool (..), Foldable (..), not, error
  )
import Control.Monad (MonadFail(..))
import Data.Aeson.KeyMap (member)
import Data.Set qualified as S
import Data.Aeson.Key (fromString)

-- Aeson stuff
-- TODO move to separte library

asText :: Value -> Result Text
asText = \case
  String t -> Success t
  v -> aesonTypeError "Text" v

opt :: (Functor f, KeyValue e b, ToJSON a) => Key -> f a -> f b
opt lbl mb = (lbl .=) <$> mb

parseObject :: Text -> Value -> Parser A.Object
parseObject errMsg val = case val of
  Object obj -> pure obj
  _ -> fail $ unpack errMsg

parseObjectMaybe :: FromJSON a => Object -> Maybe a
parseObjectMaybe = parseMaybe parseJSON . Object

objectOrThrow :: ToJSON a => Text -> a -> A.Object
objectOrThrow errMsg val = 
  case A.toJSON val of
    Object obj -> obj
    _ -> error $ unpack errMsg


-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
lsbToText :: LBS.ByteString -> Text
lsbToText = decodeUtf8 . LBS.toStrict

jsonToText :: Value -> Text
jsonToText = lsbToText . encodePretty

prettyPrintJson :: Value -> IO ()
prettyPrintJson v = do
  e <- (try @SomeException @_) $ T.putStrLn (jsonToText v)
  either (print . displayException) print e

parseJson :: Text -> Either String Value
parseJson input =
  eitherDecodeStrict (encodeUtf8 input)

bodyText' :: Result Value -> Key -> Result Text
bodyText' v k = v >>= lookupTxt k

aesonTypeErrorMessage :: Text -> Value -> Text
aesonTypeErrorMessage t v = "Expected JSON Value to be of type: " <> t <> "\nbut got:\n" <> jsonToText v

aesonTypeError :: Text -> Value -> Result a
aesonTypeError t v = Error . unpack $ aesonTypeErrorMessage t v

jsonPrettyString :: Value -> String
jsonPrettyString = unpack . jsonToText

lookupTxt :: Key -> Value -> Result Text
lookupTxt k v = lookup k v >>= asText

lookup :: Key -> Value -> Result Value
lookup k v =
  v & \case
    Object o -> AKM.lookup k o & maybe (Error ("the key: " <> show k <> "does not exist in the object:\n" <> jsonPrettyString v)) pure
    _ -> aesonTypeError "Object" v

nonEmpty :: Value -> Bool
nonEmpty = not . empty


emptyObj :: Value
emptyObj = Object AKM.empty

empty :: Value -> Bool
empty = \case
  Object o -> AKM.null o
  Array a -> null a
  String t -> T.null t 
  Null -> True
  Bool _ -> False
  Number _ -> False

forceNonEmpty :: Value -> Maybe Value
forceNonEmpty v = if empty v
  then Nothing
  else Just v

parseOpt :: forall a. FromJSON a => A.Object -> Key -> Parser (Maybe a)
parseOpt o k = do 
  mv <- o .:? k
  case mv >>= forceNonEmpty of
    Nothing -> pure Nothing
    Just v -> Just <$> A.parseJSON v

subtractProps :: [Text] -> Object -> Object
subtractProps keys obj = AKM.filterWithKey (\k _ -> k `S.member` keySet) obj
  where 
    keySet = S.fromList $ fromString . unpack <$> keys




