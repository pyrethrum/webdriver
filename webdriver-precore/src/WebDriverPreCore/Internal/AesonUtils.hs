module WebDriverPreCore.Internal.AesonUtils
  ( aesonTypeError,
    aesonTypeErrorMessage,
    asText,
    bodyText',
    emptyObj,
    enumCamelCase,
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
    resultToEither,
    parseObjectEither,
    toJSONOmitNothing,
    parseJSONOmitNothing
  )
where

-- \| Utility functions for working with Aeson (JSON) values.
import Control.Exception (Exception (displayException), SomeException, try)
import Control.Monad (MonadFail (..))
import Data.Aeson
  ( FromJSON (..),
    Key,
    KeyValue ((.=)),
    Object,
    Options (..),
    Result (..),
    ToJSON (),
    Value (..),
    defaultOptions,
    eitherDecodeStrict,
    (.:?),
  )
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types (Parser, parse, parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Either (Either, either)
import Data.Function (($), (&), (.))
import Data.Functor (Functor, (<$>))
import Data.Set qualified as S
import Data.String (String)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as T
import GHC.Base (IO)
import GHC.Generics (Generic, Rep)
import GHC.Show (Show (..))
import System.IO (print)
import Prelude
  ( Bool (..),
    Either (..),
    Foldable (..),
    Maybe (..),
    error,
    maybe,
    not,
    pure,
    (<>),
    (>>=),
  )

-- Aeson stuff
-- TODO move to separte library

toJSONOmitNothing :: ( Generic a, A.GToJSON' Value A.Zero (Rep a)) =>a -> Value
toJSONOmitNothing = A.genericToJSON  defaultOptions {omitNothingFields = True}

parseJSONOmitNothing :: (Generic a, A.GFromJSON A.Zero (Rep a)) => Value -> Parser a
parseJSONOmitNothing = A.genericParseJSON defaultOptions {omitNothingFields = True}


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

resultToEither :: forall a. Result a -> Either Text a
resultToEither = \case
  Success a -> Right a
  Error e -> Left $ pack e

parseObjectEither :: (FromJSON a) => Object -> Either Text a
parseObjectEither = resultToEither . parse parseJSON . Object

parseObjectMaybe :: (FromJSON a) => Object -> Maybe a
parseObjectMaybe = parseMaybe parseJSON . Object

aesonConstructorName :: Value -> Text
aesonConstructorName = \case
  Object _ -> "Object"
  Array _ -> "Array"
  String _ -> "String"
  Number _ -> "Number"
  Bool _ -> "Bool"
  Null -> "Null"

objectOrThrow :: (ToJSON a) => Text -> a -> A.Object
objectOrThrow errMsg val =
  let val' = A.toJSON val
   in case val' of
        Object obj -> obj
        _ ->
          error . unpack $
            errMsg
              <> "\n"
              <> "JSON Value must be of JSON type: Object"
              <> "\n"
              <> "The actual JSON type was: "
              <> aesonConstructorName val'
              <> "\n"
              <> "The actual JSON value was: "
              <> jsonToText val'

lowerFirst :: String -> String
lowerFirst = \case
  c : cs -> toLower c : cs
  [] -> []

lwrFirstOptions :: Options
lwrFirstOptions =
  defaultOptions
    { constructorTagModifier = lowerFirst
    }

enumCamelCase :: (Generic a, A.GToJSON' Value A.Zero (Rep a)) => a -> Value
enumCamelCase = A.genericToJSON lwrFirstOptions

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
forceNonEmpty v =
  if empty v
    then Nothing
    else Just v

parseOpt :: forall a. (FromJSON a) => A.Object -> Key -> Parser (Maybe a)
parseOpt o k = do
  mv <- o .:? k
  case mv >>= forceNonEmpty of
    Nothing -> pure Nothing
    Just v -> Just <$> A.parseJSON v

subtractProps :: [Text] -> Object -> Object
subtractProps keys obj = AKM.filterWithKey (\k _ -> k `S.member` keySet) obj
  where
    keySet = S.fromList $ fromString . unpack <$> keys
