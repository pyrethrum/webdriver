module WebDriverPreCore.Internal.AesonUtils
  ( aesonTypeError,
    aesonTypeErrorMessage,
    asObject,
    asText,
    bodyText',
    emptyObj,
    fromJSONCamelCase,
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
    objToText,
    parseObject,
    parseObjectMaybe,
    parseOpt,
    prettyJSON,
    parseJson,
    parseObjectEither,
    toJSONOmitNothing,
    parseJSONOmitNothing,
    addProps,
    objToString,
    parseThrow,
  )
where

-- \| Utility functions for working with Aeson (JSON) values.
import Control.Exception (Exception (displayException), SomeException, try)
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
    object,
    (.:?),
  )
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser, parse, parseEither, parseMaybe)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Function ((&))
import Data.Set qualified as S
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic, Rep)
import Prelude hiding (lookup)




toJSONOmitNothing :: (Generic a, A.GToJSON' Value A.Zero (Rep a)) => a -> Value
toJSONOmitNothing = A.genericToJSON defaultOptions {omitNothingFields = True}

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

parseThrow :: (FromJSON a, MonadFail m) => Text -> Value -> m a
parseThrow errMsg val =
  parseEither parseJSON val
    & either
      ( \err ->
          fail . unpack $
            errMsg
              <> "\n"
              <> "Parser error was: "
              <> "\n"
              <> pack err
              <> "\n"
              <> "The actual JSON value was: "
              <> jsonToText val
      )
      pure

parseObjectEither :: (FromJSON a) => Object -> Either Text a
parseObjectEither = first pack . parseEither parseJSON . Object

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

asObject :: (ToJSON a) => Text -> a -> Parser A.Object
asObject errMsg val =
  let val' = A.toJSON val
   in case val' of
        Object obj -> pure obj
        _ ->
          fail . unpack $
            errMsg
              <> "\n"
              <> "JSON Value must be of JSON type: Object"
              <> "\n"
              <> "The actual JSON type was: "
              <> aesonConstructorName val'
              <> "\n"
              <> "The actual JSON value was: "
              <> jsonToText val'

objectOrThrow :: (ToJSON a) => Text -> a -> A.Object
objectOrThrow errMsg val =
  case result of
    Error e -> error . unpack $ errMsg <> "\n" <> "Error was: " <> pack e
    Success o -> o
  where
    result = parse (asObject errMsg) $ A.toJSON val

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

fromJSONCamelCase :: (Generic a, A.GFromJSON A.Zero (Rep a)) => Value -> Parser a
fromJSONCamelCase = A.genericParseJSON lwrFirstOptions

-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
lsbToText :: LBS.ByteString -> Text
lsbToText = decodeUtf8 . LBS.toStrict

objToString :: Object -> String
objToString = unpack . objToText

objToText :: Object -> Text
objToText = jsonToText . Object

jsonToText :: Value -> Text
jsonToText = lsbToText . encodePretty

prettyJSON :: Text -> Value -> IO Text
prettyJSON msg v = do
  r <- try @SomeException @_ (pure (jsonToText v))
  let jTxt = either (pack . displayException) id r
  pure $ msg <> "\n" <> jTxt


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
subtractProps keys obj = AKM.filterWithKey (\k _ -> k `S.notMember` excludeSet) obj
  where
    excludeSet = S.fromList $ fromString . unpack <$> keys

addProps :: Text -> [Pair] -> Value -> Value
addProps errMsg ps v =
  object $ ps <> KeyMap.toList (objectOrThrow errMsg v)
