module WebDriverPreCore.BiDi.ResponseEvent where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent)
import WebDriverPreCore.BiDi.Command (Command, commandValue)
import WebDriverPreCore.BiDi.CoreTypes (EmptyResult (..), JSUInt)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.Internal.AesonUtils (jsonToText, parseObjectEither, subtractProps)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude

parseResponse :: (FromJSON r) => JSUInt -> Either JSONEncodeError ResponseObject -> Either ResponseError (Maybe (MatchedResponse r))
parseResponse id' response = first EncodeError response >>= matchResponseObject id'

matchResponseObject :: forall a. (FromJSON a) => JSUInt -> ResponseObject -> Either ResponseError (Maybe (MatchedResponse a))
matchResponseObject msgId = \case
  NoID {} -> Right Nothing
  WithID id' obj ->
    bool
      (Right Nothing)
      matchedResult
      (id' == msgId)
    where
      success :: Either Text (Success a)
      success = parseObjectEither obj

      matchedResult :: Either ResponseError (Maybe (MatchedResponse a))
      matchedResult =
        success
          & either
            (\e -> Left $ ParseError {object = obj, error = e})
            (\s -> Right . Just $ MkMatchedResponse {response = s.result, object = obj})

decodeResponse :: ByteString -> Either JSONEncodeError ResponseObject
decodeResponse =
  (=<<) parseResponseObj . packLeft . eitherDecode
  where
    packLeft = first (MkJSONEncodeError . pack)

    parseResponseObj :: Object -> Either JSONEncodeError ResponseObject
    parseResponseObj =
      packLeft . parseEither (\o' -> maybe NoID WithID <$> o' .:? "id" <*> pure o')

data MatchedResponse a = MkMatchedResponse
  { response :: a,
    object :: Object
  }
  deriving (Show, Generic)

data ResponseObject
  = NoID {object :: Object}
  | WithID {id :: JSUInt, object :: Object}
  deriving (Show, Generic)

newtype JSONEncodeError = MkJSONEncodeError Text deriving (Show, Eq, Generic)

data ResponseError
  = BiDIError Error
  | EncodeError JSONEncodeError
  | ParseError
      { object :: Object,
        error :: Text
      }
  | BiDiTimeoutError {ms :: Int}
  deriving (Show, Eq, Generic)

displayResponseError :: (Show c) => Command c r -> Value -> ResponseError -> Text
displayResponseError c json err =
  "Error executing BiDi command: "
    <> (txt c)
    <> "\n"
    <> "With JSON: \n"
    <> (jsonToText json)
    <> "\n"
    <> case err of
      BiDIError e -> "BiDi driver error: \n" <> txt e
      EncodeError (MkJSONEncodeError e) -> "Failed to encode driver response to JSON: \n" <> txt e
      ParseError {object, error = e} ->
        "Failed to decode the 'result' property of JSON returned by driver to response type: \n"
          <> jsonToText (Object object)
          <> "\nError message: \n"
          <> e
      BiDiTimeoutError {ms} -> "Timed out waiting for matching command response from driver (" <> txt ms <> "milliseconds)"

data Success a = MkSuccess
  { id :: JSUInt,
    result :: a,
    extensions :: EmptyResult
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Success a) where
  parseJSON :: Value -> Parser (Success a)
  parseJSON = withObject "Success" $ \o -> do
    id' <- o .: "id"
    result <- o .: "result"
    pure $
      MkSuccess
        { id = id',
          result,
          extensions = MkEmptyResult $ subtractProps ["id", "result"] o
        }

-- typ :: Text, -- "error"
data Error = MkError
  { id :: Maybe JSUInt,
    error :: ErrorCode,
    message :: Text,
    stacktrace :: Maybe Text,
    extensions :: EmptyResult
  }
  deriving (Show, Generic, Eq)

instance FromJSON Error where
  parseJSON :: Value -> Parser Error
  parseJSON = withObject "Error" $ \o -> do
    id' <- o .: "id"
    error' <- o .: "error"
    message <- o .: "message"
    stacktrace <- o .: "stacktrace"
    pure $
      MkError
        { id = id',
          error = error',
          message,
          stacktrace,
          extensions =
            MkEmptyResult $
              subtractProps
                [ "id",
                  "error",
                  "message",
                  "stacktrace"
                ]
                o
        }

-- typ :: Text, -- "event"
data Event = MkEvent
  { eventData :: EventData,
    extensions :: EmptyResult
  }
  deriving (Show, Generic)

data EventData
  = BrowsingContextEvent BrowsingContextEvent
  | -- | InputEvent InputEvent
    InputEvent FileDialogOpened
  | -- method: "log.entryAdded"
    LogEvent Entry
  deriving
    ( Show,
      Generic
    )

-- instance FromJSON ResultData where
--   parseJSON :: Value -> Parser ResultData
--   parseJSON = withObject "ResultData" $ \o -> do
--     typ <- o .: "typ"
--     case typ of
--       "browsingContext" -> BrowsingContextResult <$> parseObject o
--       "browser" -> BrowserResult <$> parseObject o
--       "emulation" -> EmulationResult <$> parseObject o
--       "input" -> InputResult <$> parseObject o
--       "network" -> NetworkResult <$> parseObject o
--       "script" -> ScriptResult <$> parseObject o
--       "session" -> SessionResult <$> parseObject o
--       "storage" -> StorageResult <$> parseObject o
--       "webExtension" -> WebExtensionResult <$> parseObject o
--       _ -> error $ "Unknown ResultData type: " <> typ <> " in " <> show o
