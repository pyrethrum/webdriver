module WebDriverPreCore.BiDi.ResponseEvent where

import Data.Aeson (FromJSON (parseJSON), Object, Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Browser (BrowserResult)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent, BrowsingContextResult)
import WebDriverPreCore.BiDi.Command (Command)
import WebDriverPreCore.BiDi.CoreTypes (EmptyResult (..), JSUInt)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogInfo, FileDialogOpened)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Network (NetworkResult (..))
import WebDriverPreCore.BiDi.Script (ScriptResult)
import WebDriverPreCore.BiDi.Session (SessionResult (..))
import WebDriverPreCore.BiDi.Storage (StorageResult (..))
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionResult (..))
import WebDriverPreCore.Internal.AesonUtils (parseObjectMaybe, subtractProps, jsonToText)
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
      success :: Maybe (Success a)
      success = parseObjectMaybe obj

      matchedResult :: Either ResponseError (Maybe (MatchedResponse a))
      matchedResult =
        success
          & maybe
            (Left $ ParseError obj)
            (\s -> Right . Just $ MkMatchedResponse s.result obj)

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
  | ParseError Object
  | BiDiTimeoutError {ms :: Int}
  deriving (Show, Eq, Generic)

displayResponseError :: (Show c) => Command c r -> ResponseError -> Text
displayResponseError c err =
  "Error executing BiDi command: "
    <> (txt c)
    <> "\n"
    <> case err of
      BiDIError e -> "BiDi driver error: \n" <> txt e
      EncodeError (MkJSONEncodeError e) -> "Failed to encode driver response to JSON: \n" <> txt e
      ParseError o -> "Failed to decode JSON returned by driver to response type: \n" <> jsonToText (Object o)
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

data ResultData
  = BrowsingContextResult BrowsingContextResult
  | BrowserResult BrowserResult
  | EmulationResult EmptyResult
  | InputResult FileDialogInfo
  | NetworkResult NetworkResult
  | ScriptResult ScriptResult
  | SessionResult SessionResult
  | StorageResult StorageResult
  | WebExtensionResult WebExtensionResult
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
