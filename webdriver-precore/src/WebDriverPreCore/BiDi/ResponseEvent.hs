module WebDriverPreCore.BiDi.ResponseEvent where

import Data.Aeson (FromJSON (parseJSON), Object, Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Browser (BrowserResult)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent, BrowsingContextResult)
import WebDriverPreCore.BiDi.CoreTypes (EmptyResult (..), JSUInt)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogInfo, FileDialogOpened)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Network (NetworkResult (..))
import WebDriverPreCore.BiDi.Script (ScriptResult)
import WebDriverPreCore.BiDi.Session (SessionResult (..))
import WebDriverPreCore.BiDi.Storage (StorageResult (..))
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionResult (..))
import WebDriverPreCore.Internal.AesonUtils (parseObjectMaybe, subtractProps)
import Prelude

-- ######### Remote #########

-- parseResponse :: forall a. (FromJSON a) => JSUInt -> Object -> Maybe (Either ResponseError (Success a))
-- parseResponse msgId obj = undefined
--   where
--     parsed :: Result (Maybe (Either ResponseError (Success a)))
--     parsed = parse (parseResponse' msgId) obj

-- parseResponse' :: forall a. (FromJSON a) => JSUInt -> Object -> Parser (Maybe (Either ResponseError (Success a)))
-- parseResponse' msgId obj = do
--   id' <- obj .:? "id"
--   pure $
--     if id' == Just msgId
--       then
--         Just $
--           success
--             & maybe
--               (Left responseError)
--               Right
--       else
--         Nothing
--   where
--     success :: Maybe (Success a)
--     success = parseObjectMaybe obj

--     responseError :: ResponseError
--     responseError =
--       parseObjectMaybe obj
--         & maybe (JSONParseError obj) BiDIError

-- parseResponse' :: forall a. (FromJSON a) => JSUInt -> Object -> Parser (Maybe (Either ResponseError (Success a)))
-- parseResponse' msgId obj = do
--   id' <- obj .:? "id"
--   pure $
--     if id' == Just msgId
--       then
--         Just $
--           success
--             & maybe
--               (Left responseError)
--               Right
--       else
--         Nothing
--   where
--     success :: Maybe (Success a)
--     success = parseObjectMaybe obj

--     responseError :: ResponseError
--     responseError =
--       parseObjectMaybe obj
--         & maybe (JSONParseError obj) BiDIError

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
      matchedResult = success & maybe (Left $ JSONParseError obj) (\s -> Right . Just $ MkMatchedResponse s.result obj)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

decodeResponse :: ByteString -> Either ResponseError ResponseObject
decodeResponse =
  (=<<) parseResponseObj . packLeft . eitherDecode
  where
    packLeft = mapLeft (JSONEncodeError . pack)

    parseResponseObj :: Object -> Either ResponseError ResponseObject
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

data ResponseError
  = JSONParseError Object
  | JSONEncodeError Text
  | BiDIError Error
  deriving (Show, Eq, Generic)

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
