module WebDriverPreCore.BiDi.ResponseEvent where

import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON,
    Value (..),
    fromJSON,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.KeyMap ((!?))
import Data.Aeson.Types (Parser, ToJSON (..), parseMaybe)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Browser (BrowserCommand, BrowserResult)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextCommand, BrowsingContextEvent, BrowsingContextResult)
import WebDriverPreCore.BiDi.BrowsingContext qualified as BC
import WebDriverPreCore.BiDi.CoreTypes (BiDiMethod (bidiMethod), EmptyResult (..), JSUInt)
import WebDriverPreCore.BiDi.Emulation (EmulationCommand)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogInfo, FileDialogOpened, InputCommand)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Network (NetworkCommand, NetworkResult (..))
import WebDriverPreCore.BiDi.Script (RemoteValue, ScriptCommand, ScriptResult, Source, StackTrace)
import WebDriverPreCore.BiDi.Session (SessionCommand, SessionResult (..), SessionSubscriptionRequest, SessionUnsubscribeParameters)
import WebDriverPreCore.BiDi.Session qualified as S
import WebDriverPreCore.BiDi.Storage (StorageCommand, StorageResult (..))
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionCommand, WebExtensionResult (..))
import WebDriverPreCore.Internal.AesonUtils (subtractProps, objectOrThrow, parseObject, parseObjectMaybe)
import Prelude

-- ######### Remote #########

-- HERE simplify pare response by applying this parser and adding an extra constructor to ResponseError
parseResponse' :: forall a. FromJSON a => JSUInt -> Object -> Parser (Maybe (Either ResponseError (Success a)))
parseResponse' msgId obj = do
  id' <- obj .:? "id"
  pure $
    if id' == Just msgId
      then
        Just $
          maybe
            (Left err)
            Right
            success
      else
        Nothing
  where
    success :: Maybe (Success a)
    success = parseObjectMaybe obj

    bidiError :: Maybe Error
    bidiError = parseObjectMaybe obj

    err :: ResponseError
    err = bidiError & maybe (UnknownResponse obj) BiDIError

data ResponseError
  = UnknownResponse Object
  | BiDIError Error
  deriving (Show, Eq, Generic)

data Success a = MkSuccess
  { id :: JSUInt,
    result :: a,
    extensions :: EmptyResult
  }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (Success a) where
  parseJSON :: Value -> Parser (Success a)
  parseJSON = withObject "Success" $ \o -> do
    id' <- o .: "id"
    result <- o .: "result"
    pure $
      MkSuccess
        { id = id',
          result = result,
          extensions = (MkEmptyResult $ subtractProps ["id", "result"] o)
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

instance FromJSON Error

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
