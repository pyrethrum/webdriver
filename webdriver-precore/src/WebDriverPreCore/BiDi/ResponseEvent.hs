module WebDriverPreCore.BiDi.ResponseEvent where

import Data.Aeson
  ( FromJSON,
    Object,
    ToJSON,
    Value (..),
    object,
    (.=),
  )
import Data.Aeson.Types (ToJSON (..))
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Browser (BrowserCommand, BrowserResult)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextCommand, BrowsingContextEvent, BrowsingContextResult)
import WebDriverPreCore.BiDi.BrowsingContext qualified as BC
import WebDriverPreCore.BiDi.CoreTypes (BiDiMethod (bidiMethod), JSUInt)
import WebDriverPreCore.BiDi.Emulation (EmulationCommand)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogOpened, InputCommand)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Network (NetworkCommand)
import WebDriverPreCore.BiDi.Script (RemoteValue, ScriptCommand, Source, StackTrace)
import WebDriverPreCore.BiDi.Session (SessionCommand, SessionSubscriptionRequest, SessionUnsubscribeParameters)
import WebDriverPreCore.BiDi.Session qualified as S
import WebDriverPreCore.BiDi.Storage (StorageCommand)
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionCommand)
import WebDriverPreCore.Internal.AesonUtils (objectOrThrow, parseObject)
import Prelude (Bool, Eq, Maybe (..), Show, error, maybe, ($), (.), (<$>), (<>))

-- ######### Remote #########

data Message
  = CommandResponse Success
  | ErrorResponse Error
  | Event Event
  deriving (Show, Generic)

data Success = MkSuccess
  { id :: JSUInt,
    result :: ResultData,
    extensions :: Maybe (Map.Map Text Value)
  }
  deriving (Show, Generic)

data Error = MkError
  { typ :: Text, -- "error"
    errorId :: Maybe JSUInt,
    error :: ErrorCode,
    message :: Text,
    stacktrace :: Maybe Text,
    extensions :: Maybe (Map.Map Text Value)
  }
  deriving (Show, Generic)

data Event = MkEvent
  { typ :: Text, -- "event"
    eventData :: EventData,
    extensions :: Maybe (Map.Map Text Value)
  }
  deriving (Show, Generic)

data EventData
  = BrowsingContextEvent BrowsingContextEvent
  | -- | InputEvent InputEvent
    InputEvent FileDialogOpened
  | -- method: "log.entryAdded"
    LogEvent Entry
  deriving
    ( -- | NetworkEvent NetworkEvent
      -- | ScriptEvent ScriptEvent
      Show,
      Generic
    )

data ResultData
  = BrowsingContextResult BrowsingContextResult
  deriving
    ( Show,
      Generic
    )

-- ToDO:
-- ResultData = (
--   BrowsingContextResult /
--   EmptyResult /
--   NetworkResult /
--   ScriptResult /
--   SessionResult /
--   StorageResult /
--   WebExtensionResult
-- )
