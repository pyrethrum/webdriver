module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Value,
  )
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Browser (BrowserCommand, BrowserResult)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextCommand, BrowsingContextEvent, BrowsingContextResult)
import WebDriverPreCore.BiDi.CoreTypes
import WebDriverPreCore.BiDi.Emulation (EmulationCommand)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogOpened, InputCommand)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Network (NetworkCommand)
import WebDriverPreCore.BiDi.Script (RemoteValue, ScriptCommand, Source, StackTrace)
import WebDriverPreCore.BiDi.Session (SessionCommand)
import WebDriverPreCore.BiDi.Storage (StorageCommand)
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionCommand)
import Prelude (Bool, Eq, Maybe, Show)

-- ######### Local #########

-- Command types

data CommandData
  = BrowserCommand BrowserCommand
  | BrowsingContext BrowsingContextCommand
  | EmulationCommand EmulationCommand
  | Input InputCommand
  | Network NetworkCommand
  | Script ScriptCommand
  | Session SessionCommand
  | Storage StorageCommand
  | WebExtension WebExtensionCommand
  deriving (Show, Eq, Generic)

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
  deriving (-- | EmptyResult
            -- | NetworkResult RemoteValue -- Placeholder for network results
            -- | ScriptResult RemoteValue -- Placeholder for script results
            -- | SessionResult RemoteValue -- Placeholder for session results
            -- | StorageResult RemoteValue -- Placeholder for storage results
            -- | WebExtensionResult RemoteValue -- Placeholder for web extension results
            Show, Generic)

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
