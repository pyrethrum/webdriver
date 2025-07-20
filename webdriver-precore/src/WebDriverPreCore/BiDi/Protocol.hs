module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Value,
  )
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent, BrowsingContextResult)
import WebDriverPreCore.BiDi.CoreTypes
import WebDriverPreCore.BiDi.Input (InputCommand, FileDialogOpened)
import WebDriverPreCore.BiDi.Network (NetworkCommand)
import WebDriverPreCore.BiDi.Script (RemoteValue, ScriptCommand, Source, StackTrace)
import WebDriverPreCore.BiDi.Session (SessionCommand)
import WebDriverPreCore.BiDi.Storage (StorageCommand)
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionCommand)
import Prelude (Bool, Eq, Maybe, Show)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Error (ErrorCode)

-- ######### Local #########

-- Command types

data CommandData
  = Session SessionCommand
  | Input InputCommand
  | Network NetworkCommand
  | Script ScriptCommand
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
  deriving (-- | NetworkEvent NetworkEvent
            -- | ScriptEvent ScriptEvent
            Show, Generic)


data ResultData
  = BrowsingContextResult BrowsingContextResult
  -- | EmptyResult
  -- | NetworkResult RemoteValue -- Placeholder for network results
  -- | ScriptResult RemoteValue -- Placeholder for script results
  -- | SessionResult RemoteValue -- Placeholder for session results
  -- | StorageResult RemoteValue -- Placeholder for storage results
  -- | WebExtensionResult RemoteValue -- Placeholder for web extension results
  deriving (Show, Generic)

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
