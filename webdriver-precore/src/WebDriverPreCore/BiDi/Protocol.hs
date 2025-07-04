module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson 
  ( Value,
  )
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextResult, BrowsingContextEvent)
import WebDriverPreCore.BiDi.CoreTypes
import Prelude (Maybe, Show, Bool)
import WebDriverPreCore.BiDi.Script (Source, StackTrace, RemoteValue)

-- Main message type
data Message
  = CommandResponse Success |
    ErrorResponse Error |
    Event Event
  deriving (
            Show, Generic)

data Success = MkSuccess
  { id :: JSUInt,
    result :: ResultData,
    extensions :: Maybe (Map.Map Text Value)
  }
  deriving (Show, Generic)

data Error = MkError
  { typ :: Text,  -- "error"
    errorId :: Maybe JSUInt,
    error :: ErrorCode,
    message :: Text,
    stacktrace :: Maybe Text,
    extensions :: Maybe (Map.Map Text Value)
  }
  deriving (Show, Generic)

data Event = MkEvent
  { typ :: Text,  -- "event"
    eventData :: EventData,
    extensions :: Maybe (Map.Map Text Value)
  }
  deriving (Show, Generic)

data EventData
  = BrowsingContextEvent BrowsingContextEvent
  -- | InputEvent InputEvent
  | InputEvent FileDialogOpened 
  -- method: "log.entryAdded"
  | LogEvent Entry 
  -- | NetworkEvent NetworkEvent
  -- | ScriptEvent ScriptEvent
  deriving (Show, Generic)

data FileDialogOpened  = MkFileDialogOpened {
      context :: BrowsingContext,
      element :: Maybe NodeRemoteValue,
      multiple :: Bool
    } 
  deriving (Show, Generic)



data Entry
  = GenericLogEntry GenericLogEntry  
  | ConsoleLogEntry ConsoleLogEntry
  | JavascriptLogEntry JavaScriptLogEntry
  deriving (Show, Generic)

data ConsoleLogEntry = MkConsoleLogEntry
  { log :: BaseLogEntry,
    typ :: Text, -- "console"
    method :: Text,
    args :: [RemoteValue]
  }
  deriving (Show, Generic)

data GenericLogEntry = MkGenericLogEntry
  { 
    typ :: Text,
    log :: BaseLogEntry
  }
  deriving (Show, Generic)

data JavaScriptLogEntry = MkJavaScriptLogEntry
  { 
    typ :: Text,
    log :: BaseLogEntry
  }
  deriving (Show, Generic)

data Level
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Generic)


data BaseLogEntry = MkBaseLogEntry
  { level :: Level,
    source :: Source,
    text :: Maybe Text,
    stackTrace :: Maybe StackTrace
  }
  deriving (Show, Generic)

  

data ErrorCode
  = InvalidArgument | 
    InvalidSelector |
    InvalidSessionId |
    InvalidWebExtension |
    MoveTargetOutOfBounds |
    NoSuchAlert |
    NoSuchElement |
    NoSuchFrame |
    NoSuchHandle |
    NoSuchHistoryEntry |
    NoSuchIntercept |
    NoSuchNode |
    NoSuchRequest |
    NoSuchScript |
    NoSuchStoragePartition |
    NoSuchUserContext |
    NoSuchWebExtension |
    SessionNotCreated |
    UnableToCaptureScreen |
    UnableToCloseBrowser |
    UnableToSetCookie |
    UnableToSetFileInput |
    UnderspecifiedStoragePartition |
    UnknownCommand |
    UnknownError |
    UnsupportedOperation 
      deriving (Show, Generic)

    {- 
    "invalid argument" /
            "invalid selector" /
            "invalid session id" /
            "invalid web extension" /
            "move target out of bounds" /
            "no such alert" /
            "no such element" /
            "no such frame" /
            "no such handle" /
            "no such history entry" /
            "no such intercept" /
            "no such node" /
            "no such request" /
            "no such script" /
            "no such storage partition" /
            "no such user context" /
            "no such web extension" /
            "session not created" /
            "unable to capture screen" /
            "unable to close browser" /
            "unable to set cookie" /
            "unable to set file input" /
            "underspecified storage partition" /
            "unknown command" /
            "unknown error" /
            "unsupported operation"

    -}



-- ResultData = (
--   BrowsingContextResult /
--   EmptyResult /
--   NetworkResult /
--   ScriptResult /
--   SessionResult /
--   StorageResult /
--   WebExtensionResult
-- )

-- See Note [Put touchable variables on the left]

data ResultData
  = BrowsingContext BrowsingContextResult {-}
                                          \| EmptyResult (Map.Map Text Value)
                                          \| NetworkResult NetworkResult
                                          \| ScriptResult ScriptResult
                                          \| SessionResult SessionResult
                                          \| StorageResult StorageResult
                                          \| WebExtensionResult WebExtensionResult
                                          -}
  deriving (Show, Generic) -- See Note [Put touchable variables on the left]

{- Spec

Message = (
  CommandResponse /
  ErrorResponse /
  Event
)

CommandResponse = {
  type: "success",
  id: js-uint,
  result: ResultData,
  Extensible
}

ErrorResponse = {
  type: "error",
  id: js-uint / null,
  error: ErrorCode,
  message: text,
  ? stacktrace: text,
  Extensible
}

EmptyResult = {
  Extensible
}

Event = {
  type: "event",
  EventData,
  Extensible
}

EventData = (
  BrowsingContextEvent //
  InputEvent //
  LogEvent //
  NetworkEvent //
  ScriptEvent
)

  -}

{-

module Protocol where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as Map

-- Base types
type JSUint = Integer  -- 0..9007199254740991
type JSInt = Integer   -- -9007199254740991..9007199254740991

-- Main message type
data Message
  = CommandResponse CommandResponse
  | ErrorResponse ErrorResponse
  | Event Event
  deriving (Show, Generic, ToJSON, FromJSON)

-- CommandResponse.hs
module CommandResponse where

data CommandResponse = CommandResponse
  { typ :: Text  -- "success"
  , id :: JSUint
  , result :: ResultData
  , extensions :: Maybe (Map.Map Text Value)
  } deriving (Show, Generic)

instance ToJSON CommandResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON CommandResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

-- ErrorResponse.hs
module ErrorResponse where

data ErrorResponse = ErrorResponse
  { typ :: Text  -- "error"
  , id :: Maybe JSUint
  , error :: ErrorCode
  , message :: Text
  , stacktrace :: Maybe Text
  , extensions :: Maybe (Map.Map Text Value)
  } deriving (Show, Generic)

instance ToJSON ErrorResponse where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON defaultOptions

data ErrorCode
  = InvalidArgument | InvalidSelector | InvalidSessionId
  -- ... other error codes ...
  deriving (Show, Generic)

-- Event.hs
module Event where

data Event = Event
  { typ :: Text  -- "event"
  , eventData :: EventData
  , extensions :: Maybe (Map.Map Text Value)
  } deriving (Show, Generic)

instance ToJSON Event where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions



-- ResultData.hs
module ResultData where

-- Session.hs
module Session where

data CapabilitiesRequest = CapabilitiesRequest
  { alwaysMatch :: Maybe CapabilityRequest
  , firstMatch :: Maybe [CapabilityRequest]
  } deriving (Show, Generic, ToJSON, FromJSON)

data CapabilityRequest = CapabilityRequest
  { acceptInsecureCerts :: Maybe Bool
  , browserName :: Maybe Text
  -- ... other fields ...
  } deriving (Show, Generic, ToJSON, FromJSON)

-- ... Additional modules for BrowsingContext, Network, Script, etc. ...

-- Storage.hs
module Storage where

data PartitionKey = PartitionKey
  { userContext :: Maybe Text
  , sourceOrigin :: Maybe Text
  , extensions :: Maybe (Map.Map Text Value)
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Log.hs
module Log where

data Level = Debug | Info | Warn | Error
  deriving (Show, Generic, ToJSON, FromJSON)

data Entry
  = GenericLogEntry GenericLogEntry
  | ConsoleLogEntry ConsoleLogEntry
  | JavascriptLogEntry JavascriptLogEntry
  deriving (Show, Generic, ToJSON, FromJSON)

  -}