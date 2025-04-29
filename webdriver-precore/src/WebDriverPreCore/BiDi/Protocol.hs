
module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson 
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as Map
import Prelude (Integer, Show, Maybe)
import Data.Word (Word64)
import Data.Int (Int64)
import WebDriverPreCore.BiDi.CoreTypes
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContext)
                        
-- Main message type
data Message
  = CommandResponse CommandResponse
  | ErrorResponse ErrorResponse
  | Event Event
  deriving (Show, Generic, ToJSON, FromJSON)

data CommandResponse = Success
  { 
    id :: JSUint
  , result :: ResultData
  , extensions :: Maybe (Map.Map Text Value)
  } deriving (Show, Generic)



-- ResultData = (
--   BrowsingContextResult /
--   EmptyResult /
--   NetworkResult /
--   ScriptResult /
--   SessionResult /
--   StorageResult /
--   WebExtensionResult
-- )
data ResultData
  = BrowsingContextResult BrowsingContextResult {-}
  | EmptyResult (Map.Map Text Value)
  | NetworkResult NetworkResult
  | ScriptResult ScriptResult
  | SessionResult SessionResult
  | StorageResult StorageResult
  | WebExtensionResult WebExtensionResult
  -}
  deriving (Show, Generic, ToJSON, FromJSON)



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
  deriving (Show, Generic, ToJSON, FromJSON)

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

data EventData
  = BrowsingContextEvent BrowsingContextEvent
  | InputEvent InputEvent
  | LogEvent LogEvent
  | NetworkEvent NetworkEvent
  | ScriptEvent ScriptEvent
  deriving (Show, Generic, ToJSON, FromJSON)

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