module WebDriverPreCore.BiDi.Protocol where

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
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextCommand (..), BrowsingContextEvent, BrowsingContextResult, Create)
import WebDriverPreCore.BiDi.CoreTypes (BiDiMethod (bidiMethod), JSUInt)
import WebDriverPreCore.BiDi.Emulation (EmulationCommand)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogOpened, InputCommand)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Network (NetworkCommand)
import WebDriverPreCore.BiDi.Script (RemoteValue, ScriptCommand, Source, StackTrace)
import WebDriverPreCore.BiDi.Session (Capabilities, SessionCommand (..), SessionSubscriptionRequest, SessionUnsubscribeParameters)
import WebDriverPreCore.BiDi.Storage (StorageCommand)
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionCommand)
import WebDriverPreCore.Internal.AesonUtils (objectOrThrow, parseObject)
import Prelude (Bool, Eq, Maybe (..), Show, error, maybe, ($), (.), (<$>), (<>))

-- ######### Local #########

-- TODO: check exceptions eg test with unsupported command - currently not getting to main thread
jsonCommand :: Text -> Object -> JSUInt -> Value
jsonCommand methodName params id =
  object
    [ "id" .= id,
      "method" .= methodName,
      "params" .= params
    ]

-- TODO: check exceptions eg test with unsupported command - currently not getting to main thread
baseCommand :: Command -> Maybe Object -> JSUInt -> Value
baseCommand cmd extensions =
  jsonCommand (method cmd) extendedParams
  where
    prmsObj = objectOrThrow "CommandData will always be an Object" cmd
    extendedParams = extensions & maybe prmsObj (prmsObj <>)
    method = \case
      -- BrowserCommand _ -> "browser"
      BrowsingContext bc -> bidiMethod bc
      -- EmulationCommand _ -> "emulation"
      -- Input _ -> "input"
      -- Network _ -> "network"
      -- Script _ -> "script"
      Session s -> bidiMethod s
      -- Storage _ -> "storage"
      -- WebExtension _ -> "webExtension"
      _ -> error "Unsupported command type for bidi method"

command :: Command -> JSUInt -> Value
command c = baseCommand c Nothing

extendedCommand :: Command -> Object -> JSUInt -> Value
extendedCommand c extensions = baseCommand c (Just extensions)

-- Command types

data Command
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

-- todo :: replace with derived instnace when donee
instance ToJSON Command where
  toJSON :: Command -> Value
  toJSON = \case
    -- BrowserCommand cmd -> toJSON cmd
    BrowsingContext cmd -> toJSON cmd
    -- EmulationCommand cmd -> toJSON cmd
    -- Input cmd -> toJSON cmd
    -- Network cmd -> toJSON cmd
    -- Script cmd -> toJSON cmd
    Session cmd -> toJSON cmd
    -- Storage cmd -> toJSON cmd
    -- WebExtension cmd -> toJSON cmd
    _ -> error "Unsupported command type for JSON serialization"

extendedSubCommand :: (a -> Command) -> a -> Object -> JSUInt -> Value
extendedSubCommand cstr a extn = extendedCommand (cstr a) extn

subCommand :: (a -> Command) -> a -> JSUInt -> Value
subCommand cstr = command . cstr

-- ~~~~~~~~ Session Commands ~~~~~~~~

sessionCommand :: SessionCommand -> JSUInt -> Value
sessionCommand = subCommand Session

extendedSessionCommand :: SessionCommand -> Object -> JSUInt -> Value
extendedSessionCommand = extendedSubCommand Session

newSession :: Capabilities -> JSUInt -> Value
newSession = sessionCommand . SessionNew

extendedNewSession :: Capabilities -> Object -> JSUInt -> Value
extendedNewSession = extendedSessionCommand . SessionNew

sessionStatus :: JSUInt -> Value
sessionStatus = sessionCommand SessionStatus

extendedSessionStatus :: Object -> JSUInt -> Value
extendedSessionStatus = extendedSessionCommand SessionStatus

sessionSubscribe :: SessionSubscriptionRequest -> JSUInt -> Value
sessionSubscribe = sessionCommand . SessionSubscribe

sessionUnsubscribe :: SessionUnsubscribeParameters -> JSUInt -> Value
sessionUnsubscribe = sessionCommand . SessionUnsubscribe

extendedSessionUnsubscribe :: SessionUnsubscribeParameters -> Object -> JSUInt -> Value
extendedSessionUnsubscribe = extendedSessionCommand . SessionUnsubscribe

sessionEnd :: JSUInt -> Value
sessionEnd = sessionCommand SessionEnd

extendedSessionEnd :: Object -> JSUInt -> Value
extendedSessionEnd = extendedSessionCommand SessionEnd

-- -- ~~~~~~~~ Browsering Context Commands ~~~~~~~~

-- browsingContextCommand :: BrowsingContextCommand -> JSUInt -> Value
-- browsingContextCommand cmd = command (BrowsingContext cmd)

-- browsingContextCommand' :: Extensions -> BrowsingContextCommand -> JSUInt -> Value
-- browsingContextCommand' extensions cmd id =
--   command' extensions (BrowsingContext cmd) id

-- browsingContextCreate :: Create -> JSUInt -> Value
-- browsingContextCreate cmd = browsingContextCommand (Create cmd)

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
    ( -- | EmptyResult
      -- | NetworkResult RemoteValue -- Placeholder for network results
      -- | ScriptResult RemoteValue -- Placeholder for script results
      -- | SessionResult RemoteValue -- Placeholder for session results
      -- | StorageResult RemoteValue -- Placeholder for storage results
      -- | WebExtensionResult RemoteValue -- Placeholder for web extension results
      Show,
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
