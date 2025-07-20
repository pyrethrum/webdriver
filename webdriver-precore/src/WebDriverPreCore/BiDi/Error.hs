module WebDriverPreCore.BiDi.Error
  ( ErrorCode (..),
  toErrorCodeText,
  fromErrorCodeText,
  )
where


import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude (Eq, Show, Ord, Maybe (..))

data ErrorCode
  = -- | Tried to perform an action with an invalid argument
    InvalidArgument
  | -- | Tried to use an invalid selector
    InvalidSelector
  | -- | Tried to use an invalid session ID
    InvalidSessionId
  | -- | Tried to install an invalid web extension
    InvalidWebExtension
  | -- | Tried to move the mouse to a position outside the viewport
    MoveTargetOutOfBounds
  | -- | Tried to interact with an alert that doesn't exist
    NoSuchAlert
  | -- | Tried to remove an unknown collector
    NoSuchNetworkCollector
  | -- | Tried to interact with an element that doesn't exist
    NoSuchElement
  | -- | Tried to switch to a frame that doesn't exist
    NoSuchFrame
  | -- | Tried to deserialize an unknown RemoteObjectReference
    NoSuchHandle
  | -- | Tried to navigate to an unknown session history entry
    NoSuchHistoryEntry
  | -- | Tried to remove an unknown network intercept
    NoSuchIntercept
  | -- | Tried to reference unknown data
    NoSuchNetworkData
  | -- | Tried to deserialize an unknown SharedReference
    NoSuchNode
  | -- | Tried to continue an unknown request
    NoSuchRequest
  | -- | Tried to remove an unknown preload script
    NoSuchScript
  | -- | Tried to access data in a non-existent storage partition
    NoSuchStoragePartition
  | -- | Tried to reference an unknown user context
    NoSuchUserContext
  | -- | Tried to reference an unknown web extension
    NoSuchWebExtension
  | -- | Failed to create a new session
    SessionNotCreated
  | -- | Failed to capture a screenshot
    UnableToCaptureScreen
  | -- | Tried to close the browser, but failed to do so
    UnableToCloseBrowser
  | -- | Tried to create a cookie, but the user agent rejected it
    UnableToSetCookie
  | -- | Tried to set a file input, but failed to do so
    UnableToSetFileInput
  | -- | Tried to get network data which was not collected or already evicted
    UnavailableNetworkData
  | -- | Tried to interact with data in a storage partition which was not adequately specified
    UnderspecifiedStoragePartition
  | -- | The command sent is not known
    UnknownCommand
  | -- | An unknown error occurred
    UnknownError
  | -- | The operation requested is not supported
    UnsupportedOperation
  deriving (Show, Eq, Ord, Generic)

-- | Maps ErrorCode enum values to their string representation in the WebDriver BiDi spec
toErrorCodeText :: ErrorCode -> Text
toErrorCodeText = \case
  InvalidArgument -> "invalid argument"
  InvalidSelector -> "invalid selector"
  InvalidSessionId -> "invalid session id"
  InvalidWebExtension -> "invalid web extension"
  MoveTargetOutOfBounds -> "move target out of bounds"
  NoSuchAlert -> "no such alert"
  NoSuchNetworkCollector -> "no such network collector"
  NoSuchElement -> "no such element"
  NoSuchFrame -> "no such frame"
  NoSuchHandle -> "no such handle"
  NoSuchHistoryEntry -> "no such history entry"
  NoSuchIntercept -> "no such intercept"
  NoSuchNetworkData -> "no such network data"
  NoSuchNode -> "no such node"
  NoSuchRequest -> "no such request"
  NoSuchScript -> "no such script"
  NoSuchStoragePartition -> "no such storage partition"
  NoSuchUserContext -> "no such user context"
  NoSuchWebExtension -> "no such web extension"
  SessionNotCreated -> "session not created"
  UnableToCaptureScreen -> "unable to capture screen"
  UnableToCloseBrowser -> "unable to close browser"
  UnableToSetCookie -> "unable to set cookie"
  UnableToSetFileInput -> "unable to set file input"
  UnavailableNetworkData -> "unavailable network data"
  UnderspecifiedStoragePartition -> "underspecified storage partition"
  UnknownCommand -> "unknown command"
  UnknownError -> "unknown error"
  UnsupportedOperation -> "unsupported operation"

fromErrorCodeText :: Text -> Maybe ErrorCode
fromErrorCodeText = \case
  "invalid argument" -> Just InvalidArgument
  "invalid selector" -> Just InvalidSelector
  "invalid session id" -> Just InvalidSessionId
  "invalid web extension" -> Just InvalidWebExtension
  "move target out of bounds" -> Just MoveTargetOutOfBounds
  "no such alert" -> Just NoSuchAlert
  "no such network collector" -> Just NoSuchNetworkCollector
  "no such element" -> Just NoSuchElement
  "no such frame" -> Just NoSuchFrame
  "no such handle" -> Just NoSuchHandle
  "no such history entry" -> Just NoSuchHistoryEntry
  "no such intercept" -> Just NoSuchIntercept
  "no such network data" -> Just NoSuchNetworkData
  "no such node" -> Just NoSuchNode
  "no such request" -> Just NoSuchRequest
  "no such script" -> Just NoSuchScript
  "no such storage partition" -> Just NoSuchStoragePartition
  "no such user context" -> Just NoSuchUserContext
  "no such web extension" -> Just NoSuchWebExtension
  "session not created" -> Just SessionNotCreated
  "unable to capture screen" -> Just UnableToCaptureScreen
  "unable to close browser" -> Just UnableToCloseBrowser
  "unable to set cookie" -> Just UnableToSetCookie
  "unable to set file input" -> Just UnableToSetFileInput
  "unavailable network data" -> Just UnavailableNetworkData
  "underspecified storage partition" -> Just UnderspecifiedStoragePartition
  "unknown command" -> Just UnknownCommand
  "unknown error" -> Just UnknownError
  "unsupported operation" -> Just UnsupportedOperation
  _ -> Nothing