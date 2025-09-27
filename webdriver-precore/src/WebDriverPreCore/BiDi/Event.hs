module WebDriverPreCore.BiDi.Event where

import Data.Aeson (FromJSON, ToJSON (..), Value)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent (..))
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (LogEntry)
import Prelude
import Data.Set (Set)
import Data.Type.Equality (apply)

mkSubscription :: forall m r. FromJSON r => SubscriptionType -> (r -> m ()) -> Subscription m
mkSubscription = MkSubscription 

mkSubscriptionN :: Set SubscriptionType -> (Event -> m ()) -> Subscription m
mkSubscriptionN = MkNSubscription

data Subscription m where
  MkSubscription :: forall m r. (FromJSON r) =>
    { subscription :: SubscriptionType,
      action :: r -> m ()
    } ->
    Subscription m 
  MkNSubscription :: { 
      subscriptions:: Set SubscriptionType,
      nAction :: Event -> m ()
    } ->
    Subscription m

data Event
  = BrowsingContextEvent BrowsingContextEvent
  | -- | InputEvent InputEvent
    InputEvent FileDialogOpened
  | -- method: "log.entryAdded"
    LogEvent LogEntry
  deriving
    ( Show,
      Eq,
      Generic
    )

data SubscriptionType
  = -- Log module
    LogEntryAdded
  | -- BrowsingContext module
    BrowsingContextContextCreated
  | BrowsingContextContextDestroyed
  | BrowsingContextNavigationStarted
  | BrowsingContextFragmentNavigated
  | BrowsingContextHistoryUpdated
  | BrowsingContextDomContentLoaded
  | BrowsingContextLoad
  | BrowsingContextDownloadWillBegin
  | BrowsingContextDownloadEnd
  | BrowsingContextNavigationAborted
  | BrowsingContextNavigationCommitted
  | BrowsingContextNavigationFailed
  | BrowsingContextUserPromptClosed
  | BrowsingContextUserPromptOpened
  | -- Network module
    NetworkAuthRequired
  | NetworkBeforeRequestSent
  | NetworkFetchError
  | NetworkResponseCompleted
  | NetworkResponseStarted
  | -- Script module
    ScriptMessage
  | ScriptRealmCreated
  | ScriptRealmDestroyed
  | -- Input module
    InputFileDialogOpened
  deriving (Show, Eq, Generic)

instance ToJSON SubscriptionType where
  toJSON :: SubscriptionType -> Value
  toJSON = \case
    -- Log module
    LogEntryAdded -> "log.entryAdded"
    -- BrowsingContext module
    BrowsingContextContextCreated -> "browsingContext.contextCreated"
    BrowsingContextContextDestroyed -> "browsingContext.contextDestroyed"
    BrowsingContextNavigationStarted -> "browsingContext.navigationStarted"
    BrowsingContextFragmentNavigated -> "browsingContext.fragmentNavigated"
    BrowsingContextHistoryUpdated -> "browsingContext.historyUpdated"
    BrowsingContextDomContentLoaded -> "browsingContext.domContentLoaded"
    BrowsingContextLoad -> "browsingContext.load"
    BrowsingContextDownloadWillBegin -> "browsingContext.downloadWillBegin"
    BrowsingContextDownloadEnd -> "browsingContext.downloadEnd"
    BrowsingContextNavigationAborted -> "browsingContext.navigationAborted"
    BrowsingContextNavigationCommitted -> "browsingContext.navigationCommitted"
    BrowsingContextNavigationFailed -> "browsingContext.navigationFailed"
    BrowsingContextUserPromptClosed -> "browsingContext.userPromptClosed"
    BrowsingContextUserPromptOpened -> "browsingContext.userPromptOpened"
    -- Network module
    NetworkAuthRequired -> "network.authRequired"
    NetworkBeforeRequestSent -> "network.beforeRequestSent"
    NetworkFetchError -> "network.fetchError"
    NetworkResponseCompleted -> "network.responseCompleted"
    NetworkResponseStarted -> "network.responseStarted"
    -- Script module
    ScriptMessage -> "script.message"
    ScriptRealmCreated -> "script.realmCreated"
    ScriptRealmDestroyed -> "script.realmDestroyed"
    -- Input module
    InputFileDialogOpened -> "input.fileDialogOpened"

{-
{
  "type": "event",
  "method": "module.eventName",
  "params": {
    // Event-specific parameters
  }
}

Event = {
  type: "event",
  EventData,
  Extensible
}

{
  "type": "event",
  "method": "browsingContext.navigationStarted",
  "params": {
    "context": "context-id",
    "navigation": "navigation-id",
    "timestamp": 1234567890,
    "url": "https://example.com"
  }
}

method which is a string literal of the form [module name].[event name]. This is the event name.I

{
  "type": "event",
  "method": "log.entryAdded",
  "params": {
    "level": "info",
    "text": "Hello world",
    "timestamp": 1234567890,
    "source": {
      "realm": "realm-id",
      "context": "context-id"
    }
  }
}

-}