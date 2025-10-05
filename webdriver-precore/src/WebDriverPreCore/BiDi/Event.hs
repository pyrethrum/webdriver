module WebDriverPreCore.BiDi.Event where

import Data.Aeson (FromJSON (..))
import Data.Set (Set)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent (..))
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, SubscriptionType, UserContext)
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (LogEntry)
import WebDriverPreCore.BiDi.Network (NetworkEvent (..))
import WebDriverPreCore.BiDi.Script (ScriptEvent (..))
import Prelude

data Subscription m where
  SingleSubscription ::
    forall m r.
    (FromJSON r) =>
    { subscriptionType :: SubscriptionType,
      browsingContexts :: Maybe [BrowsingContext],
      userContexts :: Maybe [UserContext],
      action :: r -> m ()
    } ->
    Subscription m
  MultiSubscription ::
    { subscriptionTypes :: Set SubscriptionType,
      browsingContexts :: Maybe [BrowsingContext],
      userContexts :: Maybe [UserContext],
      nAction :: Event -> m ()
    } ->
    Subscription m

data Event
  = BrowsingContextEvent BrowsingContextEvent
  | InputEvent FileDialogOpened
  | LogEvent LogEntry
  | NetworkEvent NetworkEvent
  | ScriptEvent ScriptEvent
  deriving
    ( Show,
      Eq,
      Generic
    )

instance FromJSON Event
