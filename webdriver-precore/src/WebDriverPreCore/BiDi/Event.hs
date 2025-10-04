module WebDriverPreCore.BiDi.Event where

import Data.Aeson (FromJSON (..))
import Data.Set (Set)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent (..))
import WebDriverPreCore.BiDi.CoreTypes (SubscriptionType)
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (LogEntry)
import WebDriverPreCore.BiDi.Network (NetworkEvent (..))
import WebDriverPreCore.BiDi.Script (ScriptEvent (..))
import Prelude

mkSubscription :: forall m r. (FromJSON r) => SubscriptionType -> (r -> m ()) -> Subscription m
mkSubscription = MkSubscription

mkSubscriptionN :: Set SubscriptionType -> (Event -> m ()) -> Subscription m
mkSubscriptionN = MkNSubscription

data Subscription m where
  MkSubscription ::
    forall m r.
    (FromJSON r) =>
    { subscription :: SubscriptionType,
      action :: r -> m ()
    } ->
    Subscription m
  MkNSubscription ::
    { subscriptions :: Set SubscriptionType,
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
