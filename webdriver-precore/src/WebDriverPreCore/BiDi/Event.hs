module WebDriverPreCore.BiDi.Event where

import Data.Aeson (FromJSON (..), Value (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text, isPrefixOf, unpack)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent (..))
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, KnownSubscriptionType (..), SubscriptionType (..), UserContext, UnknownSubscriptionType (..))
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (LogEvent)
import WebDriverPreCore.BiDi.Network (NetworkEvent (..))
import WebDriverPreCore.BiDi.Script (ScriptEvent (..))

mkSingleSubscription ::
  forall m r.
  (FromJSON r) =>
  KnownSubscriptionType ->
  [BrowsingContext] ->
  [UserContext] ->
  (r -> m ()) ->
  Subscription m
mkSingleSubscription subType =
  SingleSubscription (KnownSubscriptionType subType)

mkMultiSubscription ::
  [KnownSubscriptionType] ->
  [BrowsingContext] ->
  [UserContext] ->
  (Event -> m ()) ->
  Subscription m
mkMultiSubscription ks =
  MultiSubscription (KnownSubscriptionType <$> ks)

mkUnknownSubscription ::
  [UnknownSubscriptionType] ->
  [BrowsingContext] ->
  [UserContext] ->
  (Value -> m ()) ->
  Subscription m
mkUnknownSubscription ks =
  UnknownSubscription (UnknownSubscriptionType <$> ks)

data Subscription m where
  SingleSubscription ::
    forall m r.
    (FromJSON r) =>
    { subscriptionType :: SubscriptionType,
      browsingContexts :: [BrowsingContext],
      userContexts :: [UserContext],
      action :: r -> m ()
    } ->
    Subscription m
  MultiSubscription ::
    { subscriptionTypes :: [SubscriptionType],
      browsingContexts :: [BrowsingContext],
      userContexts :: [UserContext],
      nAction :: Event -> m ()
    } ->
    Subscription m
  UnknownSubscription ::
    { subscriptionTypes :: [SubscriptionType],
      browsingContexts :: [BrowsingContext],
      userContexts :: [UserContext],
      nValueAction :: Value -> m ()
    } ->
    Subscription m

data Event
  = BrowsingContextEvent BrowsingContextEvent
  | InputEvent FileDialogOpened
  | LogEvent LogEvent
  | NetworkEvent NetworkEvent
  | ScriptEvent ScriptEvent
  deriving
    ( Show,
      Eq,
      Generic
    )

instance FromJSON Event where
  parseJSON :: Value -> Parser Event
  parseJSON = withObject "Event" $ \o -> do
    m <- o .: "method"
    let methodPrefix :: Text -> Bool
        methodPrefix = (`isPrefixOf` m)
        parseVal :: forall a. (FromJSON a) => Parser a
        parseVal = parseJSON (Object o)
    if
      | methodPrefix "browsingContext" -> BrowsingContextEvent <$> parseVal
      | methodPrefix "input" -> InputEvent <$> parseVal
      | methodPrefix "log" -> LogEvent <$> parseVal
      | methodPrefix "network" -> NetworkEvent <$> parseVal
      | methodPrefix "script" -> ScriptEvent <$> parseVal
      | otherwise -> fail $ "Unknown event type: " <> unpack m
