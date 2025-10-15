module WebDriverPreCore.BiDi.Event where

import Data.Aeson (FromJSON (..), Value (..), withObject, (.:))
import Data.Aeson.Types (Parser, parse)
import Data.Set (Set)
import Data.Text (Text, isPrefixOf, unpack)
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
      browsingContexts :: [BrowsingContext],
      userContexts :: [UserContext],
      action :: r -> m ()
    } ->
    Subscription m
  MultiSubscription ::
    { browsingContexts :: [BrowsingContext],
      userContexts :: [UserContext],
      subscriptionTypes :: [SubscriptionType],
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

instance FromJSON Event where
  parseJSON :: Value -> Parser Event
  parseJSON = withObject "Event" $ \o -> do
    m <- o .: "method"
    let methodPrefix :: Text -> Bool
        methodPrefix = (`isPrefixOf` m)
        parseVal :: forall a. FromJSON a => Parser a
        parseVal = parseJSON (Object o)
    if
      | methodPrefix "browsingContext" -> BrowsingContextEvent <$> parseVal
      | methodPrefix "input" -> InputEvent <$> parseVal
      | methodPrefix "log" -> LogEvent <$> parseVal
      | methodPrefix "network" -> NetworkEvent <$> parseVal
      | methodPrefix "script" -> ScriptEvent <$> parseVal
      | otherwise -> fail $ "Unknown event type: " <> unpack m
    

