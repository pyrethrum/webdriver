module WebDriverPreCore.BiDi.Session where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Capabilities (CapabilitiesResult)
import Prelude (Applicative ((<*>)), Bool (..), Eq (..), Maybe (..), Show (..), ($), (<$>), maybe, Semigroup (..))
import WebDriverPreCore.Internal.AesonUtils (toJSONOmitNothing)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, UserContext)

-- ######### Remote #########

-- | Subscription
newtype Subscription = MkSubscription {subscriptionId :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- | Subscription Request
data SessionSubscriptionRequest = MkSessionSubscriptionRequest
  { events :: [Text],
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionSubscriptionRequest where
  toJSON :: SessionSubscriptionRequest -> Value
  toJSON = toJSONOmitNothing

-- | Unsubscribe Parameters
data SessionUnsubscribe
  = UnsubscribeByID 
      { subscriptions :: [Subscription] }
  | UnsubscribeByAttributes 
      { unsubEvents :: [Text], 
        unsubContexts :: Maybe [BrowsingContext] }
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribe where
  toJSON :: SessionUnsubscribe -> Value
  toJSON (UnsubscribeByID {subscriptions}) = 
    object ["subscriptions" .= subscriptions]
  toJSON (UnsubscribeByAttributes {unsubEvents, unsubContexts}) = 
    object $ 
      [ "events" .= unsubEvents ] <>
       maybe [] (\contexts -> ["contexts" .= contexts]) unsubContexts

-- ######### Local #########

-- | Session New Result
data SessionNewResult = MkSessionNewResult
  { sessionId :: Text,
    capabilities :: CapabilitiesResult
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionNewResult where
  toJSON :: SessionNewResult -> Value
  toJSON (MkSessionNewResult sessionId capabilities) =
    object
      [ "sessionId" .= sessionId,
        "capabilities" .= capabilities
      ]

instance FromJSON SessionNewResult where
  parseJSON :: Value -> Parser SessionNewResult
  parseJSON = withObject "SessionNewResult" $ \v ->
    MkSessionNewResult
      <$> v .: "sessionId"
      <*> v .: "capabilities"

-- | Session Status Result
data SessionStatusResult = MkSessionStatusResult
  { ready :: Bool,
    message :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SessionStatusResult

-- | Session Subscribe Result
newtype SessionSubscribeResult = MkSessionSubscribeResult
  { subscription :: Subscription
  }
  deriving (Show, Eq, Generic)

instance FromJSON SessionSubscribeResult
