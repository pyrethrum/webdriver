module WebDriverPreCore.BiDi.Session
  ( SubscriptionId (..),
    SessionSubscibe (..),
    SessionUnsubscribe (..),
    SessionNewResult (..),
    SessionStatusResult (..),
    SessionSubscribeResult (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Capabilities (CapabilitiesResult)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, UserContext, SubscriptionType)
import AesonUtils (toJSONOmitNothing)

-- ######### Remote #########

-- | Subscription
newtype SubscriptionId = MkSubscriptionId {subscriptionId :: Text}
  deriving newtype (Show, Eq, FromJSON, ToJSON)

-- | Subscription Request
data SessionSubscibe = MkSessionSubscribe
  { events :: [SubscriptionType],
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionSubscibe where
  toJSON :: SessionSubscibe -> Value
  toJSON = toJSONOmitNothing

-- | Unsubscribe Parameters
data SessionUnsubscribe
  = UnsubscribeById
      {subscriptions :: [SubscriptionId]}
  | UnsubscribeByAttributes
      { unsubEvents :: [SubscriptionType]
      }
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribe where
  toJSON :: SessionUnsubscribe -> Value
  toJSON (UnsubscribeById {subscriptions}) =
    object ["subscriptions" .= subscriptions]
  toJSON (UnsubscribeByAttributes {unsubEvents}) =
    object $
      ["events" .= unsubEvents]

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
  { subscription :: SubscriptionId
  }
  deriving (Show, Eq, Generic)

instance FromJSON SessionSubscribeResult
