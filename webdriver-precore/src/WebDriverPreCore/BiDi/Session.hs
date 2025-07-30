module WebDriverPreCore.BiDi.Session where

import Data.Aeson (FromJSON (..), Key, ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (fromList)
import Data.Word (Word8)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (BiDiMethod (..))
import WebDriverPreCore.Internal.AesonUtils (emptyObj, opt)
import Prelude (Applicative ((<*>)), Bool (..), Eq (..), Maybe (..), Show (..), ($), (.), (<$>))

webSocketUrlKey :: Key
webSocketUrlKey = "webSocketUrl"

-- ######### Remote #########

data SessionCommand
  = New Capabilities
  | Status
  | Subscribe SessionSubscriptionRequest
  | Unsubscribe SessionUnsubscribeParameters
  | End
  deriving (Show, Eq, Generic)

instance BiDiMethod SessionCommand where
  bidiMethod :: SessionCommand -> Text
  bidiMethod = \case
    New _ -> "session.new"
    Status -> "session.status"
    Subscribe _ -> "session.subscribe"
    Unsubscribe _ -> "session.unsubscribe"
    End -> "session.end"

instance ToJSON SessionCommand where
  toJSON :: SessionCommand -> Value
  toJSON = \case
    New capabilities -> toJSON capabilities
    Subscribe request -> toJSON request
    Unsubscribe params -> toJSON params
    Status -> emptyObj
    End -> emptyObj

-- | Subscription
newtype Subscription = MkSubscription {subscriptionId :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Subscription where
  toJSON :: Subscription -> Value
  toJSON (MkSubscription subId) = String subId

-- | Subscription Request
data SessionSubscriptionRequest = MkSessionSubscriptionRequest
  { events :: [Text],
    contexts :: Maybe [Text],
    userContexts :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionSubscriptionRequest

-- | Unsubscribe Parameters
data SessionUnsubscribeParameters
  = UnsubscribeByID SessionUnsubscribeByIDRequest
  | UnsubscribeByAttributes SessionUnsubscribeByAttributesRequest
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribeParameters

-- | Unsubscribe By ID Request
newtype SessionUnsubscribeByIDRequest = MkSessionUnsubscribeByIDRequest
  { subscriptions :: [Subscription]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribeByIDRequest

-- | Unsubscribe By Attributes Request
data SessionUnsubscribeByAttributesRequest = MkSessionUnsubscribeByAttributesRequest
  { unsubEvents :: [Text],
    unsubContexts :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribeByAttributesRequest

-- ######### Local #########

-- | Session Result
data SessionResult
  = SessionNewResult SessionNewResult
  | SessionStatusResult SessionStatusResult
  | SessionSubscribeResult SessionSubscribeResult
  deriving (Show, Eq, Generic)

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

-- | Session Subscribe Result
newtype SessionSubscribeResult = MkSessionSubscribeResult
  { subscription :: Subscription
  }
  deriving (Show, Eq, Generic)
