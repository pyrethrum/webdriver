{-|
Module: WebDriverPreCore.BiDiRunnerBase.Types
Description: Core types for BiDi runner (decoupled from webdriver-precore)

This module provides types needed by the BiDi runner that are independent
of the webdriver-precore type definitions. Some types like JSUInt are
duplicated here to avoid dependencies.
-}
module WebDriverPreCore.BiDiRunnerBase.Types
  ( -- * Core Types
    JSUInt (..),
    
    -- * Socket Types
    SocketCommand (..),
    SocketSubscription (..),
    SocketSubscriptionId (..),
    SocketSubscriptionType (..),
    SocketUnregister (..),
    RegisteredSubscription (..),
    Request (..),
    
    -- * BiDi URL
    BiDiUrl (..),
    parseBiDiUrl,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Utils (JSUInt (..))
import Text.Read (readMaybe)

-- | A command to send over the BiDi socket
data SocketCommand a r = MkSocketCommand
  { method :: a,
    params :: Value
  }
  deriving (Show, Eq)

-- | A subscription handler
data SocketSubscription m where
  SingleSubscription ::
    forall m r.
    (FromJSON r) =>
    { subscriptionType :: SocketSubscriptionType,
      action :: r -> m ()
    } ->
    SocketSubscription m
  MultiSubscription ::
    { subscriptionTypes :: Set SocketSubscriptionType,
      nAction :: Value -> m ()
    } ->
    SocketSubscription m

-- | Subscription identifier
newtype SocketSubscriptionId = MkSocketSubscriptionId {subscriptionId :: Text}
  deriving (Show, Eq, Generic, Ord)
  deriving newtype (FromJSON, ToJSON)

-- | Subscription event type identifier
newtype SocketSubscriptionType = MkSocketSubscriptionType {subscriptionType :: Text}
  deriving (Generic)
  deriving newtype (Show, Eq, Ord)

-- | Request to unregister subscriptions
data SocketUnregister
  = UnregisterById {subscriptionIds :: Set SocketSubscriptionId}
  | UnregisterByAttributes {subscriptionTypes :: Set SocketSubscriptionType}
  deriving (Show, Eq, Generic)

-- | A registered subscription with its ID
data RegisteredSubscription m = MkRegisteredSubscription
  { subscriptionId :: SocketSubscriptionId,
    subscription :: SocketSubscription m
  }

-- | A request with ID for matching responses
data Request = MkRequest
  { id :: JSUInt,
    payload :: Value
  }
  deriving (Show, Generic)

-- | BiDi WebSocket URL components
data BiDiUrl = MkBiDiUrl
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show, Eq)

-- | Parse a WebSocket URL into BiDi components
-- Example: "ws://127.0.0.1:9222/session/abc123"
parseBiDiUrl :: Text -> Maybe BiDiUrl
parseBiDiUrl url = do
  -- Strip ws:// prefix
  rest <- T.stripPrefix "ws://" url
  -- Split host:port from path
  let (hostPort, pathWithSlash) = T.break (== '/') rest
      path = if T.null pathWithSlash then "/" else pathWithSlash
  -- Split host from port
  case T.break (== ':') hostPort of
    (host, portStr) -> do
      port <- readMaybe . T.unpack =<< T.stripPrefix ":" portStr
      pure $ MkBiDiUrl {host, port, path}
