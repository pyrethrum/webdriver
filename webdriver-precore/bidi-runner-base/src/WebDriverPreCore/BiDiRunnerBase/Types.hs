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
    Request (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import WebDriverPreCore.Types.BaseTypes (JSUInt (..))
import Text.Read (readMaybe)
import UnliftIO (Exception)

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


