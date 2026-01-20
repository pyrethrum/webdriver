{-|
Module: WebDriverPreCore.BiDiRunner
Description: Typed BiDi runner for WebDriver commands

This module provides a typed BiDi runner that works with webdriver-precore
Command types, built on top of the JSON-based runner in BiDiRunnerBase.
-}
module WebDriverPreCore.BiDiRunner
  ( -- * BiDi Runner
    withBiDi,
    BiDiRunner (..),
    mkBiDiRunner,
    
    -- * Subscription Management
    subscribe,
    unsubscribe,
    
    -- * Re-exports from base
    BiDiUrl (..),
    parseBiDiUrl,
    SocketActions (..),
    ResponseException (..),
  )
where

import Control.Exception (throw)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, Value (..), object, toJSON, (.=), parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text (Text)
import UnliftIO (catchAny, throwIO)
import UnliftIO.STM (STM, atomically)
import WebDriverPreCore.BiDiRunnerBase
  ( BiDiUrl (..),
    ChannelActions (..),
    SocketActions (..),
    mkChannelActions,
    parseBiDiUrl,
    withBiDiBase,
  )
import WebDriverPreCore.BiDiRunnerBase qualified as Base
import WebDriverPreCore.BiDiRunnerBase.Response (ResponseException (..))
import WebDriverPreCore.BiDiRunnerBase.Socket qualified as Socket
import WebDriverPreCore.BiDiRunnerBase.Types qualified as BaseTypes
import WebDriverPreCore.BiDi.Protocol as P
  ( Command (..),
    CommandMethod (..),
    JSUInt (..),
    OffSpecCommand (..),
    SessionSubscribeResult (..),
    SessionSubscibe (..),
    SessionUnsubscribe (..),
    Subscription (..),
    SubscriptionId (..),
    SubscriptionType (..),
    subscriptionTypeToText,
    knownCommandToText,
  )
import Prelude hiding (log)

-- | Typed BiDi runner
data BiDiRunner = MkBiDiRunner
  { -- | Execute a typed command
    run :: forall r. (FromJSON r) => Command r -> IO r,
    -- | Get the underlying socket actions
    socketActions :: SocketActions
  }

-- | Create a typed BiDi runner from socket actions
mkBiDiRunner :: SocketActions -> BiDiRunner
mkBiDiRunner sa = MkBiDiRunner
  { run = runTypedCommand sa,
    socketActions = sa
  }

-- | Run a BiDi session with typed commands
withBiDi 
  :: Maybe (Text -> IO ())  -- ^ Optional logger
  -> BiDiUrl 
  -> (BiDiRunner -> IO ()) 
  -> IO ()
withBiDi mLogger bidiUrl action =
  withBiDiBase mLogger bidiUrl $ \sa ->
    action (mkBiDiRunner sa)

-- | Execute a typed command
runTypedCommand :: forall r. (FromJSON r) => SocketActions -> Command r -> IO r
runTypedCommand sa cmd = do
  let socketCmd = commandToSocketCommand cmd
  Socket.sendCommand (coerceSocketActions sa) socketCmd

-- | Convert a typed Command to a SocketCommand
commandToSocketCommand :: Command r -> BaseTypes.SocketCommand Text r
commandToSocketCommand cmd = BaseTypes.MkSocketCommand
  { method = toCommandText cmd.method,
    params = toJSON cmd.params
  }
  where
    toCommandText :: CommandMethod -> Text
    toCommandText = \case
      KnownCommand k -> knownCommandToText k
      OffSpecCommand (P.MkOffSpecCommand cmdText) -> cmdText

-- | Coerce socket actions between the typed and base versions
coerceSocketActions :: SocketActions -> Base.SocketActions
coerceSocketActions = coerce

-- | Subscribe to events with a typed handler
subscribe ::
  SocketActions ->
  (SessionSubscibe -> IO SessionSubscribeResult) ->
  Subscription IO ->
  IO SubscriptionId
subscribe sa callSubscribe subscription = do
  -- Subscribe with a dummy ID first
  atomically $ subscribeWithId dummySubId
  catchAny
    ( do
        subId <- callSubscribe $ mkRequest subscription
        atomically $ do
          removeDummySub
          subscribeWithId $ coerce subId.subscription
        pure subId.subscription
    )
    ( \e -> do
        atomically removeDummySub
        throwIO e
    )
  where
    mkRequest :: Subscription IO -> SessionSubscibe
    mkRequest s = case s of
      P.SingleSubscription {subscriptionType} ->
        MkSessionSubscribe
          { events = [coerce subscriptionType],
            contexts,
            userContexts
          }
      P.MultiSubscription {subscriptionTypes} ->
        MkSessionSubscribe
          { events = coerce <$> subscriptionTypes,
            contexts,
            userContexts
          }
      P.OffSpecSubscription {subscriptionTypes} ->
        MkSessionSubscribe
          { events = coerce <$> subscriptionTypes,
            contexts,
            userContexts
          }
      where
        contexts = maybeList s.browsingContexts
        userContexts = maybeList s.userContexts
        maybeList = \case
          [] -> Nothing
          xs -> Just xs

    mkRegistration :: Subscription IO -> BaseTypes.SocketSubscription
    mkRegistration = \case
      P.SingleSubscription {subscriptionType, action} ->
        BaseTypes.SingleSubscription
          { subscriptionType = toSocketSubType subscriptionType,
            action
          }
      s' -> case s' of
        P.MultiSubscription {nAction} ->
          BaseTypes.MultiSubscription
            { subscriptionTypes = socketSubtypes s',
              nAction = \v -> case parseEither parseJSON v of
                Left _ -> pure ()
                Right r -> nAction r
            }
        P.OffSpecSubscription {nValueAction} ->
          BaseTypes.MultiSubscription
            { subscriptionTypes = socketSubtypes s',
              nAction = nValueAction
            }
      where
        socketSubtypes s = Set.fromList $ toSocketSubType <$> s.subscriptionTypes

    dummySubId = BaseTypes.MkSocketSubscriptionId "dummy"

    subscribeWithId :: BaseTypes.SocketSubscriptionId -> STM ()
    subscribeWithId subId =
      (coerceSocketActions sa).registerSubscription (mkRegistration subscription) subId

    removeDummySub :: STM ()
    removeDummySub = 
      (coerceSocketActions sa).unregisterSubscription $ 
        BaseTypes.UnregisterById $ Set.singleton dummySubId

-- | Unsubscribe from events
unsubscribe :: SocketActions -> (SessionUnsubscribe -> IO ()) -> SessionUnsubscribe -> IO ()
unsubscribe sa callUnsubscribe unsub = do
  callUnsubscribe unsub
  atomically $ (coerceSocketActions sa).unregisterSubscription (toSocketUnregister unsub)
  where
    toSocketUnregister :: SessionUnsubscribe -> BaseTypes.SocketUnregister
    toSocketUnregister = \case
      UnsubscribeById {subscriptions} ->
        BaseTypes.UnregisterById . Set.fromList $ 
          BaseTypes.MkSocketSubscriptionId . coerce <$> subscriptions
      UnsubscribeByAttributes {unsubEvents} ->
        BaseTypes.UnregisterByAttributes . Set.fromList $ 
          toSocketSubType <$> unsubEvents

-- | Convert SubscriptionType to SocketSubscriptionType  
toSocketSubType :: SubscriptionType -> BaseTypes.SocketSubscriptionType
toSocketSubType = BaseTypes.MkSocketSubscriptionType . subscriptionTypeToText
