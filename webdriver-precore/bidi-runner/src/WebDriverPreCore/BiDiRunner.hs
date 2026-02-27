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
        -- * Low-level commands
    runNoWait,
    runOffSpecNoWait,
        -- * Subscription Management
    subscribe,
    unsubscribe,
    
    -- * Re-exports from base
    BiDiUrl (..),
    parseBiDiUrl,
    SocketActions (..),
    ResponseException (..),
    Request,
  )
where

import Control.Exception (fromException)
import Data.Aeson (FromJSON, Object, toJSON, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Coerce (coerce)
import Data.Set qualified as Set
import Data.Text (Text)
import UnliftIO (MonadUnliftIO, catchAny, throwIO)
import UnliftIO.STM (STM, atomically)
import WebDriverPreCore.BiDiRunnerBase
  ( BiDiUrl (..),
    SocketActions (..),
    parseBiDiUrl,
    withBiDiBase,
  )
import WebDriverPreCore.BiDiRunnerBase qualified as Base
import WebDriverPreCore.BiDiRunnerBase.Response (ResponseException (..))
import WebDriverPreCore.BiDiRunnerBase.Socket qualified as Socket
import WebDriverPreCore.BiDiRunnerBase.Types (Request)
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
    ParseFailure(..),
    subscriptionTypeToText,
    knownCommandToText,
    parseFailToWDException, 
  )
import Prelude hiding (log)

-- | Typed BiDi runner
data BiDiRunner m = MkBiDiRunner
  { -- | Execute a typed command
    run :: forall r. (FromJSON r) => Command r -> m r,
    -- | Get the underlying socket actions
    socketActions :: SocketActions m,
    -- | Execute a typed command with an explicit message ID
    runWithId :: forall r. (FromJSON r) => JSUInt -> Command r -> m r,
    -- | Send an off-spec command with an explicit message ID
    runOffSpecWithId :: JSUInt -> Text -> Object -> m Object
  }

-- | Create a typed BiDi runner from socket actions
mkBiDiRunner :: (MonadUnliftIO m, MonadFail m) => SocketActions m -> BiDiRunner m
mkBiDiRunner sa = MkBiDiRunner
  { run = runTypedCommand sa,
    socketActions = sa,
    runWithId = \(MkJSUInt msgId) cmd ->
      Socket.sendCommand' (coerceSocketActions sa) (BaseTypes.MkJSUInt msgId) (commandToSocketCommand cmd),
    runOffSpecWithId = \(MkJSUInt msgId) method params ->
      Socket.sendCommand' (coerceSocketActions sa) (BaseTypes.MkJSUInt msgId) $
        BaseTypes.MkSocketCommand method (toJSON params)
  }

-- | Send a typed 'Command' without waiting for a response.
runNoWait :: (MonadUnliftIO m, MonadFail m) => BiDiRunner m -> Command r -> m Request
runNoWait MkBiDiRunner {socketActions} cmd =
  Socket.sendCommandNoWait (coerceSocketActions socketActions) (commandToSocketCommand cmd)

-- | Send an off-spec command without waiting for a response.
runOffSpecNoWait :: (MonadUnliftIO m, MonadFail m) => BiDiRunner m -> Text -> Object -> m Request
runOffSpecNoWait MkBiDiRunner {socketActions} method params =
  Socket.sendCommandNoWait (coerceSocketActions socketActions) $
    BaseTypes.MkSocketCommand method (toJSON params)

-- | Run a BiDi session with typed commands
withBiDi
  :: (MonadUnliftIO m, MonadFail m)
  => Maybe (Text -> m ())  -- ^ Optional logger
  -> BiDiUrl
  -> (BiDiRunner m -> m ())
  -> m ()
withBiDi mLogger bidiUrl action =
  withBiDiBase mLogger bidiUrl $ \sa ->
    action (mkBiDiRunner sa)

-- | Execute a typed command
runTypedCommand :: forall m r. (FromJSON r, MonadUnliftIO m, MonadFail m) => SocketActions m -> Command r -> m r
runTypedCommand sa cmd = do
  let socketCmd = commandToSocketCommand cmd
  Socket.sendCommand (coerceSocketActions sa) socketCmd
    `catchAny` \e -> case fromException e :: Maybe ResponseException of
      Just (BiDIError errorValue) -> 
        throwIO . parseFailToWDException $ MkParseFailure "BiDi error response" errorValue
      _ -> throwIO e

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
coerceSocketActions :: SocketActions m -> Base.SocketActions m
coerceSocketActions = coerce

-- | Subscribe to events with a typed handler
subscribe ::
  forall m.
  MonadUnliftIO m =>
  SocketActions m ->
  (SessionSubscibe -> m SessionSubscribeResult) ->
  Subscription m ->
  m SubscriptionId
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
    mkRequest :: Subscription m -> SessionSubscibe
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

    mkRegistration :: Subscription m -> BaseTypes.SocketSubscription m
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
unsubscribe :: MonadUnliftIO m => SocketActions m -> (SessionUnsubscribe -> m ()) -> SessionUnsubscribe -> m ()
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
