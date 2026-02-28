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
    hoistBiDiRunner,
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
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, Object, toJSON, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Coerce (coerce)
import Data.Set qualified as Set
import Data.Text (Text)
import UnliftIO (MonadUnliftIO, catchAny, throwIO)
import UnliftIO.STM (STM, atomically)
import WebDriverPreCore.BiDiRunnerBase  as B hiding (JSUInt(..))

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
mkBiDiRunner :: (MonadUnliftIO m, MonadThrow m) => SocketActions m -> BiDiRunner m
mkBiDiRunner sa = MkBiDiRunner
  { run = runTypedCommand sa,
    socketActions = sa,
    runWithId = \(MkJSUInt msgId) cmd ->
      B.sendCommand' (coerceSocketActions sa) (MkJSUInt msgId) (commandToSocketCommand cmd),
    runOffSpecWithId = \(MkJSUInt msgId) method params ->
      B.sendCommand' (coerceSocketActions sa) (MkJSUInt msgId) $
        MkSocketCommand method (toJSON params)
  }

-- | Send a typed 'Command' without waiting for a response.
runNoWait :: (MonadUnliftIO m, MonadThrow m) => BiDiRunner m -> Command r -> m Request
runNoWait MkBiDiRunner {socketActions} cmd =
  B.sendCommandNoWait (coerceSocketActions socketActions) (commandToSocketCommand cmd)

-- | Send an off-spec command without waiting for a response.
runOffSpecNoWait :: (MonadUnliftIO m, MonadThrow m) => BiDiRunner m -> Text -> Object -> m Request
runOffSpecNoWait MkBiDiRunner {socketActions} method params =
  B.sendCommandNoWait (coerceSocketActions socketActions) $
    MkSocketCommand method (toJSON params)

-- | Run a BiDi session with typed commands
withBiDi
  :: forall a m. (MonadUnliftIO m, MonadThrow m)
  => Maybe (Text -> m ())  -- ^ Optional logger
  -> BiDiUrl
  -> (BiDiRunner m -> m a)
  -> m a
withBiDi mLogger bidiUrl action =
  withBiDiBase mLogger bidiUrl $ \sa ->
    action (mkBiDiRunner sa)

-- | Execute a typed command
runTypedCommand :: forall m r. (FromJSON r, MonadUnliftIO m, MonadThrow m) => SocketActions m -> Command r -> m r
runTypedCommand sa cmd = do
  let socketCmd = commandToSocketCommand cmd
  sendCommand (coerceSocketActions sa) socketCmd
    `catchAny` \e -> case fromException e :: Maybe ResponseException of
      Just (BiDIError errorValue) -> 
        throwIO . parseFailToWDException $ MkParseFailure "BiDi error response" errorValue
      _ -> throwIO e

-- | Convert a typed Command to a SocketCommand
commandToSocketCommand :: Command r -> SocketCommand Text r
commandToSocketCommand cmd = MkSocketCommand
  { method = toCommandText cmd.method,
    params = toJSON cmd.params
  }
  where
    toCommandText :: CommandMethod -> Text
    toCommandText = \case
      KnownCommand k -> knownCommandToText k
      OffSpecCommand c -> coerce c

-- | Coerce socket actions between the typed and base versions
coerceSocketActions :: SocketActions m -> B.SocketActions m
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

    mkRegistration :: Subscription m -> SocketSubscription m
    mkRegistration = \case
      P.SingleSubscription {subscriptionType, action} ->
        B.SingleSubscription
          { subscriptionType = toSocketSubType subscriptionType,
            action
          }
      s' -> case s' of
        P.MultiSubscription {nAction} ->
          B.MultiSubscription
            { subscriptionTypes = socketSubtypes s',
              nAction = \v -> case parseEither parseJSON v of
                Left _ -> pure ()
                Right r -> nAction r
            }
        P.OffSpecSubscription {nValueAction} ->
          B.MultiSubscription
            { subscriptionTypes = socketSubtypes s',
              nAction = nValueAction
            }
      where
        socketSubtypes s = Set.fromList $ toSocketSubType <$> s.subscriptionTypes

    dummySubId = MkSocketSubscriptionId "dummy"

    subscribeWithId :: SocketSubscriptionId -> STM ()
    subscribeWithId subId =
      (coerceSocketActions sa).registerSubscription (mkRegistration subscription) subId

    removeDummySub :: STM ()
    removeDummySub = 
      (coerceSocketActions sa).unregisterSubscription $ 
        UnregisterById $ Set.singleton dummySubId

-- | Unsubscribe from events
unsubscribe :: MonadUnliftIO m => SocketActions m -> (SessionUnsubscribe -> m ()) -> SessionUnsubscribe -> m ()
unsubscribe sa callUnsubscribe unsub = do
  callUnsubscribe unsub
  atomically $ (coerceSocketActions sa).unregisterSubscription (toSocketUnregister unsub)
  where
    toSocketUnregister :: SessionUnsubscribe -> SocketUnregister
    toSocketUnregister = \case
      UnsubscribeById {subscriptions} ->
        UnregisterById . Set.fromList $ 
          MkSocketSubscriptionId . coerce <$> subscriptions
      UnsubscribeByAttributes {unsubEvents} ->
        UnregisterByAttributes . Set.fromList $ 
          toSocketSubType <$> unsubEvents

-- | Convert SubscriptionType to SocketSubscriptionType
toSocketSubType :: SubscriptionType -> SocketSubscriptionType
toSocketSubType = MkSocketSubscriptionType . subscriptionTypeToText

-- | Convert a 'BiDiRunner' from one monad to another.
--
-- The @lift@ function converts command actions from @m@ to @n@ (e.g., 'liftIO').
-- The @unlift@ function converts subscription callbacks from @n@ to @m@
-- (e.g., @runRIO env@), so that callbacks stored in the event system are
-- invoked in the base monad @m@.
hoistBiDiRunner ::
  (forall a. m a -> n a) ->
  (forall a. n a -> m a) ->
  BiDiRunner m ->
  BiDiRunner n
hoistBiDiRunner lift' unlift' MkBiDiRunner {run = mRun, socketActions = mSA, runWithId = mRWI, runOffSpecWithId = mROS} =
  MkBiDiRunner
    { run = lift' . mRun,
      socketActions = hoistSA unlift' mSA,
      runWithId = \i cmd -> lift' (mRWI i cmd),
      runOffSpecWithId = \i m p -> lift' (mROS i m p)
    }
  where
    hoistSA :: (forall a. n a -> m a) -> B.SocketActions m -> B.SocketActions n
    hoistSA unlift B.MkSocketActions {registerSubscription = mRegSub, ..} =
      B.MkSocketActions
        { registerSubscription = \sub subId -> mRegSub (unliftSub unlift sub) subId,
          ..
        }

    unliftSub :: (forall a. n a -> m a) -> B.SocketSubscription n -> B.SocketSubscription m
    unliftSub unlift = \case
      B.SingleSubscription {subscriptionType, action} ->
        B.SingleSubscription {subscriptionType, action = unlift . action}
      B.MultiSubscription {subscriptionTypes, nAction} ->
        B.MultiSubscription {subscriptionTypes, nAction = unlift . nAction}
