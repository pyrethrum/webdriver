-- |
-- Module: WebDriverPreCore.BiDiRunnerBase.Socket
-- Description: WebSocket channel management for BiDi
--
-- This module provides the low-level WebSocket channel management for BiDi
-- communication, decoupled from webdriver-precore types.
module WebDriverPreCore.BiDiRunnerBase.Socket
  ( -- * Socket Actions
    SocketActions (..),
    Channels (..),

    -- * Channel Management
    initChannels,
    mkSocketActions,

    -- * Command Execution
    sendCommand,
    sendCommand',
    sendCommandNoWait,
    sendCommandNoWait',

    -- * Counter Utilities
    counterVar,
    mkAtomicCounter,
  )
where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson (FromJSON, Object, ToJSON, Value (..), object, toJSON, (.=))
import Data.Function ((&))
import Data.Set qualified as Set
import UnliftIO (MonadIO, MonadUnliftIO, finally, timeout)
import UnliftIO.Exception (Exception (..), SomeException, catch)
import UnliftIO.STM
  ( STM,
    TChan,
    TMVar,
    TVar,
    atomically,
    modifyTVar',
    newTChanIO,
    newTMVarIO,
    newTVarIO,
    readTChan,
    readTVar,
    takeTMVar,
    tryPutTMVar,
    writeTChan,
  )
import WebDriverPreCore.BiDiRunnerBase.Response
import WebDriverPreCore.BiDiRunnerBase.Types
import WebDriverPreCore.Utils.Timeout (Timeout (..), seconds)
import WebDriverPreCore.Utils.Utils (db)
import Prelude hiding (id, log)

-- | Communication channels for BiDi
data Channels m = MkChannels
  { sendChan :: TChan Value,
    receiveChan :: TChan (Either JSONEncodeException ResponseObject),
    eventChan :: TChan Object,
    counterVar :: TVar JSUInt,
    subscriptions :: TVar [RegisteredSubscription m],
    commandSlot :: TMVar ()
  }

-- | Actions available on the socket
data SocketActions m = MkSocketActions
  { nextId :: STM JSUInt,
    send :: forall a. (ToJSON a, Show a) => a -> STM (),
    getNext :: STM (Either JSONEncodeException ResponseObject),
    takeCommandSlot :: STM (),
    releaseCommandSlot :: STM (),
    registerSubscription :: SocketSubscription m -> SocketSubscriptionId -> STM (),
    unregisterSubscription :: SocketUnregister -> STM ()
  }

-- | Initialise communication channels
initChannels :: (MonadIO m) => m (Channels m)
initChannels =
  MkChannels
    <$> newTChanIO
    <*> newTChanIO
    <*> newTChanIO
    <*> counterVar
    <*> newTVarIO []
    <*> newTMVarIO ()

-- | Create a counter variable
counterVar :: (MonadIO m) => m (TVar JSUInt)
counterVar = newTVarIO $ MkJSUInt 0

-- | Create an atomic counter
mkAtomicCounter :: TVar JSUInt -> STM JSUInt
mkAtomicCounter var = do
  modifyTVar' var succ
  readTVar var

-- | Create socket actions from channels
mkSocketActions :: Channels m -> SocketActions m
mkSocketActions c =
  MkSocketActions
    { nextId = mkAtomicCounter c.counterVar,
      send,
      getNext = readTChan c.receiveChan,
      takeCommandSlot = takeTMVar c.commandSlot,
      releaseCommandSlot = void (tryPutTMVar c.commandSlot ()),
      registerSubscription = \sub subid -> registerSubscription' c.subscriptions sub subid,
      unregisterSubscription = unregisterSubscription' c.subscriptions
    }
  where
    send :: forall a. (ToJSON a) => a -> STM ()
    send a = do
      -- make strict so serialisation errors come from here
      let !json = toJSON a
      writeTChan c.sendChan json

-- | Send a command without waiting for response
sendCommandNoWait' ::
  forall a m r.
  (Show a, ToJSON a, MonadUnliftIO m, MonadThrow m) =>
  SocketActions m ->
  SocketCommand a r ->
  JSUInt ->
  m Request
sendCommandNoWait' MkSocketActions {send} command id = do
  (atomically $ send payload)
    `catch` \(e :: SomeException) -> do
      throwM . userError $
        "Send command failed: \n"
          <> show command
          <> "\n ---- Exception -----\n"
          <> displayException e
  pure $ MkRequest {id = id, payload}
  where
    payload =
      object
        [ "id" .= id,
          "method" .= command.method,
          "params" .= command.params
        ]

-- | Send a command without waiting (auto-generates ID)
--
-- TODO: fire-and-forget commands bypass the multiplexing guard in
-- 'sendCommand'\/'sendCommand''. Two known consequences to address later:
--
--   1. Their responses accumulate in 'receiveChan' until a later waiting
--      command drains them (unbounded channel growth).
--   2. Error responses are silently discarded, so a failed fire-and-forget
--      command is never surfaced.
sendCommandNoWait ::
  forall a m r.
  (Show a, ToJSON a, MonadUnliftIO m, MonadThrow m) =>
  SocketActions m ->
  SocketCommand a r ->
  m Request
sendCommandNoWait sa command =
  atomically sa.nextId >>= sendCommandNoWait' sa command

-- | Send a command with specific ID and wait for response
sendCommand' ::
  forall a m r.
  (FromJSON r, Show a, ToJSON a, MonadUnliftIO m, MonadThrow m) =>
  SocketActions m ->
  JSUInt ->
  SocketCommand a r ->
  m r
sendCommand' sa@MkSocketActions {getNext, takeCommandSlot, releaseCommandSlot} id' command =
  do
    atomically takeCommandSlot
    do
      MkRequest {payload} <- sendCommandNoWait' sa command id'
      matchedRequest getNext payload id'
    `finally` atomically releaseCommandSlot

-- | Send a command and wait for response (auto-generates ID)
sendCommand ::
  forall a m r.
  (FromJSON r, Show a, ToJSON a, MonadUnliftIO m, MonadThrow m) =>
  SocketActions m ->
  SocketCommand a r ->
  m r
sendCommand sa command = do
   atomically sa.nextId >>= \id' -> sendCommand' sa id' command


-- | Default time (microseconds) to wait for a command response before
-- failing with 'BiDiTimeoutError'. Kept as a single named constant so it can
-- be threaded through as a parameter later if needed.
defaultCommandResponseTimeout :: Timeout
defaultCommandResponseTimeout = 10 * seconds

-- | Wait for a response matching the given ID, failing after
-- 'defaultCommandResponseTimeout' if the driver never answers.
matchedRequest ::
  forall m r.
  (FromJSON r, MonadUnliftIO m) =>
  STM (Either JSONEncodeException ResponseObject) ->
  Value ->
  JSUInt ->
  m r
matchedRequest getNext request id' = do
  timeout defaultCommandResponseTimeout.microseconds getMatched
    >>= maybe
      (throw $ BiDiTimeoutError {request, ms})
      pure
  where
    ms = defaultCommandResponseTimeout.microseconds `div` 1000
    getMatched = do
      response <- atomically getNext
      parseResponse id' response
        & maybe getMatched (either throw (pure . (.response)))

-- | Register a subscription
registerSubscription' ::
  TVar [RegisteredSubscription m] ->
  SocketSubscription m ->
  SocketSubscriptionId ->
  STM ()
registerSubscription' allSubs subscription subId = do
  modifyTVar' allSubs (MkRegisteredSubscription subId subscription :)

-- | Unregister subscriptions
unregisterSubscription' :: TVar [RegisteredSubscription m] -> SocketUnregister -> STM ()
unregisterSubscription' allSubs unsub =
  case unsub of
    UnregisterById {subscriptionIds = subs} ->
      modifyTVar' allSubs $ filter (\s -> not (s.subscriptionId `elem` subs))
    UnregisterByAttributes {subscriptionTypes = unregTypes} ->
      modifyTVar' allSubs $
        filter (not . subscriptionIsEmpty . (.subscription))
          . fmap removeSubFromMultiSocketRegistration
      where
        removeSubFromMultiSocketRegistration :: RegisteredSubscription m -> RegisteredSubscription m
        removeSubFromMultiSocketRegistration regSub@MkRegisteredSubscription {subscription} =
          regSub
            { subscription = removeSubscriptionFromMulti subscription
            }

        removeSubscriptionFromMulti :: SocketSubscription m -> SocketSubscription m
        removeSubscriptionFromMulti = \case
          MultiSubscription {subscriptionTypes = subTypes, nAction} ->
            MultiSubscription
              { subscriptionTypes = subTypes `Set.difference` unregTypes,
                nAction
              }
          s@SingleSubscription {} -> s

        subscriptionIsEmpty :: SocketSubscription m -> Bool
        subscriptionIsEmpty = \case
          MultiSubscription {subscriptionTypes} -> Set.null subscriptionTypes
          SingleSubscription {subscriptionType} -> subscriptionType `Set.member` unregTypes
