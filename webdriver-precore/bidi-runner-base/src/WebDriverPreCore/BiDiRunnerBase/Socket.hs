{-|
Module: WebDriverPreCore.BiDiRunnerBase.Socket
Description: WebSocket channel management for BiDi

This module provides the low-level WebSocket channel management for BiDi
communication, decoupled from webdriver-precore types.
-}
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
    matchedRequest,
    
    -- * Counter Utilities
    counterVar,
    mkAtomicCounter,
  )
where

import Control.Exception (throw)
import Data.Aeson (FromJSON, Object, ToJSON, Value (..), encode, object, toJSON, (.=))
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import UnliftIO.Exception (Exception (..), SomeException, catch)
import UnliftIO.STM
  ( STM,
    TChan,
    TVar,
    atomically,
    modifyTVar',
    newTChanIO,
    newTVarIO,
    readTChan,
    readTVar,
    writeTChan,
  )
import WebDriverPreCore.BiDiRunnerBase.Types
import WebDriverPreCore.BiDiRunnerBase.Response
import Prelude hiding (id, log)

-- | Communication channels for BiDi
data Channels = MkChannels
  { sendChan :: TChan Value,
    receiveChan :: TChan (Either JSONEncodeException ResponseObject),
    eventChan :: TChan Object,
    counterVar :: TVar JSUInt,
    subscriptions :: TVar [RegisteredSubscription IO]
  }

-- | Actions available on the socket
data SocketActions = MkSocketActions
  { nextId :: STM JSUInt,
    send :: forall a. (ToJSON a, Show a) => a -> STM (),
    getNext :: STM (Either JSONEncodeException ResponseObject),
    registerSubscription :: SocketSubscription -> SocketSubscriptionId -> STM (),
    unregisterSubscription :: SocketUnregister -> STM ()
  }

-- | Initialize communication channels
initChannels :: IO Channels
initChannels =
  MkChannels
    <$> newTChanIO
    <*> newTChanIO
    <*> newTChanIO
    <*> counterVar
    <*> newTVarIO []

-- | Create a counter variable
counterVar :: IO (TVar JSUInt)
counterVar = newTVarIO $ MkJSUInt 0

-- | Create an atomic counter
mkAtomicCounter :: TVar JSUInt -> STM JSUInt
mkAtomicCounter var = do
  modifyTVar' var succ
  readTVar var

-- | Create socket actions from channels
mkSocketActions :: Channels -> SocketActions
mkSocketActions c =
  MkSocketActions
    { nextId = mkAtomicCounter c.counterVar,
      send,
      getNext = readTChan c.receiveChan,
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
sendCommandNoWait' :: forall a r. (Show a, ToJSON a) 
  => SocketActions 
  -> SocketCommand a r 
  -> JSUInt 
  -> IO Request
sendCommandNoWait' MkSocketActions {send} command id = do
  (atomically $ send payload)
    `catch` \(e :: SomeException) -> do
      fail $
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
sendCommandNoWait :: forall a r. (Show a, ToJSON a) 
  => SocketActions 
  -> SocketCommand a r 
  -> IO Request
sendCommandNoWait sa command =
  atomically sa.nextId >>= sendCommandNoWait' sa command

-- | Send a command with specific ID and wait for response
sendCommand' :: forall a r. (FromJSON r, Show a, ToJSON a) 
  => SocketActions 
  -> JSUInt 
  -> SocketCommand a r 
  -> IO r
sendCommand' sa id' command = do
  MkRequest {payload} <- sendCommandNoWait' sa command id'
  matchedRequest sa.getNext payload id'

-- | Send a command and wait for response (auto-generates ID)
sendCommand :: forall a r. (FromJSON r, Show a, ToJSON a) 
  => SocketActions 
  -> SocketCommand a r 
  -> IO r
sendCommand sa@MkSocketActions {getNext} command = do
  MkRequest {id = id', payload} <- sendCommandNoWait sa command
  matchedRequest getNext payload id'

-- | Wait for a response matching the given ID
matchedRequest :: forall r. (FromJSON r) 
  => STM (Either JSONEncodeException ResponseObject) 
  -> Value 
  -> JSUInt 
  -> IO r
matchedRequest getNext request id' = do
  response <- atomically getNext
  parseResponse id' response
    & maybe
      (matchedRequest getNext request id')
      (either throw (pure . (.response)))

-- | Register a subscription
registerSubscription' ::
  TVar [RegisteredSubscription IO] ->
  SocketSubscription ->
  SocketSubscriptionId ->
  STM ()
registerSubscription' allSubs subscription subId = do
  modifyTVar' allSubs (MkRegisteredSubscription subId subscription :)

-- | Unregister subscriptions
unregisterSubscription' :: TVar [RegisteredSubscription IO] -> SocketUnregister -> STM ()
unregisterSubscription' allSubs unsub =
  case unsub of
    UnregisterById {subscriptionIds = subs} ->
      modifyTVar' allSubs $ filter (\s -> not (s.subscriptionId `elem` subs))
    UnregisterByAttributes {subscriptionTypes = unregTypes} ->
      modifyTVar' allSubs $
        filter (not . subscriptionIsEmpty . (.subscription))
          . fmap removeSubFromMultiSocketRegistration
      where
        removeSubFromMultiSocketRegistration :: RegisteredSubscription IO -> RegisteredSubscription IO
        removeSubFromMultiSocketRegistration regSub@MkRegisteredSubscription {subscription} =
          regSub
            { subscription = removeSubscriptionFromMulti subscription
            }

        removeSubscriptionFromMulti :: SocketSubscription -> SocketSubscription
        removeSubscriptionFromMulti = \case
          MultiSubscription {subscriptionTypes = subTypes, nAction} ->
            MultiSubscription
              { subscriptionTypes = subTypes `Set.difference` unregTypes,
                nAction
              }
          s@SingleSubscription {} -> s

        subscriptionIsEmpty :: SocketSubscription -> Bool
        subscriptionIsEmpty = \case
          MultiSubscription {subscriptionTypes} -> Set.null subscriptionTypes
          SingleSubscription {subscriptionType} -> subscriptionType `Set.member` unregTypes
