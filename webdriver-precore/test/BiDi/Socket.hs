module BiDi.Socket
  ( SocketSubscription (..),
    SocketSubscriptionId (..),
    SocketCommand (..),
    SocketActions (..),
    SocketUnregister (..),
    SocketSubscriptionType (..),
    Channels (..),
    RegisteredSubscription (..),
    Request (..),
    initChannels,
    mkSocketActions,
    sendCommand,
    sendCommand',
    sendCommandNoWait,
    sendCommandNoWait',
    matchedRequest,
    counterVar,
    mkAtomicCounter,
  )
where

import BiDi.Response
  ( MatchedResponse (..),
    ResponseObject (..),
    parseResponse,
  )
---

import Control.Exception (throw)
import Data.Aeson (FromJSON, Object, ToJSON, Value (..), object, toJSON, (.=))
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
import Utils (txt)
import WebDriverPreCore.BiDi.Protocol (JSUInt (..), JSONEncodeException (..))
import Prelude hiding (id, log)

-- TODO: ON LIBRARY SPLIT - test / fix / handle - timeouts / exceptions thrown from channel threads
-- eg test ux when serialisation error in get set handle subscription

data SocketCommand a r = MkSocketCommand
  { method :: a,
    params :: Value
  }
  deriving (Show, Eq)

data SocketSubscription where
  SingleSubscription ::
    forall r.
    (FromJSON r) =>
    { subscriptionType :: SocketSubscriptionType,
      action :: r -> IO ()
    } ->
    SocketSubscription
  MultiSubscription ::
    { subscriptionTypes :: Set SocketSubscriptionType,
      nAction :: Value -> IO ()
    } ->
    SocketSubscription

data SocketUnregister
  = UnregisterById
      {subscriptionIds :: Set SocketSubscriptionId}
  | UnregisterByAttributes
      {subscriptionTypes :: Set SocketSubscriptionType}
  deriving (Show, Eq, Generic)

newtype SocketSubscriptionId = MkSocketSubscriptionId {subscriptionId :: Text}
  deriving (Show, Eq, Generic, Ord, FromJSON, ToJSON)

newtype SocketSubscriptionType = MkSocketSubscriptionType {subscriptionType :: Text}
  deriving (Generic)
  deriving newtype (Show, Eq, Ord)

-- | WebDriver BiDi client with communication channels
data SocketActions = MkBiDiSocket
  { nextId :: STM JSUInt,
    send :: forall a. (ToJSON a, Show a) => a -> STM (),
    getNext :: STM (Either JSONEncodeException ResponseObject),
    registerSubscription :: SocketSubscription -> SocketSubscriptionId -> STM (),
    unregisterSubscription :: SocketUnregister -> STM ()
  }

data Channels = MkChannels
  { sendChan :: TChan Value,
    receiveChan :: TChan (Either JSONEncodeException ResponseObject),
    eventChan :: TChan Object,
    counterVar :: TVar JSUInt,
    subscriptions :: TVar [RegisteredSubscription IO]
  }

data RegisteredSubscription m = MkRegisteredSubscription
  { subscriptionId :: SocketSubscriptionId,
    subscription :: SocketSubscription
  }

data Request = MkRequest
  { id :: JSUInt,
    payload :: Value
  }
  deriving (Show, Generic)

initChannels :: IO Channels
initChannels =
  MkChannels
    <$> newTChanIO
    <*> newTChanIO
    <*> newTChanIO
    <*> counterVar
    <*> newTVarIO []

counterVar :: IO (TVar JSUInt)
counterVar = newTVarIO $ MkJSUInt 0

mkAtomicCounter :: TVar JSUInt -> STM JSUInt
mkAtomicCounter var = do
  modifyTVar' var succ
  readTVar var

mkSocketActions :: Channels -> SocketActions
mkSocketActions c =
  MkBiDiSocket
    { nextId = mkAtomicCounter c.counterVar,
      send,
      getNext = readTChan c.receiveChan,
      registerSubscription = \sub subid -> registerSubscription c.subscriptions sub subid,
      unregisterSubscription = unregisterSubscription c.subscriptions
    }
  where
    send :: forall a. (ToJSON a) => a -> STM ()
    send a = do
      -- make strict so serialisation errors come from here and not in the logger
      let !json = toJSON a
      writeTChan c.sendChan json

sendCommandNoWait' :: forall a r. (Show a, ToJSON a) => SocketActions -> SocketCommand a r -> JSUInt -> IO Request
sendCommandNoWait' MkBiDiSocket {send} command id = do
  (atomically $ send payload)
    `catch` \(e :: SomeException) -> do
      fail $
        "Send command failed: \n"
          <> unpack (txt command)
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

sendCommandNoWait :: forall a r. (Show a, ToJSON a) => SocketActions -> SocketCommand a r -> IO Request
sendCommandNoWait sa command =
  atomically sa.nextId >>= sendCommandNoWait' sa command

sendCommand' :: forall a r. (FromJSON r, Show a, ToJSON a) => SocketActions -> JSUInt -> SocketCommand a r -> IO r
sendCommand' sa id' command = do
  MkRequest {payload} <- sendCommandNoWait' sa command id'
  matchedRequest sa.getNext payload id'

sendCommand :: forall a r. (FromJSON r, Show a, ToJSON a) => SocketActions -> SocketCommand a r -> IO r
sendCommand m@MkBiDiSocket {getNext} command = do
  MkRequest {id = id', payload} <- sendCommandNoWait m command
  matchedRequest getNext payload id'

matchedRequest :: forall r. (FromJSON r) => STM (Either JSONEncodeException ResponseObject) -> Value -> JSUInt -> IO r
matchedRequest getNext request id' = do
  response <- atomically getNext
  parseResponse id' response
    & maybe
      (matchedRequest getNext request id')
      (either throw (pure . (.response)))

registerSubscription ::
  TVar [RegisteredSubscription IO] ->
  SocketSubscription ->
  SocketSubscriptionId ->
  STM ()
registerSubscription allSubs subscription subId = do
  modifyTVar' allSubs (MkRegisteredSubscription subId subscription :)

unregisterSubscription :: TVar [RegisteredSubscription IO] -> SocketUnregister -> STM ()
unregisterSubscription allSubs unsub =
  case unsub of
    UnregisterById {subscriptionIds = subs} ->
      modifyTVar' allSubs $ filter (\s -> not (s.subscriptionId `elem` subs))
    UnregisterByAttributes {subscriptionTypes = unregTypes} ->
      modifyTVar' allSubs $
        -- remove multi-subscriptions with empty types and single subscriptions matching unsubscribed events
        filter (not . subscriptionIsEmpty . (.subscription))
          -- remove subscriptions matching any of the unsubscribed events in multi subscriptions
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
