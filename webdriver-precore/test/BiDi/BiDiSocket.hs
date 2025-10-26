module BiDi.BiDiSocket 
  ( BiDiSocket (..),
    Channels (..),
    ActiveSubscription (..),
    CommandRequestInfo (..),
    mkChannels,
    mkBiDiSocket,
    sendCommand,
    sendCommand',
    sendCommandNoWait,
    sendCommandNoWait',
    matchedResponse,
    subscribe,
    unsubscribe,
    removeSubscription
  ) where

import Control.Exception (Exception (displayException), SomeException, catch)
import Control.Monad (when)
import Data.Aeson (ToJSON, FromJSON, Value (..), encode, toJSON, Object)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, unpack)
import GHC.Generics (Generic)
import IOUtils (QueLog (..))
import UnliftIO (catchAny, throwIO)
import UnliftIO.STM
import WebDriverPreCore.BiDi.Command
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Event (Subscription (..))
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext,
    Event,
    MatchedResponse (..),
    SessionSubscribeResult (..),
    SessionSubscriptionRequest (..),
    SessionUnsubscribe,
    SubscriptionId (..),
    SubscriptionType (..),
    UserContext,
  )
import WebDriverPreCore.BiDi.Response (JSONDecodeError, ResponseObject (..), parseResponse, displayResponseError)
import WebDriverPreCore.BiDi.Session (SessionUnsubscribe (..))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log)

-- | WebDriver BiDi client with communication channels
data BiDiSocket = MkBiDiSocket
  { nextId :: IO JSUInt,
    send :: forall a. (ToJSON a, Show a) => a -> IO (),
    getNext :: IO (Either JSONDecodeError ResponseObject),
    subscribe ::
      (SessionSubscriptionRequest -> IO SessionSubscribeResult) ->
      Subscription IO ->
      IO SubscriptionId,
    unsubscribe :: (SessionUnsubscribe -> IO Object) -> SubscriptionId -> IO ()
  }

data Channels = MkChannels
  { sendChan :: TChan Value,
    receiveChan :: TChan (Either JSONDecodeError ResponseObject),
    eventChan :: TChan Object,
    counterVar :: TVar JSUInt,
    subscriptions :: TVar [ActiveSubscription IO],
    queueLog :: QueLog
  }

data ActiveSubscription m = MkActiveSubscription
  { subscriptionId :: SubscriptionId,
    subscription :: Subscription m
  }

data CommandRequestInfo = MkCommandRequestInfo
  { id :: JSUInt,
    request :: Value
  }
  deriving (Show, Generic)

mkChannels :: QueLog -> IO Channels
mkChannels queueLog =
  MkChannels
    <$> newTChanIO
    <*> newTChanIO
    <*> newTChanIO
    <*> counterVar
    <*> newTVarIO []
    <*> (pure queueLog)

counterVar :: IO (TVar JSUInt)
counterVar = newTVarIO $ MkJSUInt 0

mkAtomicCounter :: TVar JSUInt -> IO JSUInt
mkAtomicCounter var = atomically $ do
  modifyTVar' var succ
  readTVar var

mkBiDiSocket :: Channels -> BiDiSocket
mkBiDiSocket c =
  MkBiDiSocket
    { nextId = mkAtomicCounter c.counterVar,
      send,
      getNext = atomically $ readTChan c.receiveChan,
      subscribe = subscribe c.subscriptions,
      unsubscribe = unsubscribe c.subscriptions
    }
  where
    send :: forall a. (ToJSON a) => a -> IO ()
    send a = do
      -- make strict so serialisation errors come from here and not in the logger
      let !json = toJSON a
      atomically . writeTChan c.sendChan $ json

sendCommandNoWait' :: forall c r. (ToJSON c, Show c) => BiDiSocket -> JSUInt -> Command c r -> IO CommandRequestInfo
sendCommandNoWait' MkBiDiSocket {send} id' command = do
  let request = commandValue command id'
  (send request)
    `catch` \(e :: SomeException) -> do
      error $
        "Send command failed: \n"
          <> unpack (txt command)
          <> "\n ---- Exception -----\n"
          <> displayException e
  pure $ MkCommandRequestInfo {id = id', request}

sendCommandNoWait :: forall c r. (ToJSON c, Show c) => BiDiSocket -> Command c r -> IO CommandRequestInfo
sendCommandNoWait MkBiDiSocket {send, nextId} command = do
  id' <- nextId
  let request = commandValue command id'
  (send request)
    `catch` \(e :: SomeException) -> do
      error $
        "Send command failed: \n"
          <> unpack (txt command)
          <> "\n ---- Exception -----\n"
          <> displayException e
  pure $ MkCommandRequestInfo {id = id', request}

sendCommand' :: forall c r. (FromJSON r, ToJSON c, Show c) => BiDiSocket -> JSUInt -> Command c r -> IO r
sendCommand' bdm id' command = do
  MkCommandRequestInfo {request} <- sendCommandNoWait' bdm id' command
  matchedResponse' request id'
  where
    matchedResponse' :: Value -> JSUInt -> IO r
    matchedResponse' = matchedResponse command bdm.getNext

sendCommand :: forall c r. (FromJSON r, ToJSON c, Show c) => BiDiSocket -> Command c r -> IO r
sendCommand m@MkBiDiSocket {getNext} command = do
  MkCommandRequestInfo {id = id', request} <- sendCommandNoWait m command
  matchedResponse' request id'
  where
    matchedResponse' :: Value -> JSUInt -> IO r
    matchedResponse' = matchedResponse command getNext

matchedResponse :: forall c r. (FromJSON r, Show c) => Command c r -> IO (Either JSONDecodeError ResponseObject) -> Value -> JSUInt -> IO r
matchedResponse command getNext request id' = do
  response <- getNext
  parseResponse id' response
    & maybe
      ( -- recurse
        matchedResponse command getNext request id'
      )
      ( either
          ( -- format and throw
            error . unpack . displayResponseError command request
          )
          ( -- get response
            pure . (.response)
          )
      )

subscribe ::
  TVar [ActiveSubscription IO] ->
  (SessionSubscriptionRequest -> IO SessionSubscribeResult) ->
  Subscription IO ->
  IO SubscriptionId
subscribe allSubs socketSubscribe subscription = do
  -- subscribe with a dummy id first so we don't miss any messages
  atomically $ subscribeWithId dummySubId
  catchAny
    ( do
        sub <- socketSubscribe subscribeParams
        let subId = sub.subscription
        -- swap in the real id
        atomically $ do
          unsubscribeDummy
          subscribeWithId subId
        pure subId
    )
    ( \e -> do
        -- on error, remove the dummy subscription
        atomically unsubscribeDummy
        throwIO e
    )
  where
    dummySubId = MkSubscriptionId "dummy"

    subscribeWithId :: SubscriptionId -> STM ()
    subscribeWithId subId =
      modifyTVar' allSubs (MkActiveSubscription subId subscription :)

    unsubscribeDummy :: STM ()
    unsubscribeDummy = removeSubscription allSubs dummySubId

    toMaybe = \case
      [] -> Nothing
      xs -> Just xs

    subscribeParams :: SessionSubscriptionRequest
    subscribeParams =
      MkSessionSubscriptionRequest
        { events = case subscription of
            SingleSubscription {subscriptionType} -> [subscriptionType]
            MultiSubscription {subscriptionTypes} -> toList subscriptionTypes,
          contexts = toMaybe subscription.browsingContexts,
          userContexts = toMaybe subscription.userContexts
        }

removeSubscription :: TVar [ActiveSubscription IO] -> SubscriptionId -> STM ()
removeSubscription allSubs subId = modifyTVar' allSubs $ filter ((subId /=) . (.subscriptionId))

unsubscribe :: TVar [ActiveSubscription IO] -> (SessionUnsubscribe -> IO Object) -> SubscriptionId -> IO ()
unsubscribe allSubs socketUnsubscribe subId = do
  socketUnsubscribe $ UnsubscribeByID [subId]
  atomically $ removeSubscription allSubs subId