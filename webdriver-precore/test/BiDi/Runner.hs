module BiDi.Runner
  ( ChannelActions (..),
    MessageLoops (..),
    mkChannelActions,
    mkFaiChannelActions,
    withBiDi,
    withBidiFailTest,
    unsubscribe,
    subscribe,
  )
where

import BiDi.BiDiSocket as S
  ( Channels (..),
    RegisteredSubscription (..),
    SocketActions (..),
    SocketSubscription (..),
    SocketSubscriptionId (..),
    SocketSubscriptionType (..),
    SocketUnregister (..),
    counterVar,
    initChannels,
    mkAtomicCounter,
    mkSocketActions,
  )
import Control.Exception (Exception (displayException), throw)
import Control.Monad (when, (>=>))
import Data.Aeson (FromJSON, Object, Value (..), encode, toJSON, withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text as T (Text, pack, take, unpack)
import Data.Word (Word64)
import GHC.Generics (Generic)
import IOUtils (DemoActions (..), Logger (..), catchLog, loopForever)
import Network.WebSockets (Connection, receiveData, runClient, sendTextData)
import UnliftIO (catchAny, throwIO, waitAnyCatch)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.STM
import WebDriverPreCore.BiDi.BiDiUrl (BiDiUrl (..))
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol as P
  ( SessionSubscribeResult (..),
    SessionSubscibe (..),
    SessionUnsubscribe,
    Subscription (..),
    SubscriptionId (..),
    SubscriptionType (..),
    subscriptionTypeToText,
  )
import WebDriverPreCore.BiDi.Response (ResponseObject (..), decodeResponse)
import WebDriverPreCore.BiDi.Session (SessionUnsubscribe (..))
import WebDriverPreCore.Internal.AesonUtils (jsonToText, objToText, parseThrow)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, print, putStrLn)

-- | Run WebDriver BiDi client and return a client interface
withBiDi :: DemoActions -> BiDiUrl -> (DemoActions -> SocketActions -> IO ()) -> IO ()
withBiDi = withBidi' mkChannelActions

-- | Run WebDriver BiDi client and return a client interface
withBidi' :: (Logger -> IO ChannelActions) -> DemoActions -> BiDiUrl -> (DemoActions -> SocketActions -> IO ()) -> IO ()
withBidi'
  mkChActions
  demoActions@MkDemoActions {logTxt}
  bidiUrl
  action = do
    ca <- mkChActions logger
    withSocket bidiUrl logger ca.messageLoops $
      action demoActions ca.socketActions
    where
      logger = MkLogger logTxt

mkChannelActions :: Logger -> IO ChannelActions
mkChannelActions logger = do
  c <- initChannels
  pure $
    MkChannelActions
      { socketActions = mkSocketActions c,
        messageLoops = demoMessageLoops logger c
      }

data ChannelActions = MkChannelActions
  { messageLoops :: MessageLoops,
    socketActions :: SocketActions
  }

data MessageActions = MkMessageActions
  { send :: Connection -> IO (),
    get :: Connection -> IO (),
    eventHandler :: IO ()
  }

demoMessageActions :: Logger -> Channels -> MessageActions
demoMessageActions logger MkChannels {sendChan, receiveChan, eventChan, subscriptions} =
  MkMessageActions
    { send = \conn -> do
        msgToSend <- atomically $ readTChan sendChan
        log $ "Sending Message: " <> jsonToText msgToSend
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend)),
      --
      get = \conn -> do
        msg <- receiveData conn
        log $ "Received raw data: " <> T.take 100 (txt msg) <> "..."
        log $ "Received raw data: " <> txt msg
        let writeReceiveChan = atomically . writeTChan receiveChan
            writeEventChan = atomically . writeTChan eventChan
            r = decodeResponse msg
        case r of
          Left {} -> writeReceiveChan r
          Right r' -> case r' of
            NoID obj -> writeEventChan obj
            WithID {} -> writeReceiveChan r,
      --
      eventHandler = do
        obj <- atomically $ readTChan eventChan
        log $ "Event received: " <> jsonToText (toJSON obj)
        applySubscriptions log obj subscriptions
    }
  where
    log :: Text -> IO ()
    log = logger.log

--
data EventProps = MkEventProps
  { msgType :: Text,
    method :: Text,
    params :: Value,
    fullObj :: Value
  }
  deriving (Show, Generic)

instance FromJSON EventProps where
  parseJSON :: Value -> Parser EventProps
  parseJSON v =
    withObject
      "EventProps"
      ( \o ->
          MkEventProps
            <$> o .: "type"
            <*> o .: "method"
            <*> o .: "params"
            <*> pure v
      )
      v

unsubscribe :: SocketActions -> (SessionUnsubscribe -> IO ()) -> SessionUnsubscribe -> IO ()
unsubscribe sa callUnsubscribe unsub = do
  callUnsubscribe unsub
  atomically . sa.unregisterSubscription $ toSocketUnregister unsub
  where
    toSocketUnregister :: SessionUnsubscribe -> SocketUnregister
    toSocketUnregister = \case
      UnsubscribeById {subscriptions} ->
        UnregisterById . Set.fromList $ MkSocketSubscriptionId . coerce <$> subscriptions
      UnsubscribeByAttributes {unsubEvents} ->
        UnregisterByAttributes . Set.fromList $ MkSocketSubscriptionType . subscriptionTypeToText <$> unsubEvents

subscribe ::
  SocketActions ->
  (SessionSubscibe -> IO SessionSubscribeResult) ->
  Subscription IO ->
  IO SubscriptionId
subscribe sa callSubscribe subscription = do
  -- subscribe with a dummy id first so we don't miss any messages
  atomically $ subscribeWithId dummySubId
  catchAny
    ( do
        subId <- callSubscribe $ mkRequest subscription
        -- swap in the real id
        atomically $ do
          removeDummySub
          subscribeWithId $ coerce subId.subscription
        pure subId.subscription
    )
    ( \e -> do
        -- on error, remove the dummy subscription
        atomically removeDummySub
        throwIO e
    )
  where
    mkRequest :: Subscription IO -> SessionSubscibe
    mkRequest s = case s of
      P.SingleSubscription
        { subscriptionType
        } ->
          MkSessionSubscribe
            { events = [coerce subscriptionType],
              contexts,
              userContexts
            }
      P.MultiSubscription
        { subscriptionTypes
        } ->
          MkSessionSubscribe
            { events = coerce <$> subscriptionTypes,
              contexts,
              userContexts
            }
      P.UnknownSubscription
        { subscriptionTypes
        } ->
          MkSessionSubscribe
            { events = coerce <$> subscriptionTypes,
              contexts,
              userContexts
            }
      where
        contexts = maybeList s.browsingContexts
        userContexts = maybeList s.userContexts
        maybeList :: [a] -> Maybe [a]
        maybeList = \case
          [] -> Nothing
          xs -> Just xs

    mkRegistration :: Subscription IO -> SocketSubscription
    mkRegistration = \case
      P.SingleSubscription
        { subscriptionType,
          action
        } ->
          S.SingleSubscription
            { subscriptionType = toSocketSubType subscriptionType,
              action
            }
      s' -> case s' of
        P.MultiSubscription
          { nAction
          } ->
            S.MultiSubscription
              { subscriptionTypes = socketSubtypes,
                nAction = parseThrow ("Could not parse Event when executing MultiSubscription action for: " <> socketSubtypesTxt) >=> nAction
              }
        P.UnknownSubscription
          { nValueAction
          } ->
            S.MultiSubscription
              { subscriptionTypes = socketSubtypes,
                nAction = parseThrow ("Could not parse Value when executing UnknownSubscription action for: " <> socketSubtypesTxt) >=> nValueAction
              }
        where
          socketSubtypes = Set.fromList $ toSocketSubType <$> s'.subscriptionTypes
          socketSubtypesTxt = txt (Set.toList socketSubtypes)

    dummySubId = MkSocketSubscriptionId "dummy"

    subscribeWithId :: SocketSubscriptionId -> STM ()
    subscribeWithId =
      sa.registerSubscription (mkRegistration subscription)

    removeDummySub :: STM ()
    removeDummySub = sa.unregisterSubscription . UnregisterById $ Set.singleton dummySubId

applySubscriptions :: (Text -> IO ()) -> Object -> TVar [RegisteredSubscription IO] -> IO ()
applySubscriptions _log obj subscriptions = do
  MkEventProps {msgType, method, fullObj, params} <-
    parseThrow "Could not parse event properties" (Object obj)
  when (msgType /= "event") $
    fail . unpack $
      "Event message expected. This is not an event message: "
        <> msgType
        <> "\n"
        <> objToText obj

  -- log $ "Parsed event: " <> txt eventProps
  subs <- readTVarIO subscriptions
  traverse_ (applySubscription (coerce method) params fullObj) ((.subscription) <$> subs)

applySubscription :: SocketSubscriptionType -> Value -> Value -> SocketSubscription -> IO ()
applySubscription subType params fullObj =
  \case
    S.SingleSubscription {subscriptionType, action} ->
      when (subType == subscriptionType) $
        parseThrow ("could not parse Event for: " <> txt subType) params
          >>= action
    S.MultiSubscription {subscriptionTypes, nAction} ->
      when (subType `Set.member` subscriptionTypes) $
        nAction fullObj

toSocketSubType :: SubscriptionType -> SocketSubscriptionType
toSocketSubType = MkSocketSubscriptionType . subscriptionTypeToText

loopActions :: Logger -> MessageActions -> MessageLoops
loopActions logger MkMessageActions {..} =
  MkMessageLoops
    { sendLoop = asyncLoop "Sender" . send,
      getLoop = asyncLoop "Receiver" . get,
      eventLoop = asyncLoop "EventHandler" eventHandler
    }
  where
    asyncLoop = loopForever logger.log

data MessageLoops = MkMessageLoops
  { sendLoop :: Connection -> IO (Async ()),
    getLoop :: Connection -> IO (Async ()),
    eventLoop :: IO (Async ())
  }

demoMessageLoops :: Logger -> Channels -> MessageLoops
demoMessageLoops logger channels =
  loopActions logger $ demoMessageActions logger channels

withSocket :: BiDiUrl -> Logger -> MessageLoops -> IO () -> IO ()
withSocket
  pth@MkBiDiUrl {host, port, path}
  logger
  messageLoops
  action =
    do
      log $ "Connecting to WebDriver at " <> txt pth
      runClient (unpack host) port (unpack path) $ \conn -> do
        eventLoop <- messageLoops.eventLoop
        getLoop <- messageLoops.getLoop conn
        sendLoop <- messageLoops.sendLoop conn

        log "WebSocket connection established"

        result <- async action

        (_asy, ethresult) <- waitAnyCatch [getLoop, sendLoop, result, eventLoop]

        -- cancelMany not reexported by UnliftIO
        traverse_ cancel [getLoop, sendLoop, result, eventLoop]

        ethresult
          & either
            ( \e -> do
                log $ "One of the BiDi client threads failed: \n" <> pack (displayException e)
                throw e
            )
            pure
    where
      log :: Text -> IO ()
      log = coerce logger

-- ####################################################################################
-- ########################  Fail Actions - for testing errors ########################
-- ####################################################################################

-- | Run WebDriver BiDi client and return a client interface
withBidiFailTest ::
  Word64 ->
  Word64 ->
  Word64 ->
  DemoActions ->
  BiDiUrl ->
  (DemoActions -> SocketActions -> IO ()) ->
  IO ()
withBidiFailTest failSendCount failGetCount failEventCount =
  withBidi' (mkFaiChannelActions failSendCount failGetCount failEventCount)

mkFaiChannelActions ::
  Word64 ->
  Word64 ->
  Word64 ->
  Logger ->
  IO ChannelActions
mkFaiChannelActions failSendCount failGetCount failEventCount logger = do
  c <- initChannels
  messageLoops <- failMessageLoops logger c failSendCount failGetCount failEventCount
  pure $
    MkChannelActions
      { socketActions = mkSocketActions c,
        messageLoops
      }

failMessageLoops ::
  Logger ->
  Channels ->
  Word64 ->
  Word64 ->
  Word64 ->
  IO MessageLoops
failMessageLoops logger channels failSendCount failGetCount failEventCount =
  loopActions logger
    <$> failMessageActions
      (demoMessageActions logger channels)
      failSendCount
      failGetCount
      failEventCount

failMessageActions :: MessageActions -> Word64 -> Word64 -> Word64 -> IO MessageActions
failMessageActions a failSendCount failGetCount failEventCount =
  do
    send <- failAction "send" failSendCount a.send
    get <- failAction "get" failGetCount a.get
    eventHandler' <- failAction "eventhandler" failEventCount $ const a.eventHandler
    pure $
      MkMessageActions
        { send,
          get,
          eventHandler = eventHandler' 1
        }

failAction :: Text -> Word64 -> (a -> IO ()) -> IO ((a -> IO ()))
failAction lbl failCallCount action = do
  counterVar' <- counterVar
  let counter = mkAtomicCounter counterVar'
  pure $ \a -> do
    n <- atomically $ counter
    if (coerce n :: Word64) == failCallCount
      then do
        fail $ "Forced failure for testing: " <> unpack lbl <> " (call #" <> show n <> ")"
      else do
        action a
