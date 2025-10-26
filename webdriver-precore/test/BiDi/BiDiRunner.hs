module BiDi.BiDiRunner 
  ( withCommands,
    BiDiClientParams (..),
    MessageLoops (..),
    mkDemoBiDiClientParams,
    mkFailBidiClientParams,
    withNewBiDiSession,
    withBiDiClient,
    httpSession,
    httpBidiCapabilities,
    newHttpSession
  ) where

import BiDi.BiDiActions (BiDiActions, mkActions)
import BiDi.BiDiSocket (BiDiSocket, Channels(..), ActiveSubscription (..), mkChannels, mkBiDiSocket, counterVar, mkAtomicCounter)
import Config (Config, loadConfig)
import Const (Timeout)
import Control.Exception (Exception (displayException), throw)
import Control.Monad (when)
import Data.Aeson (FromJSON, Object, Value (..), encode, toJSON, withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, pack, take, unpack)
import Data.Text.IO (putStrLn)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), deleteSession, newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import IOUtils (DemoUtils, QueLog (..), bidiDemoUtils, catchLog, loopForever)
import Network.WebSockets (Connection, receiveData, runClient, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import UnliftIO (bracket, waitAnyCatch)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.STM
import WebDriverPreCore.BiDi.BiDiUrl (BiDiUrl (..), getBiDiUrl)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Event (Subscription (..))
import WebDriverPreCore.BiDi.Protocol
  ( SubscriptionType (..)
  )
import WebDriverPreCore.BiDi.Response (ResponseObject (..), decodeResponse)
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (jsonToText, objToText, parseThrow)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, print, putStrLn)

withCommands :: BiDiClientParams -> (DemoUtils -> BiDiActions -> IO ()) -> IO ()
withCommands params action =
  withNewBiDiSession params $ \utils -> action utils . mkActions





mkDemoBiDiClientParams :: Maybe QueLog -> Timeout -> IO BiDiClientParams
mkDemoBiDiClientParams mQueLog pauseTimeout = do
  let qLog = fromMaybe (MkQueLog $ const $ pure ()) mQueLog
  c <- mkChannels qLog
  pure $
    MkBiDiClientParams
      { biDiMethods = mkBiDiSocket c,
        queueLog = qLog,
        messageLoops = demoMessageLoops mQueLog c,
        demoUtils = bidiDemoUtils qLog pauseTimeout
      }

-- TODO Add fail for event test
mkFailBidiClientParams ::
  Maybe QueLog ->
  Timeout ->
  Word64 ->
  Word64 ->
  Word64 ->
  IO BiDiClientParams
mkFailBidiClientParams mQueLog pauseTimeout failSendCount failGetCount failEventCount = do
  let qLog = fromMaybe (MkQueLog $ const $ pure ()) mQueLog
  c <- mkChannels qLog
  messageLoops <- failMessageLoops mQueLog c failSendCount failGetCount failEventCount
  pure $
    MkBiDiClientParams
      { biDiMethods = mkBiDiSocket c,
        queueLog = qLog,
        messageLoops,
        demoUtils = bidiDemoUtils qLog pauseTimeout
      }

withNewBiDiSession :: BiDiClientParams -> (DemoUtils -> BiDiSocket -> IO ()) -> IO ()
withNewBiDiSession params action =
  bracket
    httpSession
    (deleteSession . (.sessionId))
    \s' -> do
      let bidiUrl = getBiDiUrl s' & either (error . T.unpack) id
      withBiDiClient params bidiUrl action





data BiDiClientParams = MkBiDiClientParams
  { queueLog :: QueLog,
    messageLoops :: MessageLoops,
    biDiMethods :: BiDiSocket,
    demoUtils :: DemoUtils
  }

-- | Run WebDriver BiDi client and return a client interface
withBiDiClient :: BiDiClientParams -> BiDiUrl -> (DemoUtils -> BiDiSocket -> IO ()) -> IO ()
withBiDiClient
  MkBiDiClientParams
    { biDiMethods,
      queueLog,
      messageLoops,
      demoUtils
    }
  bidiUrl
  action =
    withClient bidiUrl queueLog messageLoops $ action demoUtils biDiMethods

failAction :: Text -> Word64 -> (a -> IO ()) -> IO ((a -> IO ()))
failAction lbl failCallCount action = do
  counterVar' <- counterVar
  let counter = mkAtomicCounter counterVar'
  pure $ \a -> do
    n <- counter
    if (coerce n :: Word64) == failCallCount
      then do
        error $ "Forced failure for testing: " <> unpack lbl <> " (call #" <> show n <> ")"
      else do
        action a

data MessageActions = MkMessageActions
  { send :: Connection -> IO (),
    get :: Connection -> IO (),
    eventHandler :: IO ()
  }

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

demoMessageActions :: Maybe QueLog -> Channels -> MessageActions
demoMessageActions mQueLog MkChannels {sendChan, receiveChan, eventChan, subscriptions} =
  MkMessageActions
    { send = \conn -> do
        msgToSend <- atomically $ readTChan sendChan
        qLog $ "Sending Message: " <> jsonToText msgToSend
        catchLog
          "Message Send Failed"
          qLog
          (sendTextData conn (BL.toStrict $ encode msgToSend)),
      --
      get = \conn -> do
        msg <- receiveData conn
        qLog $ "Received raw data: " <> T.take 100 (txt msg) <> "..."
        qLog $ "Received raw data: " <> txt msg
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
        qLog $ "Event received: " <> jsonToText (toJSON obj)
        applySubscriptions qLog obj subscriptions
    }
  where
    qLog :: Text -> IO ()
    qLog = maybe (const $ pure ()) coerce mQueLog

data EventProps = MkEventProps
  { msgType :: Text,
    method :: SubscriptionType,
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

applySubscriptions :: (Text -> IO ()) -> Object -> TVar [ActiveSubscription IO] -> IO ()
applySubscriptions _log obj subscriptions = do
  MkEventProps {msgType, method, params, fullObj} <-
    parseThrow "Could not parse event properties" (Object obj)
  when (msgType /= "event") $
    fail . unpack $
      "Event message expected. This is not an event message: "
        <> msgType
        <> "\n"
        <> objToText obj

  -- log $ "Parsed event: " <> txt eventProps
  subs <- readTVarIO subscriptions
  traverse_ (applySubscription method params fullObj) subs

applySubscription :: SubscriptionType -> Value -> Value -> ActiveSubscription IO -> IO ()
applySubscription subType params fullObj sub =
  case sub.subscription of
    SingleSubscription {subscriptionType, action} ->
      when (subscriptionType == subType) $ do
        prms <- parseThrow ("could not parse event params (in SingleSubscription) for " <> txt subscriptionType) params
        action prms
    MultiSubscription {subscriptionTypes, nAction} -> do
      when (subType `elem` subscriptionTypes) $ do
        prms <- parseThrow ("could not parse Event for (in MultiSubscription) for " <> txt subType) fullObj
        nAction prms

loopActions :: QueLog -> MessageActions -> MessageLoops
loopActions ql MkMessageActions {..} =
  MkMessageLoops
    { sendLoop = asyncLoop "Sender" . send,
      getLoop = asyncLoop "Receiver" . get,
      eventLoop = asyncLoop "EventHandler" eventHandler
    }
  where
    asyncLoop = loopForever ql.queueLog

data MessageLoops = MkMessageLoops
  { sendLoop :: Connection -> IO (Async ()),
    getLoop :: Connection -> IO (Async ()),
    eventLoop :: IO (Async ())
  }

demoMessageLoops :: Maybe QueLog -> Channels -> MessageLoops
demoMessageLoops mQueLog channels =
  loopActions channels.queueLog $ demoMessageActions mQueLog channels

failMessageLoops ::
  Maybe QueLog ->
  Channels ->
  Word64 ->
  Word64 ->
  Word64 ->
  IO MessageLoops
failMessageLoops mQueLog channels failSendCount failGetCount failEventCount =
  loopActions channels.queueLog
    <$> failMessageActions
      (demoMessageActions mQueLog channels)
      failSendCount
      failGetCount
      failEventCount

withClient :: BiDiUrl -> QueLog -> MessageLoops -> IO () -> IO ()
withClient
  pth@MkBiDiUrl {host, port, path}
  queueLog
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
                -- the logger is dead now so print direct to the console instead
                putStrLn $ "One of the BiDi client threads failed: \n" <> pack (displayException e)
                throw e
            )
            pure
    where
      log :: Text -> IO ()
      log = coerce queueLog

httpSession :: IO SessionResponse
httpSession = loadConfig >>= newSessionFull . httpBidiCapabilities

-- Bidi capabilities request is the same as regular HTTP capabilities,
-- but with the `webSocketUrl` field set to `True`
httpBidiCapabilities :: Config -> Http.FullCapabilities
httpBidiCapabilities cfg =
  (httpFullCapabilities cfg)
    { Http.alwaysMatch =
        Just $
          (httpCapabilities cfg)
            { Caps.webSocketUrl = Just True
            }
    }

newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull
