-- |
-- Module: WebDriverPreCore.BiDiRunnerBase
-- Description: JSON-based BiDi runner for WebDriver
--
-- This module provides a BiDi WebSocket runner that works with JSON Values
-- rather than typed WebDriver commands.
module WebDriverPreCore.BiDiRunnerBase
  ( -- * BiDi Runner
    withBiDiBase,
    withBiDiWithActions,

    -- * Socket Actions
    SocketActions (..),
    Channels (..),

    -- * Message Loops
    MessageLoops (..),
    MessageActions (..),
    loopActions,
    mkMessageActions,

    -- * Channel Actions
    ChannelActions (..),
    mkChannelActions,

    -- * Re-exports
    module WebDriverPreCore.BiDiRunnerBase.Types,
    module WebDriverPreCore.BiDiRunnerBase.Response,
    module WebDriverPreCore.BiDiRunnerBase.Socket,
  )
where

import Control.Exception (Exception (displayException), throw)
import Control.Monad (when)
import Data.Aeson (Object, Value (..), encode, parseJSON, toJSON, withObject, (.:))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text (Text, pack, take, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.WebSockets (ClientApp, Connection, receiveData, sendTextData)
import Network.WebSockets qualified as WS
import UnliftIO (MonadIO, MonadUnliftIO, catchAny, liftIO, throwIO, waitAnyCatch)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.STM (TVar, atomically, readTChan, readTVarIO, writeTChan)
import WebDriverPreCore.BiDiRunnerBase.Response
import WebDriverPreCore.BiDiRunnerBase.Socket
import WebDriverPreCore.BiDiRunnerBase.Types
import Prelude hiding (log, take)

runClient :: forall a m. (MonadIO m) => String -> Int -> String -> ClientApp a -> m a
runClient host port path app =
  liftIO $ WS.runClient host port path app

-- | Logger type alias
type Logger m = Text -> m ()

-- | Null logger
nullLogger :: Applicative m => Logger m
nullLogger = const $ pure ()

-- | Combined channel and socket actions
data ChannelActions m = MkChannelActions
  { messageLoops :: MessageLoops m,
    socketActions :: SocketActions
  }

-- | Message handling actions
data MessageActions m = MkMessageActions
  { send :: Connection -> m (),
    get :: Connection -> m (),
    eventHandler :: m ()
  }

-- | Async message loops
data MessageLoops m = MkMessageLoops
  { sendLoop :: Connection -> m (Async ()),
    getLoop :: Connection -> m (Async ()),
    eventLoop :: m (Async ())
  }

-- | Create channel actions with a logger
mkChannelActions :: (MonadUnliftIO m) => Logger m -> m (ChannelActions m)
mkChannelActions logger = do
  c <- initChannels
  pure $
    MkChannelActions
      { socketActions = mkSocketActions c,
        messageLoops = mkMessageLoops logger c
      }

-- | Run a BiDi session
withBiDiBase ::
  (MonadUnliftIO m) =>
  Maybe (Logger m) ->
  BiDiUrl ->
  (SocketActions -> m ()) ->
  m ()
withBiDiBase mLogger bidiUrl action = do
  let logger = maybe nullLogger id mLogger
  ca <- mkChannelActions logger
  withSocket bidiUrl logger ca.messageLoops $
    action ca.socketActions

-- | Run a BiDi session with custom message actions
withBiDiWithActions ::
  (MonadUnliftIO m) =>
  Maybe (Logger m) ->
  BiDiUrl ->
  (Logger m -> m (ChannelActions m)) ->
  (SocketActions -> m ()) ->
  m ()
withBiDiWithActions mLogger bidiUrl mkActions action = do
  let logger = maybe nullLogger id mLogger
  ca <- mkActions logger
  withSocket bidiUrl logger ca.messageLoops $
    action ca.socketActions

-- | Create message actions for handling WebSocket communication
mkMessageActions :: (MonadUnliftIO m) => Logger m -> Channels m -> MessageActions m
mkMessageActions log' MkChannels {sendChan, receiveChan, eventChan, subscriptions} =
  MkMessageActions
    { send = \conn -> do
        msgToSend <- atomically $ readTChan sendChan
        log' $ "Sending Message: " <> jsonToText msgToSend
        catchLog "Message Send Failed" log' $
          liftIO $ sendTextData conn (BL.toStrict $ encode msgToSend),
      --
      get = \conn -> do
        msg <- liftIO $ receiveData conn
        log' $ "Received raw data: " <> Data.Text.take 100 (decodeUtf8 msg) <> "..."
        let writeReceiveChan = atomically . writeTChan receiveChan
            writeEventChan = atomically . writeTChan eventChan
            r = decodeResponse (BL.fromStrict msg)
        case r of
          Left {} -> writeReceiveChan r
          Right r' -> case r' of
            NoID obj -> writeEventChan obj
            WithID {} -> writeReceiveChan r,
      --
      eventHandler = do
        obj <- atomically $ readTChan eventChan
        log' $ "Event received: " <> jsonToText (toJSON obj)
        applySubscriptions log' obj subscriptions
    }

-- | Create message loops from actions
mkMessageLoops :: (MonadUnliftIO m) => Logger m -> Channels m -> MessageLoops m
mkMessageLoops logger channels =
  loopActions logger $ mkMessageActions logger channels

loopActions :: (MonadUnliftIO m) => Logger m -> MessageActions m -> MessageLoops m
loopActions logger MkMessageActions {..} =
  MkMessageLoops
    { sendLoop = asyncLoop "Sender" . send,
      getLoop = asyncLoop "Receiver" . get,
      eventLoop = asyncLoop "EventHandler" eventHandler
    }
  where
    asyncLoop name action = loopForever logger name action

-- | Run an action forever in a loop
loopForever :: (MonadUnliftIO m) => Logger m -> Text -> m () -> m (Async ())
loopForever logger name action = async go
  where
    go = do
      catchAny action $ \e -> do
        logger $ "Loop " <> name <> " error: " <> pack (displayException e)
        throwIO e
      go

-- | Catch and log exceptions
catchLog :: (MonadUnliftIO m) => Text -> Logger m -> m () -> m ()
catchLog msg logger action =
  catchAny action $ \e ->
    logger $ msg <> ": " <> pack (displayException e)

-- | Run a WebSocket client
withSocket :: (MonadIO m) => BiDiUrl -> Logger m -> MessageLoops m -> m () -> m ()
withSocket pth@MkBiDiUrl {host, port, path} logger messageLoops action = do
  logger $ "Connecting to WebDriver at " <> pack (show pth)
  runClient (unpack host) port (unpack path) $ \conn -> do
    eventLoop <- messageLoops.eventLoop
    getLoop <- messageLoops.getLoop conn
    sendLoop <- messageLoops.sendLoop conn

    logger "WebSocket connection established"

    result <- async action

    (_asy, ethresult) <- waitAnyCatch [getLoop, sendLoop, result, eventLoop]

    traverse_ cancel [getLoop, sendLoop, result, eventLoop]

    ethresult
      & either
        ( \e -> do
            logger $ "One of the BiDi client threads failed: \n" <> pack (displayException e)
            throw e
        )
        pure

-- | Apply subscriptions to an event
applySubscriptions :: (MonadIO m) => Logger m -> Object -> TVar [RegisteredSubscription m] -> m ()
applySubscriptions log' obj subscriptions = do
  case parseEither parseEventProps (Object obj) of
    Left err -> log' $ "Could not parse event properties: " <> pack err
    Right MkEventProps {msgType, method, fullObj, params} -> do
      when (msgType /= "event") $
        log' $
          "Not an event message: " <> msgType
      subs <- readTVarIO subscriptions
      traverse_ (applySubscription (MkSocketSubscriptionType method) params fullObj) ((.subscription) <$> subs)

-- | Event properties parsed from a message
data EventProps = MkEventProps
  { msgType :: Text,
    method :: Text,
    params :: Value,
    fullObj :: Value
  }

parseEventProps :: Value -> Parser EventProps
parseEventProps = withObject "EventProps" $ \o ->
  MkEventProps
    <$> o .: "type"
    <*> o .: "method"
    <*> o .: "params"
    <*> pure (Object o)

-- | Apply a subscription handler to an event
applySubscription :: SocketSubscriptionType -> Value -> Value -> SocketSubscription -> IO ()
applySubscription subType params fullObj = \case
  SingleSubscription {subscriptionType, action} ->
    when (subType == subscriptionType) $
      case parseEither parseJSON params of
        Left _ -> pure () -- Type mismatch, skip
        Right r -> action r
  MultiSubscription {subscriptionTypes, nAction} ->
    when (subType `Set.member` subscriptionTypes) $
      nAction fullObj

-- | Convert JSON to pretty text
jsonToText :: Value -> Text
jsonToText = decodeUtf8 . BL.toStrict . encodePretty
