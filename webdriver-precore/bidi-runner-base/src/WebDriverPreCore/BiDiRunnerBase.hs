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

    -- * BiDi Resource (acquire/release)
    BiDiResource (..),
    acquireBiDiBase,
    releaseBiDiBase,

    -- * Socket Actions
    SocketActions (..),
    Channels (..),

    -- * Message Loops
    MessageLoops (..),
    MessageActions (..),
    loopActions,
    mkMessageActions,

    -- * Channel ActionsWebDriverPreCore.BiDiRunnerBase
    ChannelActions (..),
    mkChannelActions,

    -- * Logger helpers
    Logger,
    nullLogger,

    -- * Re-exports
    module WebDriverPreCore.BiDiRunnerBase.Types,
    module WebDriverPreCore.BiDiRunnerBase.Response,
    module WebDriverPreCore.BiDiRunnerBase.Socket,
    module WebDriverPreCore.Types.BiDiUrl,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (when)
import Data.Aeson (Object, Value (..), encode, parseJSON, toJSON, withObject, (.:))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Set qualified as Set
import Data.Text (Text, pack, take, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.Socket qualified as NS
  ( Socket,
    SocketType (Stream),
    AddrInfo (addrAddress, addrFamily, addrSocketType),
    ShutdownCmd (ShutdownBoth),
    connect,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    shutdown,
    socket,
  )
import Network.WebSockets (Connection, receiveData, sendTextData)
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as WSStream
import UnliftIO
  ( MonadIO,
    MonadUnliftIO,
    bracket,
    catchAny,
    catchIO,
    liftIO,
    throwIO,
    throwString,
    waitAnyCatch,
  )
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.STM (TVar, atomically, readTChan, readTVarIO, writeTChan)
import WebDriverPreCore.BiDiRunnerBase.Response
import WebDriverPreCore.BiDiRunnerBase.Socket
import WebDriverPreCore.BiDiRunnerBase.Types
import WebDriverPreCore.Types.BaseTypes (Logger, nullLogger)
import WebDriverPreCore.Types.BiDiUrl
import Prelude hiding (log, take)

-- | Combined channel and socket actions
data ChannelActions m = MkChannelActions
  { messageLoops :: MessageLoops m,
    socketActions :: SocketActions m
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

-- | An acquired BiDi resource: the 'SocketActions' for issuing commands, the
-- running message-loop 'Async's (for status and fail-fast reporting), and a
-- close action that shuts down the transport and loops.
--
-- Acquire with 'acquireBiDiBase', release with 'releaseBiDiBase' (or hold the
-- resource across many tests and close it in a test framework hook).
data BiDiResource m = MkBiDiResource
  { bidiSocketActions :: SocketActions m,
    bidiLoops :: [Async ()],
    bidiClose :: m ()
  }

-- | Open the WebSocket and start the send/get/event loops.
-- Returns the loop handles and the close action.
openBiDi ::
  forall m.
  (MonadUnliftIO m) =>
  Logger m ->
  BiDiUrl ->
  MessageLoops m ->
  m ([Async ()], m ())
openBiDi log' bidiUrl@MkBiDiUrl {host, port, path} MkMessageLoops {getLoop, sendLoop, eventLoop} = do
  log' $ "Connecting to WebDriver at " <> pack (show bidiUrl)
  (sock, stream, conn) <- liftIO $ openClientConn host port path
  log' "WebSocket connection established"
  asyncSendLoop <- sendLoop conn
  asyncGetLoop <- getLoop conn
  asyncEventLoop <- eventLoop
  let loops = [asyncSendLoop, asyncGetLoop, asyncEventLoop]
  pure
    ( loops,
      do
        -- Best-effort friendly close; "peer already gone" is expected noise.
        catchLog "sendClose failed (ignoring)" log' $
          liftIO $ WS.sendClose conn ("" :: BL.ByteString)
        -- Wake a reader blocked in recv, then release the fd.
        ignoreIO log' "shutdown" $
          liftIO $ NS.shutdown sock NS.ShutdownBoth
        ignoreIO log' "stream close" $
          liftIO $ WSStream.close stream
        -- The loops are now unblocked, so cancellation completes promptly.
        traverse_ cancel loops
    )

-- | Create a BiDi session and return its resource handle.
acquireBiDiBase :: (MonadUnliftIO m) => Logger m -> BiDiUrl -> m (BiDiResource m)
acquireBiDiBase logger bidiUrl = do
  ca <- mkChannelActions logger
  (loops, close') <- openBiDi logger bidiUrl ca.messageLoops
  pure
    MkBiDiResource
      { bidiSocketActions = ca.socketActions,
        bidiLoops = loops,
        bidiClose = close'
      }

-- | Release a 'BiDiResource'.
releaseBiDiBase :: BiDiResource m -> m ()
releaseBiDiBase = (.bidiClose)

-- | Run a BiDi session (bracket form, kept for convenience).
withBiDiBase ::
  forall a m.
  (MonadUnliftIO m) =>
  Logger m ->
  BiDiUrl ->
  (SocketActions m -> m a) ->
  m a
withBiDiBase logger bidiUrl action =
  bracket (acquireBiDiBase logger bidiUrl) releaseBiDiBase $
    runBiDi logger action

-- | Run a BiDi session with custom message actions (bracket form).
withBiDiWithActions ::
  (MonadUnliftIO m) =>
  Logger m ->
  BiDiUrl ->
  (Logger m -> m (ChannelActions m)) ->
  (SocketActions m -> m a) ->
  m a
withBiDiWithActions logger bidiUrl mkActions action =
  bracket acquire releaseBiDiBase $
    runBiDi logger action
  where
    acquire = do
      ca <- mkActions logger
      (loops, close') <- openBiDi logger bidiUrl ca.messageLoops
      pure
        MkBiDiResource
          { bidiSocketActions = ca.socketActions,
            bidiLoops = loops,
            bidiClose = close'
          }

-- | Run an action against an acquired resource, failing fast and reporting if
-- any message loop dies (mirrors the old 'withSocket' behaviour).
runBiDi ::
  forall a m.
  (MonadUnliftIO m) =>
  Logger m ->
  (SocketActions m -> m a) ->
  BiDiResource m ->
  m a
runBiDi logger action r = do
  actionAsync <- async $ action r.bidiSocketActions
  let asyncs :: [Async (Maybe a)]
      asyncs = (Just <$> actionAsync) : ((Nothing <$) <$> r.bidiLoops)
  (_completed, ethresult) <- waitAnyCatch asyncs
  cancel actionAsync
  case ethresult of
    Left e -> do
      logger $ "One of the BiDi client threads failed: \n" <> pack (displayException e)
      throwIO e
    Right (Just a) -> pure a
    Right Nothing ->
      throwString "BiDi client threads did not return a result, likely due to WebSocket closure."

-- | Create message actions for handling WebSocket communication
mkMessageActions :: (MonadUnliftIO m) => Logger m -> Channels m -> MessageActions m
mkMessageActions log' MkChannels {sendChan, receiveChan, eventChan, subscriptions} =
  MkMessageActions
    { send = \conn -> do
        msgToSend <- atomically $ readTChan sendChan
        log' $ "Sending Message: " <> jsonToText msgToSend
        catchLog "Message Send Failed" log'
          $ liftIO
          $ sendTextData conn (BL.toStrict $ encode msgToSend),
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

-- | Open a raw TCP socket and perform the WebSocket client handshake.
openClientConn :: Text -> Int -> Text -> IO (NS.Socket, WSStream.Stream, Connection)
openClientConn host' port' path' = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just (unpack host')) (Just (show port'))
  addr <- case addrs of
    a : _ -> pure a
    [] ->
      ioError . userError $
        "openClientConn: no address for " <> unpack host' <> ":" <> show port'
  sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
  NS.connect sock (NS.addrAddress addr)
  stream <- WSStream.makeSocketStream sock
  conn <-
    WS.newClientConnection
      stream
      (unpack host')
      (unpack path')
      WS.defaultConnectionOptions
      []
  pure (sock, stream, conn)

-- | Run an IO cleanup action, swallowing only 'IOException's (logging them).
-- Any other exception propagates, so bugs are not silently masked.
ignoreIO :: (MonadUnliftIO m) => Logger m -> Text -> m () -> m ()
ignoreIO log' what action =
  catchIO action $ \e ->
    log' $ what <> " failed (ignoring): " <> pack (displayException e)

-- | Apply subscriptions to an event
applySubscriptions :: (MonadIO m) => Logger m -> Object -> TVar [RegisteredSubscription m] -> m ()
applySubscriptions log' obj subscriptions = do
  case parseEither parseEventProps (Object obj) of
    Left err -> log' $ "Could not parse event properties: " <> pack err
    Right MkEventProps {msgType, method, fullObj, params} -> do
      when (msgType /= "event")
        $ log'
        $ "Not an event message: " <> msgType
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
applySubscription :: (Monad m) => SocketSubscriptionType -> Value -> Value -> SocketSubscription m -> m ()
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
