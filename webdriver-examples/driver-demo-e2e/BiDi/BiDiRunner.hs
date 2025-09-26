module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Exception (Exception (displayException), Handler (..), SomeException, catch, catches, throw)
import Control.Monad (void)
import Data.Aeson (FromJSON, Object, ToJSON, Value, encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Text as T (Text, pack, unpack)
import Data.Text.IO (putStrLn)
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), deleteSession, newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import IOUtils (DemoUtils, Logger (..), bidiDemoUtils, mkLogger)
import Network.WebSockets (Connection, receiveData, runClient, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import UnliftIO (AsyncCancelled, bracket, throwIO, waitAnyCatch)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.STM
import WebDriverPreCore.BiDi.API qualified as P
import WebDriverPreCore.BiDi.BiDiUrl (BiDiUrl (..), getBiDiUrl)
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Event (Subscription)
import WebDriverPreCore.BiDi.Protocol
  ( Activate,
    AddDataCollector,
    AddDataCollectorResult,
    AddIntercept,
    AddInterceptResult,
    AddPreloadScript,
    AddPreloadScriptResult,
    BrowsingContext,
    CallFunction,
    CaptureScreenshot,
    CaptureScreenshotResult,
    ClientWindowInfo,
    Close,
    ContinueRequest,
    ContinueResponse,
    ContinueWithAuth,
    Create,
    CreateUserContext,
    DeleteCookies,
    DeleteCookiesResult,
    Disown,
    DisownData,
    Evaluate,
    EvaluateResult,
    Event,
    FailRequest,
    GetClientWindowsResult,
    GetCookies,
    GetCookiesResult,
    GetData,
    GetDataResult,
    GetRealms,
    GetRealmsResult,
    GetTree,
    GetTreeResult,
    GetUserContextsResult,
    HandleUserPrompt,
    LocateNodes,
    LocateNodesResult,
    Navigate,
    NavigateResult,
    PerformActions,
    Print,
    PrintResult,
    ProvideResponse,
    ReleaseActions,
    Reload,
    RemoveDataCollector,
    RemoveIntercept,
    RemovePreloadScript,
    RemoveUserContext,
    SessionNewResult,
    SessionStatusResult,
    SessionSubscribeResult,
    SessionSubscriptionRequest,
    SessionUnsubscribe,
    SetCacheBehavior,
    SetClientWindowState,
    SetCookie,
    SetCookieResult,
    SetFiles,
    SetGeolocationOverride,
    SetLocaleOverride,
    SetScreenOrientationOverride,
    SetTimezoneOverride,
    SetViewport,
    SubscriptionId,
    TraverseHistory,
    TraverseHistoryResult,
    UserContext,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
  )
import WebDriverPreCore.BiDi.Response (JSONDecodeError, MatchedResponse (..), ResponseObject (..), decodeResponse, displayResponseError, parseResponse)
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (jsonToText)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, print, putStrLn)

-- TODO: geric command
-- TODO: handle event

withCommands :: BiDiClientParams -> (DemoUtils -> Commands -> IO ()) -> IO ()
withCommands params action =
  -- withNewBiDiSession $ action . mkCommands
  withNewBiDiSession params $ \utils -> action utils . mkCommands

data Commands = MkCommands
  { -- Session commands
    sessionNew :: Capabilities -> IO SessionNewResult,
    sessionStatus :: IO SessionStatusResult,
    sessionEnd :: IO Object,
    sessionSubScribe :: SessionSubscriptionRequest -> IO SessionSubscribeResult,
    sessionUnsubscribe :: SessionUnsubscribe -> IO Object,
    -- BrowsingContext commands
    browsingContextActivate :: Activate -> IO Object,
    browsingContextCaptureScreenshot :: CaptureScreenshot -> IO CaptureScreenshotResult,
    browsingContextClose :: Close -> IO Object,
    -- | Create a browsing context
    browsingContextCreate :: Create -> IO BrowsingContext,
    browsingContextGetTree :: GetTree -> IO GetTreeResult,
    browsingContextHandleUserPrompt :: HandleUserPrompt -> IO Object,
    browsingContextLocateNodes :: LocateNodes -> IO LocateNodesResult,
    browsingContextNavigate :: Navigate -> IO NavigateResult,
    browsingContextPrint :: Print -> IO PrintResult,
    browsingContextReload :: Reload -> IO Object,
    browsingContextSetViewport :: SetViewport -> IO Object,
    browsingContextTraverseHistory :: TraverseHistory -> IO TraverseHistoryResult,
    -- Browser commands
    browserClose :: IO Object,
    browserCreateUserContext :: CreateUserContext -> IO UserContext,
    browserGetClientWindows :: IO GetClientWindowsResult,
    browserGetUserContexts :: IO GetUserContextsResult,
    browserRemoveUserContext :: RemoveUserContext -> IO Object,
    browserSetClientWindowState :: SetClientWindowState -> IO ClientWindowInfo,
    -- Emulation commands
    emulationSetGeolocationOverride :: SetGeolocationOverride -> IO Object,
    emulationSetLocaleOverride :: SetLocaleOverride -> IO Object,
    emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> IO Object,
    emulationSetTimezoneOverride :: SetTimezoneOverride -> IO Object,
    -- Input commands
    inputPerformActions :: PerformActions -> IO Object,
    inputReleaseActions :: ReleaseActions -> IO Object,
    inputSetFiles :: SetFiles -> IO Object,
    -- Network commands
    networkAddDataCollector :: AddDataCollector -> IO AddDataCollectorResult,
    networkAddIntercept :: AddIntercept -> IO AddInterceptResult,
    networkContinueRequest :: ContinueRequest -> IO Object,
    networkContinueResponse :: ContinueResponse -> IO Object,
    networkContinueWithAuth :: ContinueWithAuth -> IO Object,
    networkDisownData :: DisownData -> IO Object,
    networkFailRequest :: FailRequest -> IO Object,
    networkGetData :: GetData -> IO GetDataResult,
    networkProvideResponse :: ProvideResponse -> IO Object,
    networkRemoveDataCollector :: RemoveDataCollector -> IO Object,
    networkRemoveIntercept :: RemoveIntercept -> IO Object,
    networkSetCacheBehavior :: SetCacheBehavior -> IO Object,
    -- Script commands
    scriptAddPreloadScript :: AddPreloadScript -> IO AddPreloadScriptResult,
    scriptCallFunction :: CallFunction -> IO EvaluateResult,
    scriptDisown :: Disown -> IO Object,
    scriptEvaluate :: Evaluate -> IO EvaluateResult,
    scriptEvaluateNoWait :: Evaluate -> IO (),
    scriptGetRealms :: GetRealms -> IO GetRealmsResult,
    scriptRemovePreloadScript :: RemovePreloadScript -> IO Object,
    -- Storage commands
    storageDeleteCookies :: DeleteCookies -> IO DeleteCookiesResult,
    storageGetCookies :: GetCookies -> IO GetCookiesResult,
    storageSetCookie :: SetCookie -> IO SetCookieResult,
    -- WebExtension commands
    webExtensionInstall :: WebExtensionInstall -> IO WebExtensionResult,
    webExtensionUninstall :: WebExtensionUninstall -> IO Object
  }

mkCommands :: BiDiMethods -> Commands
mkCommands client =
  MkCommands
    { -- Session commands
      sessionNew = send . P.sessionNew,
      sessionStatus = send P.sessionStatus,
      sessionEnd = send P.sessionEnd,
      sessionSubScribe = send . P.sessionSubScribe,
      sessionUnsubscribe = send . P.sessionUnsubscribe,
      -- BrowsingContext commands
      browsingContextActivate = send . P.browsingContextActivate,
      browsingContextCaptureScreenshot = send . P.browsingContextCaptureScreenshot,
      browsingContextClose = send . P.browsingContextClose,
      browsingContextCreate = send . P.browsingContextCreate,
      browsingContextGetTree = send . P.browsingContextGetTree,
      browsingContextHandleUserPrompt = send . P.browsingContextHandleUserPrompt,
      browsingContextLocateNodes = send . P.browsingContextLocateNodes,
      browsingContextNavigate = send . P.browsingContextNavigate,
      browsingContextPrint = send . P.browsingContextPrint,
      browsingContextReload = send . P.browsingContextReload,
      browsingContextSetViewport = send . P.browsingContextSetViewport,
      browsingContextTraverseHistory = send . P.browsingContextTraverseHistory,
      -- Browser commands
      browserClose = send P.browserClose,
      browserCreateUserContext = send . P.browserCreateUserContext,
      browserGetClientWindows = send P.browserGetClientWindows,
      browserGetUserContexts = send P.browserGetUserContexts,
      browserRemoveUserContext = send . P.browserRemoveUserContext,
      browserSetClientWindowState = send . P.browserSetClientWindowState,
      -- Emulation commands
      emulationSetGeolocationOverride = send . P.emulationSetGeolocationOverride,
      emulationSetLocaleOverride = send . P.emulationSetLocaleOverride,
      emulationSetScreenOrientationOverride = send . P.emulationSetScreenOrientationOverride,
      emulationSetTimezoneOverride = send . P.emulationSetTimezoneOverride,
      -- Input commands
      inputPerformActions = send . P.inputPerformActions,
      inputReleaseActions = send . P.inputReleaseActions,
      inputSetFiles = send . P.inputSetFiles,
      -- Network commands
      networkAddDataCollector = send . P.networkAddDataCollector,
      networkAddIntercept = send . P.networkAddIntercept,
      networkContinueRequest = send . P.networkContinueRequest,
      networkContinueResponse = send . P.networkContinueResponse,
      networkContinueWithAuth = send . P.networkContinueWithAuth,
      networkDisownData = send . P.networkDisownData,
      networkFailRequest = send . P.networkFailRequest,
      networkGetData = send . P.networkGetData,
      networkProvideResponse = send . P.networkProvideResponse,
      networkRemoveDataCollector = send . P.networkRemoveDataCollector,
      networkRemoveIntercept = send . P.networkRemoveIntercept,
      networkSetCacheBehavior = send . P.networkSetCacheBehavior,
      -- Script commands
      scriptAddPreloadScript = send . P.scriptAddPreloadScript,
      scriptCallFunction = send . P.scriptCallFunction,
      scriptDisown = send . P.scriptDisown,
      scriptEvaluate = send . P.scriptEvaluate,
      scriptEvaluateNoWait = sendCommandNoWait client . P.scriptEvaluate, -- alias
      scriptGetRealms = send . P.scriptGetRealms,
      scriptRemovePreloadScript = send . P.scriptRemovePreloadScript,
      -- Storage commands
      storageDeleteCookies = send . P.storageDeleteCookies,
      storageGetCookies = send . P.storageGetCookies,
      storageSetCookie = send . P.storageSetCookie,
      -- WebExtension commands
      webExtensionInstall = send . P.webExtensionInstall,
      webExtensionUninstall = send . P.webExtensionUninstall
    }
  where
    send :: forall c r. (FromJSON r, ToJSON c, Show c) => Command c r -> IO r
    send = sendCommand client

-- note: just throws an exception if an error is encountered
-- no timeout implemented - will just hang if bidi does not behave

sendCommandNoWait :: forall c r. (ToJSON c, Show c) => BiDiMethods -> Command c r -> IO ()
sendCommandNoWait m = void . sendCommandNoWait' m

sendCommandNoWait' :: forall c r. (ToJSON c, Show c) => BiDiMethods -> Command c r -> IO (JSUInt, Value)
sendCommandNoWait' MkBiDiMethods {send, nextId} command = do
  id' <- nextId
  let cValue = commandValue command id'
  (send cValue)
    `catch` \(e :: SomeException) -> do
      error $
        "Send command failed: \n"
          <> unpack (txt command)
          <> "\n ---- Exception -----\n"
          <> displayException e
  pure (id', cValue)

sendCommand :: forall c r. (FromJSON r, ToJSON c, Show c) => BiDiMethods -> Command c r -> IO r
sendCommand m@MkBiDiMethods {getNext} command = do
  (id', cValue) <- sendCommandNoWait' m command
  matchedResponse cValue id'
  where
    matchedResponse :: Value -> JSUInt -> IO r
    matchedResponse cValue id' = do
      response <- getNext
      parseResponse id' response
        & maybe
          ( -- recurse
            matchedResponse cValue id'
          )
          ( either
              ( -- format and throw
                error . unpack . displayResponseError command cValue
              )
              ( -- get response
                pure . (.response)
              )
          )

mkDemoBiDiClientParams :: Int -> IO BiDiClientParams
mkDemoBiDiClientParams pauseMs = do
  c <- mkChannels
  logger@MkLogger {log} <- mkLogger (c.logChan)
  pure $
    MkBiDiClientParams
      { biDiMethods = mkBiDIMethods c,
        logger,
        messageLoops = demoMessageLoops log c,
        demoUtils = bidiDemoUtils log pauseMs
      }

-- TODO Add fail event
mkFailBidiClientParams :: Int -> Word64 -> Word64 -> Word64 -> IO BiDiClientParams
mkFailBidiClientParams pauseMs failSendCount failGetCount failPrintCount = do
  c <- mkChannels
  logger@MkLogger {log} <- mkLogger (c.logChan)
  messageLoops <- failMessageLoops log c failSendCount failGetCount failPrintCount
  pure $
    MkBiDiClientParams
      { biDiMethods = mkBiDIMethods c,
        logger,
        messageLoops,
        demoUtils = bidiDemoUtils log pauseMs
      }

withNewBiDiSession :: BiDiClientParams -> (DemoUtils -> BiDiMethods -> IO ()) -> IO ()
withNewBiDiSession params action =
  bracket
    httpSession
    (deleteSession . (.sessionId))
    \s' -> do
      let bidiUrl = getBiDiUrl s' & either (error . T.unpack) id
      withBiDiClient params bidiUrl action

-- | WebDriver BiDi client with communication channels
data BiDiMethods = MkBiDiMethods
  { nextId :: IO JSUInt,
    send :: forall a. (ToJSON a, Show a) => a -> IO (),
    getNext :: IO (Either JSONDecodeError ResponseObject),
    subscribe :: Subscribed IO -> IO (),
    unsubscribe :: SubscriptionId -> IO ()
  }

data Channels = MkChannels
  { sendChan :: TChan Value,
    receiveChan :: TChan (Either JSONDecodeError ResponseObject),
    eventChan :: TChan Object,
    logChan :: TChan Text,
    counterVar :: TVar JSUInt,
    subscriptions :: TVar [Subscribed IO]
  }

mkChannels :: IO Channels
mkChannels =
  MkChannels
    <$> newTChanIO
    <*> newTChanIO
    <*> newTChanIO
    <*> newTChanIO
    <*> counterVar
    <*> newTVarIO []

counterVar :: IO (TVar JSUInt)
counterVar = newTVarIO $ MkJSUInt 0

mkAtomicCounter :: TVar JSUInt -> IO JSUInt
mkAtomicCounter var = atomically $ do
  modifyTVar' var succ
  readTVar var

mkBiDIMethods :: Channels -> BiDiMethods
mkBiDIMethods c =
  MkBiDiMethods
    { nextId = mkAtomicCounter c.counterVar,
      send = \a -> do
        -- make strict so serialisation errors come from here and not in the logger
        let !json = toJSON a
        atomically . writeTChan c.sendChan $ json,
      getNext = atomically $ readTChan c.receiveChan,
      subscribe = atomically . modifyTVar' c.subscriptions . (:),
      unsubscribe = \sid -> atomically . modifyTVar' c.subscriptions . filter $ \s -> s.subscriptionId /= sid
    }

data Subscribed m = MkSubscribed
  { subscriptionId :: SubscriptionId,
    subscription :: Subscription m
  }

-- | Subscribe to events with a filter function
subscribe :: Subscribed IO -> TVar [Subscribed IO] -> STM (TVar [Subscribed IO])
subscribe subscribed subscriptions = do
  modifyTVar' subscriptions (subscribed :)
  pure subscriptions

-- | Parse incoming WebSocket message to determine if it's an event or command response
-- For now, we'll use a simple heuristic: messages with "id" are responses, others are events
parseIncomingMessage :: (Text -> IO ()) -> Value -> IO (Either ResponseObject Event)
parseIncomingMessage log json = do
  -- todo:: FIX THIS should not encode then decode
  let jsonBytes = encode json
  -- Try to parse as command response first
  case decodeResponse jsonBytes of
    Right responseObj -> do
      log $ "Parsed as command response: " <> jsonToText json
      pure $ Left responseObj
    Left _ -> do
      -- TODO :: Treat as potential event - we'll improve this later when Event has FromJSON
      log $ "Treating as event: " <> jsonToText json
      -- TODO: implement
      pure $ Right undefined

data BiDiClientParams = MkBiDiClientParams
  { logger :: Logger,
    messageLoops :: MessageLoops,
    biDiMethods :: BiDiMethods,
    demoUtils :: DemoUtils
  }

-- | Run WebDriver BiDi client and return a client interface
withBiDiClient :: BiDiClientParams -> BiDiUrl -> (DemoUtils -> BiDiMethods -> IO ()) -> IO ()
withBiDiClient
  MkBiDiClientParams
    { biDiMethods,
      logger,
      messageLoops,
      demoUtils
    }
  bidiUrl
  action =
    withClient bidiUrl logger messageLoops $ action demoUtils biDiMethods

failAction :: Text -> Word64 -> (a -> IO ()) -> IO ((a -> IO ()))
failAction lbl failCallCount action = do
  counterVar' <- counterVar
  let counter = mkAtomicCounter counterVar'
  pure $ \a -> do
    n <- counter
    if (coerce n) == failCallCount
      then do
        error $ "Forced failure for testing: " <> unpack lbl <> " (call #" <> show n <> ")"
      else do
        action a

data MessageActions = MkMessageActions
  { send :: Connection -> IO (),
    get :: Connection -> IO (),
    print :: IO (),
    eventHandler :: IO ()
  }

failMessageActions :: MessageActions -> Word64 -> Word64 -> Word64 -> Word64 -> IO MessageActions
failMessageActions a failSendCount failGetCount failPrintCount failEventCount =
  do
    send <- failAction "send" failSendCount a.send
    get <- failAction "get" failGetCount a.get
    print' <- failAction "print" failPrintCount $ const a.print
    eventHandler' <- failAction "eventhandler" failEventCount $ const a.eventhandler
    pure $
      MkMessageActions
        { send,
          get,
          print = print' 1,
          eventHandler = eventHandler' 1
        }

demoMessageActions :: (Text -> IO ()) -> Channels -> MessageActions
demoMessageActions log channels =
  MkMessageActions
    { send = \conn -> do
        msgToSend <- atomically $ readTChan channels.sendChan
        log $ "Sending Message: " <> jsonToText msgToSend
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend)),
      --
      get = \conn -> do
        msg <- receiveData conn
        -- log $ "Received raw data: " <> T.take 100 (txt msg) <> "..."
        log $ "Received raw data: " <> txt msg
        let writeReceiveChan = atomically . writeTChan channels.receiveChan
            writeEventChan = atomically . writeTChan channels.eventChan
            r = decodeResponse msg
        case r of
          Left {} -> writeReceiveChan r
          Right r' -> case r' of
            NoID obj -> writeEventChan obj
            WithID {} -> writeReceiveChan r,
      --
      print = do
        msg <- atomically $ readTChan channels.logChan
        TIO.putStrLn $ "[LOG] " <> msg,
      --
      eventHandler = do
        obj <- atomically $ readTChan channels.eventChan
        log $ "Event received: " <> jsonToText (toJSON obj)
        subs <- readTVarIO channels.subscriptions

       -- TODO
       -- finish this
       -- set up async loops and emptying the event chan
       -- add subscribe unsubscribe to API object
       -- start demos
    }

loopActions :: (Text -> IO ()) -> MessageActions -> MessageLoops
loopActions log MkMessageActions {..} =
  MkMessageLoops
    { sendLoop = \conn -> asyncLoop "Sender" $ send conn,
      getLoop = \conn -> asyncLoop "Receiver" $ get conn,
      printLoop = asyncLoop "Logger" print
    }
  where
    asyncLoop = loopForever log

data MessageLoops = MkMessageLoops
  { sendLoop :: Connection -> IO (Async ()),
    getLoop :: Connection -> IO (Async ()),
    printLoop :: IO (Async ())
  }

demoMessageLoops :: (Text -> IO ()) -> Channels -> MessageLoops
demoMessageLoops log channels =
  loopActions log $ demoMessageActions log channels

failMessageLoops :: (Text -> IO ()) -> Channels -> Word64 -> Word64 -> Word64 -> IO MessageLoops
failMessageLoops log channels failSendCount failGetCount failPrintCount =
  loopActions log
    <$> failMessageActions (demoMessageActions log channels) failSendCount failGetCount failPrintCount

withClient :: BiDiUrl -> Logger -> MessageLoops -> IO () -> IO ()
withClient
  pth@MkBiDiUrl {host, port, path}
  logger
  messageLoops
  action =
    do
      log $ "Connecting to WebDriver at " <> txt pth
      runClient (unpack host) port (unpack path) $ \conn -> do
        printLoop <- messageLoops.printLoop
        getLoop <- messageLoops.getLoop conn
        sendLoop <- messageLoops.sendLoop conn

        log "WebSocket connection established"

        result <- async action

        (_asy, ethresult) <- waitAnyCatch [getLoop, sendLoop, result, printLoop]

        -- cancelMany not reexported by UnliftIO
        cancel getLoop
        cancel sendLoop
        cancel result

        logger.waitEmpty
        cancel printLoop
        ethresult
          & either
            ( \e -> do
                -- the logger is dead now so print direc to the console instead
                putStrLn $ "One of the BiDi client threads failed: \n" <> pack (displayException e)
                throw e
            )
            (pure)
        putStrLn "Disconnecting client"
    where
      log = logger.log

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

catchLog :: Text -> (Text -> IO ()) -> IO () -> IO ()
catchLog name log action =
  action
    `catches` [ Handler $ \(e :: AsyncCancelled) -> do
                  log $ name <> " thread cancelled"
                  throwIO e,
                Handler $ \(e :: SomeException) -> do
                  log $ "Exception thrown in " <> name <> " thread" <> ": " <> (pack $ displayException e)
                  throwIO e
              ]

-- like forever but, unlike forever, it fails if an exception is thrown
loopForever :: (Text -> IO ()) -> Text -> IO () -> IO (Async ())
loopForever log name action = async $ do
  log $ "Starting " <> name <> " thread"
  loop
  where
    loop = catchLog name log action >> loop

newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull
