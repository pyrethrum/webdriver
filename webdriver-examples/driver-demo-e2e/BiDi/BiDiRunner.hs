module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), Handler (..), SomeException, catch, catches, throw)
import Control.Exception.Backtrace (displayBacktraces)
import Control.Monad (forever)
import Data.Aeson (FromJSON, Object, ToJSON, Value, encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, pack, take, unpack)
import Data.Text.IO (putStrLn)
import Data.Text.IO qualified as TIO
import GHC.Base (undefined)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), deleteSession, newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import IOUtils (DemoUtils, Logger (..), bidiDemoUtils, mkLogger)
import IOUtils qualified as Utils
import Network.Socket (socket)
import Network.WebSockets (Connection, receiveData, runClient, sendClose, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import UnliftIO (AsyncCancelled, bracket, throwIO, waitAnyCancel, waitAnyCatchCancel, waitEither)
import UnliftIO.Async (Async, async, cancel, wait, waitCatch)
import UnliftIO.STM
import WebDriverPreCore.BiDi.BiDiUrl (BiDiUrl (..), getBiDiUrl)
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
  ( AddDataCollector,
    AddDataCollectorResult,
    AddIntercept,
    AddInterceptResult,
    AddPreloadScript,
    AddPreloadScriptResult,
    BrowsingContext,
    CallFunction,
    CallFunctionResult,
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
    SessionSubscribeResult,
    SessionSubscriptionRequest,
    SessionUnsubscribeParameters,
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
    TraverseHistory,
    TraverseHistoryResult,
    UserContext,
    WebExtension,
    WebExtensionData,
    WebExtensionResult,
  )
import WebDriverPreCore.BiDi.Protocol qualified as P
import WebDriverPreCore.BiDi.ResponseEvent (JSONEncodeError, MatchedResponse (..), ResponseObject, decodeResponse, displayResponseError, parseResponse)
import WebDriverPreCore.BiDi.Session (SessionNewResult, SessionStatusResult)
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, putStrLn)

withCommands :: (DemoUtils -> Commands -> IO ()) -> IO ()
withCommands action =
  -- withNewBiDiSession $ action . mkCommands
  withNewBiDiSession $ \utils -> action utils . mkCommands

data Commands = MkCommands
  { -- Session commands
    sessionNew :: Capabilities -> IO SessionNewResult,
    sessionStatus :: IO SessionStatusResult,
    sessionEnd :: IO Object,
    sessionSubScribe :: SessionSubscriptionRequest -> IO SessionSubscribeResult,
    sessionUnsubscribe :: SessionUnsubscribeParameters -> IO Object,
    -- BrowsingContext commands
    browsingContextActivate :: BrowsingContext -> IO Object,
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
    scriptCallFunction :: CallFunction -> IO CallFunctionResult,
    scriptDisown :: Disown -> IO Object,
    scriptEvaluate :: Evaluate -> IO EvaluateResult,
    scriptGetRealms :: GetRealms -> IO GetRealmsResult,
    scriptRemovePreloadScript :: RemovePreloadScript -> IO Object,
    -- Storage commands
    storageDeleteCookies :: DeleteCookies -> IO DeleteCookiesResult,
    storageGetCookies :: GetCookies -> IO GetCookiesResult,
    storageSetCookie :: SetCookie -> IO SetCookieResult,
    -- WebExtension commands
    webExtensionInstall :: WebExtensionData -> IO WebExtensionResult,
    webExtensionUninstall :: WebExtension -> IO Object
  }

mkCommands :: WebDriverBiDiClient -> Commands
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

sendCommand :: forall c r. (FromJSON r, ToJSON c, Show c) => WebDriverBiDiClient -> Command c r -> IO r
sendCommand MkWebDriverBiDiClient {sendMessage, receiveChannel, nextId} command = do
  id' <- nextId
  (sendMessage $ commandValue command id')
    `catch` \(e :: SomeException) -> do
      error $
        "Failed to send command \n"
          <> unpack (txt command)
          <> "\n ---- Exception -----\n"
          <> displayException e
  matchedResponse id'
  where
    matchedResponse :: JSUInt -> IO r
    matchedResponse id' = do
      response <- atomically $ readTChan receiveChannel
      parseResponse id' response
        & either
          ( -- format and throw
            error . unpack . displayResponseError command
          )
          ( maybe
              ( -- recurse
                matchedResponse id'
              )
              ( -- get response
                pure . (.response)
              )
          )

withNewBiDiSession :: (DemoUtils -> WebDriverBiDiClient -> IO ()) -> IO ()
withNewBiDiSession action =
  bracket
    httpSession
    (deleteSession . (.sessionId))
    \s' -> do
      let bidiUrl = getBiDiUrl s' & either (error . T.unpack) id
      withBiDiClient bidiUrl action

-- | WebDriver BiDi client with communication channels
data WebDriverBiDiClient = MkWebDriverBiDiClient
  { log :: Text -> IO (),
    nextId :: IO JSUInt,
    sendMessage :: forall a. (ToJSON a, Show a) => a -> IO (),
    receiveChannel :: TChan (Either JSONEncodeError ResponseObject)
  }

data Channels = MkChannels
  { sendChan :: TChan Value,
    receiveChan :: TChan (Either JSONEncodeError ResponseObject),
    logChan :: TChan Text
  }

-- | Run WebDriver BiDi client and return a client interface
withBiDiClient :: BiDiUrl -> (Logger -> DemoUtils) -> (DemoUtils -> WebDriverBiDiClient -> IO ()) -> IO ()
withBiDiClient bidiUrl demoUtils action = do
  -- Create communication channels
  sendChan <- newTChanIO
  receiveChan <- newTChanIO
  logChan <- newTChanIO
  counter <- newTVarIO $ MkJSUInt 0
  logger@MkLogger {log} <- mkLogger logChan

  let channels =
        MkChannels
          { sendChan,
            receiveChan,
            logChan
          }
      wdClient =
        MkWebDriverBiDiClient
          { log = logger.log,
            nextId = atomically $ do
              modifyTVar' counter succ
              readTVar counter,
            sendMessage = \a -> do
              log "ABOUT TO SEND"
              let val = toJSON a
              log "ABOUT TO SEND 2"
              log $ "Writing to sendChan: " <> txt val
              atomically . writeTChan sendChan $ val,
            receiveChannel = receiveChan
          }
      demoUtils = bidiDemoUtils logger
      socketAction = action demoUtils wdClient
      socketRunners = testSocketLoops logger channels
  withClient bidiUrl logger socketRunners socketAction

data SocketLoops = MkSocketLoops
  { sendLoop :: Connection -> IO (Async ()),
    receiveLoop :: Connection -> IO (Async ()),
    printLoop :: IO (Async ())
  }

printLoop :: TChan Text -> IO (Async ())
printLoop logChan = async printLoop'
  where
    printLoop' = do
      msg <- atomically $ readTChan logChan
      TIO.putStrLn $ "Next log....."
      TIO.putStrLn $ "[LOG] " <> msg
      printLoop'

testSocketLoops :: Logger -> Channels -> SocketLoops
testSocketLoops MkLogger {log} channels =
  MkSocketLoops
    { sendLoop = \conn -> asyncLoop "Sender" $ do
        msgToSend <- atomically $ readTChan channels.sendChan
        log $ "Sending Message: " <> txt msgToSend
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend)),
      --
      receiveLoop = \conn -> asyncLoop "Receiver" $ do
        msg <- receiveData conn
        log $ "Received raw data: " <> T.take 100 (txt msg) <> "..."
        atomically . writeTChan channels.receiveChan $ decodeResponse msg,
      --
      printLoop =
        let printLoop' = do
              msg <- atomically $ readTChan channels.logChan
              TIO.putStrLn $ "Next log....."
              TIO.putStrLn $ "[LOG] " <> msg
              printLoop'
         in async printLoop'
    }
  where
    asyncLoop = loopForever log

withClient :: BiDiUrl -> Logger -> SocketLoops -> IO () -> IO ()
withClient
  pth@MkBiDiUrl {host, port, path}
  logger
  socketRunners
  action =
    do
      log $ "Connecting to WebDriver at " <> txt pth
      runClient (unpack host) port (unpack path) $ \conn -> do
        printLoop <- socketRunners.printLoop
        receiveLoop <- socketRunners.receiveLoop conn
        sendLoop <- socketRunners.sendLoop conn

        log "WebSocket connection established"

        result <- async action

        log "Disconnecting client"
        (asy, ethresult) <- waitAnyCatchCancel [receiveLoop, sendLoop, result]
        logger.waitEmpty
        prntErr <- waitCatch printLoop
        ethresult
          & either
            ( \e -> do
                -- the logger is dead now so print direc to the console instead
                putStrLn $ "One of the BiDi client threads failed: " <> pack (displayException e)
                throw e
            )
            (pure)
        prntErr
          & either
            ( \e -> do
                putStrLn $ "The printLoop thread failed: " <> pack (displayException e)
                throw e
            )
            (pure)
        cancel asy
    where
      log = logger.log

{-
      -- Wait for any thread to fail (they shouldn't unless there's an error)
      (waitAnyCancel [receiver, sender] >> log "One of the WebSocket threads terminated")
        `catch` \(e :: AsyncCancelled) -> do
          log "WebSocket threads cancelled"
          throw e

      -- Send close frame to cleanly close the WebSocket connection
      log "Closing webSocket connection"
      sendClose conn ("Connection closing" :: Text)
        `catch` \(e :: SomeException) -> do
          log $ "Failed to send close frame: " <> pack (show e)
          throw e
          -}

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
  let loop = catchLog name log action >> loop
  loop

newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull

-- TODO: handle driver not running - getting STM Eadlock
{-
theGhostJW: last session ended with this exception: Exception: thread blocked indefinitely in an STM transaction
how could this happen?

GitHub Copilot: I can see you're getting an STM deadlock exception when running the BiDi demo. Let me investigate the codebase to understand what might be causing this issue. Now let me look at the BiDiRunner code more specifically to understand the STM usage: Now let me look at the specific parts where STM transactions occur and check for potential deadlock issues: Let me check if GeckoDriver is actually running by checking the terminal output:

theGhostJW: it is not running because I have restarted

GitHub Copilot: I understand the issue now. The STM deadlock is happening because GeckoDriver isn't running, so the WebSocket connection cannot be established. Let me start GeckoDriver for you and then explain what's causing the STM deadlock. Perfect! Now GeckoDriver is running. Let me explain what was causing the STM deadlock:

## Why the STM Deadlock Occurred

The **"thread blocked indefinitely in an STM transaction"** exception happens when an STM transaction is waiting for a condition that will never be satisfied. In your BiDi demo, here's what was happening:

### The Problem Chain:

1. **WebSocket Connection Failure**: When GeckoDriver isn't running, the WebSocket connection in BiDiRunner.hs fails to establish
2. **Receiver Thread Dies**: The receiver thread that reads from the WebSocket and writes to `receiveChan` terminates due to the connection failure
3. **Sender Thread May Also Die**: The sender thread may also terminate
4. **STM Deadlock**: In `sendCommand` function (line 270 of BiDiRunner.hs), this line blocks forever:
   ```haskell
   response <- atomically $ readTChan receiveChannel
   ```

### Why the STM Transaction Blocks Indefinitely:

Looking at the `sendCommand` function in BiDiRunner.hs:

```haskell
sendCommand :: forall c r. (FromJSON r, ToJSON c, Show c) => WebDriverBiDiClient -> Command c r -> IO r
sendCommand MkWebDriverBiDiClient {sendMessage, receiveChannel, nextId} command = do
  id' <- nextId
  sendMessage $ commandValue command id'
  matchedResponse id'
  where
    matchedResponse :: JSUInt -> IO r
    matchedResponse id' = do
      response <- atomically $ readTChan receiveChannel  -- ← BLOCKS HERE
      -- ... rest of function
```

When the receiver thread dies (because GeckoDriver isn't running), no new messages are being written to `receiveChannel`. The `readTChan` operation waits for data that will never come, causing the STM system to detect this as a deadlock.

### The Architecture:

The BiDi client uses this architecture:
- **Sender Thread**: Reads from `sendChan` and sends messages over WebSocket
- **Receiver Thread**: Receives WebSocket messages and writes to `receiveChan`
- **Main Thread**: Writes to `sendChan` and reads from `receiveChan`

When the WebSocket connection fails, both worker threads die, leaving the main thread stuck waiting for responses that will never arrive.

### Solution:

Now that GeckoDriver is running, you should be able to run your demo successfully:

```haskell
-- This should now work:
runDemo browsingContext
```

### Prevention:

To make the code more robust against this issue, you could:

1. **Add timeouts** to STM transactions
2. **Better error handling** in the WebSocket connection logic
3. **Connection health checks** before sending commands
4. **Graceful degradation** when the connection fails

But for now, ensuring GeckoDriver is running before executing the BiDi demo should resolve the immediate issue.
-}
