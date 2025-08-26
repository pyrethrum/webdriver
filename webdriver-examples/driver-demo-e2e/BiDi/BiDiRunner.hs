module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), Handler (..), SomeException, catch, catches)
import Control.Monad (forever)
import Data.Aeson (FromJSON, Object, ToJSON, Value, encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, pack, unpack)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), deleteSession, newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))

import Network.WebSockets (receiveData, runClient, sendClose, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import UnliftIO (AsyncCancelled, bracket, throwIO, waitAnyCancel)
import UnliftIO.Async (Async, async, cancel)
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

withCommands :: Maybe (Text -> IO ()) -> (Commands -> IO a) -> IO a
withCommands mLogger action =
  withNewBiDiSession mLogger $ action . mkCommands

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
sendCommand
  MkWebDriverBiDiClient {sendMessage, receiveChannel, nextId}
  command = do
    id' <- nextId
    sendMessage $ commandValue command id'
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

withNewBiDiSession :: Maybe (Text -> IO ()) -> (WebDriverBiDiClient -> IO a) -> IO a
withNewBiDiSession mLogger action =
  bracket
    httpSession
    (deleteSession . (.sessionId))
    \s' -> do
      let bidiUrl = getBiDiUrl s' & either (error . T.unpack) id
      withRunningBiDiSession mLogger bidiUrl action

withRunningBiDiSession :: Maybe (Text -> IO ()) -> BiDiUrl -> (WebDriverBiDiClient -> IO a) -> IO a
withRunningBiDiSession mLogger bidiUrl action =
  bracket
    (getBiDiClient mLogger bidiUrl)
    ( \client ->
        (fromMaybe (const $ pure ()) mLogger) "Ending BiDi session"
          >> client.disconnect
    )
    action

-- | WebDriver BiDi client with communication channels
data WebDriverBiDiClient = MkWebDriverBiDiClient
  { log :: Text -> IO (),
    nextId :: IO JSUInt,
    sendMessage :: forall a. (ToJSON a, Show a) => a -> IO (),
    receiveChannel :: TChan (Either JSONEncodeError ResponseObject),
    disconnect :: IO ()
  }

-- | Run WebDriver BiDi client and return a client interface
getBiDiClient :: Maybe (Text -> IO ()) -> BiDiUrl -> IO WebDriverBiDiClient
getBiDiClient mLogger bidiPth = do
  -- Create communication channels
  sendChan <- newTChanIO
  receiveChan <- newTChanIO
  counter <- newTVarIO $ MkJSUInt 1

  let log = fromMaybe (const $ pure ()) mLogger

  clientAsync <- startClient bidiPth log sendChan receiveChan

  pure $
    MkWebDriverBiDiClient
      { log,
        nextId = atomically $ do
          i <- readTVar counter
          writeTVar counter $ succ i
          pure i,
        sendMessage = \a -> do
          log $ "Before Writing to sendChan: " <> txt a
          let m = toJSON a
          log $ "Writing to sendChan: " <> txt m
          atomically . writeTChan sendChan $ m,
        receiveChannel = receiveChan,
        disconnect = do
          log "Disconnecting client"
          cancel clientAsync
      }

startClient :: BiDiUrl -> (Text -> IO ()) -> TChan Value -> TChan (Either JSONEncodeError ResponseObject) -> IO (Async ())
startClient pth@MkBiDiUrl {host, port, path} log sendChan receiveChan =
  async $ do
    log $ "Connecting to WebDriver at " <> txt pth
    runSocketClient
      `catches` [ Handler
                    (\(_e :: AsyncCancelled) -> threadDelay 1000_000 >> pure ()),
                  Handler
                    ( \(e :: SomeException) -> do
                        log $ "WebSocket failure: " <> pack (displayException e)
                        -- flush log channelnewSessionFull .
                        threadDelay 1000_000
                        throwIO e
                    )
                ]
  where
    runSocketClient = runClient (unpack host) port (unpack path) $ \conn -> do
      log "WebSocket connection established"

      let runForever = asyncForever log

      receiver <- runForever "Receiver" $ do
        msg <- receiveData conn
        log $ "Received raw data: " <> pack (take 100 (show msg)) <> "..."
        atomically . writeTChan receiveChan $ decodeResponse msg

      sender <- runForever "Sender" $ do
        msgToSend <- atomically $ readTChan sendChan
        log $ "Sending Message: " <> pack (show msgToSend)
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend))

      -- Wait for any thread to fail (they shouldn't unless there's an error)
      (waitAnyCancel [receiver, sender] >> log "One of the WebSocket threads terminated")
        `catch` \(_e :: AsyncCancelled) -> pure ()

      -- Send close frame to cleanly close the WebSocket connection
      log "Closing webSocket connection"
      sendClose conn ("Connection closing" :: Text)
        `catch` \(e :: SomeException) ->
          log $ "Failed to send close frame: " <> pack (show e)

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
                  log $ "Exception thrown in " <> name <> " thread" <> ": " <> pack (show e)
                  throwIO e
              ]

asyncForever :: (Text -> IO ()) -> Text -> IO () -> IO (Async ())
asyncForever log name action = async $ do
  log $ "Starting " <> name <> " thread"
  forever $
    catchLog name log action

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
