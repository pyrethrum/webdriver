module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), SomeException, catch)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, Value, encode, toJSON, Object)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Text as T (Text, pack, unpack)
import Data.Text.IO as T (putStrLn)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), deleteSession, newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import Network.WebSockets (receiveData, runClient, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import UnliftIO (bracket, throwIO)
import UnliftIO.Async (Async, async, cancel, waitAny)
import UnliftIO.STM
import WebDriverPreCore.BiDi.BiDiPath (BiDiPath (..), getBiDiPath)
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..), BrowsingContext)
import WebDriverPreCore.BiDi.Protocol qualified as P
import WebDriverPreCore.BiDi.Protocol
  ( SessionSubscriptionRequest,
    SessionSubscribeResult,
    SessionUnsubscribeParameters,
    -- BrowsingContext types
    Activate,
    CaptureScreenshot,
    CaptureScreenshotResult,
    Close,
    Create,
    GetTree,
    HandleUserPrompt,
    LocateNodes,
    Navigate,
    NavigateResult,
    Print,
    PrintResult,
    Reload,
    SetViewport,
    TraverseHistory,
    TraverseHistoryResult,
    -- Browser types
    CreateUserContext,
    UserContextInfo,
    GetClientWindowsResult,
    GetUserContextsResult,
    RemoveUserContext,
    SetClientWindowState,
    ClientWindowInfo,
    -- Emulation types
    SetGeolocationOverride,
    SetLocaleOverride,
    SetScreenOrientationOverride,
    SetTimezoneOverride,
    -- Input types
    PerformActions,
    ReleaseActions,
    SetFiles,
    -- Network types
    AddDataCollector,
    AddDataCollectorResult,
    AddIntercept,
    AddInterceptResult,
    ContinueRequest,
    ContinueResponse,
    ContinueWithAuth,
    DisownData,
    FailRequest,
    GetData,
    GetDataResult,
    ProvideResponse,
    RemoveDataCollector,
    RemoveIntercept,
    SetCacheBehavior,
    -- Script types
    AddPreloadScript,
    AddPreloadScriptResult,
    CallFunction,
    CallFunctionResult,
    Disown,
    Evaluate,
    EvaluateResult,
    GetRealms,
    GetRealmsResult,
    RemovePreloadScript,
    -- Storage types
    DeleteCookies,
    DeleteCookiesResult,
    GetCookies,
    GetCookiesResult,
    SetCookie,
    SetCookieResult,
    -- WebExtension types
    WebExtensionData,
    WebExtensionResult,
    WebExtension, GetTreeResult, LocateNodesResult, CreateResult
  )
import WebDriverPreCore.BiDi.ResponseEvent (MatchedResponse (..), ResponseError, ResponseObject, decodeResponse, parseResponse)
import WebDriverPreCore.BiDi.Session (SessionNewResult, SessionStatusResult)
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, putStrLn)

data Commands = MkCommands
  { -- Session commands
    sessionNew :: Capabilities -> IO SessionNewResult,
    sessionStatus :: IO SessionStatusResult,
    sessionEnd :: IO Object,
    sessionSubScribe :: SessionSubscriptionRequest -> IO SessionSubscribeResult,
    sessionUnsubscribe :: SessionUnsubscribeParameters -> IO Object,
    
    -- BrowsingContext commands
    browsingContextActivate :: Activate -> IO Object,
    browsingContextCaptureScreenshot :: CaptureScreenshot -> IO CaptureScreenshotResult,
    browsingContextClose :: Close -> IO Object,
    browsingContextCreate :: Create -> IO CreateResult,
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
    browserCreateUserContext :: CreateUserContext -> IO UserContextInfo,
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
      sessionNew = lift . P.sessionNew,
      sessionStatus = lift P.sessionStatus,
      sessionEnd = lift P.sessionEnd,
      sessionSubScribe = lift . P.sessionSubScribe,
      sessionUnsubscribe = lift . P.sessionUnsubscribe,
      
      -- BrowsingContext commands
      browsingContextActivate = lift . P.browsingContextActivate,
      browsingContextCaptureScreenshot = lift . P.browsingContextCaptureScreenshot,
      browsingContextClose = lift . P.browsingContextClose,
      browsingContextCreate = lift . P.browsingContextCreate,
      browsingContextGetTree = lift . P.browsingContextGetTree,
      browsingContextHandleUserPrompt = lift . P.browsingContextHandleUserPrompt,
      browsingContextLocateNodes = lift . P.browsingContextLocateNodes,
      browsingContextNavigate = lift . P.browsingContextNavigate,
      browsingContextPrint = lift . P.browsingContextPrint,
      browsingContextReload = lift . P.browsingContextReload,
      browsingContextSetViewport = lift . P.browsingContextSetViewport,
      browsingContextTraverseHistory = lift . P.browsingContextTraverseHistory,
      
      -- Browser commands
      browserClose = lift P.browserClose,
      browserCreateUserContext = lift . P.browserCreateUserContext,
      browserGetClientWindows = lift P.browserGetClientWindows,
      browserGetUserContexts = lift P.browserGetUserContexts,
      browserRemoveUserContext = lift . P.browserRemoveUserContext,
      browserSetClientWindowState = lift . P.browserSetClientWindowState,
      
      -- Emulation commands
      emulationSetGeolocationOverride = lift . P.emulationSetGeolocationOverride,
      emulationSetLocaleOverride = lift . P.emulationSetLocaleOverride,
      emulationSetScreenOrientationOverride = lift . P.emulationSetScreenOrientationOverride,
      emulationSetTimezoneOverride = lift . P.emulationSetTimezoneOverride,
      
      -- Input commands
      inputPerformActions = lift . P.inputPerformActions,
      inputReleaseActions = lift . P.inputReleaseActions,
      inputSetFiles = lift . P.inputSetFiles,
      
      -- Network commands
      networkAddDataCollector = lift . P.networkAddDataCollector,
      networkAddIntercept = lift . P.networkAddIntercept,
      networkContinueRequest = lift . P.networkContinueRequest,
      networkContinueResponse = lift . P.networkContinueResponse,
      networkContinueWithAuth = lift . P.networkContinueWithAuth,
      networkDisownData = lift . P.networkDisownData,
      networkFailRequest = lift . P.networkFailRequest,
      networkGetData = lift . P.networkGetData,
      networkProvideResponse = lift . P.networkProvideResponse,
      networkRemoveDataCollector = lift . P.networkRemoveDataCollector,
      networkRemoveIntercept = lift . P.networkRemoveIntercept,
      networkSetCacheBehavior = lift . P.networkSetCacheBehavior,
      
      -- Script commands
      scriptAddPreloadScript = lift . P.scriptAddPreloadScript,
      scriptCallFunction = lift . P.scriptCallFunction,
      scriptDisown = lift . P.scriptDisown,
      scriptEvaluate = lift . P.scriptEvaluate,
      scriptGetRealms = lift . P.scriptGetRealms,
      scriptRemovePreloadScript = lift . P.scriptRemovePreloadScript,
      
      -- Storage commands
      storageDeleteCookies = lift . P.storageDeleteCookies,
      storageGetCookies = lift . P.storageGetCookies,
      storageSetCookie = lift . P.storageSetCookie,
      
      -- WebExtension commands
      webExtensionInstall = lift . P.webExtensionInstall,
      webExtensionUninstall = lift . P.webExtensionUninstall
    }
  where
    lift :: forall c r. (FromJSON r, ToJSON c) => Command c r -> IO r
    lift = sendCommand client

-- note: just throws an exception if an error is encountered
-- no timeout implemented - will just hang if bidi does not behave
sendCommand :: forall c r. (FromJSON r, ToJSON c) => WebDriverBiDiClient -> Command c r -> IO r
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
            (error . unpack . txt)
            ( maybe
                (matchedResponse id')
                (pure . (.response))
            )

withBiDiSession :: (WebDriverBiDiClient -> IO a) -> IO a
withBiDiSession action =
  bracket
    httpSession
    (deleteSession . (.sessionId))
    \s' -> do
      let bidiPath = getBiDiPath s' & either (error . T.unpack) id
      withBiDi bidiPath action

withBiDi :: BiDiPath -> (WebDriverBiDiClient -> IO a) -> IO a
withBiDi bidiPath action =
  bracket
    (runWebDriverBiDi bidiPath)
    ( \client ->
        putStrLn "Ending BiDi session - will need some clean up here"
          >> client.disconnect
    )
    action

-- | WebDriver BiDi client with communication channels
data WebDriverBiDiClient = MkWebDriverBiDiClient
  { log :: Text -> IO (),
    nextId :: IO JSUInt,
    sendMessage :: forall a. (ToJSON a) => a -> IO (),
    receiveChannel :: TChan (Either ResponseError ResponseObject),
    disconnect :: IO ()
  }

-- | Run WebDriver BiDi client and return a client interface
runWebDriverBiDi :: BiDiPath -> IO WebDriverBiDiClient
runWebDriverBiDi bidiPth = do
  -- Create communication channels
  sendChan <- newTChanIO
  receiveChan <- newTChanIO
  logChan <- newTChanIO
  counter <- newTVarIO $ MkJSUInt 1

  loggerAsync <- async . forever $ do
    msg <- atomically $ readTChan logChan
    putStrLn $ "[LOG] " <> msg

  let log = atomically . writeTChan logChan

  clientAsync <- startClient bidiPth log sendChan receiveChan

  pure $
    MkWebDriverBiDiClient
      { log,
        nextId = atomically $ do
          i <- readTVar counter
          writeTVar counter $ succ i
          pure i,
        sendMessage = atomically . writeTChan sendChan . toJSON,
        receiveChannel = receiveChan,
        disconnect = do
          log "Disconnecting client... TODO: implement proper cleanup"
          cancel clientAsync
          threadDelay 1000_000
          cancel loggerAsync
      }

startClient :: BiDiPath -> (Text -> IO ()) -> TChan Value -> TChan (Either ResponseError ResponseObject) -> IO (Async ())
startClient pth@MkBiDiPath {host, port, path} log sendChan receiveChan =
  async $ do
    log $ "Connecting to WebDriver at " <> txt pth
    catch
      runSocketClient
      ( \(e :: SomeException) -> do
          log $ "WebSocket failure: " <> pack (displayException e)
          -- flush log channelnewSessionFull .
          threadDelay 1000_000
          throwIO e
      )
  where
    runSocketClient = runClient (unpack host) port (unpack path) $ \conn -> do
      log "WebSocket connection established!"

      let runForever = asyncForever log

      receiver <- runForever "receiver" $ do
        msg <- receiveData conn
        log $ "Received raw data: " <> pack (take 100 (show msg)) <> "..."
        atomically . writeTChan receiveChan $ decodeResponse msg

      sender <- runForever "sender" $ do
        msgToSend <- atomically $ readTChan sendChan
        log $ "Sending Message: " <> pack (show msgToSend)
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend))

      -- Wait for any thread to fail (they shouldn't unless there's an error)
      waitAny [receiver, sender]
      threadDelay 1_000_000 -- Wait a bit before closing
      putStrLn "One of the WebSocket threads terminated, closing connection."

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
catchLog message log action =
  action `catch` \(e :: SomeException) -> do
    log $ message <> ": " <> pack (show e)
    threadDelay 1000_000 -- Wait a bit before rethrowing
    throwIO e

asyncForever :: (Text -> IO ()) -> Text -> IO () -> IO (Async ())
asyncForever log name action = async $ do
  log message
  forever $ catchLog message log action
  where
    message = "Starting " <> name <> " thread"

newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull
