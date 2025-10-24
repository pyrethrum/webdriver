module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Const (Timeout)
import Control.Exception (Exception (displayException), Handler (..), SomeException, catch, catches, throw)
import Control.Monad (forever, unless, when)
import Data.Aeson (FromJSON, Object, ToJSON, Value (..), encode, toJSON, withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList), traverse_)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text as T (Text, pack, take, unpack)
import Data.Text.IO (putStrLn)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), deleteSession, newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import IOUtils (DemoUtils, QueLog (..), bidiDemoUtils)
import Network.WebSockets (Connection, receiveData, runClient, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import UnliftIO (AsyncCancelled, bracket, catchAny, throwIO, waitAnyCatch)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM
import WebDriverPreCore.BiDi.API qualified as P
import WebDriverPreCore.BiDi.BiDiUrl (BiDiUrl (..), getBiDiUrl)
import WebDriverPreCore.BiDi.BrowsingContext
  ( DownloadEnd,
    DownloadWillBegin,
    HistoryUpdated,
    NavigationInfo,
    UserPromptClosed,
    UserPromptOpened,
  )
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Event (Subscription (..))
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (LogEntry)
import WebDriverPreCore.BiDi.Network
  ( AuthRequired,
    BeforeRequestSent,
    FetchError,
    ResponseCompleted,
    ResponseStarted,
  )
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
    Info,
    LocateNodes,
    LocateNodesResult,
    Navigate,
    NavigateResult,
    PerformActions,
    Print,
    PrintResult,
    ProvideResponse,
    RealmDestroyed,
    ReleaseActions,
    Reload,
    RemoveDataCollector,
    RemoveIntercept,
    RemovePreloadScript,
    RemoveUserContext,
    SessionNewResult,
    SessionStatusResult,
    SessionSubscribeResult (..),
    SessionSubscriptionRequest (..),
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
    SubscriptionId (..),
    SubscriptionType (..),
    TraverseHistory,
    TraverseHistoryResult,
    UserContext,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
  )
import WebDriverPreCore.BiDi.Response (JSONDecodeError, MatchedResponse (..), ResponseObject (..), decodeResponse, displayResponseError, parseResponse)
import WebDriverPreCore.BiDi.Script
  ( Message,
    RealmInfo,
  )
import WebDriverPreCore.BiDi.Session (SessionUnsubscribe (..))
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (jsonToText, objToText, parseThrow)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, print, putStrLn)

-- TODO: geric command
-- TODO: handle event

withCommands :: BiDiClientParams -> (DemoUtils -> BiDiActions -> IO ()) -> IO ()
withCommands params action =
  -- withNewBiDiSession $ action . mkCommands
  withNewBiDiSession params $ \utils -> action utils . mkCommands

data BiDiActions = MkCommands
  { -- Session commands
    sessionNew :: Capabilities -> IO SessionNewResult,
    sessionStatus :: IO SessionStatusResult,
    sessionEnd :: IO Object,
    -- sessionSubscribe and sessionUnsubscribe are exposed for demo commands but they would not be used normally
    -- in client code. They would be called via the subscribe/unsubscribe methods below so the runner can
    -- locally track subscriptions
    sessionSubscribe :: SessionSubscriptionRequest -> IO SessionSubscribeResult,
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
    scriptEvaluateNoWait :: Evaluate -> IO CommandRequestInfo,
    scriptGetRealms :: GetRealms -> IO GetRealmsResult,
    scriptRemovePreloadScript :: RemovePreloadScript -> IO Object,
    -- Storage commands
    storageDeleteCookies :: DeleteCookies -> IO DeleteCookiesResult,
    storageGetCookies :: GetCookies -> IO GetCookiesResult,
    storageSetCookie :: SetCookie -> IO SetCookieResult,
    -- WebExtension commands
    webExtensionInstall :: WebExtensionInstall -> IO WebExtensionResult,
    webExtensionUninstall :: WebExtensionUninstall -> IO Object,
    -- ########## EVENTS ##########

    subscribeMany :: [SubscriptionType] -> (Event -> IO ()) -> IO SubscriptionId,
    subscribeMany' :: [BrowsingContext] -> [UserContext] -> [SubscriptionType] -> (Event -> IO ()) -> IO SubscriptionId,
    -- Log
    subscribeLogEntryAdded :: (LogEntry -> IO ()) -> IO SubscriptionId,
    subscribeLogEntryAdded' :: [BrowsingContext] -> [UserContext] -> (LogEntry -> IO ()) -> IO SubscriptionId,
    -- BrowsingContext
    subscribeBrowsingContextCreated :: (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextCreated' :: [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDestroyed :: (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDestroyed' :: [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationStarted :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationStarted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextFragmentNavigated :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextFragmentNavigated' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextHistoryUpdated :: (HistoryUpdated -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextHistoryUpdated' :: [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDomContentLoaded :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDomContentLoaded' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextLoad :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextLoad' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadWillBegin :: (DownloadWillBegin -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadWillBegin' :: [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadEnd :: (DownloadEnd -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadEnd' :: [BrowsingContext] -> [UserContext] -> (DownloadEnd -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationAborted :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationAborted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationCommitted :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationCommitted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationFailed :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationFailed' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptClosed :: (UserPromptClosed -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptClosed' :: [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptOpened :: (UserPromptOpened -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptOpened' :: [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> IO ()) -> IO SubscriptionId,
    -- Network
    subscribeNetworkAuthRequired :: (AuthRequired -> IO ()) -> IO SubscriptionId,
    subscribeNetworkAuthRequired' :: [BrowsingContext] -> [UserContext] -> (AuthRequired -> IO ()) -> IO SubscriptionId,
    subscribeNetworkBeforeRequestSent :: (BeforeRequestSent -> IO ()) -> IO SubscriptionId,
    subscribeNetworkBeforeRequestSent' :: [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> IO ()) -> IO SubscriptionId,
    subscribeNetworkFetchError :: (FetchError -> IO ()) -> IO SubscriptionId,
    subscribeNetworkFetchError' :: [BrowsingContext] -> [UserContext] -> (FetchError -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseCompleted :: (ResponseCompleted -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseCompleted' :: [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseStarted :: (ResponseStarted -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseStarted' :: [BrowsingContext] -> [UserContext] -> (ResponseStarted -> IO ()) -> IO SubscriptionId,
    -- Script
    subscribeScriptMessage :: (Message -> IO ()) -> IO SubscriptionId,
    subscribeScriptMessage' :: [BrowsingContext] -> [UserContext] -> (Message -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmCreated :: (RealmInfo -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmCreated' :: [BrowsingContext] -> [UserContext] -> (RealmInfo -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmDestroyed :: (RealmDestroyed -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmDestroyed' :: [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> IO ()) -> IO SubscriptionId,
    -- Input
    subscribeInputFileDialogOpened :: (FileDialogOpened -> IO ()) -> IO SubscriptionId,
    subscribeInputFileDialogOpened' :: [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> IO ()) -> IO SubscriptionId,
    --
    unsubscribe :: SubscriptionId -> IO (),
    --
    sendCommandNoWait :: forall c r. (ToJSON c, Show c) => Command c r -> IO CommandRequestInfo,
    sendCommand' :: forall c r. (FromJSON r, ToJSON c, Show c) => JSUInt -> Command c r -> IO r
  }

mkCommands :: BiDiMethods -> BiDiActions
mkCommands socket =
  MkCommands
    { -- ########## COMMANDS ##########
      -- Session commands
      sessionNew = send . P.sessionNew,
      sessionStatus = send P.sessionStatus,
      sessionEnd = send P.sessionEnd,
      sessionSubscribe,
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
      scriptEvaluateNoWait = sendCommandNoWait socket . P.scriptEvaluate,
      scriptGetRealms = send . P.scriptGetRealms,
      scriptRemovePreloadScript = send . P.scriptRemovePreloadScript,
      -- Storage commands
      storageDeleteCookies = send . P.storageDeleteCookies,
      storageGetCookies = send . P.storageGetCookies,
      storageSetCookie = send . P.storageSetCookie,
      -- WebExtension commands
      webExtensionInstall = send . P.webExtensionInstall,
      webExtensionUninstall = send . P.webExtensionUninstall,
      --
      -- ############## Subscriptions (Events) ##############

      subscribeMany = \sts -> subscribeMany' [] [] sts,
      subscribeMany',
      -- Log
      subscribeLogEntryAdded = sendSub P.subscribeLogEntryAdded,
      subscribeLogEntryAdded' = sendSub' P.subscribeLogEntryAdded,
      -- BrowsingContext
      subscribeBrowsingContextCreated = sendSub P.subscribeBrowsingContextCreated,
      subscribeBrowsingContextCreated' = sendSub' P.subscribeBrowsingContextCreated,
      subscribeBrowsingContextDestroyed = sendSub P.subscribeBrowsingContextDestroyed,
      subscribeBrowsingContextDestroyed' = sendSub' P.subscribeBrowsingContextDestroyed,
      subscribeBrowsingContextNavigationStarted = sendSub P.subscribeBrowsingContextNavigationStarted,
      subscribeBrowsingContextNavigationStarted' = sendSub' P.subscribeBrowsingContextNavigationStarted,
      subscribeBrowsingContextFragmentNavigated = sendSub P.subscribeBrowsingContextFragmentNavigated,
      subscribeBrowsingContextFragmentNavigated' = sendSub' P.subscribeBrowsingContextFragmentNavigated,
      subscribeBrowsingContextHistoryUpdated = sendSub P.subscribeBrowsingContextHistoryUpdated,
      subscribeBrowsingContextHistoryUpdated' = sendSub' P.subscribeBrowsingContextHistoryUpdated,
      subscribeBrowsingContextDomContentLoaded = sendSub P.subscribeBrowsingContextDomContentLoaded,
      subscribeBrowsingContextDomContentLoaded' = sendSub' P.subscribeBrowsingContextDomContentLoaded,
      subscribeBrowsingContextLoad = sendSub P.subscribeBrowsingContextLoad,
      subscribeBrowsingContextLoad' = sendSub' P.subscribeBrowsingContextLoad,
      subscribeBrowsingContextDownloadWillBegin = sendSub P.subscribeBrowsingContextDownloadWillBegin,
      subscribeBrowsingContextDownloadWillBegin' = sendSub' P.subscribeBrowsingContextDownloadWillBegin,
      subscribeBrowsingContextDownloadEnd = sendSub P.subscribeBrowsingContextDownloadEnd,
      subscribeBrowsingContextDownloadEnd' = sendSub' P.subscribeBrowsingContextDownloadEnd,
      subscribeBrowsingContextNavigationAborted = sendSub P.subscribeBrowsingContextNavigationAborted,
      subscribeBrowsingContextNavigationAborted' = sendSub' P.subscribeBrowsingContextNavigationAborted,
      subscribeBrowsingContextNavigationCommitted = sendSub P.subscribeBrowsingContextNavigationCommitted,
      subscribeBrowsingContextNavigationCommitted' = sendSub' P.subscribeBrowsingContextNavigationCommitted,
      subscribeBrowsingContextNavigationFailed = sendSub P.subscribeBrowsingContextNavigationFailed,
      subscribeBrowsingContextNavigationFailed' = sendSub' P.subscribeBrowsingContextNavigationFailed,
      subscribeBrowsingContextUserPromptClosed = sendSub P.subscribeBrowsingContextUserPromptClosed,
      subscribeBrowsingContextUserPromptClosed' = sendSub' P.subscribeBrowsingContextUserPromptClosed,
      subscribeBrowsingContextUserPromptOpened = sendSub P.subscribeBrowsingContextUserPromptOpened,
      subscribeBrowsingContextUserPromptOpened' = sendSub' P.subscribeBrowsingContextUserPromptOpened,
      -- Network
      subscribeNetworkAuthRequired = sendSub P.subscribeNetworkAuthRequired,
      subscribeNetworkAuthRequired' = sendSub' P.subscribeNetworkAuthRequired,
      subscribeNetworkBeforeRequestSent = sendSub P.subscribeNetworkBeforeRequestSent,
      subscribeNetworkBeforeRequestSent' = sendSub' P.subscribeNetworkBeforeRequestSent,
      subscribeNetworkFetchError = sendSub P.subscribeNetworkFetchError,
      subscribeNetworkFetchError' = sendSub' P.subscribeNetworkFetchError,
      subscribeNetworkResponseCompleted = sendSub P.subscribeNetworkResponseCompleted,
      subscribeNetworkResponseCompleted' = sendSub' P.subscribeNetworkResponseCompleted,
      subscribeNetworkResponseStarted = sendSub P.subscribeNetworkResponseStarted,
      subscribeNetworkResponseStarted' = sendSub' P.subscribeNetworkResponseStarted,
      -- Script
      subscribeScriptMessage = sendSub P.subscribeScriptMessage,
      subscribeScriptMessage' = sendSub' P.subscribeScriptMessage,
      subscribeScriptRealmCreated = sendSub P.subscribeScriptRealmCreated,
      subscribeScriptRealmCreated' = sendSub' P.subscribeScriptRealmCreated,
      subscribeScriptRealmDestroyed = sendSub P.subscribeScriptRealmDestroyed,
      subscribeScriptRealmDestroyed' = sendSub' P.subscribeScriptRealmDestroyed,
      -- Input
      subscribeInputFileDialogOpened = sendSub P.subscribeInputFileDialogOpened,
      subscribeInputFileDialogOpened' = sendSub' P.subscribeInputFileDialogOpened,
      --
      unsubscribe = socket.unsubscribe sessionUnsubscribe,
      --
      sendCommandNoWait = sendCommandNoWait socket,
      sendCommand' = sendCommand' socket
    }
  where
    send :: forall c r. (FromJSON r, ToJSON c, Show c) => Command c r -> IO r
    send = sendCommand socket

    sessionSubscribe :: SessionSubscriptionRequest -> IO SessionSubscribeResult
    sessionSubscribe = send . P.sessionSubscribe

    sessionUnsubscribe :: SessionUnsubscribe -> IO Object
    sessionUnsubscribe = send . P.sessionUnsubscribe

    subscribeMany' ::
      [BrowsingContext] ->
      [UserContext] ->
      [SubscriptionType] ->
      (Event -> IO ()) ->
      IO SubscriptionId
    subscribeMany' bcs ucs sts = socket.subscribe sessionSubscribe . P.subscribeMany bcs ucs sts

    sendSub ::
      forall a.
      ( [BrowsingContext] ->
        [UserContext] ->
        (a -> IO ()) ->
        Subscription IO
      ) ->
      (a -> IO ()) ->
      IO SubscriptionId
    sendSub apiSubscription = socket.subscribe sessionSubscribe . apiSubscription [] []

    sendSub' ::
      forall a.
      ( [BrowsingContext] ->
        [UserContext] ->
        (a -> IO ()) ->
        Subscription IO
      ) ->
      [BrowsingContext] ->
      [UserContext] ->
      (a -> IO ()) ->
      IO SubscriptionId
    sendSub' apiSubscription bcs ucs action = socket.subscribe sessionSubscribe (apiSubscription bcs ucs action)

-- note: just throws an exception if an error is encountered
-- no timeout implemented - will just hang if bidi does not behave

data CommandRequestInfo = MkCommandRequestInfo
  { id :: JSUInt,
    request :: Value
  }
  deriving (Show, Generic)

sendCommandNoWait' :: forall c r. (ToJSON c, Show c) => BiDiMethods -> JSUInt -> Command c r -> IO CommandRequestInfo
sendCommandNoWait' MkBiDiMethods {send} id' command = do
  let request = commandValue command id'
  (send request)
    `catch` \(e :: SomeException) -> do
      error $
        "Send command failed: \n"
          <> unpack (txt command)
          <> "\n ---- Exception -----\n"
          <> displayException e
  pure $ MkCommandRequestInfo {id = id', request}

sendCommandNoWait :: forall c r. (ToJSON c, Show c) => BiDiMethods -> Command c r -> IO CommandRequestInfo
sendCommandNoWait MkBiDiMethods {send, nextId} command = do
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

sendCommand' :: forall c r. (FromJSON r, ToJSON c, Show c) => BiDiMethods -> JSUInt -> Command c r -> IO r
sendCommand' bdm id' command =  do
  MkCommandRequestInfo {request} <- sendCommandNoWait' bdm id' command
  matchedResponse' request id'
  where
    matchedResponse' :: Value -> JSUInt -> IO r
    matchedResponse' = matchedResponse command bdm.getNext

sendCommand :: forall c r. (FromJSON r, ToJSON c, Show c) => BiDiMethods -> Command c r -> IO r
sendCommand m@MkBiDiMethods {getNext} command = do
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

doNothingLogQueue :: LogQueue
doNothingLogQueue =
  MkLogQueue
    { waitEmpty = pure (),
      queueLog = MkQueLog . const $ pure (),
      -- just block the print thread forever
      deQueueLog = do
        forever $ threadDelay 1_000_000
        pure ""
    }

mkDemoBiDiClientParams :: Maybe Printer -> Timeout -> IO BiDiClientParams
mkDemoBiDiClientParams mPrinter pauseTimeout =
  do
    c <- mkChannels
    let logQueue =
          mPrinter
            & maybe
              doNothingLogQueue
              (const $ c.logQueue)

    pure $
      MkBiDiClientParams
        { biDiMethods = mkBiDIMethods c,
          logQueue,
          messageLoops = demoMessageLoops mPrinter c,
          demoUtils = bidiDemoUtils c.logQueue.queueLog pauseTimeout
        }

-- TODO Add fail for event test
mkFailBidiClientParams ::
  Maybe Printer ->
  Timeout ->
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  IO BiDiClientParams
mkFailBidiClientParams mPrinter pauseTimeout failSendCount failGetCount failPrintCount failEventCount = do
  c <- mkChannels
  messageLoops <- failMessageLoops mPrinter c failSendCount failGetCount failPrintCount failEventCount
  pure $
    MkBiDiClientParams
      { biDiMethods = mkBiDIMethods c,
        -- no logging
        logQueue = c.logQueue,
        messageLoops,
        demoUtils = bidiDemoUtils c.logQueue.queueLog pauseTimeout
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
    logQueue :: LogQueue
  }

mkChannels :: IO Channels
mkChannels = do
  logChan <- newTChanIO
  let waitEmpty' :: Int -> IO ()
      waitEmpty' attempt = do
        empty <- atomically $ isEmptyTChan logChan
        unless (empty || attempt > 500) $ do
          threadDelay 10_000
          waitEmpty' $ succ attempt
      logQ :: LogQueue
      logQ =
        MkLogQueue
          { queueLog = MkQueLog $ atomically . writeTChan logChan,
            deQueueLog = atomically $ readTChan logChan,
            waitEmpty = waitEmpty' 0
          }

  MkChannels
    <$> newTChanIO
    <*> newTChanIO
    <*> newTChanIO
    <*> counterVar
    <*> newTVarIO []
    <*> (pure logQ)

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

data ActiveSubscription m = MkActiveSubscription
  { subscriptionId :: SubscriptionId,
    subscription :: Subscription m
  }

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
  { logQueue :: LogQueue,
    messageLoops :: MessageLoops,
    biDiMethods :: BiDiMethods,
    demoUtils :: DemoUtils
  }

-- | Run WebDriver BiDi client and return a client interface
withBiDiClient :: BiDiClientParams -> BiDiUrl -> (DemoUtils -> BiDiMethods -> IO ()) -> IO ()
withBiDiClient
  MkBiDiClientParams
    { biDiMethods,
      logQueue,
      messageLoops,
      demoUtils
    }
  bidiUrl
  action =
    withClient bidiUrl logQueue messageLoops $ action demoUtils biDiMethods

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
    eventHandler' <- failAction "eventhandler" failEventCount $ const a.eventHandler
    pure $
      MkMessageActions
        { send,
          get,
          print = print' 1,
          eventHandler = eventHandler' 1
        }

demoMessageActions :: Maybe Printer -> Channels -> MessageActions
demoMessageActions mPrinter MkChannels {sendChan, receiveChan, eventChan, logQueue, subscriptions} =
  MkMessageActions
    { send = \conn -> do
        msgToSend <- atomically $ readTChan sendChan
        queueLog $ "Sending Message: " <> jsonToText msgToSend
        catchLog
          "Message Send Failed"
          queueLog
          (sendTextData conn (BL.toStrict $ encode msgToSend)),
      --
      get = \conn -> do
        msg <- receiveData conn
        queueLog $ "Received raw data: " <> T.take 100 (txt msg) <> "..."
        queueLog $ "Received raw data: " <> txt msg
        let writeReceiveChan = atomically . writeTChan receiveChan
            writeEventChan = atomically . writeTChan eventChan
            r = decodeResponse msg
        case r of
          Left {} -> writeReceiveChan r
          Right r' -> case r' of
            NoID obj -> writeEventChan obj
            WithID {} -> writeReceiveChan r,
      --
      print =
        maybe
          (pure ())
          (logQueue.deQueueLog >>=)
          ((.printLog) <$> mPrinter),
      --
      eventHandler = do
        obj <- atomically $ readTChan eventChan
        queueLog $ "Event received: " <> jsonToText (toJSON obj)
        applySubscriptions queueLog obj subscriptions
    }
  where
    queueLog :: Text -> IO ()
    queueLog = when (isJust mPrinter) . coerce logQueue.queueLog

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
      printLoop = asyncLoop "Logger" print,
      eventLoop = asyncLoop "EventHandler" eventHandler
    }
  where
    asyncLoop = loopForever ql.queueLog

data MessageLoops = MkMessageLoops
  { sendLoop :: Connection -> IO (Async ()),
    getLoop :: Connection -> IO (Async ()),
    printLoop :: IO (Async ()),
    eventLoop :: IO (Async ())
  }

newtype Printer = MkPrinter
  { printLog :: Text -> IO ()
  }

demoMessageLoops :: Maybe Printer -> Channels -> MessageLoops
demoMessageLoops mPrnter channels =
  loopActions channels.logQueue.queueLog $ demoMessageActions mPrnter channels

failMessageLoops ::
  Maybe Printer ->
  Channels ->
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  IO MessageLoops
failMessageLoops mPrinter channels failSendCount failGetCount failPrintCount failEventCount =
  loopActions channels.logQueue.queueLog
    <$> failMessageActions
      (demoMessageActions mPrinter channels)
      failSendCount
      failGetCount
      failPrintCount
      failEventCount

data LogQueue = MkLogQueue
  { queueLog :: QueLog,
    deQueueLog :: IO Text,
    waitEmpty :: IO ()
  }

withClient :: BiDiUrl -> LogQueue -> MessageLoops -> IO () -> IO ()
withClient
  pth@MkBiDiUrl {host, port, path}
  logQueue
  messageLoops
  action =
    do
      log $ "Connecting to WebDriver at " <> txt pth
      runClient (unpack host) port (unpack path) $ \conn -> do
        printLoop <- messageLoops.printLoop
        eventLoop <- messageLoops.eventLoop
        getLoop <- messageLoops.getLoop conn
        sendLoop <- messageLoops.sendLoop conn

        log "WebSocket connection established"

        result <- async action

        (_asy, ethresult) <- waitAnyCatch [getLoop, sendLoop, result, printLoop, eventLoop]

        -- cancelMany not reexported by UnliftIO
        cancel getLoop
        cancel sendLoop
        cancel result
        cancel eventLoop

        logQueue.waitEmpty
        cancel printLoop
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
      log = coerce logQueue.queueLog

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
loopForever queueLog name action = async $ do
  queueLog $ "Starting " <> name <> " thread"
  loop
  where
    loop = catchLog name queueLog action >> loop

newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull
