module BiDi.BiDiActions 
  ( BiDiActions (..),
    mkActions
  ) where

import Data.Aeson (FromJSON, Object, ToJSON)
import BiDi.BiDiSocket as Socket (BiDiSocket(..), CommandRequestInfo, sendCommand, sendCommand', sendCommandNoWait) 
import WebDriverPreCore.BiDi.API qualified as P
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
import WebDriverPreCore.BiDi.Script
  ( Message,
    RealmInfo,
  )
import Prelude

data BiDiActions = MkBiDiActions
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

mkActions :: BiDiSocket -> BiDiActions
mkActions socket =
  MkBiDiActions
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
      sendCommand' = Socket.sendCommand' socket
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