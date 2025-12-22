module BiDi.BiDiActions
  ( BiDiActions (..),
    mkActions,
  )
where

import BiDi.Socket qualified as Socket
import BiDi.Runner qualified as Runner
import Data.Aeson (FromJSON, Object, Value (..))
import Data.Text (Text)
import WebDriverPreCore.BiDi.API qualified as API
import WebDriverPreCore.BiDi.Protocol
  ( Capabilities,
    Command (..),
    DownloadEnd,
    DownloadWillBegin,
    HistoryUpdated,
    JSUInt (..),
    NavigationInfo,
    Subscription (..),
    UserPromptClosed,
    UserPromptOpened,
    toCommandText,
  )
import WebDriverPreCore.BiDi.Protocol as P
  ( Activate,
    AddDataCollector,
    AddDataCollectorResult,
    AddIntercept,
    AddInterceptResult,
    AddPreloadScript,
    AddPreloadScriptResult,
    AuthRequired,
    BeforeRequestSent,
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
    FetchError,
    FileDialogOpened,
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
    KnownSubscriptionType (..),
    LocateNodes,
    LocateNodesResult,
    LogEntry,
    Message,
    Navigate,
    NavigateResult,
    PerformActions,
    Print,
    PrintResult,
    ProvideResponse,
    RealmDestroyed,
    RealmInfo,
    ReleaseActions,
    Reload,
    RemoveDataCollector,
    RemoveIntercept,
    RemovePreloadScript,
    RemoveUserContext,
    ResponseCompleted,
    ResponseStarted,
    SessionNewResult,
    SessionStatusResult,
    SessionSubscibe (..),
    SessionSubscribeResult (..),
    SessionUnsubscribe (..),
    SetCacheBehavior,
    SetClientWindowState,
    SetCookie,
    SetCookieResult,
    SetDownloadBehavior,
    SetExtraHeaders,
    SetFiles,
    SetForcedColorsModeThemeOverride,
    SetGeolocationOverride,
    SetLocaleOverride,
    SetNetworkConditions,
    SetScreenOrientationOverride,
    SetScreenSettingsOverride,
    SetScriptingEnabled,
    SetTimezoneOverride,
    SetUserAgentOverride,
    SetViewport,
    SubscriptionId (..),
    TraverseHistory,
    TraverseHistoryResult,
    OffSpecSubscriptionType,
    UserContext,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
  )

_for_implementation_see_mkActions :: Socket.SocketActions -> BiDiActions
_for_implementation_see_mkActions = mkActions

data BiDiActions = MkBiDiActions
  { -- Session commands
    sessionNew :: Capabilities -> IO SessionNewResult,
    sessionStatus :: IO SessionStatusResult,
    sessionEnd :: IO (),
    -- sessionSubscribe and sessionUnsubscribe are exposed for demo commands but they would not be used normally
    -- in client code. They would be called via the subscribe/unsubscribe methods below so the runner can
    -- locally track subscriptions
    sessionSubscribe :: SessionSubscibe -> IO SubscriptionId,
    sessionUnsubscribe :: SessionUnsubscribe -> IO (),
    -- BrowsingContext commands
    browsingContextActivate :: Activate -> IO (),
    browsingContextCaptureScreenshot :: CaptureScreenshot -> IO CaptureScreenshotResult,
    browsingContextClose :: Close -> IO (),
    -- | Create a browsing context
    browsingContextCreate :: Create -> IO BrowsingContext,
    browsingContextGetTree :: GetTree -> IO GetTreeResult,
    browsingContextHandleUserPrompt :: HandleUserPrompt -> IO (),
    browsingContextLocateNodes :: LocateNodes -> IO LocateNodesResult,
    browsingContextNavigate :: Navigate -> IO NavigateResult,
    browsingContextPrint :: Print -> IO PrintResult,
    browsingContextReload :: Reload -> IO (),
    browsingContextSetViewport :: SetViewport -> IO (),
    browsingContextTraverseHistory :: TraverseHistory -> IO TraverseHistoryResult,
    -- Browser commandssendCommandNoWait socket
    browserClose :: IO (),
    browserCreateUserContext :: CreateUserContext -> IO UserContext,
    browserGetClientWindows :: IO GetClientWindowsResult,
    browserGetUserContexts :: IO GetUserContextsResult,
    browserRemoveUserContext :: RemoveUserContext -> IO (),
    browserSetClientWindowState :: SetClientWindowState -> IO ClientWindowInfo,
    browserSetDownloadBehavior :: SetDownloadBehavior -> IO (),
    -- Emulation commands
    emulationSetForcedColorsModeThemeOverride :: SetForcedColorsModeThemeOverride -> IO (),
    emulationSetGeolocationOverride :: SetGeolocationOverride -> IO (),
    emulationSetLocaleOverride :: SetLocaleOverride -> IO (),
    emulationSetNetworkConditions :: SetNetworkConditions -> IO (),
    emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> IO (),
    emulationSetScreenSettingsOverride :: SetScreenSettingsOverride -> IO (),
    emulationSetScriptingEnabled :: SetScriptingEnabled -> IO (),
    emulationSetTimezoneOverride :: SetTimezoneOverride -> IO (),
    emulationSetUserAgentOverride :: SetUserAgentOverride -> IO (),
    -- Input commands
    inputPerformActions :: PerformActions -> IO (),
    inputReleaseActions :: ReleaseActions -> IO (),
    inputSetFiles :: SetFiles -> IO (),
    -- Network commands
    networkAddDataCollector :: AddDataCollector -> IO AddDataCollectorResult,
    networkAddIntercept :: AddIntercept -> IO AddInterceptResult,
    networkContinueRequest :: ContinueRequest -> IO (),
    networkContinueResponse :: ContinueResponse -> IO (),
    networkContinueWithAuth :: ContinueWithAuth -> IO (),
    networkDisownData :: DisownData -> IO (),
    networkFailRequest :: FailRequest -> IO (),
    networkGetData :: GetData -> IO GetDataResult,
    networkProvideResponse :: ProvideResponse -> IO (),
    networkRemoveDataCollector :: RemoveDataCollector -> IO (),
    networkRemoveIntercept :: RemoveIntercept -> IO (),
    networkSetCacheBehavior :: SetCacheBehavior -> IO (),
    networkSetExtraHeaders :: SetExtraHeaders -> IO (),
    -- Script commands
    scriptAddPreloadScript :: AddPreloadScript -> IO AddPreloadScriptResult,
    scriptCallFunction :: CallFunction -> IO EvaluateResult,
    scriptDisown :: Disown -> IO (),
    scriptEvaluate :: Evaluate -> IO EvaluateResult,
    scriptEvaluateNoWait :: Evaluate -> IO Socket.Request,
    scriptGetRealms :: GetRealms -> IO GetRealmsResult,
    scriptRemovePreloadScript :: RemovePreloadScript -> IO (),
    -- Storage commands
    storageDeleteCookies :: DeleteCookies -> IO DeleteCookiesResult,
    storageGetCookies :: GetCookies -> IO GetCookiesResult,
    storageSetCookie :: SetCookie -> IO SetCookieResult,
    -- WebExtension commands
    webExtensionInstall :: WebExtensionInstall -> IO WebExtensionResult,
    webExtensionUninstall :: WebExtensionUninstall -> IO (),
    -- ########## EVENTS ##########

    subscribeMany :: [KnownSubscriptionType] -> (Event -> IO ()) -> IO SubscriptionId,
    subscribeMany' :: [BrowsingContext] -> [UserContext] -> [KnownSubscriptionType] -> (Event -> IO ()) -> IO SubscriptionId,
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
    --
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
    sendCommand :: forall r. (FromJSON r) => Command r -> IO r,
    sendCommand' :: forall r. (FromJSON r) => JSUInt -> Command r -> IO r,
    sendCommandNoWait :: forall r. Command r -> IO Socket.Request,
    sendOffSpecCommand' :: JSUInt -> Text -> Object -> IO Object,
    sendOffSpecCommandNoWait :: Text -> Object -> IO Socket.Request,
    -- fallback subscriptions
    subscribeUnknownMany ::
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId,
    subscribeUnknownMany' ::
      [BrowsingContext] ->
      [UserContext] ->
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId
  }

mkActions :: Socket.SocketActions -> BiDiActions
mkActions socket =
  MkBiDiActions
    { -- ########## COMMANDS ##########
      -- Session commands
      sessionNew = send . API.sessionNew,
      sessionStatus = send API.sessionStatus,
      sessionEnd = send API.sessionEnd,
      sessionSubscribe = fmap (.subscription) . sessionSubscribe,
      sessionUnsubscribe = send . API.sessionUnsubscribe,
      -- BrowsingContext commands
      browsingContextActivate = send . API.browsingContextActivate,
      browsingContextCaptureScreenshot = send . API.browsingContextCaptureScreenshot,
      browsingContextClose = send . API.browsingContextClose,
      browsingContextCreate = send . API.browsingContextCreate,
      browsingContextGetTree = send . API.browsingContextGetTree,
      browsingContextHandleUserPrompt = send . API.browsingContextHandleUserPrompt,
      browsingContextLocateNodes = send . API.browsingContextLocateNodes,
      browsingContextNavigate = send . API.browsingContextNavigate,
      browsingContextPrint = send . API.browsingContextPrint,
      browsingContextReload = send . API.browsingContextReload,
      browsingContextSetViewport = send . API.browsingContextSetViewport,
      browsingContextTraverseHistory = send . API.browsingContextTraverseHistory,
      -- Browser commands
      browserClose = send API.browserClose,
      browserCreateUserContext = send . API.browserCreateUserContext,
      browserGetClientWindows = send API.browserGetClientWindows,
      browserGetUserContexts = send API.browserGetUserContexts,
      browserRemoveUserContext = send . API.browserRemoveUserContext,
      browserSetClientWindowState = send . API.browserSetClientWindowState,
      browserSetDownloadBehavior = send . API.browserSetDownloadBehavior,
      -- Emulation commands
      emulationSetForcedColorsModeThemeOverride = send . API.emulationSetForcedColorsModeThemeOverride,
      emulationSetGeolocationOverride = send . API.emulationSetGeolocationOverride,
      emulationSetLocaleOverride = send . API.emulationSetLocaleOverride,
      emulationSetNetworkConditions = send . API.emulationSetNetworkConditions,
      emulationSetScreenOrientationOverride = send . API.emulationSetScreenOrientationOverride,
      emulationSetScreenSettingsOverride = send . API.emulationSetScreenSettingsOverride,
      emulationSetScriptingEnabled = send . API.emulationSetScriptingEnabled,
      emulationSetTimezoneOverride = send . API.emulationSetTimezoneOverride,
      emulationSetUserAgentOverride = send . API.emulationSetUserAgentOverride,
      -- Input commands
      inputPerformActions = send . API.inputPerformActions,
      inputReleaseActions = send . API.inputReleaseActions,
      inputSetFiles = send . API.inputSetFiles,
      -- Network commands
      networkAddDataCollector = send . API.networkAddDataCollector,
      networkAddIntercept = send . API.networkAddIntercept,
      networkContinueRequest = send . API.networkContinueRequest,
      networkContinueResponse = send . API.networkContinueResponse,
      networkContinueWithAuth = send . API.networkContinueWithAuth,
      networkDisownData = send . API.networkDisownData,
      networkFailRequest = send . API.networkFailRequest,
      networkGetData = send . API.networkGetData,
      networkProvideResponse = send . API.networkProvideResponse,
      networkRemoveDataCollector = send . API.networkRemoveDataCollector,
      networkRemoveIntercept = send . API.networkRemoveIntercept,
      networkSetCacheBehavior = send . API.networkSetCacheBehavior,
      networkSetExtraHeaders = send . API.networkSetExtraHeaders,
      -- Script commands
      scriptAddPreloadScript = send . API.scriptAddPreloadScript,
      scriptCallFunction = send . API.scriptCallFunction,
      scriptDisown = send . API.scriptDisown,
      scriptEvaluate = send . API.scriptEvaluate,
      scriptEvaluateNoWait = sendCommandNoWait . API.scriptEvaluate,
      scriptGetRealms = send . API.scriptGetRealms,
      scriptRemovePreloadScript = send . API.scriptRemovePreloadScript,
      -- Storage commands
      storageDeleteCookies = send . API.storageDeleteCookies,
      storageGetCookies = send . API.storageGetCookies,
      storageSetCookie = send . API.storageSetCookie,
      -- WebExtension commands
      webExtensionInstall = send . API.webExtensionInstall,
      webExtensionUninstall = send . API.webExtensionUninstall,
      --
      -- ############## Subscriptions (Events) ##############

      subscribeMany = \sts -> subscribeMany' [] [] sts,
      subscribeMany',
      -- Log
      subscribeLogEntryAdded = sendSub API.subscribeLogEntryAdded,
      subscribeLogEntryAdded' = sendSub' API.subscribeLogEntryAdded,
      -- BrowsingContext
      subscribeBrowsingContextCreated = sendSub API.subscribeBrowsingContextCreated,
      subscribeBrowsingContextCreated' = sendSub' API.subscribeBrowsingContextCreated,
      subscribeBrowsingContextDestroyed = sendSub API.subscribeBrowsingContextDestroyed,
      subscribeBrowsingContextDestroyed' = sendSub' API.subscribeBrowsingContextDestroyed,
      subscribeBrowsingContextNavigationStarted = sendSub API.subscribeBrowsingContextNavigationStarted,
      subscribeBrowsingContextNavigationStarted' = sendSub' API.subscribeBrowsingContextNavigationStarted,
      subscribeBrowsingContextFragmentNavigated = sendSub API.subscribeBrowsingContextFragmentNavigated,
      subscribeBrowsingContextFragmentNavigated' = sendSub' API.subscribeBrowsingContextFragmentNavigated,
      subscribeBrowsingContextHistoryUpdated = sendSub API.subscribeBrowsingContextHistoryUpdated,
      subscribeBrowsingContextHistoryUpdated' = sendSub' API.subscribeBrowsingContextHistoryUpdated,
      subscribeBrowsingContextDomContentLoaded = sendSub API.subscribeBrowsingContextDomContentLoaded,
      subscribeBrowsingContextDomContentLoaded' = sendSub' API.subscribeBrowsingContextDomContentLoaded,
      subscribeBrowsingContextLoad = sendSub API.subscribeBrowsingContextLoad,
      subscribeBrowsingContextLoad' = sendSub' API.subscribeBrowsingContextLoad,
      subscribeBrowsingContextDownloadWillBegin = sendSub API.subscribeBrowsingContextDownloadWillBegin,
      subscribeBrowsingContextDownloadWillBegin' = sendSub' API.subscribeBrowsingContextDownloadWillBegin,
      subscribeBrowsingContextDownloadEnd = sendSub API.subscribeBrowsingContextDownloadEnd,
      subscribeBrowsingContextDownloadEnd' = sendSub' API.subscribeBrowsingContextDownloadEnd,
      subscribeBrowsingContextNavigationAborted = sendSub API.subscribeBrowsingContextNavigationAborted,
      subscribeBrowsingContextNavigationAborted' = sendSub' API.subscribeBrowsingContextNavigationAborted,
      subscribeBrowsingContextNavigationCommitted = sendSub API.subscribeBrowsingContextNavigationCommitted,
      subscribeBrowsingContextNavigationCommitted' = sendSub' API.subscribeBrowsingContextNavigationCommitted,
      subscribeBrowsingContextNavigationFailed = sendSub API.subscribeBrowsingContextNavigationFailed,
      subscribeBrowsingContextNavigationFailed' = sendSub' API.subscribeBrowsingContextNavigationFailed,
      subscribeBrowsingContextUserPromptClosed = sendSub API.subscribeBrowsingContextUserPromptClosed,
      subscribeBrowsingContextUserPromptClosed' = sendSub' API.subscribeBrowsingContextUserPromptClosed,
      subscribeBrowsingContextUserPromptOpened = sendSub API.subscribeBrowsingContextUserPromptOpened,
      subscribeBrowsingContextUserPromptOpened' = sendSub' API.subscribeBrowsingContextUserPromptOpened,
      -- Network
      subscribeNetworkAuthRequired = sendSub API.subscribeNetworkAuthRequired,
      subscribeNetworkAuthRequired' = sendSub' API.subscribeNetworkAuthRequired,
      subscribeNetworkBeforeRequestSent = sendSub API.subscribeNetworkBeforeRequestSent,
      subscribeNetworkBeforeRequestSent' = sendSub' API.subscribeNetworkBeforeRequestSent,
      subscribeNetworkFetchError = sendSub API.subscribeNetworkFetchError,
      subscribeNetworkFetchError' = sendSub' API.subscribeNetworkFetchError,
      subscribeNetworkResponseCompleted = sendSub API.subscribeNetworkResponseCompleted,
      subscribeNetworkResponseCompleted' = sendSub' API.subscribeNetworkResponseCompleted,
      subscribeNetworkResponseStarted = sendSub API.subscribeNetworkResponseStarted,
      subscribeNetworkResponseStarted' = sendSub' API.subscribeNetworkResponseStarted,
      -- Script
      subscribeScriptMessage = sendSub API.subscribeScriptMessage,
      subscribeScriptMessage' = sendSub' API.subscribeScriptMessage,
      subscribeScriptRealmCreated = sendSub API.subscribeScriptRealmCreated,
      subscribeScriptRealmCreated' = sendSub' API.subscribeScriptRealmCreated,
      subscribeScriptRealmDestroyed = sendSub API.subscribeScriptRealmDestroyed,
      subscribeScriptRealmDestroyed' = sendSub' API.subscribeScriptRealmDestroyed,
      -- Input
      subscribeInputFileDialogOpened = sendSub API.subscribeInputFileDialogOpened,
      subscribeInputFileDialogOpened' = sendSub' API.subscribeInputFileDialogOpened,
      --
      unsubscribe,
      -- fallback command methods
      sendCommandNoWait,
      sendCommand',
      --
      sendCommand = send,
      sendOffSpecCommand',
      sendOffSpecCommandNoWait,
      -- fallback subscriptions
      subscribeUnknownMany,
      subscribeUnknownMany'
    }
  where
    runnerUnsubscribe :: SessionUnsubscribe -> IO ()
    runnerUnsubscribe = Runner.unsubscribe socket sessionUnsubscribe

    unsubscribe :: SubscriptionId -> IO ()
    unsubscribe subId = unsubscribeMany [subId]

    unsubscribeMany :: [SubscriptionId] -> IO ()
    unsubscribeMany subIds = runnerUnsubscribe (UnsubscribeById subIds)

    toBiDi :: Command r -> Socket.SocketCommand Text r
    toBiDi MkCommand {method, params} =
      Socket.MkSocketCommand
        { method = toCommandText method,
          params = Object params
        }

    send :: forall r. (FromJSON r) => Command r -> IO r
    send = Socket.sendCommand socket . toBiDi

    sendCommand' :: forall r. (FromJSON r) => JSUInt -> Command r -> IO r
    sendCommand' id' = Socket.sendCommand' socket id' . toBiDi

    sendCommandNoWait :: forall r. Command r -> IO Socket.Request
    sendCommandNoWait = Socket.sendCommandNoWait socket . toBiDi

    sendOffSpecCommand' :: JSUInt -> Text -> Object -> IO Object
    sendOffSpecCommand' id' method =
      Socket.sendCommand' socket id' . Socket.MkSocketCommand method . Object

    sendOffSpecCommandNoWait :: Text -> Object -> IO Socket.Request
    sendOffSpecCommandNoWait method =
      Socket.sendCommandNoWait socket . Socket.MkSocketCommand method . Object

    sessionSubscribe :: SessionSubscibe -> IO SessionSubscribeResult
    sessionSubscribe = send . API.sessionSubscribe

    sessionUnsubscribe :: SessionUnsubscribe -> IO ()
    sessionUnsubscribe = send . API.sessionUnsubscribe

    subscribeMany' ::
      [BrowsingContext] ->
      [UserContext] ->
      [KnownSubscriptionType] ->
      (Event -> IO ()) ->
      IO SubscriptionId
    subscribeMany' bcs ucs sts = Runner.subscribe socket sessionSubscribe . API.subscribeMany sts bcs ucs

    subscribeUnknownMany ::
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId
    subscribeUnknownMany sts = Runner.subscribe socket sessionSubscribe . API.subscribeOffSpecMany sts [] []

    subscribeUnknownMany' ::
      [BrowsingContext] ->
      [UserContext] ->
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId
    subscribeUnknownMany' bcs ucs sts = Runner.subscribe socket sessionSubscribe . API.subscribeOffSpecMany sts bcs ucs

    sendSub ::
      ( [BrowsingContext] ->
        [UserContext] ->
        (a -> IO ()) ->
        Subscription IO
      ) ->
      (a -> IO ()) ->
      IO SubscriptionId
    sendSub mkSubscription =
      Runner.subscribe socket sessionSubscribe . mkSubscription [] []

    sendSub' ::
      ( [BrowsingContext] ->
        [UserContext] ->
        (a -> IO ()) ->
        Subscription IO
      ) ->
      [BrowsingContext] ->
      [UserContext] ->
      (a -> IO ()) ->
      IO SubscriptionId
    sendSub' mkSubscription bcs ucs =
      Runner.subscribe socket sessionSubscribe . mkSubscription bcs ucs
