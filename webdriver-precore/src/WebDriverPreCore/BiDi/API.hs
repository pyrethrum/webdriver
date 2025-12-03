module WebDriverPreCore.BiDi.API
  ( -- * Session Commands
    sessionNew,
    sessionStatus,
    sessionEnd,
    sessionSubscribe,
    sessionUnsubscribe,

    -- * BrowsingContext Commands
    browsingContextActivate,
    browsingContextCaptureScreenshot,
    browsingContextClose,
    browsingContextCreate,
    browsingContextGetTree,
    browsingContextHandleUserPrompt,
    browsingContextLocateNodes,
    browsingContextNavigate,
    browsingContextPrint,
    browsingContextReload,
    browsingContextSetViewport,
    browsingContextTraverseHistory,

    -- * Browser Commands
    browserClose,
    browserCreateUserContext,
    browserGetClientWindows,
    browserGetUserContexts,
    browserRemoveUserContext,
    browserSetClientWindowState,
    browserSetDownloadBehavior,

    -- * Emulation Commands
    emulationSetForcedColorsModeThemeOverride,
    emulationSetGeolocationOverride,
    emulationSetLocaleOverride,
    emulationSetNetworkConditions,
    emulationSetScreenOrientationOverride,
    emulationSetScreenSettingsOverride,
    emulationSetScriptingEnabled,
    emulationSetTimezoneOverride,
    emulationSetUserAgentOverride,

    -- * Input Commands
    inputPerformActions,
    inputReleaseActions,
    inputSetFiles,

    -- * Network Commands
    networkAddDataCollector,
    networkAddIntercept,
    networkContinueRequest,
    networkContinueResponse,
    networkContinueWithAuth,
    networkDisownData,
    networkFailRequest,
    networkGetData,
    networkProvideResponse,
    networkRemoveDataCollector,
    networkRemoveIntercept,
    networkSetCacheBehavior,
    networkSetExtraHeaders,

    -- * Script Commands
    scriptAddPreloadScript,
    scriptCallFunction,
    scriptDisown,
    scriptEvaluate,
    scriptGetRealms,
    scriptRemovePreloadScript,

    -- * Storage Commands
    storageDeleteCookies,
    storageGetCookies,
    storageSetCookie,

    -- * WebExtension Commands
    webExtensionInstall,
    webExtensionUninstall,

    -- * Subscriptions
    subscribeLogEntryAdded,
    subscribeBrowsingContextCreated,
    subscribeBrowsingContextDestroyed,
    subscribeBrowsingContextNavigationStarted,
    subscribeBrowsingContextFragmentNavigated,
    subscribeBrowsingContextHistoryUpdated,
    subscribeBrowsingContextDomContentLoaded,
    subscribeBrowsingContextLoad,
    subscribeBrowsingContextDownloadWillBegin,
    subscribeBrowsingContextDownloadEnd,
    subscribeBrowsingContextNavigationAborted,
    subscribeBrowsingContextNavigationCommitted,
    subscribeBrowsingContextNavigationFailed,
    subscribeBrowsingContextUserPromptClosed,
    subscribeBrowsingContextUserPromptOpened,
    subscribeNetworkAuthRequired,
    subscribeNetworkBeforeRequestSent,
    subscribeNetworkFetchError,
    subscribeNetworkResponseCompleted,
    subscribeNetworkResponseStarted,
    subscribeScriptMessage,
    subscribeScriptRealmCreated,
    subscribeScriptRealmDestroyed,
    subscribeInputFileDialogOpened,
    subscribeMany,

    -- * Fallback Subscriptions
    subscribeUnknownMany,

    -- * Fallback Commands
    module FallbackCommand,
  )
where

import Data.Aeson (Value)
import WebDriverPreCore.BiDi.Protocol
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
    Capabilities,
    CaptureScreenshot,
    CaptureScreenshotResult,
    ClientWindowInfo,
    Close,
    Command,
    ContinueRequest,
    ContinueResponse,
    ContinueWithAuth,
    Create,
    CreateUserContext,
    DeleteCookies,
    DeleteCookiesResult,
    Disown,
    DisownData,
    DownloadEnd,
    DownloadWillBegin,
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
    HistoryUpdated,
    Info,
    KnownCommand (..),
    KnownSubscriptionType (..),
    LocateNodes,
    LocateNodesResult,
    LogEntry,
    Navigate,
    NavigateResult,
    NavigationInfo,
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
    Subscription,
    TraverseHistory,
    TraverseHistoryResult,
    UnknownSubscriptionType,
    UserContext,
    UserPromptClosed,
    UserPromptOpened,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
    mkMultiSubscription,
    mkSingleSubscription,
    mkUnknownSubscription,
  )
import WebDriverPreCore.BiDi.Protocol as FallbackCommand
  ( emptyCommand,
    extendCommand,
    extendCommandAny,
    loosenCommand,
    mkAnyCommand,
    mkCommand,
  )
import WebDriverPreCore.BiDi.Script (Message, RealmInfo)

--- ############## Commands ##############

---- Session ----

sessionNew :: Capabilities -> Command SessionNewResult
sessionNew = mkCommand SessionNew

sessionStatus :: Command SessionStatusResult
sessionStatus = emptyCommand SessionStatus

sessionEnd :: Command ()
sessionEnd = emptyCommand SessionEnd

sessionSubscribe :: SessionSubscibe -> Command SessionSubscribeResult
sessionSubscribe = mkCommand SessionSubscribe

sessionUnsubscribe :: SessionUnsubscribe -> Command ()
sessionUnsubscribe = mkCommand SessionUnsubscribe

---- Browsing Context ----

browsingContextActivate :: Activate -> Command ()
browsingContextActivate = mkCommand BrowsingContextActivate

browsingContextCaptureScreenshot :: CaptureScreenshot -> Command CaptureScreenshotResult
browsingContextCaptureScreenshot = mkCommand BrowsingContextCaptureScreenshot

browsingContextClose :: Close -> Command ()
browsingContextClose = mkCommand BrowsingContextClose

browsingContextCreate :: Create -> Command BrowsingContext
browsingContextCreate = mkCommand BrowsingContextCreate

browsingContextGetTree :: GetTree -> Command GetTreeResult
browsingContextGetTree = mkCommand BrowsingContextGetTree

browsingContextHandleUserPrompt :: HandleUserPrompt -> Command ()
browsingContextHandleUserPrompt = mkCommand BrowsingContextHandleUserPrompt

browsingContextLocateNodes :: LocateNodes -> Command LocateNodesResult
browsingContextLocateNodes = mkCommand BrowsingContextLocateNodes

browsingContextNavigate :: Navigate -> Command NavigateResult
browsingContextNavigate = mkCommand BrowsingContextNavigate

browsingContextPrint :: Print -> Command PrintResult
browsingContextPrint = mkCommand BrowsingContextPrint

browsingContextReload :: Reload -> Command ()
browsingContextReload = mkCommand BrowsingContextReload

browsingContextSetViewport :: SetViewport -> Command ()
browsingContextSetViewport = mkCommand BrowsingContextSetViewport

browsingContextTraverseHistory :: TraverseHistory -> Command TraverseHistoryResult
browsingContextTraverseHistory = mkCommand BrowsingContextTraverseHistory

---- Browser ----

browserClose :: Command ()
browserClose = emptyCommand BrowserClose

browserCreateUserContext :: CreateUserContext -> Command UserContext
browserCreateUserContext = mkCommand BrowserCreateUserContext

browserGetClientWindows :: Command GetClientWindowsResult
browserGetClientWindows = emptyCommand BrowserGetClientWindows

browserGetUserContexts :: Command GetUserContextsResult
browserGetUserContexts = emptyCommand BrowserGetUserContexts

browserRemoveUserContext :: RemoveUserContext -> Command ()
browserRemoveUserContext = mkCommand BrowserRemoveUserContext

browserSetClientWindowState :: SetClientWindowState -> Command ClientWindowInfo
browserSetClientWindowState = mkCommand BrowserSetClientWindowState

-- since 18-09-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250918
browserSetDownloadBehavior :: SetDownloadBehavior -> Command ()
browserSetDownloadBehavior = mkCommand BrowserSetDownloadBehavior

---- Emulation ----

-- since 29-07-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250729
emulationSetForcedColorsModeThemeOverride :: SetForcedColorsModeThemeOverride -> Command ()
emulationSetForcedColorsModeThemeOverride = mkCommand EmulationSetForcedColorsModeThemeOverride

emulationSetGeolocationOverride :: SetGeolocationOverride -> Command ()
emulationSetGeolocationOverride = mkCommand EmulationSetGeolocationOverride

emulationSetLocaleOverride :: SetLocaleOverride -> Command ()
emulationSetLocaleOverride = mkCommand EmulationSetLocaleOverride

-- since 07-10-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007
emulationSetNetworkConditions :: SetNetworkConditions -> Command ()
emulationSetNetworkConditions = mkCommand EmulationSetNetworkConditions

emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> Command ()
emulationSetScreenOrientationOverride = mkCommand EmulationSetScreenOrientationOverride

-- since 20-11-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120
emulationSetScreenSettingsOverride :: SetScreenSettingsOverride -> Command ()
emulationSetScreenSettingsOverride = mkCommand EmulationSetScreenSettingsOverride

-- since 11-08-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811
emulationSetScriptingEnabled :: SetScriptingEnabled -> Command ()
emulationSetScriptingEnabled = mkCommand EmulationSetScriptingEnabled

emulationSetTimezoneOverride :: SetTimezoneOverride -> Command ()
emulationSetTimezoneOverride = mkCommand EmulationSetTimezoneOverride

-- since 10-09-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250910
emulationSetUserAgentOverride :: SetUserAgentOverride -> Command ()
emulationSetUserAgentOverride = mkCommand EmulationSetUserAgentOverride

---- Input ----

inputPerformActions :: PerformActions -> Command ()
inputPerformActions = mkCommand InputPerformActions

inputReleaseActions :: ReleaseActions -> Command ()
inputReleaseActions = mkCommand InputReleaseActions

inputSetFiles :: SetFiles -> Command ()
inputSetFiles = mkCommand InputSetFiles

---- Network ----

networkAddDataCollector :: AddDataCollector -> Command AddDataCollectorResult
networkAddDataCollector = mkCommand NetworkAddDataCollector

networkAddIntercept :: AddIntercept -> Command AddInterceptResult
networkAddIntercept = mkCommand NetworkAddIntercept

networkContinueRequest :: ContinueRequest -> Command ()
networkContinueRequest = mkCommand NetworkContinueRequest

networkContinueResponse :: ContinueResponse -> Command ()
networkContinueResponse = mkCommand NetworkContinueResponse

networkContinueWithAuth :: ContinueWithAuth -> Command ()
networkContinueWithAuth = mkCommand NetworkContinueWithAuth

networkDisownData :: DisownData -> Command ()
networkDisownData = mkCommand NetworkDisownData

networkFailRequest :: FailRequest -> Command ()
networkFailRequest = mkCommand NetworkFailRequest

networkGetData :: GetData -> Command GetDataResult
networkGetData = mkCommand NetworkGetData

networkProvideResponse :: ProvideResponse -> Command ()
networkProvideResponse = mkCommand NetworkProvideResponse

networkRemoveDataCollector :: RemoveDataCollector -> Command ()
networkRemoveDataCollector = mkCommand NetworkRemoveDataCollector

networkRemoveIntercept :: RemoveIntercept -> Command ()
networkRemoveIntercept = mkCommand NetworkRemoveIntercept

networkSetCacheBehavior :: SetCacheBehavior -> Command ()
networkSetCacheBehavior = mkCommand NetworkSetCacheBehavior

networkSetExtraHeaders :: SetExtraHeaders -> Command ()
networkSetExtraHeaders = mkCommand NetworkSetExtraHeaders

---- Script ----

scriptAddPreloadScript :: AddPreloadScript -> Command AddPreloadScriptResult
scriptAddPreloadScript = mkCommand ScriptAddPreloadScript

scriptCallFunction :: CallFunction -> Command EvaluateResult
scriptCallFunction = mkCommand ScriptCallFunction

scriptDisown :: Disown -> Command ()
scriptDisown = mkCommand ScriptDisown

scriptEvaluate :: Evaluate -> Command EvaluateResult
scriptEvaluate = mkCommand ScriptEvaluate

scriptGetRealms :: GetRealms -> Command GetRealmsResult
scriptGetRealms = mkCommand ScriptGetRealms

scriptRemovePreloadScript :: RemovePreloadScript -> Command ()
scriptRemovePreloadScript = mkCommand ScriptRemovePreloadScript

---- Storage ----

storageDeleteCookies :: DeleteCookies -> Command DeleteCookiesResult
storageDeleteCookies = mkCommand StorageDeleteCookies

storageGetCookies :: GetCookies -> Command GetCookiesResult
storageGetCookies = mkCommand StorageGetCookies

storageSetCookie :: SetCookie -> Command SetCookieResult
storageSetCookie = mkCommand StorageSetCookie

---- WebExtension ----

webExtensionInstall :: WebExtensionInstall -> Command WebExtensionResult
webExtensionInstall = mkCommand WebExtensionInstall

webExtensionUninstall :: WebExtensionUninstall -> Command ()
webExtensionUninstall = mkCommand WebExtensionUninstall

-- ############## Subscriptions (Events) ##############

subscribeMany ::
  [KnownSubscriptionType] ->
  [BrowsingContext] ->
  [UserContext] ->
  (Event -> m ()) ->
  Subscription m
subscribeMany = mkMultiSubscription

subscribeUnknownMany ::
  [UnknownSubscriptionType] ->
  [BrowsingContext] ->
  [UserContext] ->
  (Value -> m ()) ->
  Subscription m
subscribeUnknownMany = mkUnknownSubscription

---- BrowsingContext ----

subscribeBrowsingContextCreated ::
  [BrowsingContext] ->
  [UserContext] ->
  (Info -> m ()) ->
  Subscription m
subscribeBrowsingContextCreated = mkSingleSubscription BrowsingContextContextCreated

subscribeBrowsingContextDestroyed ::
  [BrowsingContext] ->
  [UserContext] ->
  (Info -> m ()) ->
  Subscription m
subscribeBrowsingContextDestroyed = mkSingleSubscription BrowsingContextContextDestroyed

subscribeBrowsingContextNavigationStarted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationStarted = mkSingleSubscription BrowsingContextNavigationStarted

subscribeBrowsingContextFragmentNavigated ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextFragmentNavigated = mkSingleSubscription BrowsingContextFragmentNavigated

subscribeBrowsingContextHistoryUpdated ::
  [BrowsingContext] ->
  [UserContext] ->
  (HistoryUpdated -> m ()) ->
  Subscription m
subscribeBrowsingContextHistoryUpdated = mkSingleSubscription BrowsingContextHistoryUpdated

subscribeBrowsingContextDomContentLoaded ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextDomContentLoaded = mkSingleSubscription BrowsingContextDomContentLoaded

subscribeBrowsingContextLoad ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextLoad = mkSingleSubscription BrowsingContextLoad

subscribeBrowsingContextDownloadWillBegin ::
  [BrowsingContext] ->
  [UserContext] ->
  (DownloadWillBegin -> m ()) ->
  Subscription m
subscribeBrowsingContextDownloadWillBegin = mkSingleSubscription BrowsingContextDownloadWillBegin

subscribeBrowsingContextDownloadEnd ::
  [BrowsingContext] ->
  [UserContext] ->
  (DownloadEnd -> m ()) ->
  Subscription m
subscribeBrowsingContextDownloadEnd = mkSingleSubscription BrowsingContextDownloadEnd

subscribeBrowsingContextNavigationAborted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationAborted = mkSingleSubscription BrowsingContextNavigationAborted

subscribeBrowsingContextNavigationCommitted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationCommitted = mkSingleSubscription BrowsingContextNavigationCommitted

subscribeBrowsingContextNavigationFailed ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationFailed = mkSingleSubscription BrowsingContextNavigationFailed

subscribeBrowsingContextUserPromptClosed ::
  [BrowsingContext] ->
  [UserContext] ->
  (UserPromptClosed -> m ()) ->
  Subscription m
subscribeBrowsingContextUserPromptClosed = mkSingleSubscription BrowsingContextUserPromptClosed

subscribeBrowsingContextUserPromptOpened ::
  [BrowsingContext] ->
  [UserContext] ->
  (UserPromptOpened -> m ()) ->
  Subscription m
subscribeBrowsingContextUserPromptOpened = mkSingleSubscription BrowsingContextUserPromptOpened

---- Log ----

subscribeLogEntryAdded ::
  [BrowsingContext] ->
  [UserContext] ->
  (LogEntry -> m ()) ->
  Subscription m
subscribeLogEntryAdded = mkSingleSubscription LogEntryAdded

---- Network ----

subscribeNetworkAuthRequired ::
  [BrowsingContext] ->
  [UserContext] ->
  (AuthRequired -> m ()) ->
  Subscription m
subscribeNetworkAuthRequired = mkSingleSubscription NetworkAuthRequired

subscribeNetworkBeforeRequestSent ::
  [BrowsingContext] ->
  [UserContext] ->
  (BeforeRequestSent -> m ()) ->
  Subscription m
subscribeNetworkBeforeRequestSent = mkSingleSubscription NetworkBeforeRequestSent

subscribeNetworkFetchError ::
  [BrowsingContext] ->
  [UserContext] ->
  (FetchError -> m ()) ->
  Subscription m
subscribeNetworkFetchError = mkSingleSubscription NetworkFetchError

subscribeNetworkResponseCompleted ::
  [BrowsingContext] ->
  [UserContext] ->
  (ResponseCompleted -> m ()) ->
  Subscription m
subscribeNetworkResponseCompleted = mkSingleSubscription NetworkResponseCompleted

subscribeNetworkResponseStarted ::
  [BrowsingContext] ->
  [UserContext] ->
  (ResponseStarted -> m ()) ->
  Subscription m
subscribeNetworkResponseStarted = mkSingleSubscription NetworkResponseStarted

---- Script ----

subscribeScriptMessage ::
  [BrowsingContext] ->
  [UserContext] ->
  (Message -> m ()) ->
  Subscription m
subscribeScriptMessage = mkSingleSubscription ScriptMessage

subscribeScriptRealmCreated ::
  [BrowsingContext] ->
  [UserContext] ->
  (RealmInfo -> m ()) ->
  Subscription m
subscribeScriptRealmCreated = mkSingleSubscription ScriptRealmCreated

subscribeScriptRealmDestroyed ::
  [BrowsingContext] ->
  [UserContext] ->
  (RealmDestroyed -> m ()) ->
  Subscription m
subscribeScriptRealmDestroyed = mkSingleSubscription ScriptRealmDestroyed

---- Input ----

subscribeInputFileDialogOpened ::
  [BrowsingContext] ->
  [UserContext] ->
  (FileDialogOpened -> m ()) ->
  Subscription m
subscribeInputFileDialogOpened = mkSingleSubscription InputFileDialogOpened
