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

    -- * Emulation Commands
    emulationSetGeolocationOverride,
    emulationSetLocaleOverride,
    emulationSetScreenOrientationOverride,
    emulationSetTimezoneOverride,

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
    subscribeMany
  )
where

import Data.Aeson (Object)
import WebDriverPreCore.BiDi.BrowsingContext (NavigationInfo, DownloadWillBegin, DownloadEnd, HistoryUpdated, UserPromptClosed, UserPromptOpened)
import WebDriverPreCore.BiDi.Command (CommandMethod(..), mkCommandTxt)
import WebDriverPreCore.BiDi.Event
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log
import WebDriverPreCore.BiDi.Network (AuthRequired, BeforeRequestSent, FetchError, ResponseCompleted, ResponseStarted)
import WebDriverPreCore.BiDi.Script (Message, RealmInfo)
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
    Info,
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
    SubscriptionType (..),
    TraverseHistory,
    TraverseHistoryResult,
    UserContext,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
    emptyCommand,
    mkCommand,
    RealmDestroyed,
  )

-- TODO: generic commands
-- TODO: noWait - runner only
-- TODO: generic subscribe
-- TODO: generic unsubscribe
-- TODO: generic event
-- TODO: do Something about Objects -newtype?

--- ############## Commands ##############

---- Session ----

sessionNew :: Capabilities -> Command Capabilities SessionNewResult
sessionNew = mkCommand SessionNew

sessionStatus :: Command Object SessionStatusResult
sessionStatus = emptyCommand SessionStatus

sessionEnd :: Command Object Object
sessionEnd = emptyCommand SessionEnd

sessionSubscribe :: SessionSubscriptionRequest -> Command SessionSubscriptionRequest SessionSubscribeResult
sessionSubscribe = mkCommand SessionSubscribe

sessionUnsubscribe :: SessionUnsubscribe -> Command SessionUnsubscribe Object
sessionUnsubscribe = mkCommand SessionUnsubscribe

---- Browsing Context ----

browsingContextActivate :: Activate -> Command Activate Object
browsingContextActivate = mkCommand BrowsingContextActivate

browsingContextCaptureScreenshot :: CaptureScreenshot -> Command CaptureScreenshot CaptureScreenshotResult
browsingContextCaptureScreenshot = mkCommand BrowsingContextCaptureScreenshot

browsingContextClose :: Close -> Command Close Object
browsingContextClose = mkCommand BrowsingContextClose

browsingContextCreate :: Create -> Command Create BrowsingContext
browsingContextCreate = mkCommand BrowsingContextCreate

browsingContextGetTree :: GetTree -> Command GetTree GetTreeResult
browsingContextGetTree = mkCommand BrowsingContextGetTree

browsingContextHandleUserPrompt :: HandleUserPrompt -> Command HandleUserPrompt Object
browsingContextHandleUserPrompt = mkCommand BrowsingContextHandleUserPrompt

browsingContextLocateNodes :: LocateNodes -> Command LocateNodes LocateNodesResult
browsingContextLocateNodes = mkCommand BrowsingContextLocateNodes

browsingContextNavigate :: Navigate -> Command Navigate NavigateResult
browsingContextNavigate = mkCommand BrowsingContextNavigate

browsingContextPrint :: Print -> Command Print PrintResult
browsingContextPrint = mkCommand BrowsingContextPrint

browsingContextReload :: Reload -> Command Reload Object
browsingContextReload = mkCommand BrowsingContextReload

browsingContextSetViewport :: SetViewport -> Command SetViewport Object
browsingContextSetViewport = mkCommand BrowsingContextSetViewport

browsingContextTraverseHistory :: TraverseHistory -> Command TraverseHistory TraverseHistoryResult
browsingContextTraverseHistory = mkCommand BrowsingContextTraverseHistory

---- Browser ----

browserClose :: Command Object Object
browserClose = emptyCommand BrowserClose

browserCreateUserContext :: CreateUserContext -> Command CreateUserContext UserContext
browserCreateUserContext = mkCommand BrowserCreateUserContext

browserGetClientWindows :: Command Object GetClientWindowsResult
browserGetClientWindows = emptyCommand BrowserGetClientWindows

browserGetUserContexts :: Command Object GetUserContextsResult
browserGetUserContexts = emptyCommand BrowserGetUserContexts

browserRemoveUserContext :: RemoveUserContext -> Command RemoveUserContext Object
browserRemoveUserContext = mkCommand BrowserRemoveUserContext

browserSetClientWindowState :: SetClientWindowState -> Command SetClientWindowState ClientWindowInfo
browserSetClientWindowState = mkCommand BrowserSetClientWindowState

---- Emulation ----

emulationSetGeolocationOverride :: SetGeolocationOverride -> Command SetGeolocationOverride Object
emulationSetGeolocationOverride = mkCommandTxt "emulation.setGeolocationOverride"

emulationSetLocaleOverride :: SetLocaleOverride -> Command SetLocaleOverride Object
emulationSetLocaleOverride = mkCommandTxt "emulation.setLocaleOverride"

emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> Command SetScreenOrientationOverride Object
emulationSetScreenOrientationOverride = mkCommandTxt "emulation.setScreenOrientationOverride"

emulationSetTimezoneOverride :: SetTimezoneOverride -> Command SetTimezoneOverride Object
emulationSetTimezoneOverride = mkCommandTxt "emulation.setTimezoneOverride"

---- Input ----

inputPerformActions :: PerformActions -> Command PerformActions Object
inputPerformActions = mkCommand InputPerformActions

inputReleaseActions :: ReleaseActions -> Command ReleaseActions Object
inputReleaseActions = mkCommand InputReleaseActions

inputSetFiles :: SetFiles -> Command SetFiles Object
inputSetFiles = mkCommand InputSetFiles

---- Network ----

networkAddDataCollector :: AddDataCollector -> Command AddDataCollector AddDataCollectorResult
networkAddDataCollector = mkCommandTxt "network.addDataCollector"

networkAddIntercept :: AddIntercept -> Command AddIntercept AddInterceptResult
networkAddIntercept = mkCommand NetworkAddIntercept

networkContinueRequest :: ContinueRequest -> Command ContinueRequest Object
networkContinueRequest = mkCommand NetworkContinueRequest

networkContinueResponse :: ContinueResponse -> Command ContinueResponse Object
networkContinueResponse = mkCommand NetworkContinueResponse

networkContinueWithAuth :: ContinueWithAuth -> Command ContinueWithAuth Object
networkContinueWithAuth = mkCommand NetworkContinueWithAuth

networkDisownData :: DisownData -> Command DisownData Object
networkDisownData = mkCommandTxt "network.disownData"

networkFailRequest :: FailRequest -> Command FailRequest Object
networkFailRequest = mkCommand NetworkFailRequest

networkGetData :: GetData -> Command GetData GetDataResult
networkGetData = mkCommandTxt "network.getData"

networkProvideResponse :: ProvideResponse -> Command ProvideResponse Object
networkProvideResponse = mkCommand NetworkProvideResponse

networkRemoveDataCollector :: RemoveDataCollector -> Command RemoveDataCollector Object
networkRemoveDataCollector = mkCommandTxt "network.removeDataCollector"

networkRemoveIntercept :: RemoveIntercept -> Command RemoveIntercept Object
networkRemoveIntercept = mkCommand NetworkRemoveIntercept

networkSetCacheBehavior :: SetCacheBehavior -> Command SetCacheBehavior Object
networkSetCacheBehavior = mkCommand NetworkSetCacheBehavior

---- Script ----

scriptAddPreloadScript :: AddPreloadScript -> Command AddPreloadScript AddPreloadScriptResult
scriptAddPreloadScript = mkCommand ScriptAddPreloadScript

scriptCallFunction :: CallFunction -> Command CallFunction EvaluateResult
scriptCallFunction = mkCommand ScriptCallFunction

scriptDisown :: Disown -> Command Disown Object
scriptDisown = mkCommand ScriptDisown

scriptEvaluate :: Evaluate -> Command Evaluate EvaluateResult
scriptEvaluate = mkCommand ScriptEvaluate

scriptGetRealms :: GetRealms -> Command GetRealms GetRealmsResult
scriptGetRealms = mkCommand ScriptGetRealms

scriptRemovePreloadScript :: RemovePreloadScript -> Command RemovePreloadScript Object
scriptRemovePreloadScript = mkCommand ScriptRemovePreloadScript

---- Storage ----

storageDeleteCookies :: DeleteCookies -> Command DeleteCookies DeleteCookiesResult
storageDeleteCookies = mkCommand StorageDeleteCookies

storageGetCookies :: GetCookies -> Command GetCookies GetCookiesResult
storageGetCookies = mkCommand StorageGetCookies

storageSetCookie :: SetCookie -> Command SetCookie SetCookieResult
storageSetCookie = mkCommand StorageSetCookie

---- WebExtension ----

webExtensionInstall :: WebExtensionInstall -> Command WebExtensionInstall WebExtensionResult
webExtensionInstall = mkCommandTxt "webExtension.install"

webExtensionUninstall :: WebExtensionUninstall -> Command WebExtensionUninstall Object
webExtensionUninstall = mkCommandTxt "webExtension.uninstall"

-- ############## Subscriptions (Events) ##############

subscribeMany :: 
  [BrowsingContext] ->
  [UserContext] ->
  [SubscriptionType] ->
  (Event -> m ()) ->
  Subscription m
subscribeMany = MultiSubscription

---- BrowsingContext ----

subscribeBrowsingContextCreated ::
  [BrowsingContext] ->
  [UserContext] ->
  (Info -> m ()) ->
  Subscription m
subscribeBrowsingContextCreated = SingleSubscription BrowsingContextContextCreated

subscribeBrowsingContextDestroyed ::
  [BrowsingContext] ->
  [UserContext] ->
  (Info -> m ()) ->
  Subscription m
subscribeBrowsingContextDestroyed = SingleSubscription BrowsingContextContextDestroyed

subscribeBrowsingContextNavigationStarted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationStarted = SingleSubscription BrowsingContextNavigationStarted

subscribeBrowsingContextFragmentNavigated ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextFragmentNavigated = SingleSubscription BrowsingContextFragmentNavigated

subscribeBrowsingContextHistoryUpdated ::
  [BrowsingContext] ->
  [UserContext] ->
  (HistoryUpdated -> m ()) ->
  Subscription m
subscribeBrowsingContextHistoryUpdated = SingleSubscription BrowsingContextHistoryUpdated

subscribeBrowsingContextDomContentLoaded ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextDomContentLoaded = SingleSubscription BrowsingContextDomContentLoaded

subscribeBrowsingContextLoad ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextLoad = SingleSubscription BrowsingContextLoad

subscribeBrowsingContextDownloadWillBegin ::
  [BrowsingContext] ->
  [UserContext] ->
  (DownloadWillBegin -> m ()) ->
  Subscription m
subscribeBrowsingContextDownloadWillBegin = SingleSubscription BrowsingContextDownloadWillBegin

subscribeBrowsingContextDownloadEnd ::
  [BrowsingContext] ->
  [UserContext] ->
  (DownloadEnd -> m ()) ->
  Subscription m
subscribeBrowsingContextDownloadEnd = SingleSubscription BrowsingContextDownloadEnd

subscribeBrowsingContextNavigationAborted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationAborted = SingleSubscription BrowsingContextNavigationAborted

subscribeBrowsingContextNavigationCommitted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationCommitted = SingleSubscription BrowsingContextNavigationCommitted

subscribeBrowsingContextNavigationFailed ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationFailed = SingleSubscription BrowsingContextNavigationFailed

subscribeBrowsingContextUserPromptClosed ::
  [BrowsingContext] ->
  [UserContext] ->
  (UserPromptClosed -> m ()) ->
  Subscription m
subscribeBrowsingContextUserPromptClosed = SingleSubscription BrowsingContextUserPromptClosed

subscribeBrowsingContextUserPromptOpened ::
  [BrowsingContext] ->
  [UserContext] ->
  (UserPromptOpened -> m ()) ->
  Subscription m
subscribeBrowsingContextUserPromptOpened = SingleSubscription BrowsingContextUserPromptOpened

---- Log ----

subscribeLogEntryAdded ::
  [BrowsingContext] ->
  [UserContext] ->
  (LogEntry -> m ()) ->
  Subscription m
subscribeLogEntryAdded = SingleSubscription LogEntryAdded

---- Network ----

subscribeNetworkAuthRequired ::
  [BrowsingContext] ->
  [UserContext] ->
  (AuthRequired -> m ()) ->
  Subscription m
subscribeNetworkAuthRequired = SingleSubscription NetworkAuthRequired

subscribeNetworkBeforeRequestSent ::
  [BrowsingContext] ->
  [UserContext] ->
  (BeforeRequestSent -> m ()) ->
  Subscription m
subscribeNetworkBeforeRequestSent = SingleSubscription NetworkBeforeRequestSent

subscribeNetworkFetchError ::
  [BrowsingContext] ->
  [UserContext] ->
  (FetchError -> m ()) ->
  Subscription m
subscribeNetworkFetchError = SingleSubscription NetworkFetchError

subscribeNetworkResponseCompleted ::
  [BrowsingContext] ->
  [UserContext] ->
  (ResponseCompleted -> m ()) ->
  Subscription m
subscribeNetworkResponseCompleted = SingleSubscription NetworkResponseCompleted

subscribeNetworkResponseStarted ::
  [BrowsingContext] ->
  [UserContext] ->
  (ResponseStarted -> m ()) ->
  Subscription m
subscribeNetworkResponseStarted = SingleSubscription NetworkResponseStarted

---- Script ----

subscribeScriptMessage ::
  [BrowsingContext] ->
  [UserContext] ->
  (Message -> m ()) ->
  Subscription m
subscribeScriptMessage = SingleSubscription ScriptMessage

subscribeScriptRealmCreated ::
  [BrowsingContext] ->
  [UserContext] ->
  (RealmInfo -> m ()) ->
  Subscription m
subscribeScriptRealmCreated = SingleSubscription ScriptRealmCreated

subscribeScriptRealmDestroyed ::
  [BrowsingContext] ->
  [UserContext] ->
  (RealmDestroyed -> m ()) ->
  Subscription m
subscribeScriptRealmDestroyed = SingleSubscription ScriptRealmDestroyed

---- Input ----

subscribeInputFileDialogOpened ::
  [BrowsingContext] ->
  [UserContext] ->
  (FileDialogOpened -> m ()) ->
  Subscription m
subscribeInputFileDialogOpened = SingleSubscription InputFileDialogOpened