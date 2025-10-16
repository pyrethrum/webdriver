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
import WebDriverPreCore.BiDi.Event
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log
import WebDriverPreCore.BiDi.Network (AuthRequired, BeforeRequestSent, FetchError, ResponseCompleted, ResponseStarted)
import WebDriverPreCore.BiDi.Script (Message, RealmInfo, Realm)
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
    mkCommand, RealmDestroyed,
  )

-- TODO: generic commands
-- TODO: generic subscribe
-- TODO: generic unsubscribe
-- TODO: generic event
-- TODO: do Something about Objects -newtype?

--- ############## Commands ##############

---- Session ----

sessionNew :: Capabilities -> Command Capabilities SessionNewResult
sessionNew = mkCommand "session.new"

sessionStatus :: Command Object SessionStatusResult
sessionStatus = emptyCommand "session.status"

sessionEnd :: Command Object Object
sessionEnd = emptyCommand "session.end"

sessionSubscribe :: SessionSubscriptionRequest -> Command SessionSubscriptionRequest SessionSubscribeResult
sessionSubscribe = mkCommand "session.subscribe"

sessionUnsubscribe :: SessionUnsubscribe -> Command SessionUnsubscribe Object
sessionUnsubscribe = mkCommand "session.unsubscribe"

---- Browsing Context ----

browsingContextActivate :: Activate -> Command Activate Object
browsingContextActivate = mkCommand "browsingContext.activate"

browsingContextCaptureScreenshot :: CaptureScreenshot -> Command CaptureScreenshot CaptureScreenshotResult
browsingContextCaptureScreenshot = mkCommand "browsingContext.captureScreenshot"

browsingContextClose :: Close -> Command Close Object
browsingContextClose = mkCommand "browsingContext.close"

browsingContextCreate :: Create -> Command Create BrowsingContext
browsingContextCreate = mkCommand "browsingContext.create"

browsingContextGetTree :: GetTree -> Command GetTree GetTreeResult
browsingContextGetTree = mkCommand "browsingContext.getTree"

browsingContextHandleUserPrompt :: HandleUserPrompt -> Command HandleUserPrompt Object
browsingContextHandleUserPrompt = mkCommand "browsingContext.handleUserPrompt"

browsingContextLocateNodes :: LocateNodes -> Command LocateNodes LocateNodesResult
browsingContextLocateNodes = mkCommand "browsingContext.locateNodes"

browsingContextNavigate :: Navigate -> Command Navigate NavigateResult
browsingContextNavigate = mkCommand "browsingContext.navigate"

browsingContextPrint :: Print -> Command Print PrintResult
browsingContextPrint = mkCommand "browsingContext.print"

browsingContextReload :: Reload -> Command Reload Object
browsingContextReload = mkCommand "browsingContext.reload"

browsingContextSetViewport :: SetViewport -> Command SetViewport Object
browsingContextSetViewport = mkCommand "browsingContext.setViewport"

browsingContextTraverseHistory :: TraverseHistory -> Command TraverseHistory TraverseHistoryResult
browsingContextTraverseHistory = mkCommand "browsingContext.traverseHistory"

---- Browser ----

browserClose :: Command Object Object
browserClose = emptyCommand "browser.close"

browserCreateUserContext :: CreateUserContext -> Command CreateUserContext UserContext
browserCreateUserContext = mkCommand "browser.createUserContext"

browserGetClientWindows :: Command Object GetClientWindowsResult
browserGetClientWindows = emptyCommand "browser.getClientWindows"

browserGetUserContexts :: Command Object GetUserContextsResult
browserGetUserContexts = emptyCommand "browser.getUserContexts"

browserRemoveUserContext :: RemoveUserContext -> Command RemoveUserContext Object
browserRemoveUserContext = mkCommand "browser.removeUserContext"

browserSetClientWindowState :: SetClientWindowState -> Command SetClientWindowState ClientWindowInfo
browserSetClientWindowState = mkCommand "browser.setClientWindowState"

---- Emulation ----

emulationSetGeolocationOverride :: SetGeolocationOverride -> Command SetGeolocationOverride Object
emulationSetGeolocationOverride = mkCommand "emulation.setGeolocationOverride"

emulationSetLocaleOverride :: SetLocaleOverride -> Command SetLocaleOverride Object
emulationSetLocaleOverride = mkCommand "emulation.setLocaleOverride"

emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> Command SetScreenOrientationOverride Object
emulationSetScreenOrientationOverride = mkCommand "emulation.setScreenOrientationOverride"

emulationSetTimezoneOverride :: SetTimezoneOverride -> Command SetTimezoneOverride Object
emulationSetTimezoneOverride = mkCommand "emulation.setTimezoneOverride"

---- Input ----

inputPerformActions :: PerformActions -> Command PerformActions Object
inputPerformActions = mkCommand "input.performActions"

inputReleaseActions :: ReleaseActions -> Command ReleaseActions Object
inputReleaseActions = mkCommand "input.releaseActions"

inputSetFiles :: SetFiles -> Command SetFiles Object
inputSetFiles = mkCommand "input.setFiles"

---- Network ----

networkAddDataCollector :: AddDataCollector -> Command AddDataCollector AddDataCollectorResult
networkAddDataCollector = mkCommand "network.addDataCollector"

networkAddIntercept :: AddIntercept -> Command AddIntercept AddInterceptResult
networkAddIntercept = mkCommand "network.addIntercept"

networkContinueRequest :: ContinueRequest -> Command ContinueRequest Object
networkContinueRequest = mkCommand "network.continueRequest"

networkContinueResponse :: ContinueResponse -> Command ContinueResponse Object
networkContinueResponse = mkCommand "network.continueResponse"

networkContinueWithAuth :: ContinueWithAuth -> Command ContinueWithAuth Object
networkContinueWithAuth = mkCommand "network.continueWithAuth"

networkDisownData :: DisownData -> Command DisownData Object
networkDisownData = mkCommand "network.disownData"

networkFailRequest :: FailRequest -> Command FailRequest Object
networkFailRequest = mkCommand "network.failRequest"

networkGetData :: GetData -> Command GetData GetDataResult
networkGetData = mkCommand "network.getData"

networkProvideResponse :: ProvideResponse -> Command ProvideResponse Object
networkProvideResponse = mkCommand "network.provideResponse"

networkRemoveDataCollector :: RemoveDataCollector -> Command RemoveDataCollector Object
networkRemoveDataCollector = mkCommand "network.removeDataCollector"

networkRemoveIntercept :: RemoveIntercept -> Command RemoveIntercept Object
networkRemoveIntercept = mkCommand "network.removeIntercept"

networkSetCacheBehavior :: SetCacheBehavior -> Command SetCacheBehavior Object
networkSetCacheBehavior = mkCommand "network.setCacheBehavior"

---- Script ----

scriptAddPreloadScript :: AddPreloadScript -> Command AddPreloadScript AddPreloadScriptResult
scriptAddPreloadScript = mkCommand "script.addPreloadScript"

scriptCallFunction :: CallFunction -> Command CallFunction EvaluateResult
scriptCallFunction = mkCommand "script.callFunction"

scriptDisown :: Disown -> Command Disown Object
scriptDisown = mkCommand "script.disown"

scriptEvaluate :: Evaluate -> Command Evaluate EvaluateResult
scriptEvaluate = mkCommand "script.evaluate"

scriptGetRealms :: GetRealms -> Command GetRealms GetRealmsResult
scriptGetRealms = mkCommand "script.getRealms"

scriptRemovePreloadScript :: RemovePreloadScript -> Command RemovePreloadScript Object
scriptRemovePreloadScript = mkCommand "script.removePreloadScript"

---- Storage ----

storageDeleteCookies :: DeleteCookies -> Command DeleteCookies DeleteCookiesResult
storageDeleteCookies = mkCommand "storage.deleteCookies"

storageGetCookies :: GetCookies -> Command GetCookies GetCookiesResult
storageGetCookies = mkCommand "storage.getCookies"

storageSetCookie :: SetCookie -> Command SetCookie SetCookieResult
storageSetCookie = mkCommand "storage.setCookie"

---- WebExtension ----

webExtensionInstall :: WebExtensionInstall -> Command WebExtensionInstall WebExtensionResult
webExtensionInstall = mkCommand "webExtension.install"

webExtensionUninstall :: WebExtensionUninstall -> Command WebExtensionUninstall Object
webExtensionUninstall = mkCommand "webExtension.uninstall"

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