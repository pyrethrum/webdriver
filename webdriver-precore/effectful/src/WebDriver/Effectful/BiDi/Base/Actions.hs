-- |
-- Module: WebDriver.Effectful.BiDi.Base.Actions
-- Description: Effectful-style BiDi WebDriver action functions
--
-- Provides smart constructors for the 'WebDriverBiDi' algebraic effect.
-- Each function simply invokes the corresponding constructor via 'send'.
--
-- Subscription functions (e.g. 'subscribeBrowsingContextDomContentLoaded')
-- accept callbacks in the caller's @Eff es@ monad.  The interpreter uses
-- 'Effectful.Dispatch.Dynamic.localSeqUnliftIO' to convert them to @IO@
-- callbacks before handing them off to the underlying WebSocket runner.
--
-- For idiomatic use in tests, use 'liftIO' inside the callback to write to a
-- 'Control.Concurrent.STM.TMVar', then call 'liftIO atomically' in the main
-- @Eff es@ computation to wait for the event.
--
-- This mirrors "WebDriver.Bluefin.BiDi.Base.Actions" but uses Effectful
-- algebraic effects rather than Bluefin handles.
module WebDriver.Effectful.BiDi.Base.Actions
  ( -- * Session Commands
    biDiSessionNew,
    biDiSessionStatus,
    biDiSessionEnd,

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
    emulationSetTouchOverride,
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
    scriptEvaluateNoWait,
    scriptGetRealms,
    scriptRemovePreloadScript,

    -- * Storage Commands
    storageDeleteCookies,
    storageGetCookies,
    storageSetCookie,

    -- * WebExtension Commands
    webExtensionInstall,
    webExtensionUninstall,

    -- * Generic / Low-level Commands
    sendBiDiCmd,
    sendBiDiCmdNoWait,
    sendBiDiOffSpecCmd,
    sendBiDiOffSpecCmdNoWait,

    -- * Log Subscriptions
    subscribeLogEntryAdded,
    subscribeLogEntryAdded',

    -- * BrowsingContext Subscriptions
    subscribeBrowsingContextCreated,
    subscribeBrowsingContextCreated',
    subscribeBrowsingContextDestroyed,
    subscribeBrowsingContextDestroyed',
    subscribeBrowsingContextNavigationStarted,
    subscribeBrowsingContextNavigationStarted',
    subscribeBrowsingContextFragmentNavigated,
    subscribeBrowsingContextFragmentNavigated',
    subscribeBrowsingContextHistoryUpdated,
    subscribeBrowsingContextHistoryUpdated',
    subscribeBrowsingContextDomContentLoaded,
    subscribeBrowsingContextDomContentLoaded',
    subscribeBrowsingContextLoad,
    subscribeBrowsingContextLoad',
    subscribeBrowsingContextDownloadWillBegin,
    subscribeBrowsingContextDownloadWillBegin',
    subscribeBrowsingContextDownloadEnd,
    subscribeBrowsingContextDownloadEnd',
    subscribeBrowsingContextNavigationAborted,
    subscribeBrowsingContextNavigationAborted',
    subscribeBrowsingContextNavigationCommitted,
    subscribeBrowsingContextNavigationCommitted',
    subscribeBrowsingContextNavigationFailed,
    subscribeBrowsingContextNavigationFailed',
    subscribeBrowsingContextUserPromptClosed,
    subscribeBrowsingContextUserPromptClosed',
    subscribeBrowsingContextUserPromptOpened,
    subscribeBrowsingContextUserPromptOpened',

    -- * Network Subscriptions
    subscribeNetworkAuthRequired,
    subscribeNetworkAuthRequired',
    subscribeNetworkBeforeRequestSent,
    subscribeNetworkBeforeRequestSent',
    subscribeNetworkFetchError,
    subscribeNetworkFetchError',
    subscribeNetworkResponseCompleted,
    subscribeNetworkResponseCompleted',
    subscribeNetworkResponseStarted,
    subscribeNetworkResponseStarted',

    -- * Script Subscriptions
    subscribeScriptMessage,
    subscribeScriptMessage',
    subscribeScriptRealmCreated,
    subscribeScriptRealmCreated',
    subscribeScriptRealmDestroyed,
    subscribeScriptRealmDestroyed',

    -- * Input Subscriptions
    subscribeInputFileDialogOpened,
    subscribeInputFileDialogOpened',

    -- * Multi-event Subscriptions
    subscribeMany,
    subscribeMany',
    subscribeUnknownMany,
    subscribeUnknownMany',

    -- * Unsubscribe
    unsubscribe,
  )
where

import Data.Aeson (FromJSON, Object, Value)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (send)
import WebDriver.Effectful.HTTP.Core (WebDriverBiDi (..))
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
    JSUInt,
    KnownSubscriptionType,
    LocateNodes,
    LocateNodesResult,
    LogEntry,
    Message,
    Navigate,
    NavigateResult,
    NavigationInfo,
    OffSpecSubscriptionType,
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
    SetTouchOverride,
    SetUserAgentOverride,
    SetViewport,
    SubscriptionId,
    TraverseHistory,
    UserContext,
    UserPromptClosed,
    UserPromptOpened,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
  )
import WebDriverPreCore.BiDiRunner (Request)

-- ---------------------------------------------------------------------------
-- Session commands
-- ---------------------------------------------------------------------------

biDiSessionNew :: (WebDriverBiDi :> es) => Capabilities -> Eff es SessionNewResult
biDiSessionNew = send . BiDiSessionNew

biDiSessionStatus :: (WebDriverBiDi :> es) => Eff es SessionStatusResult
biDiSessionStatus = send BiDiSessionStatus

biDiSessionEnd :: (WebDriverBiDi :> es) => Eff es ()
biDiSessionEnd = send BiDiSessionEnd

-- ---------------------------------------------------------------------------
-- BrowsingContext commands
-- ---------------------------------------------------------------------------

browsingContextActivate :: (WebDriverBiDi :> es) => Activate -> Eff es ()
browsingContextActivate = send . BrowsingContextActivate

browsingContextCaptureScreenshot :: (WebDriverBiDi :> es) => CaptureScreenshot -> Eff es CaptureScreenshotResult
browsingContextCaptureScreenshot = send . BrowsingContextCaptureScreenshot

browsingContextClose :: (WebDriverBiDi :> es) => Close -> Eff es ()
browsingContextClose = send . BrowsingContextClose

browsingContextCreate :: (WebDriverBiDi :> es) => Create -> Eff es BrowsingContext
browsingContextCreate = send . BrowsingContextCreate

browsingContextGetTree :: (WebDriverBiDi :> es) => GetTree -> Eff es GetTreeResult
browsingContextGetTree = send . BrowsingContextGetTree

browsingContextHandleUserPrompt :: (WebDriverBiDi :> es) => HandleUserPrompt -> Eff es ()
browsingContextHandleUserPrompt = send . BrowsingContextHandleUserPrompt

browsingContextLocateNodes :: (WebDriverBiDi :> es) => LocateNodes -> Eff es LocateNodesResult
browsingContextLocateNodes = send . BrowsingContextLocateNodes

browsingContextNavigate :: (WebDriverBiDi :> es) => Navigate -> Eff es NavigateResult
browsingContextNavigate = send . BrowsingContextNavigate

browsingContextPrint :: (WebDriverBiDi :> es) => Print -> Eff es PrintResult
browsingContextPrint = send . BrowsingContextPrint

browsingContextReload :: (WebDriverBiDi :> es) => Reload -> Eff es ()
browsingContextReload = send . BrowsingContextReload

browsingContextSetViewport :: (WebDriverBiDi :> es) => SetViewport -> Eff es ()
browsingContextSetViewport = send . BrowsingContextSetViewport

browsingContextTraverseHistory :: (WebDriverBiDi :> es) => TraverseHistory -> Eff es ()
browsingContextTraverseHistory = send . BrowsingContextTraverseHistory

-- ---------------------------------------------------------------------------
-- Browser commands
-- ---------------------------------------------------------------------------

browserClose :: (WebDriverBiDi :> es) => Eff es ()
browserClose = send BrowserClose

browserCreateUserContext :: (WebDriverBiDi :> es) => CreateUserContext -> Eff es UserContext
browserCreateUserContext = send . BrowserCreateUserContext

browserGetClientWindows :: (WebDriverBiDi :> es) => Eff es GetClientWindowsResult
browserGetClientWindows = send BrowserGetClientWindows

browserGetUserContexts :: (WebDriverBiDi :> es) => Eff es GetUserContextsResult
browserGetUserContexts = send BrowserGetUserContexts

browserRemoveUserContext :: (WebDriverBiDi :> es) => RemoveUserContext -> Eff es ()
browserRemoveUserContext = send . BrowserRemoveUserContext

browserSetClientWindowState :: (WebDriverBiDi :> es) => SetClientWindowState -> Eff es ClientWindowInfo
browserSetClientWindowState = send . BrowserSetClientWindowState

browserSetDownloadBehavior :: (WebDriverBiDi :> es) => SetDownloadBehavior -> Eff es ()
browserSetDownloadBehavior = send . BrowserSetDownloadBehavior

-- ---------------------------------------------------------------------------
-- Emulation commands
-- ---------------------------------------------------------------------------

emulationSetForcedColorsModeThemeOverride :: (WebDriverBiDi :> es) => SetForcedColorsModeThemeOverride -> Eff es ()
emulationSetForcedColorsModeThemeOverride = send . EmulationSetForcedColorsModeThemeOverride

emulationSetGeolocationOverride :: (WebDriverBiDi :> es) => SetGeolocationOverride -> Eff es ()
emulationSetGeolocationOverride = send . EmulationSetGeolocationOverride

emulationSetLocaleOverride :: (WebDriverBiDi :> es) => SetLocaleOverride -> Eff es ()
emulationSetLocaleOverride = send . EmulationSetLocaleOverride

emulationSetNetworkConditions :: (WebDriverBiDi :> es) => SetNetworkConditions -> Eff es ()
emulationSetNetworkConditions = send . EmulationSetNetworkConditions

emulationSetScreenOrientationOverride :: (WebDriverBiDi :> es) => SetScreenOrientationOverride -> Eff es ()
emulationSetScreenOrientationOverride = send . EmulationSetScreenOrientationOverride

emulationSetScreenSettingsOverride :: (WebDriverBiDi :> es) => SetScreenSettingsOverride -> Eff es ()
emulationSetScreenSettingsOverride = send . EmulationSetScreenSettingsOverride

emulationSetScriptingEnabled :: (WebDriverBiDi :> es) => SetScriptingEnabled -> Eff es ()
emulationSetScriptingEnabled = send . EmulationSetScriptingEnabled

emulationSetTimezoneOverride :: (WebDriverBiDi :> es) => SetTimezoneOverride -> Eff es ()
emulationSetTimezoneOverride = send . EmulationSetTimezoneOverride

emulationSetTouchOverride :: (WebDriverBiDi :> es) => SetTouchOverride -> Eff es ()
emulationSetTouchOverride = send . EmulationSetTouchOverride

emulationSetUserAgentOverride :: (WebDriverBiDi :> es) => SetUserAgentOverride -> Eff es ()
emulationSetUserAgentOverride = send . EmulationSetUserAgentOverride

-- ---------------------------------------------------------------------------
-- Input commands
-- ---------------------------------------------------------------------------

inputPerformActions :: (WebDriverBiDi :> es) => PerformActions -> Eff es ()
inputPerformActions = send . InputPerformActions

inputReleaseActions :: (WebDriverBiDi :> es) => ReleaseActions -> Eff es ()
inputReleaseActions = send . InputReleaseActions

inputSetFiles :: (WebDriverBiDi :> es) => SetFiles -> Eff es ()
inputSetFiles = send . InputSetFiles

-- ---------------------------------------------------------------------------
-- Network commands
-- ---------------------------------------------------------------------------

networkAddDataCollector :: (WebDriverBiDi :> es) => AddDataCollector -> Eff es AddDataCollectorResult
networkAddDataCollector = send . NetworkAddDataCollector

networkAddIntercept :: (WebDriverBiDi :> es) => AddIntercept -> Eff es AddInterceptResult
networkAddIntercept = send . NetworkAddIntercept

networkContinueRequest :: (WebDriverBiDi :> es) => ContinueRequest -> Eff es ()
networkContinueRequest = send . NetworkContinueRequest

networkContinueResponse :: (WebDriverBiDi :> es) => ContinueResponse -> Eff es ()
networkContinueResponse = send . NetworkContinueResponse

networkContinueWithAuth :: (WebDriverBiDi :> es) => ContinueWithAuth -> Eff es ()
networkContinueWithAuth = send . NetworkContinueWithAuth

networkDisownData :: (WebDriverBiDi :> es) => DisownData -> Eff es ()
networkDisownData = send . NetworkDisownData

networkFailRequest :: (WebDriverBiDi :> es) => FailRequest -> Eff es ()
networkFailRequest = send . NetworkFailRequest

networkGetData :: (WebDriverBiDi :> es) => GetData -> Eff es GetDataResult
networkGetData = send . NetworkGetData

networkProvideResponse :: (WebDriverBiDi :> es) => ProvideResponse -> Eff es ()
networkProvideResponse = send . NetworkProvideResponse

networkRemoveDataCollector :: (WebDriverBiDi :> es) => RemoveDataCollector -> Eff es ()
networkRemoveDataCollector = send . NetworkRemoveDataCollector

networkRemoveIntercept :: (WebDriverBiDi :> es) => RemoveIntercept -> Eff es ()
networkRemoveIntercept = send . NetworkRemoveIntercept

networkSetCacheBehavior :: (WebDriverBiDi :> es) => SetCacheBehavior -> Eff es ()
networkSetCacheBehavior = send . NetworkSetCacheBehavior

networkSetExtraHeaders :: (WebDriverBiDi :> es) => SetExtraHeaders -> Eff es ()
networkSetExtraHeaders = send . NetworkSetExtraHeaders

-- ---------------------------------------------------------------------------
-- Script commands
-- ---------------------------------------------------------------------------

scriptAddPreloadScript :: (WebDriverBiDi :> es) => AddPreloadScript -> Eff es AddPreloadScriptResult
scriptAddPreloadScript = send . ScriptAddPreloadScript

scriptCallFunction :: (WebDriverBiDi :> es) => CallFunction -> Eff es EvaluateResult
scriptCallFunction = send . ScriptCallFunction

scriptDisown :: (WebDriverBiDi :> es) => Disown -> Eff es ()
scriptDisown = send . ScriptDisown

scriptEvaluate :: (WebDriverBiDi :> es) => Evaluate -> Eff es EvaluateResult
scriptEvaluate = send . ScriptEvaluate

-- | Evaluate a script expression without waiting for the result.
scriptEvaluateNoWait :: (WebDriverBiDi :> es) => Evaluate -> Eff es Request
scriptEvaluateNoWait = send . ScriptEvaluateNoWait

scriptGetRealms :: (WebDriverBiDi :> es) => GetRealms -> Eff es GetRealmsResult
scriptGetRealms = send . ScriptGetRealms

scriptRemovePreloadScript :: (WebDriverBiDi :> es) => RemovePreloadScript -> Eff es ()
scriptRemovePreloadScript = send . ScriptRemovePreloadScript

-- ---------------------------------------------------------------------------
-- Storage commands
-- ---------------------------------------------------------------------------

storageDeleteCookies :: (WebDriverBiDi :> es) => DeleteCookies -> Eff es DeleteCookiesResult
storageDeleteCookies = send . StorageDeleteCookies

storageGetCookies :: (WebDriverBiDi :> es) => GetCookies -> Eff es GetCookiesResult
storageGetCookies = send . StorageGetCookies

storageSetCookie :: (WebDriverBiDi :> es) => SetCookie -> Eff es SetCookieResult
storageSetCookie = send . StorageSetCookie

-- ---------------------------------------------------------------------------
-- WebExtension commands
-- ---------------------------------------------------------------------------

webExtensionInstall :: (WebDriverBiDi :> es) => WebExtensionInstall -> Eff es WebExtensionResult
webExtensionInstall = send . WebExtensionInstall

webExtensionUninstall :: (WebDriverBiDi :> es) => WebExtensionUninstall -> Eff es ()
webExtensionUninstall = send . WebExtensionUninstall

-- ---------------------------------------------------------------------------
-- Generic / low-level commands
-- ---------------------------------------------------------------------------

-- | Send any typed BiDi 'Command' through the effect.
sendBiDiCmd :: (WebDriverBiDi :> es, FromJSON r) => Command r -> Eff es r
sendBiDiCmd = send . SendBiDiCmd

-- | Send a typed 'Command' without waiting for a response.
sendBiDiCmdNoWait :: (WebDriverBiDi :> es) => Command r -> Eff es Request
sendBiDiCmdNoWait = send . SendBiDiCmdNoWait

-- | Send an off-spec command with an explicit message ID.
sendBiDiOffSpecCmd :: (WebDriverBiDi :> es) => JSUInt -> Text -> Object -> Eff es Object
sendBiDiOffSpecCmd mid m = send . SendBiDiOffSpecCmd mid m

-- | Send an off-spec command without waiting for a response.
sendBiDiOffSpecCmdNoWait :: (WebDriverBiDi :> es) => Text -> Object -> Eff es Request
sendBiDiOffSpecCmdNoWait m = send . SendBiDiOffSpecCmdNoWait m

-- ---------------------------------------------------------------------------
-- Log subscriptions
-- ---------------------------------------------------------------------------

subscribeLogEntryAdded :: (WebDriverBiDi :> es) => (LogEntry -> Eff es ()) -> Eff es SubscriptionId
subscribeLogEntryAdded = send . SubscribeLogEntryAdded

subscribeLogEntryAdded' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (LogEntry -> Eff es ()) -> Eff es SubscriptionId
subscribeLogEntryAdded' b u = send . SubscribeLogEntryAdded' b u

-- ---------------------------------------------------------------------------
-- BrowsingContext subscriptions
-- ---------------------------------------------------------------------------

subscribeBrowsingContextCreated :: (WebDriverBiDi :> es) => (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextCreated = send . SubscribeBrowsingContextCreated

subscribeBrowsingContextCreated' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextCreated' b u = send . SubscribeBrowsingContextCreated' b u

subscribeBrowsingContextDestroyed :: (WebDriverBiDi :> es) => (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDestroyed = send . SubscribeBrowsingContextDestroyed

subscribeBrowsingContextDestroyed' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDestroyed' b u = send . SubscribeBrowsingContextDestroyed' b u

subscribeBrowsingContextNavigationStarted :: (WebDriverBiDi :> es) => (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationStarted = send . SubscribeBrowsingContextNavigationStarted

subscribeBrowsingContextNavigationStarted' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationStarted' b u = send . SubscribeBrowsingContextNavigationStarted' b u

subscribeBrowsingContextFragmentNavigated :: (WebDriverBiDi :> es) => (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextFragmentNavigated = send . SubscribeBrowsingContextFragmentNavigated

subscribeBrowsingContextFragmentNavigated' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextFragmentNavigated' b u = send . SubscribeBrowsingContextFragmentNavigated' b u

subscribeBrowsingContextHistoryUpdated :: (WebDriverBiDi :> es) => (HistoryUpdated -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextHistoryUpdated = send . SubscribeBrowsingContextHistoryUpdated

subscribeBrowsingContextHistoryUpdated' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextHistoryUpdated' b u = send . SubscribeBrowsingContextHistoryUpdated' b u

-- | Subscribe to @browsingContext.domContentLoaded@ events.
--
-- Pass the returned 'SubscriptionId' to 'unsubscribe' to de-register.
-- In the callback, use 'liftIO' to write to a 'Control.Concurrent.STM.TMVar'
-- and @liftIO atomically@ in the main @Eff@ stack to wait for the event.
subscribeBrowsingContextDomContentLoaded :: (WebDriverBiDi :> es) => (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDomContentLoaded = send . SubscribeBrowsingContextDomContentLoaded

subscribeBrowsingContextDomContentLoaded' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDomContentLoaded' b u = send . SubscribeBrowsingContextDomContentLoaded' b u

subscribeBrowsingContextLoad :: (WebDriverBiDi :> es) => (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextLoad = send . SubscribeBrowsingContextLoad

subscribeBrowsingContextLoad' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextLoad' b u = send . SubscribeBrowsingContextLoad' b u

subscribeBrowsingContextDownloadWillBegin :: (WebDriverBiDi :> es) => (DownloadWillBegin -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadWillBegin = send . SubscribeBrowsingContextDownloadWillBegin

subscribeBrowsingContextDownloadWillBegin' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadWillBegin' b u = send . SubscribeBrowsingContextDownloadWillBegin' b u

subscribeBrowsingContextDownloadEnd :: (WebDriverBiDi :> es) => (DownloadEnd -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadEnd = send . SubscribeBrowsingContextDownloadEnd

subscribeBrowsingContextDownloadEnd' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (DownloadEnd -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadEnd' b u = send . SubscribeBrowsingContextDownloadEnd' b u

subscribeBrowsingContextNavigationAborted :: (WebDriverBiDi :> es) => (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationAborted = send . SubscribeBrowsingContextNavigationAborted

subscribeBrowsingContextNavigationAborted' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationAborted' b u = send . SubscribeBrowsingContextNavigationAborted' b u

subscribeBrowsingContextNavigationCommitted :: (WebDriverBiDi :> es) => (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationCommitted = send . SubscribeBrowsingContextNavigationCommitted

subscribeBrowsingContextNavigationCommitted' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationCommitted' b u = send . SubscribeBrowsingContextNavigationCommitted' b u

subscribeBrowsingContextNavigationFailed :: (WebDriverBiDi :> es) => (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationFailed = send . SubscribeBrowsingContextNavigationFailed

subscribeBrowsingContextNavigationFailed' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationFailed' b u = send . SubscribeBrowsingContextNavigationFailed' b u

subscribeBrowsingContextUserPromptClosed :: (WebDriverBiDi :> es) => (UserPromptClosed -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptClosed = send . SubscribeBrowsingContextUserPromptClosed

subscribeBrowsingContextUserPromptClosed' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptClosed' b u = send . SubscribeBrowsingContextUserPromptClosed' b u

subscribeBrowsingContextUserPromptOpened :: (WebDriverBiDi :> es) => (UserPromptOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptOpened = send . SubscribeBrowsingContextUserPromptOpened

subscribeBrowsingContextUserPromptOpened' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptOpened' b u = send . SubscribeBrowsingContextUserPromptOpened' b u

-- ---------------------------------------------------------------------------
-- Network subscriptions
-- ---------------------------------------------------------------------------

subscribeNetworkAuthRequired :: (WebDriverBiDi :> es) => (AuthRequired -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkAuthRequired = send . SubscribeNetworkAuthRequired

subscribeNetworkAuthRequired' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (AuthRequired -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkAuthRequired' b u = send . SubscribeNetworkAuthRequired' b u

subscribeNetworkBeforeRequestSent :: (WebDriverBiDi :> es) => (BeforeRequestSent -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkBeforeRequestSent = send . SubscribeNetworkBeforeRequestSent

subscribeNetworkBeforeRequestSent' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkBeforeRequestSent' b u = send . SubscribeNetworkBeforeRequestSent' b u

subscribeNetworkFetchError :: (WebDriverBiDi :> es) => (FetchError -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkFetchError = send . SubscribeNetworkFetchError

subscribeNetworkFetchError' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (FetchError -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkFetchError' b u = send . SubscribeNetworkFetchError' b u

subscribeNetworkResponseCompleted :: (WebDriverBiDi :> es) => (ResponseCompleted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseCompleted = send . SubscribeNetworkResponseCompleted

subscribeNetworkResponseCompleted' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseCompleted' b u = send . SubscribeNetworkResponseCompleted' b u

subscribeNetworkResponseStarted :: (WebDriverBiDi :> es) => (ResponseStarted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseStarted = send . SubscribeNetworkResponseStarted

subscribeNetworkResponseStarted' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (ResponseStarted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseStarted' b u = send . SubscribeNetworkResponseStarted' b u

-- ---------------------------------------------------------------------------
-- Script subscriptions
-- ---------------------------------------------------------------------------

subscribeScriptMessage :: (WebDriverBiDi :> es) => (Message -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptMessage = send . SubscribeScriptMessage

subscribeScriptMessage' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (Message -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptMessage' b u = send . SubscribeScriptMessage' b u

subscribeScriptRealmCreated :: (WebDriverBiDi :> es) => (RealmInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmCreated = send . SubscribeScriptRealmCreated

subscribeScriptRealmCreated' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (RealmInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmCreated' b u = send . SubscribeScriptRealmCreated' b u

subscribeScriptRealmDestroyed :: (WebDriverBiDi :> es) => (RealmDestroyed -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmDestroyed = send . SubscribeScriptRealmDestroyed

subscribeScriptRealmDestroyed' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmDestroyed' b u = send . SubscribeScriptRealmDestroyed' b u

-- ---------------------------------------------------------------------------
-- Input subscriptions
-- ---------------------------------------------------------------------------

subscribeInputFileDialogOpened :: (WebDriverBiDi :> es) => (FileDialogOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeInputFileDialogOpened = send . SubscribeInputFileDialogOpened

subscribeInputFileDialogOpened' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeInputFileDialogOpened' b u = send . SubscribeInputFileDialogOpened' b u

-- ---------------------------------------------------------------------------
-- Multi-event subscriptions
-- ---------------------------------------------------------------------------

-- | Subscribe to multiple known event types (no context filters).
subscribeMany :: (WebDriverBiDi :> es) => [KnownSubscriptionType] -> (Event -> Eff es ()) -> Eff es SubscriptionId
subscribeMany sts = send . SubscribeMany sts

-- | Subscribe to multiple known event types with context filters.
subscribeMany' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> [KnownSubscriptionType] -> (Event -> Eff es ()) -> Eff es SubscriptionId
subscribeMany' b u sts = send . SubscribeMany' b u sts

-- | Subscribe to unknown off-spec event types (no context filters).
subscribeUnknownMany :: (WebDriverBiDi :> es) => [OffSpecSubscriptionType] -> (Value -> Eff es ()) -> Eff es SubscriptionId
subscribeUnknownMany sts = send . SubscribeUnknownMany sts

-- | Subscribe to unknown off-spec event types with context filters.
subscribeUnknownMany' :: (WebDriverBiDi :> es) => [BrowsingContext] -> [UserContext] -> [OffSpecSubscriptionType] -> (Value -> Eff es ()) -> Eff es SubscriptionId
subscribeUnknownMany' b u sts = send . SubscribeUnknownMany' b u sts

-- ---------------------------------------------------------------------------
-- Unsubscribe
-- ---------------------------------------------------------------------------

-- | Unsubscribe using a previously obtained 'SubscriptionId'.
unsubscribe :: (WebDriverBiDi :> es) => SubscriptionId -> Eff es ()
unsubscribe = send . Unsubscribe
