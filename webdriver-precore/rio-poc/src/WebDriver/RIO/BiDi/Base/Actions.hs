-- |
-- Module: WebDriver.RIO.BiDi.Base.Actions
-- Description: RIO-based BiDi WebDriver action functions
--
-- Provides monadic BiDi WebDriver actions for RIO. All functions require
-- 'HasBiDiRunner' in the environment, which supplies the underlying
-- 'BiDiRunner'. Subscription callbacks are plain 'IO' actions since they
-- are invoked asynchronously by the WebSocket reader loop.
module WebDriver.RIO.BiDi.Base.Actions
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
    scriptGetRealms,
    scriptRemovePreloadScript,

    -- * Storage Commands
    storageDeleteCookies,
    storageGetCookies,
    storageSetCookie,

    -- * WebExtension Commands
    webExtensionInstall,
    webExtensionUninstall,

    -- * Generic and Low-level Commands
    sendCommand,
    sendCommand',
    sendCommandNoWait,
    sendOffSpecCommand',
    sendOffSpecCommandNoWait,

    -- * Script No-wait Convenience
    scriptEvaluateNoWait,

    -- * Subscriptions – Log
    subscribeLogEntryAdded,
    subscribeLogEntryAdded',

    -- * Subscriptions – BrowsingContext
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

    -- * Subscriptions – Network
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

    -- * Subscriptions – Script
    subscribeScriptMessage,
    subscribeScriptMessage',
    subscribeScriptRealmCreated,
    subscribeScriptRealmCreated',
    subscribeScriptRealmDestroyed,
    subscribeScriptRealmDestroyed',

    -- * Subscriptions – Input
    subscribeInputFileDialogOpened,
    subscribeInputFileDialogOpened',

    -- * Multi-event Subscriptions
    subscribeMany,
    subscribeMany',

    -- * Fallback Subscriptions
    subscribeUnknownMany,
    subscribeUnknownMany',

    -- * Unsubscribe
    unsubscribe,
  )
where

import Data.Aeson (FromJSON, Object, Value)
import Data.Text (Text)
import RIO (RIO, liftIO)
import WebDriver.RIO.HTTP.Core (HasBiDiRunner (..), getBiDiRunner)
import WebDriverPreCore.BiDi.API qualified as API
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
    SessionSubscibe,
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
    SetTouchOverride,
    SetUserAgentOverride,
    SetViewport,
    Subscription,
    SubscriptionId (..),
    TraverseHistory,
    UserContext,
    UserPromptClosed,
    UserPromptOpened,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
  )
import WebDriverPreCore.BiDiRunner (BiDiRunner (..), Request)
import WebDriverPreCore.BiDiRunner qualified as Runner

-- ###########################################################################
-- ############################### Helpers ###################################
-- ###########################################################################

-- | Execute a BiDi command via the runner in the environment.
viaRunner :: (HasBiDiRunner env) => (BiDiRunner -> IO a) -> RIO env a
viaRunner f = getBiDiRunner >>= liftIO . f

-- | Run a typed command through the BiDi runner.
runCmd :: (HasBiDiRunner env, FromJSON r) => Command r -> RIO env r
runCmd cmd = viaRunner $ \(MkBiDiRunner {run}) -> run cmd

-- | Extract the 'SubscriptionId' from a subscribe response.
extractSubscription :: SessionSubscribeResult -> SubscriptionId
extractSubscription (MkSessionSubscribeResult {subscription}) = subscription

-- | Subscribe helper – no browsing-context / user-context filters.
viaSub ::
  (HasBiDiRunner env) =>
  ([BrowsingContext] -> [UserContext] -> (a -> IO ()) -> Subscription IO) ->
  (a -> IO ()) ->
  RIO env SubscriptionId
viaSub mkSubscription handler =
  viaRunner $ \(MkBiDiRunner {run, socketActions}) ->
    Runner.subscribe socketActions (run . API.sessionSubscribe) (mkSubscription [] [] handler)

-- | Subscribe helper – with browsing-context and user-context filters.
viaSub' ::
  (HasBiDiRunner env) =>
  ([BrowsingContext] -> [UserContext] -> (a -> IO ()) -> Subscription IO) ->
  [BrowsingContext] ->
  [UserContext] ->
  (a -> IO ()) ->
  RIO env SubscriptionId
viaSub' mkSubscription bcs ucs handler =
  viaRunner $ \(MkBiDiRunner {run, socketActions}) ->
    Runner.subscribe socketActions (run . API.sessionSubscribe) (mkSubscription bcs ucs handler)

-- ###########################################################################
-- ########################### Session Commands ##############################
-- ###########################################################################

sessionNew :: (HasBiDiRunner env) => Capabilities -> RIO env SessionNewResult
sessionNew = runCmd . API.sessionNew

sessionStatus :: (HasBiDiRunner env) => RIO env SessionStatusResult
sessionStatus = runCmd API.sessionStatus

sessionEnd :: (HasBiDiRunner env) => RIO env ()
sessionEnd = runCmd API.sessionEnd

-- | Subscribe and return the bare 'SubscriptionId' (without the wrapper type).
sessionSubscribe :: (HasBiDiRunner env) => SessionSubscibe -> RIO env SubscriptionId
sessionSubscribe sub = extractSubscription <$> runCmd (API.sessionSubscribe sub)

sessionUnsubscribe :: (HasBiDiRunner env) => SessionUnsubscribe -> RIO env ()
sessionUnsubscribe unsub =
  viaRunner $ \(MkBiDiRunner {run, socketActions}) ->
    Runner.unsubscribe socketActions (run . API.sessionUnsubscribe) unsub

-- ###########################################################################
-- ####################### BrowsingContext Commands ##########################
-- ###########################################################################

browsingContextActivate :: (HasBiDiRunner env) => Activate -> RIO env ()
browsingContextActivate = runCmd . API.browsingContextActivate

browsingContextCaptureScreenshot :: (HasBiDiRunner env) => CaptureScreenshot -> RIO env CaptureScreenshotResult
browsingContextCaptureScreenshot = runCmd . API.browsingContextCaptureScreenshot

browsingContextClose :: (HasBiDiRunner env) => Close -> RIO env ()
browsingContextClose = runCmd . API.browsingContextClose

browsingContextCreate :: (HasBiDiRunner env) => Create -> RIO env BrowsingContext
browsingContextCreate = runCmd . API.browsingContextCreate

browsingContextGetTree :: (HasBiDiRunner env) => GetTree -> RIO env GetTreeResult
browsingContextGetTree = runCmd . API.browsingContextGetTree

browsingContextHandleUserPrompt :: (HasBiDiRunner env) => HandleUserPrompt -> RIO env ()
browsingContextHandleUserPrompt = runCmd . API.browsingContextHandleUserPrompt

browsingContextLocateNodes :: (HasBiDiRunner env) => LocateNodes -> RIO env LocateNodesResult
browsingContextLocateNodes = runCmd . API.browsingContextLocateNodes

browsingContextNavigate :: (HasBiDiRunner env) => Navigate -> RIO env NavigateResult
browsingContextNavigate = runCmd . API.browsingContextNavigate

browsingContextPrint :: (HasBiDiRunner env) => Print -> RIO env PrintResult
browsingContextPrint = runCmd . API.browsingContextPrint

browsingContextReload :: (HasBiDiRunner env) => Reload -> RIO env ()
browsingContextReload = runCmd . API.browsingContextReload

browsingContextSetViewport :: (HasBiDiRunner env) => SetViewport -> RIO env ()
browsingContextSetViewport = runCmd . API.browsingContextSetViewport

browsingContextTraverseHistory :: (HasBiDiRunner env) => TraverseHistory -> RIO env ()
browsingContextTraverseHistory = runCmd . API.browsingContextTraverseHistory

-- ###########################################################################
-- ########################### Browser Commands ##############################
-- ###########################################################################

browserClose :: (HasBiDiRunner env) => RIO env ()
browserClose = runCmd API.browserClose

browserCreateUserContext :: (HasBiDiRunner env) => CreateUserContext -> RIO env UserContext
browserCreateUserContext = runCmd . API.browserCreateUserContext

browserGetClientWindows :: (HasBiDiRunner env) => RIO env GetClientWindowsResult
browserGetClientWindows = runCmd API.browserGetClientWindows

browserGetUserContexts :: (HasBiDiRunner env) => RIO env GetUserContextsResult
browserGetUserContexts = runCmd API.browserGetUserContexts

browserRemoveUserContext :: (HasBiDiRunner env) => RemoveUserContext -> RIO env ()
browserRemoveUserContext = runCmd . API.browserRemoveUserContext

browserSetClientWindowState :: (HasBiDiRunner env) => SetClientWindowState -> RIO env ClientWindowInfo
browserSetClientWindowState = runCmd . API.browserSetClientWindowState

browserSetDownloadBehavior :: (HasBiDiRunner env) => SetDownloadBehavior -> RIO env ()
browserSetDownloadBehavior = runCmd . API.browserSetDownloadBehavior

-- ###########################################################################
-- ########################## Emulation Commands #############################
-- ###########################################################################

emulationSetForcedColorsModeThemeOverride :: (HasBiDiRunner env) => SetForcedColorsModeThemeOverride -> RIO env ()
emulationSetForcedColorsModeThemeOverride = runCmd . API.emulationSetForcedColorsModeThemeOverride

emulationSetGeolocationOverride :: (HasBiDiRunner env) => SetGeolocationOverride -> RIO env ()
emulationSetGeolocationOverride = runCmd . API.emulationSetGeolocationOverride

emulationSetLocaleOverride :: (HasBiDiRunner env) => SetLocaleOverride -> RIO env ()
emulationSetLocaleOverride = runCmd . API.emulationSetLocaleOverride

emulationSetNetworkConditions :: (HasBiDiRunner env) => SetNetworkConditions -> RIO env ()
emulationSetNetworkConditions = runCmd . API.emulationSetNetworkConditions

emulationSetScreenOrientationOverride :: (HasBiDiRunner env) => SetScreenOrientationOverride -> RIO env ()
emulationSetScreenOrientationOverride = runCmd . API.emulationSetScreenOrientationOverride

emulationSetScreenSettingsOverride :: (HasBiDiRunner env) => SetScreenSettingsOverride -> RIO env ()
emulationSetScreenSettingsOverride = runCmd . API.emulationSetScreenSettingsOverride

emulationSetScriptingEnabled :: (HasBiDiRunner env) => SetScriptingEnabled -> RIO env ()
emulationSetScriptingEnabled = runCmd . API.emulationSetScriptingEnabled

emulationSetTimezoneOverride :: (HasBiDiRunner env) => SetTimezoneOverride -> RIO env ()
emulationSetTimezoneOverride = runCmd . API.emulationSetTimezoneOverride

emulationSetTouchOverride :: (HasBiDiRunner env) => SetTouchOverride -> RIO env ()
emulationSetTouchOverride = runCmd . API.emulationSetTouchOverride

emulationSetUserAgentOverride :: (HasBiDiRunner env) => SetUserAgentOverride -> RIO env ()
emulationSetUserAgentOverride = runCmd . API.emulationSetUserAgentOverride

-- ###########################################################################
-- ############################ Input Commands ###############################
-- ###########################################################################

inputPerformActions :: (HasBiDiRunner env) => PerformActions -> RIO env ()
inputPerformActions = runCmd . API.inputPerformActions

inputReleaseActions :: (HasBiDiRunner env) => ReleaseActions -> RIO env ()
inputReleaseActions = runCmd . API.inputReleaseActions

inputSetFiles :: (HasBiDiRunner env) => SetFiles -> RIO env ()
inputSetFiles = runCmd . API.inputSetFiles

-- ###########################################################################
-- ########################### Network Commands ##############################
-- ###########################################################################

networkAddDataCollector :: (HasBiDiRunner env) => AddDataCollector -> RIO env AddDataCollectorResult
networkAddDataCollector = runCmd . API.networkAddDataCollector

networkAddIntercept :: (HasBiDiRunner env) => AddIntercept -> RIO env AddInterceptResult
networkAddIntercept = runCmd . API.networkAddIntercept

networkContinueRequest :: (HasBiDiRunner env) => ContinueRequest -> RIO env ()
networkContinueRequest = runCmd . API.networkContinueRequest

networkContinueResponse :: (HasBiDiRunner env) => ContinueResponse -> RIO env ()
networkContinueResponse = runCmd . API.networkContinueResponse

networkContinueWithAuth :: (HasBiDiRunner env) => ContinueWithAuth -> RIO env ()
networkContinueWithAuth = runCmd . API.networkContinueWithAuth

networkDisownData :: (HasBiDiRunner env) => DisownData -> RIO env ()
networkDisownData = runCmd . API.networkDisownData

networkFailRequest :: (HasBiDiRunner env) => FailRequest -> RIO env ()
networkFailRequest = runCmd . API.networkFailRequest

networkGetData :: (HasBiDiRunner env) => GetData -> RIO env GetDataResult
networkGetData = runCmd . API.networkGetData

networkProvideResponse :: (HasBiDiRunner env) => ProvideResponse -> RIO env ()
networkProvideResponse = runCmd . API.networkProvideResponse

networkRemoveDataCollector :: (HasBiDiRunner env) => RemoveDataCollector -> RIO env ()
networkRemoveDataCollector = runCmd . API.networkRemoveDataCollector

networkRemoveIntercept :: (HasBiDiRunner env) => RemoveIntercept -> RIO env ()
networkRemoveIntercept = runCmd . API.networkRemoveIntercept

networkSetCacheBehavior :: (HasBiDiRunner env) => SetCacheBehavior -> RIO env ()
networkSetCacheBehavior = runCmd . API.networkSetCacheBehavior

networkSetExtraHeaders :: (HasBiDiRunner env) => SetExtraHeaders -> RIO env ()
networkSetExtraHeaders = runCmd . API.networkSetExtraHeaders

-- ###########################################################################
-- ########################### Script Commands ###############################
-- ###########################################################################

scriptAddPreloadScript :: (HasBiDiRunner env) => AddPreloadScript -> RIO env AddPreloadScriptResult
scriptAddPreloadScript = runCmd . API.scriptAddPreloadScript

scriptCallFunction :: (HasBiDiRunner env) => CallFunction -> RIO env EvaluateResult
scriptCallFunction = runCmd . API.scriptCallFunction

scriptDisown :: (HasBiDiRunner env) => Disown -> RIO env ()
scriptDisown = runCmd . API.scriptDisown

scriptEvaluate :: (HasBiDiRunner env) => Evaluate -> RIO env EvaluateResult
scriptEvaluate = runCmd . API.scriptEvaluate

scriptGetRealms :: (HasBiDiRunner env) => GetRealms -> RIO env GetRealmsResult
scriptGetRealms = runCmd . API.scriptGetRealms

scriptRemovePreloadScript :: (HasBiDiRunner env) => RemovePreloadScript -> RIO env ()
scriptRemovePreloadScript = runCmd . API.scriptRemovePreloadScript

-- ###########################################################################
-- ########################### Storage Commands ##############################
-- ###########################################################################

storageDeleteCookies :: (HasBiDiRunner env) => DeleteCookies -> RIO env DeleteCookiesResult
storageDeleteCookies = runCmd . API.storageDeleteCookies

storageGetCookies :: (HasBiDiRunner env) => GetCookies -> RIO env GetCookiesResult
storageGetCookies = runCmd . API.storageGetCookies

storageSetCookie :: (HasBiDiRunner env) => SetCookie -> RIO env SetCookieResult
storageSetCookie = runCmd . API.storageSetCookie

-- ###########################################################################
-- ######################### WebExtension Commands ###########################
-- ###########################################################################

webExtensionInstall :: (HasBiDiRunner env) => WebExtensionInstall -> RIO env WebExtensionResult
webExtensionInstall = runCmd . API.webExtensionInstall

webExtensionUninstall :: (HasBiDiRunner env) => WebExtensionUninstall -> RIO env ()
webExtensionUninstall = runCmd . API.webExtensionUninstall

-- ###########################################################################
-- ########################## Generic Command ################################
-- ###########################################################################

-- | Send any typed 'Command' through the BiDi runner.
sendCommand :: (HasBiDiRunner env, FromJSON r) => Command r -> RIO env r
sendCommand = runCmd

-- | Send a typed 'Command' using a specific message ID.
sendCommand' :: (HasBiDiRunner env, FromJSON r) => JSUInt -> Command r -> RIO env r
sendCommand' msgId cmd =
  viaRunner $ \(MkBiDiRunner {runWithId}) -> runWithId msgId cmd

-- | Send a typed 'Command' without waiting for a response.
sendCommandNoWait :: (HasBiDiRunner env) => Command r -> RIO env Request
sendCommandNoWait cmd =
  viaRunner $ \r -> Runner.runNoWait r cmd

-- | Send an off-spec command with an explicit message ID.
sendOffSpecCommand' :: (HasBiDiRunner env) => JSUInt -> Text -> Object -> RIO env Object
sendOffSpecCommand' msgId method params =
  viaRunner $ \(MkBiDiRunner {runOffSpecWithId}) -> runOffSpecWithId msgId method params

-- | Send an off-spec command without waiting for a response.
sendOffSpecCommandNoWait :: (HasBiDiRunner env) => Text -> Object -> RIO env Request
sendOffSpecCommandNoWait method params =
  viaRunner $ \r -> Runner.runOffSpecNoWait r method params

-- | Evaluate a script expression without waiting for the result.
scriptEvaluateNoWait :: (HasBiDiRunner env) => Evaluate -> RIO env Request
scriptEvaluateNoWait cmd =
  viaRunner $ \r -> Runner.runNoWait r (API.scriptEvaluate cmd)

-- ###########################################################################
-- ######################### Subscription Helpers ############################
-- ###########################################################################

-- | Subscribe to multiple known event types (no context filters).
subscribeMany ::
  (HasBiDiRunner env) =>
  [KnownSubscriptionType] ->
  (Event -> IO ()) ->
  RIO env SubscriptionId
subscribeMany sts = subscribeMany' [] [] sts

-- | Subscribe to multiple known event types with context and user-context filters.
subscribeMany' ::
  (HasBiDiRunner env) =>
  [BrowsingContext] ->
  [UserContext] ->
  [KnownSubscriptionType] ->
  (Event -> IO ()) ->
  RIO env SubscriptionId
subscribeMany' bcs ucs sts handler =
  viaRunner $ \(MkBiDiRunner {run, socketActions}) ->
    Runner.subscribe socketActions (run . API.sessionSubscribe) (API.subscribeMany sts bcs ucs handler)

-- | Subscribe to unknown / off-spec event types (no context filters).
subscribeUnknownMany ::
  (HasBiDiRunner env) =>
  [OffSpecSubscriptionType] ->
  (Value -> IO ()) ->
  RIO env SubscriptionId
subscribeUnknownMany sts = subscribeUnknownMany' [] [] sts

-- | Subscribe to unknown / off-spec event types with context filters.
subscribeUnknownMany' ::
  (HasBiDiRunner env) =>
  [BrowsingContext] ->
  [UserContext] ->
  [OffSpecSubscriptionType] ->
  (Value -> IO ()) ->
  RIO env SubscriptionId
subscribeUnknownMany' bcs ucs sts handler =
  viaRunner $ \(MkBiDiRunner {run, socketActions}) ->
    Runner.subscribe socketActions (run . API.sessionSubscribe) (API.subscribeOffSpecMany sts bcs ucs handler)

-- | Unsubscribe using a previously obtained 'SubscriptionId'.
unsubscribe :: (HasBiDiRunner env) => SubscriptionId -> RIO env ()
unsubscribe subId =
  viaRunner $ \(MkBiDiRunner {run, socketActions}) ->
    Runner.unsubscribe socketActions (run . API.sessionUnsubscribe) (UnsubscribeById [subId])

-- ###########################################################################
-- ######################### Log Subscriptions ###############################
-- ###########################################################################

subscribeLogEntryAdded :: (HasBiDiRunner env) => (LogEntry -> IO ()) -> RIO env SubscriptionId
subscribeLogEntryAdded = viaSub API.subscribeLogEntryAdded

subscribeLogEntryAdded' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (LogEntry -> IO ()) -> RIO env SubscriptionId
subscribeLogEntryAdded' = viaSub' API.subscribeLogEntryAdded

-- ###########################################################################
-- ################### BrowsingContext Subscriptions #########################
-- ###########################################################################

subscribeBrowsingContextCreated :: (HasBiDiRunner env) => (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextCreated = viaSub API.subscribeBrowsingContextCreated

subscribeBrowsingContextCreated' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextCreated' = viaSub' API.subscribeBrowsingContextCreated

subscribeBrowsingContextDestroyed :: (HasBiDiRunner env) => (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDestroyed = viaSub API.subscribeBrowsingContextDestroyed

subscribeBrowsingContextDestroyed' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDestroyed' = viaSub' API.subscribeBrowsingContextDestroyed

subscribeBrowsingContextNavigationStarted :: (HasBiDiRunner env) => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationStarted = viaSub API.subscribeBrowsingContextNavigationStarted

subscribeBrowsingContextNavigationStarted' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationStarted' = viaSub' API.subscribeBrowsingContextNavigationStarted

subscribeBrowsingContextFragmentNavigated :: (HasBiDiRunner env) => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextFragmentNavigated = viaSub API.subscribeBrowsingContextFragmentNavigated

subscribeBrowsingContextFragmentNavigated' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextFragmentNavigated' = viaSub' API.subscribeBrowsingContextFragmentNavigated

subscribeBrowsingContextHistoryUpdated :: (HasBiDiRunner env) => (HistoryUpdated -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextHistoryUpdated = viaSub API.subscribeBrowsingContextHistoryUpdated

subscribeBrowsingContextHistoryUpdated' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextHistoryUpdated' = viaSub' API.subscribeBrowsingContextHistoryUpdated

subscribeBrowsingContextDomContentLoaded :: (HasBiDiRunner env) => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDomContentLoaded = viaSub API.subscribeBrowsingContextDomContentLoaded

subscribeBrowsingContextDomContentLoaded' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDomContentLoaded' = viaSub' API.subscribeBrowsingContextDomContentLoaded

subscribeBrowsingContextLoad :: (HasBiDiRunner env) => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextLoad = viaSub API.subscribeBrowsingContextLoad

subscribeBrowsingContextLoad' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextLoad' = viaSub' API.subscribeBrowsingContextLoad

subscribeBrowsingContextDownloadWillBegin :: (HasBiDiRunner env) => (DownloadWillBegin -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadWillBegin = viaSub API.subscribeBrowsingContextDownloadWillBegin

subscribeBrowsingContextDownloadWillBegin' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadWillBegin' = viaSub' API.subscribeBrowsingContextDownloadWillBegin

subscribeBrowsingContextDownloadEnd :: (HasBiDiRunner env) => (DownloadEnd -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadEnd = viaSub API.subscribeBrowsingContextDownloadEnd

subscribeBrowsingContextDownloadEnd' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (DownloadEnd -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadEnd' = viaSub' API.subscribeBrowsingContextDownloadEnd

subscribeBrowsingContextNavigationAborted :: (HasBiDiRunner env) => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationAborted = viaSub API.subscribeBrowsingContextNavigationAborted

subscribeBrowsingContextNavigationAborted' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationAborted' = viaSub' API.subscribeBrowsingContextNavigationAborted

subscribeBrowsingContextNavigationCommitted :: (HasBiDiRunner env) => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationCommitted = viaSub API.subscribeBrowsingContextNavigationCommitted

subscribeBrowsingContextNavigationCommitted' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationCommitted' = viaSub' API.subscribeBrowsingContextNavigationCommitted

subscribeBrowsingContextNavigationFailed :: (HasBiDiRunner env) => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationFailed = viaSub API.subscribeBrowsingContextNavigationFailed

subscribeBrowsingContextNavigationFailed' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationFailed' = viaSub' API.subscribeBrowsingContextNavigationFailed

subscribeBrowsingContextUserPromptClosed :: (HasBiDiRunner env) => (UserPromptClosed -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptClosed = viaSub API.subscribeBrowsingContextUserPromptClosed

subscribeBrowsingContextUserPromptClosed' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptClosed' = viaSub' API.subscribeBrowsingContextUserPromptClosed

subscribeBrowsingContextUserPromptOpened :: (HasBiDiRunner env) => (UserPromptOpened -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptOpened = viaSub API.subscribeBrowsingContextUserPromptOpened

subscribeBrowsingContextUserPromptOpened' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptOpened' = viaSub' API.subscribeBrowsingContextUserPromptOpened

-- ###########################################################################
-- ####################### Network Subscriptions #############################
-- ###########################################################################

subscribeNetworkAuthRequired :: (HasBiDiRunner env) => (AuthRequired -> IO ()) -> RIO env SubscriptionId
subscribeNetworkAuthRequired = viaSub API.subscribeNetworkAuthRequired

subscribeNetworkAuthRequired' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (AuthRequired -> IO ()) -> RIO env SubscriptionId
subscribeNetworkAuthRequired' = viaSub' API.subscribeNetworkAuthRequired

subscribeNetworkBeforeRequestSent :: (HasBiDiRunner env) => (BeforeRequestSent -> IO ()) -> RIO env SubscriptionId
subscribeNetworkBeforeRequestSent = viaSub API.subscribeNetworkBeforeRequestSent

subscribeNetworkBeforeRequestSent' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> IO ()) -> RIO env SubscriptionId
subscribeNetworkBeforeRequestSent' = viaSub' API.subscribeNetworkBeforeRequestSent

subscribeNetworkFetchError :: (HasBiDiRunner env) => (FetchError -> IO ()) -> RIO env SubscriptionId
subscribeNetworkFetchError = viaSub API.subscribeNetworkFetchError

subscribeNetworkFetchError' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (FetchError -> IO ()) -> RIO env SubscriptionId
subscribeNetworkFetchError' = viaSub' API.subscribeNetworkFetchError

subscribeNetworkResponseCompleted :: (HasBiDiRunner env) => (ResponseCompleted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseCompleted = viaSub API.subscribeNetworkResponseCompleted

subscribeNetworkResponseCompleted' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseCompleted' = viaSub' API.subscribeNetworkResponseCompleted

subscribeNetworkResponseStarted :: (HasBiDiRunner env) => (ResponseStarted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseStarted = viaSub API.subscribeNetworkResponseStarted

subscribeNetworkResponseStarted' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (ResponseStarted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseStarted' = viaSub' API.subscribeNetworkResponseStarted

-- ###########################################################################
-- ####################### Script Subscriptions ##############################
-- ###########################################################################

subscribeScriptMessage :: (HasBiDiRunner env) => (Message -> IO ()) -> RIO env SubscriptionId
subscribeScriptMessage = viaSub API.subscribeScriptMessage

subscribeScriptMessage' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (Message -> IO ()) -> RIO env SubscriptionId
subscribeScriptMessage' = viaSub' API.subscribeScriptMessage

subscribeScriptRealmCreated :: (HasBiDiRunner env) => (RealmInfo -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmCreated = viaSub API.subscribeScriptRealmCreated

subscribeScriptRealmCreated' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (RealmInfo -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmCreated' = viaSub' API.subscribeScriptRealmCreated

subscribeScriptRealmDestroyed :: (HasBiDiRunner env) => (RealmDestroyed -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmDestroyed = viaSub API.subscribeScriptRealmDestroyed

subscribeScriptRealmDestroyed' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmDestroyed' = viaSub' API.subscribeScriptRealmDestroyed

-- ###########################################################################
-- ####################### Input Subscriptions ###############################
-- ###########################################################################

subscribeInputFileDialogOpened :: (HasBiDiRunner env) => (FileDialogOpened -> IO ()) -> RIO env SubscriptionId
subscribeInputFileDialogOpened = viaSub API.subscribeInputFileDialogOpened

subscribeInputFileDialogOpened' :: (HasBiDiRunner env) => [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> IO ()) -> RIO env SubscriptionId
subscribeInputFileDialogOpened' = viaSub' API.subscribeInputFileDialogOpened
