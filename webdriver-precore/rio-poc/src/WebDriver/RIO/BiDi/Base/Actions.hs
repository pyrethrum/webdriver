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
import WebDriverPreCore.Extended.BiDi.Base.Actions qualified as A
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
    KnownCommand (..),
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
    SubscriptionId (..),
    TraverseHistory,
    mkCommand,
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
viaRunner :: HasBiDiRunner env => (BiDiRunner -> IO a) -> RIO env a
viaRunner f = getBiDiRunner >>= liftIO . f

-- | Run a typed command through the BiDi runner.
run :: (HasBiDiRunner env, FromJSON r) => Command r -> RIO env r
run cmd = viaRunner $ \(MkBiDiRunner {run}) -> run cmd

-- | Extract the 'SubscriptionId' from a subscribe response.
extractSubscription :: SessionSubscribeResult -> SubscriptionId
extractSubscription MkSessionSubscribeResult {subscription} = subscription

-- | Build an 'A.SendSub IO a' from a 'BiDiRunner', subscribing to all contexts.
mkSendSub :: BiDiRunner -> A.SendSub IO a
mkSendSub MkBiDiRunner {run, socketActions} mkSub handler =
  Runner.subscribe socketActions (run . A.sessionSubscribe) (mkSub [] [] handler)

-- | Build an 'A.SendSub\' IO a' from a 'BiDiRunner', with context filters.
mkSendSub' :: BiDiRunner -> A.SendSub' IO a
mkSendSub' MkBiDiRunner {run, socketActions} mkSub bcs ucs handler =
  Runner.subscribe socketActions (run . A.sessionSubscribe) (mkSub bcs ucs handler)

-- | Build an 'A.SendSubMany\' IO' from a 'BiDiRunner', with context filters.
mkSendSubMany' :: BiDiRunner -> A.SendSubMany' IO
mkSendSubMany' MkBiDiRunner {run, socketActions} mkSub sts bcs ucs handler =
  Runner.subscribe socketActions (run . A.sessionSubscribe) (mkSub sts bcs ucs handler)

-- | Build an 'A.SendSubOffSpecMany\' IO' from a 'BiDiRunner', with context filters.
mkSendSubOffSpecMany' :: BiDiRunner -> A.SendSubOffSpecMany' IO
mkSendSubOffSpecMany' MkBiDiRunner {run, socketActions} mkSub sts bcs ucs handler =
  Runner.subscribe socketActions (run . A.sessionSubscribe) (mkSub sts bcs ucs handler)

-- | Subscribe via an Extended-style subscription function (no context filters).
viaSub ::
  HasBiDiRunner env =>
  (A.SendSub IO a -> (a -> IO ()) -> IO SubscriptionId) ->
  (a -> IO ()) ->
  RIO env SubscriptionId
viaSub extFn handler = viaRunner $ \runner -> extFn (mkSendSub runner) handler

-- | Subscribe via an Extended-style subscription function (with context filters).
viaSub' ::
  HasBiDiRunner env =>
  (A.SendSub' IO a -> [BrowsingContext] -> [UserContext] -> (a -> IO ()) -> IO SubscriptionId) ->
  [BrowsingContext] ->
  [UserContext] ->
  (a -> IO ()) ->
  RIO env SubscriptionId
viaSub' extFn bcs ucs handler = viaRunner $ \runner -> extFn (mkSendSub' runner) bcs ucs handler

-- ###########################################################################
-- ########################### Session Commands ##############################
-- ###########################################################################

sessionNew :: HasBiDiRunner env => Capabilities -> RIO env SessionNewResult
sessionNew = A.sessionNew run

sessionStatus :: HasBiDiRunner env => RIO env SessionStatusResult
sessionStatus = A.sessionStatus run

sessionEnd :: HasBiDiRunner env => RIO env ()
sessionEnd = A.sessionEnd run

-- | Subscribe and return the bare 'SubscriptionId' (without the wrapper type).
sessionSubscribe :: HasBiDiRunner env => SessionSubscibe -> RIO env SubscriptionId
sessionSubscribe sub = extractSubscription <$> run (A.sessionSubscribe sub)

sessionUnsubscribe :: HasBiDiRunner env => SessionUnsubscribe -> RIO env ()
sessionUnsubscribe unsub =
  viaRunner $ \MkBiDiRunner {run, socketActions} ->
    Runner.unsubscribe socketActions (run . A.sessionUnsubscribe) unsub

-- ###########################################################################
-- ####################### BrowsingContext Commands ##########################
-- ###########################################################################

browsingContextActivate :: HasBiDiRunner env => Activate -> RIO env ()
browsingContextActivate = A.browsingContextActivate run

browsingContextCaptureScreenshot :: HasBiDiRunner env => CaptureScreenshot -> RIO env CaptureScreenshotResult
browsingContextCaptureScreenshot = A.browsingContextCaptureScreenshot run

browsingContextClose :: HasBiDiRunner env => Close -> RIO env ()
browsingContextClose = A.browsingContextClose run

browsingContextCreate :: HasBiDiRunner env => Create -> RIO env BrowsingContext
browsingContextCreate = A.browsingContextCreate run

browsingContextGetTree :: HasBiDiRunner env => GetTree -> RIO env GetTreeResult
browsingContextGetTree = A.browsingContextGetTree run

browsingContextHandleUserPrompt :: HasBiDiRunner env => HandleUserPrompt -> RIO env ()
browsingContextHandleUserPrompt = A.browsingContextHandleUserPrompt run

browsingContextLocateNodes :: HasBiDiRunner env => LocateNodes -> RIO env LocateNodesResult
browsingContextLocateNodes = A.browsingContextLocateNodes run

browsingContextNavigate :: HasBiDiRunner env => Navigate -> RIO env NavigateResult
browsingContextNavigate = A.browsingContextNavigate run

browsingContextPrint :: HasBiDiRunner env => Print -> RIO env PrintResult
browsingContextPrint = A.browsingContextPrint run

browsingContextReload :: HasBiDiRunner env => Reload -> RIO env ()
browsingContextReload = A.browsingContextReload run

browsingContextSetViewport :: HasBiDiRunner env => SetViewport -> RIO env ()
browsingContextSetViewport = A.browsingContextSetViewport run

browsingContextTraverseHistory :: HasBiDiRunner env => TraverseHistory -> RIO env ()
browsingContextTraverseHistory = A.browsingContextTraverseHistory run

-- ###########################################################################
-- ########################### Browser Commands ##############################
-- ###########################################################################

browserClose :: HasBiDiRunner env => RIO env ()
browserClose = A.browserClose run

browserCreateUserContext :: HasBiDiRunner env => CreateUserContext -> RIO env UserContext
browserCreateUserContext = A.browserCreateUserContext run

browserGetClientWindows :: HasBiDiRunner env => RIO env GetClientWindowsResult
browserGetClientWindows = A.browserGetClientWindows run

browserGetUserContexts :: HasBiDiRunner env => RIO env GetUserContextsResult
browserGetUserContexts = A.browserGetUserContexts run

browserRemoveUserContext :: HasBiDiRunner env => RemoveUserContext -> RIO env ()
browserRemoveUserContext = A.browserRemoveUserContext run

browserSetClientWindowState :: HasBiDiRunner env => SetClientWindowState -> RIO env ClientWindowInfo
browserSetClientWindowState = A.browserSetClientWindowState run

browserSetDownloadBehavior :: HasBiDiRunner env => SetDownloadBehavior -> RIO env ()
browserSetDownloadBehavior = A.browserSetDownloadBehavior run

-- ###########################################################################
-- ########################## Emulation Commands #############################
-- ###########################################################################

emulationSetForcedColorsModeThemeOverride :: HasBiDiRunner env => SetForcedColorsModeThemeOverride -> RIO env ()
emulationSetForcedColorsModeThemeOverride = A.emulationSetForcedColorsModeThemeOverride run

emulationSetGeolocationOverride :: HasBiDiRunner env => SetGeolocationOverride -> RIO env ()
emulationSetGeolocationOverride = A.emulationSetGeolocationOverride run

emulationSetLocaleOverride :: HasBiDiRunner env => SetLocaleOverride -> RIO env ()
emulationSetLocaleOverride = A.emulationSetLocaleOverride run

emulationSetNetworkConditions :: HasBiDiRunner env => SetNetworkConditions -> RIO env ()
emulationSetNetworkConditions = A.emulationSetNetworkConditions run

emulationSetScreenOrientationOverride :: HasBiDiRunner env => SetScreenOrientationOverride -> RIO env ()
emulationSetScreenOrientationOverride = A.emulationSetScreenOrientationOverride run

emulationSetScreenSettingsOverride :: HasBiDiRunner env => SetScreenSettingsOverride -> RIO env ()
emulationSetScreenSettingsOverride = A.emulationSetScreenSettingsOverride run

emulationSetScriptingEnabled :: HasBiDiRunner env => SetScriptingEnabled -> RIO env ()
emulationSetScriptingEnabled = A.emulationSetScriptingEnabled run

emulationSetTimezoneOverride :: HasBiDiRunner env => SetTimezoneOverride -> RIO env ()
emulationSetTimezoneOverride = A.emulationSetTimezoneOverride run

emulationSetTouchOverride :: HasBiDiRunner env => SetTouchOverride -> RIO env ()
emulationSetTouchOverride = A.emulationSetTouchOverride run

emulationSetUserAgentOverride :: HasBiDiRunner env => SetUserAgentOverride -> RIO env ()
emulationSetUserAgentOverride = A.emulationSetUserAgentOverride run

-- ###########################################################################
-- ############################ Input Commands ###############################
-- ###########################################################################

inputPerformActions :: HasBiDiRunner env => PerformActions -> RIO env ()
inputPerformActions = A.inputPerformActions run

inputReleaseActions :: HasBiDiRunner env => ReleaseActions -> RIO env ()
inputReleaseActions = A.inputReleaseActions run

inputSetFiles :: HasBiDiRunner env => SetFiles -> RIO env ()
inputSetFiles = A.inputSetFiles run

-- ###########################################################################
-- ########################### Network Commands ##############################
-- ###########################################################################

networkAddDataCollector :: HasBiDiRunner env => AddDataCollector -> RIO env AddDataCollectorResult
networkAddDataCollector = A.networkAddDataCollector run

networkAddIntercept :: HasBiDiRunner env => AddIntercept -> RIO env AddInterceptResult
networkAddIntercept = A.networkAddIntercept run

networkContinueRequest :: HasBiDiRunner env => ContinueRequest -> RIO env ()
networkContinueRequest = A.networkContinueRequest run

networkContinueResponse :: HasBiDiRunner env => ContinueResponse -> RIO env ()
networkContinueResponse = A.networkContinueResponse run

networkContinueWithAuth :: HasBiDiRunner env => ContinueWithAuth -> RIO env ()
networkContinueWithAuth = A.networkContinueWithAuth run

networkDisownData :: HasBiDiRunner env => DisownData -> RIO env ()
networkDisownData = A.networkDisownData run

networkFailRequest :: HasBiDiRunner env => FailRequest -> RIO env ()
networkFailRequest = A.networkFailRequest run

networkGetData :: HasBiDiRunner env => GetData -> RIO env GetDataResult
networkGetData = A.networkGetData run

networkProvideResponse :: HasBiDiRunner env => ProvideResponse -> RIO env ()
networkProvideResponse = A.networkProvideResponse run

networkRemoveDataCollector :: HasBiDiRunner env => RemoveDataCollector -> RIO env ()
networkRemoveDataCollector = A.networkRemoveDataCollector run

networkRemoveIntercept :: HasBiDiRunner env => RemoveIntercept -> RIO env ()
networkRemoveIntercept = A.networkRemoveIntercept run

networkSetCacheBehavior :: HasBiDiRunner env => SetCacheBehavior -> RIO env ()
networkSetCacheBehavior = A.networkSetCacheBehavior run

networkSetExtraHeaders :: HasBiDiRunner env => SetExtraHeaders -> RIO env ()
networkSetExtraHeaders = A.networkSetExtraHeaders run

-- ###########################################################################
-- ########################### Script Commands ###############################
-- ###########################################################################

scriptAddPreloadScript :: HasBiDiRunner env => AddPreloadScript -> RIO env AddPreloadScriptResult
scriptAddPreloadScript = A.scriptAddPreloadScript run

scriptCallFunction :: HasBiDiRunner env => CallFunction -> RIO env EvaluateResult
scriptCallFunction = A.scriptCallFunction run

scriptDisown :: HasBiDiRunner env => Disown -> RIO env ()
scriptDisown = A.scriptDisown run

scriptEvaluate :: HasBiDiRunner env => Evaluate -> RIO env EvaluateResult
scriptEvaluate = A.scriptEvaluate run

scriptGetRealms :: HasBiDiRunner env => GetRealms -> RIO env GetRealmsResult
scriptGetRealms = A.scriptGetRealms run

scriptRemovePreloadScript :: HasBiDiRunner env => RemovePreloadScript -> RIO env ()
scriptRemovePreloadScript = A.scriptRemovePreloadScript run

-- ###########################################################################
-- ########################### Storage Commands ##############################
-- ###########################################################################

storageDeleteCookies :: HasBiDiRunner env => DeleteCookies -> RIO env DeleteCookiesResult
storageDeleteCookies = A.storageDeleteCookies run

storageGetCookies :: HasBiDiRunner env => GetCookies -> RIO env GetCookiesResult
storageGetCookies = A.storageGetCookies run

storageSetCookie :: HasBiDiRunner env => SetCookie -> RIO env SetCookieResult
storageSetCookie = A.storageSetCookie run

-- ###########################################################################
-- ######################### WebExtension Commands ###########################
-- ###########################################################################

webExtensionInstall :: HasBiDiRunner env => WebExtensionInstall -> RIO env WebExtensionResult
webExtensionInstall = A.webExtensionInstall run

webExtensionUninstall :: HasBiDiRunner env => WebExtensionUninstall -> RIO env ()
webExtensionUninstall = A.webExtensionUninstall run

-- ###########################################################################
-- ########################## Generic Command ################################
-- ###########################################################################

-- | Send any typed 'Command' through the BiDi runner.
sendCommand :: (HasBiDiRunner env, FromJSON r) => Command r -> RIO env r
sendCommand = run

-- | Send a typed 'Command' using a specific message ID.
sendCommand' :: (HasBiDiRunner env, FromJSON r) => JSUInt -> Command r -> RIO env r
sendCommand' msgId cmd =
  viaRunner $ \(MkBiDiRunner {runWithId}) -> runWithId msgId cmd

-- | Send a typed 'Command' without waiting for a response.
sendCommandNoWait :: HasBiDiRunner env => Command r -> RIO env Request
sendCommandNoWait cmd =
  viaRunner $ \r -> Runner.runNoWait r cmd

-- | Send an off-spec command with an explicit message ID.
sendOffSpecCommand' :: HasBiDiRunner env => JSUInt -> Text -> Object -> RIO env Object
sendOffSpecCommand' msgId method params =
  viaRunner $ \(MkBiDiRunner {runOffSpecWithId}) -> runOffSpecWithId msgId method params

-- | Send an off-spec command without waiting for a response.
sendOffSpecCommandNoWait :: HasBiDiRunner env => Text -> Object -> RIO env Request
sendOffSpecCommandNoWait method params =
  viaRunner $ \r -> Runner.runOffSpecNoWait r method params

-- | Evaluate a script expression without waiting for the result.
scriptEvaluateNoWait :: HasBiDiRunner env => Evaluate -> RIO env Request
scriptEvaluateNoWait cmd =
  viaRunner $ \r -> Runner.runNoWait r (mkCommand ScriptEvaluate cmd)

-- ###########################################################################
-- ######################### Subscription Helpers ############################
-- ###########################################################################

-- | Subscribe to multiple known event types (no context filters).
subscribeMany ::
  HasBiDiRunner env =>
  [KnownSubscriptionType] ->
  (Event -> IO ()) ->
  RIO env SubscriptionId
subscribeMany sts = subscribeMany' [] [] sts

-- | Subscribe to multiple known event types with context and user-context filters.
subscribeMany' ::
  HasBiDiRunner env =>
  [BrowsingContext] ->
  [UserContext] ->
  [KnownSubscriptionType] ->
  (Event -> IO ()) ->
  RIO env SubscriptionId
subscribeMany' bcs ucs sts handler =
  viaRunner $ \runner -> A.subscribeMany' (mkSendSubMany' runner) sts bcs ucs handler

-- | Subscribe to unknown / off-spec event types (no context filters).
subscribeUnknownMany ::
  HasBiDiRunner env =>
  [OffSpecSubscriptionType] ->
  (Value -> IO ()) ->
  RIO env SubscriptionId
subscribeUnknownMany sts = subscribeUnknownMany' [] [] sts

-- | Subscribe to unknown / off-spec event types with context filters.
subscribeUnknownMany' ::
  HasBiDiRunner env =>
  [BrowsingContext] ->
  [UserContext] ->
  [OffSpecSubscriptionType] ->
  (Value -> IO ()) ->
  RIO env SubscriptionId
subscribeUnknownMany' bcs ucs sts handler =
  viaRunner $ \runner -> A.subscribeOffSpecMany' (mkSendSubOffSpecMany' runner) sts bcs ucs handler

-- | Unsubscribe using a previously obtained 'SubscriptionId'.
unsubscribe :: HasBiDiRunner env => SubscriptionId -> RIO env ()
unsubscribe subId =
  viaRunner $ \MkBiDiRunner {run, socketActions} ->
    Runner.unsubscribe socketActions (run . A.sessionUnsubscribe) (UnsubscribeById [subId])

-- ###########################################################################
-- ######################### Log Subscriptions ###############################
-- ###########################################################################

subscribeLogEntryAdded :: HasBiDiRunner env => (LogEntry -> IO ()) -> RIO env SubscriptionId
subscribeLogEntryAdded = viaSub A.subscribeLogEntryAdded

subscribeLogEntryAdded' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (LogEntry -> IO ()) -> RIO env SubscriptionId
subscribeLogEntryAdded' = viaSub' A.subscribeLogEntryAdded'

-- ###########################################################################
-- ################### BrowsingContext Subscriptions #########################
-- ###########################################################################

subscribeBrowsingContextCreated :: HasBiDiRunner env => (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextCreated = viaSub A.subscribeBrowsingContextCreated

subscribeBrowsingContextCreated' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextCreated' = viaSub' A.subscribeBrowsingContextCreated'

subscribeBrowsingContextDestroyed :: HasBiDiRunner env => (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDestroyed = viaSub A.subscribeBrowsingContextDestroyed

subscribeBrowsingContextDestroyed' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDestroyed' = viaSub' A.subscribeBrowsingContextDestroyed'

subscribeBrowsingContextNavigationStarted :: HasBiDiRunner env => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationStarted = viaSub A.subscribeBrowsingContextNavigationStarted

subscribeBrowsingContextNavigationStarted' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationStarted' = viaSub' A.subscribeBrowsingContextNavigationStarted'

subscribeBrowsingContextFragmentNavigated :: HasBiDiRunner env => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextFragmentNavigated = viaSub A.subscribeBrowsingContextFragmentNavigated

subscribeBrowsingContextFragmentNavigated' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextFragmentNavigated' = viaSub' A.subscribeBrowsingContextFragmentNavigated'

subscribeBrowsingContextHistoryUpdated :: HasBiDiRunner env => (HistoryUpdated -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextHistoryUpdated = viaSub A.subscribeBrowsingContextHistoryUpdated

subscribeBrowsingContextHistoryUpdated' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextHistoryUpdated' = viaSub' A.subscribeBrowsingContextHistoryUpdated'

subscribeBrowsingContextDomContentLoaded :: HasBiDiRunner env => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDomContentLoaded = viaSub A.subscribeBrowsingContextDomContentLoaded

subscribeBrowsingContextDomContentLoaded' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDomContentLoaded' = viaSub' A.subscribeBrowsingContextDomContentLoaded'

subscribeBrowsingContextLoad :: HasBiDiRunner env => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextLoad = viaSub A.subscribeBrowsingContextLoad

subscribeBrowsingContextLoad' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextLoad' = viaSub' A.subscribeBrowsingContextLoad'

subscribeBrowsingContextDownloadWillBegin :: HasBiDiRunner env => (DownloadWillBegin -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadWillBegin = viaSub A.subscribeBrowsingContextDownloadWillBegin

subscribeBrowsingContextDownloadWillBegin' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadWillBegin' = viaSub' A.subscribeBrowsingContextDownloadWillBegin'

subscribeBrowsingContextDownloadEnd :: HasBiDiRunner env => (DownloadEnd -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadEnd = viaSub A.subscribeBrowsingContextDownloadEnd

subscribeBrowsingContextDownloadEnd' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (DownloadEnd -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextDownloadEnd' = viaSub' A.subscribeBrowsingContextDownloadEnd'

subscribeBrowsingContextNavigationAborted :: HasBiDiRunner env => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationAborted = viaSub A.subscribeBrowsingContextNavigationAborted

subscribeBrowsingContextNavigationAborted' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationAborted' = viaSub' A.subscribeBrowsingContextNavigationAborted'

subscribeBrowsingContextNavigationCommitted :: HasBiDiRunner env => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationCommitted = viaSub A.subscribeBrowsingContextNavigationCommitted

subscribeBrowsingContextNavigationCommitted' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationCommitted' = viaSub' A.subscribeBrowsingContextNavigationCommitted'

subscribeBrowsingContextNavigationFailed :: HasBiDiRunner env => (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationFailed = viaSub A.subscribeBrowsingContextNavigationFailed

subscribeBrowsingContextNavigationFailed' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextNavigationFailed' = viaSub' A.subscribeBrowsingContextNavigationFailed'

subscribeBrowsingContextUserPromptClosed :: HasBiDiRunner env => (UserPromptClosed -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptClosed = viaSub A.subscribeBrowsingContextUserPromptClosed

subscribeBrowsingContextUserPromptClosed' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptClosed' = viaSub' A.subscribeBrowsingContextUserPromptClosed'

subscribeBrowsingContextUserPromptOpened :: HasBiDiRunner env => (UserPromptOpened -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptOpened = viaSub A.subscribeBrowsingContextUserPromptOpened

subscribeBrowsingContextUserPromptOpened' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> IO ()) -> RIO env SubscriptionId
subscribeBrowsingContextUserPromptOpened' = viaSub' A.subscribeBrowsingContextUserPromptOpened'

-- ###########################################################################
-- ####################### Network Subscriptions #############################
-- ###########################################################################

subscribeNetworkAuthRequired :: HasBiDiRunner env => (AuthRequired -> IO ()) -> RIO env SubscriptionId
subscribeNetworkAuthRequired = viaSub A.subscribeNetworkAuthRequired

subscribeNetworkAuthRequired' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (AuthRequired -> IO ()) -> RIO env SubscriptionId
subscribeNetworkAuthRequired' = viaSub' A.subscribeNetworkAuthRequired'

subscribeNetworkBeforeRequestSent :: HasBiDiRunner env => (BeforeRequestSent -> IO ()) -> RIO env SubscriptionId
subscribeNetworkBeforeRequestSent = viaSub A.subscribeNetworkBeforeRequestSent

subscribeNetworkBeforeRequestSent' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> IO ()) -> RIO env SubscriptionId
subscribeNetworkBeforeRequestSent' = viaSub' A.subscribeNetworkBeforeRequestSent'

subscribeNetworkFetchError :: HasBiDiRunner env => (FetchError -> IO ()) -> RIO env SubscriptionId
subscribeNetworkFetchError = viaSub A.subscribeNetworkFetchError

subscribeNetworkFetchError' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (FetchError -> IO ()) -> RIO env SubscriptionId
subscribeNetworkFetchError' = viaSub' A.subscribeNetworkFetchError'

subscribeNetworkResponseCompleted :: HasBiDiRunner env => (ResponseCompleted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseCompleted = viaSub A.subscribeNetworkResponseCompleted

subscribeNetworkResponseCompleted' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseCompleted' = viaSub' A.subscribeNetworkResponseCompleted'

subscribeNetworkResponseStarted :: HasBiDiRunner env => (ResponseStarted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseStarted = viaSub A.subscribeNetworkResponseStarted

subscribeNetworkResponseStarted' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (ResponseStarted -> IO ()) -> RIO env SubscriptionId
subscribeNetworkResponseStarted' = viaSub' A.subscribeNetworkResponseStarted'

-- ###########################################################################
-- ####################### Script Subscriptions ##############################
-- ###########################################################################

subscribeScriptMessage :: HasBiDiRunner env => (Message -> IO ()) -> RIO env SubscriptionId
subscribeScriptMessage = viaSub A.subscribeScriptMessage

subscribeScriptMessage' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (Message -> IO ()) -> RIO env SubscriptionId
subscribeScriptMessage' = viaSub' A.subscribeScriptMessage'

subscribeScriptRealmCreated :: HasBiDiRunner env => (RealmInfo -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmCreated = viaSub A.subscribeScriptRealmCreated

subscribeScriptRealmCreated' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (RealmInfo -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmCreated' = viaSub' A.subscribeScriptRealmCreated'

subscribeScriptRealmDestroyed :: HasBiDiRunner env => (RealmDestroyed -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmDestroyed = viaSub A.subscribeScriptRealmDestroyed

subscribeScriptRealmDestroyed' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> IO ()) -> RIO env SubscriptionId
subscribeScriptRealmDestroyed' = viaSub' A.subscribeScriptRealmDestroyed'

-- ###########################################################################
-- ####################### Input Subscriptions ###############################
-- ###########################################################################

subscribeInputFileDialogOpened :: HasBiDiRunner env => (FileDialogOpened -> IO ()) -> RIO env SubscriptionId
subscribeInputFileDialogOpened = viaSub A.subscribeInputFileDialogOpened

subscribeInputFileDialogOpened' :: HasBiDiRunner env => [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> IO ()) -> RIO env SubscriptionId
subscribeInputFileDialogOpened' = viaSub' A.subscribeInputFileDialogOpened'
