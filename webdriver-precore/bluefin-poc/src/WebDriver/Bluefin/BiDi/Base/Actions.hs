-- |
-- Module: WebDriver.Bluefin.BiDi.Base.Actions
-- Description: Bluefin-style BiDi WebDriver action functions
--
-- Provides BiDi WebDriver actions for Bluefin.  All functions take an
-- explicit 'BiDiEnv' handle rather than using typeclass constraints.
--
-- Subscription callbacks are @IO ()@ actions (not 'Eff') because the
-- underlying 'BiDiRunner' is @IO@-based and callbacks are invoked
-- asynchronously by the WebSocket reader loop.
--
-- This mirrors 'WebDriver.RIO.BiDi.Base.Actions' but with explicit handles.
module WebDriver.Bluefin.BiDi.Base.Actions
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
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (effIO)
import WebDriver.Bluefin.HTTP.Core (BiDiEnv (..), runBiDiCommand)
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

-- | Run a typed BiDi command via the IO runner, lifted into 'Eff'.
run :: (e :> es, FromJSON r) => BiDiEnv e -> Command r -> Eff es r
run = runBiDiCommand

-- | Extract the @run@ function from a 'BiDiEnv'.
--
-- Uses record-pattern matching because 'run' is rank-2 polymorphic and cannot
-- be accessed via 'OverloadedRecordDot'.
bidiRun :: (FromJSON r) => BiDiEnv e -> Command r -> IO r
bidiRun MkBiDiEnv {biDiRunner = MkBiDiRunner {run = r}} = r

-- | Extract the @runWithId@ function from a 'BiDiEnv'.
bidiRunWithId :: (FromJSON r) => BiDiEnv e -> JSUInt -> Command r -> IO r
bidiRunWithId MkBiDiEnv {biDiRunner = MkBiDiRunner {runWithId = rwi}} = rwi

-- | Extract the 'SubscriptionId' from a subscribe response.
extractSubscription :: SessionSubscribeResult -> SubscriptionId
extractSubscription MkSessionSubscribeResult {subscription} = subscription

-- | Build an 'A.SendSub IO a' from a 'BiDiRunner IO', subscribing to all contexts.
mkSendSub :: BiDiRunner IO -> A.SendSub IO a
mkSendSub MkBiDiRunner {run = r, socketActions} mkSub handler =
  Runner.subscribe socketActions (r . A.sessionSubscribe) (mkSub [] [] handler)

-- | Build an 'A.SendSub\' IO a' from a 'BiDiRunner IO', with context filters.
mkSendSub' :: BiDiRunner IO -> A.SendSub' IO a
mkSendSub' MkBiDiRunner {run = r, socketActions} mkSub bcs ucs handler =
  Runner.subscribe socketActions (r . A.sessionSubscribe) (mkSub bcs ucs handler)

-- | Build an 'A.SendSubMany\' IO' from a 'BiDiRunner IO', with context filters.
mkSendSubMany' :: BiDiRunner IO -> A.SendSubMany' IO
mkSendSubMany' MkBiDiRunner {run = r, socketActions} mkSub sts bcs ucs handler =
  Runner.subscribe socketActions (r . A.sessionSubscribe) (mkSub sts bcs ucs handler)

-- | Build an 'A.SendSubOffSpecMany\' IO' from a 'BiDiRunner IO'.
mkSendSubOffSpecMany' :: BiDiRunner IO -> A.SendSubOffSpecMany' IO
mkSendSubOffSpecMany' MkBiDiRunner {run = r, socketActions} mkSub sts bcs ucs handler =
  Runner.subscribe socketActions (r . A.sessionSubscribe) (mkSub sts bcs ucs handler)

-- | Subscribe (no context filters), lifting result into 'Eff'.
viaSub ::
  (e :> es) =>
  (A.SendSub IO a -> (a -> IO ()) -> IO SubscriptionId) ->
  BiDiEnv e ->
  (a -> IO ()) ->
  Eff es SubscriptionId
viaSub extFn bidi handler =
  effIO bidi.biDiIO $ extFn (mkSendSub bidi.biDiRunner) handler

-- | Subscribe with context filters, lifting result into 'Eff'.
viaSub' ::
  (e :> es) =>
  (A.SendSub' IO a -> [BrowsingContext] -> [UserContext] -> (a -> IO ()) -> IO SubscriptionId) ->
  BiDiEnv e ->
  [BrowsingContext] ->
  [UserContext] ->
  (a -> IO ()) ->
  Eff es SubscriptionId
viaSub' extFn bidi bcs ucs handler =
  effIO bidi.biDiIO $ extFn (mkSendSub' bidi.biDiRunner) bcs ucs handler

-- ###########################################################################
-- ########################### Session Commands ##############################
-- ###########################################################################

sessionNew :: (e :> es) => BiDiEnv e -> Capabilities -> Eff es SessionNewResult
sessionNew bidi = effIO bidi.biDiIO . A.sessionNew (bidiRun bidi)

sessionStatus :: (e :> es) => BiDiEnv e -> Eff es SessionStatusResult
sessionStatus bidi = effIO bidi.biDiIO $ A.sessionStatus (bidiRun bidi)

sessionEnd :: (e :> es) => BiDiEnv e -> Eff es ()
sessionEnd bidi = effIO bidi.biDiIO $ A.sessionEnd (bidiRun bidi)

-- | Subscribe and return the bare 'SubscriptionId'.
sessionSubscribe :: (e :> es) => BiDiEnv e -> SessionSubscibe -> Eff es SubscriptionId
sessionSubscribe bidi sub =
  extractSubscription <$> run bidi (A.sessionSubscribe sub)

sessionUnsubscribe :: (e :> es) => BiDiEnv e -> SessionUnsubscribe -> Eff es ()
sessionUnsubscribe bidi unsub =
  effIO bidi.biDiIO $
    Runner.unsubscribe bidi.biDiRunner.socketActions ((bidiRun bidi) . A.sessionUnsubscribe) unsub

-- ###########################################################################
-- ####################### BrowsingContext Commands ##########################
-- ###########################################################################

browsingContextActivate :: (e :> es) => BiDiEnv e -> Activate -> Eff es ()
browsingContextActivate bidi = effIO bidi.biDiIO . A.browsingContextActivate (bidiRun bidi)

browsingContextCaptureScreenshot :: (e :> es) => BiDiEnv e -> CaptureScreenshot -> Eff es CaptureScreenshotResult
browsingContextCaptureScreenshot bidi = effIO bidi.biDiIO . A.browsingContextCaptureScreenshot (bidiRun bidi)

browsingContextClose :: (e :> es) => BiDiEnv e -> Close -> Eff es ()
browsingContextClose bidi = effIO bidi.biDiIO . A.browsingContextClose (bidiRun bidi)

browsingContextCreate :: (e :> es) => BiDiEnv e -> Create -> Eff es BrowsingContext
browsingContextCreate bidi = effIO bidi.biDiIO . A.browsingContextCreate (bidiRun bidi)

browsingContextGetTree :: (e :> es) => BiDiEnv e -> GetTree -> Eff es GetTreeResult
browsingContextGetTree bidi = effIO bidi.biDiIO . A.browsingContextGetTree (bidiRun bidi)

browsingContextHandleUserPrompt :: (e :> es) => BiDiEnv e -> HandleUserPrompt -> Eff es ()
browsingContextHandleUserPrompt bidi = effIO bidi.biDiIO . A.browsingContextHandleUserPrompt (bidiRun bidi)

browsingContextLocateNodes :: (e :> es) => BiDiEnv e -> LocateNodes -> Eff es LocateNodesResult
browsingContextLocateNodes bidi = effIO bidi.biDiIO . A.browsingContextLocateNodes (bidiRun bidi)

browsingContextNavigate :: (e :> es) => BiDiEnv e -> Navigate -> Eff es NavigateResult
browsingContextNavigate bidi = effIO bidi.biDiIO . A.browsingContextNavigate (bidiRun bidi)

browsingContextPrint :: (e :> es) => BiDiEnv e -> Print -> Eff es PrintResult
browsingContextPrint bidi = effIO bidi.biDiIO . A.browsingContextPrint (bidiRun bidi)

browsingContextReload :: (e :> es) => BiDiEnv e -> Reload -> Eff es ()
browsingContextReload bidi = effIO bidi.biDiIO . A.browsingContextReload (bidiRun bidi)

browsingContextSetViewport :: (e :> es) => BiDiEnv e -> SetViewport -> Eff es ()
browsingContextSetViewport bidi = effIO bidi.biDiIO . A.browsingContextSetViewport (bidiRun bidi)

browsingContextTraverseHistory :: (e :> es) => BiDiEnv e -> TraverseHistory -> Eff es ()
browsingContextTraverseHistory bidi = effIO bidi.biDiIO . A.browsingContextTraverseHistory (bidiRun bidi)

-- ###########################################################################
-- ########################### Browser Commands ##############################
-- ###########################################################################

browserClose :: (e :> es) => BiDiEnv e -> Eff es ()
browserClose bidi = effIO bidi.biDiIO $ A.browserClose (bidiRun bidi)

browserCreateUserContext :: (e :> es) => BiDiEnv e -> CreateUserContext -> Eff es UserContext
browserCreateUserContext bidi = effIO bidi.biDiIO . A.browserCreateUserContext (bidiRun bidi)

browserGetClientWindows :: (e :> es) => BiDiEnv e -> Eff es GetClientWindowsResult
browserGetClientWindows bidi = effIO bidi.biDiIO $ A.browserGetClientWindows (bidiRun bidi)

browserGetUserContexts :: (e :> es) => BiDiEnv e -> Eff es GetUserContextsResult
browserGetUserContexts bidi = effIO bidi.biDiIO $ A.browserGetUserContexts (bidiRun bidi)

browserRemoveUserContext :: (e :> es) => BiDiEnv e -> RemoveUserContext -> Eff es ()
browserRemoveUserContext bidi = effIO bidi.biDiIO . A.browserRemoveUserContext (bidiRun bidi)

browserSetClientWindowState :: (e :> es) => BiDiEnv e -> SetClientWindowState -> Eff es ClientWindowInfo
browserSetClientWindowState bidi = effIO bidi.biDiIO . A.browserSetClientWindowState (bidiRun bidi)

browserSetDownloadBehavior :: (e :> es) => BiDiEnv e -> SetDownloadBehavior -> Eff es ()
browserSetDownloadBehavior bidi = effIO bidi.biDiIO . A.browserSetDownloadBehavior (bidiRun bidi)

-- ###########################################################################
-- ########################## Emulation Commands #############################
-- ###########################################################################

emulationSetForcedColorsModeThemeOverride :: (e :> es) => BiDiEnv e -> SetForcedColorsModeThemeOverride -> Eff es ()
emulationSetForcedColorsModeThemeOverride bidi = effIO bidi.biDiIO . A.emulationSetForcedColorsModeThemeOverride (bidiRun bidi)

emulationSetGeolocationOverride :: (e :> es) => BiDiEnv e -> SetGeolocationOverride -> Eff es ()
emulationSetGeolocationOverride bidi = effIO bidi.biDiIO . A.emulationSetGeolocationOverride (bidiRun bidi)

emulationSetLocaleOverride :: (e :> es) => BiDiEnv e -> SetLocaleOverride -> Eff es ()
emulationSetLocaleOverride bidi = effIO bidi.biDiIO . A.emulationSetLocaleOverride (bidiRun bidi)

emulationSetNetworkConditions :: (e :> es) => BiDiEnv e -> SetNetworkConditions -> Eff es ()
emulationSetNetworkConditions bidi = effIO bidi.biDiIO . A.emulationSetNetworkConditions (bidiRun bidi)

emulationSetScreenOrientationOverride :: (e :> es) => BiDiEnv e -> SetScreenOrientationOverride -> Eff es ()
emulationSetScreenOrientationOverride bidi = effIO bidi.biDiIO . A.emulationSetScreenOrientationOverride (bidiRun bidi)

emulationSetScreenSettingsOverride :: (e :> es) => BiDiEnv e -> SetScreenSettingsOverride -> Eff es ()
emulationSetScreenSettingsOverride bidi = effIO bidi.biDiIO . A.emulationSetScreenSettingsOverride (bidiRun bidi)

emulationSetScriptingEnabled :: (e :> es) => BiDiEnv e -> SetScriptingEnabled -> Eff es ()
emulationSetScriptingEnabled bidi = effIO bidi.biDiIO . A.emulationSetScriptingEnabled (bidiRun bidi)

emulationSetTimezoneOverride :: (e :> es) => BiDiEnv e -> SetTimezoneOverride -> Eff es ()
emulationSetTimezoneOverride bidi = effIO bidi.biDiIO . A.emulationSetTimezoneOverride (bidiRun bidi)

emulationSetTouchOverride :: (e :> es) => BiDiEnv e -> SetTouchOverride -> Eff es ()
emulationSetTouchOverride bidi = effIO bidi.biDiIO . A.emulationSetTouchOverride (bidiRun bidi)

emulationSetUserAgentOverride :: (e :> es) => BiDiEnv e -> SetUserAgentOverride -> Eff es ()
emulationSetUserAgentOverride bidi = effIO bidi.biDiIO . A.emulationSetUserAgentOverride (bidiRun bidi)

-- ###########################################################################
-- ############################ Input Commands ###############################
-- ###########################################################################

inputPerformActions :: (e :> es) => BiDiEnv e -> PerformActions -> Eff es ()
inputPerformActions bidi = effIO bidi.biDiIO . A.inputPerformActions (bidiRun bidi)

inputReleaseActions :: (e :> es) => BiDiEnv e -> ReleaseActions -> Eff es ()
inputReleaseActions bidi = effIO bidi.biDiIO . A.inputReleaseActions (bidiRun bidi)

inputSetFiles :: (e :> es) => BiDiEnv e -> SetFiles -> Eff es ()
inputSetFiles bidi = effIO bidi.biDiIO . A.inputSetFiles (bidiRun bidi)

-- ###########################################################################
-- ########################### Network Commands ##############################
-- ###########################################################################

networkAddDataCollector :: (e :> es) => BiDiEnv e -> AddDataCollector -> Eff es AddDataCollectorResult
networkAddDataCollector bidi = effIO bidi.biDiIO . A.networkAddDataCollector (bidiRun bidi)

networkAddIntercept :: (e :> es) => BiDiEnv e -> AddIntercept -> Eff es AddInterceptResult
networkAddIntercept bidi = effIO bidi.biDiIO . A.networkAddIntercept (bidiRun bidi)

networkContinueRequest :: (e :> es) => BiDiEnv e -> ContinueRequest -> Eff es ()
networkContinueRequest bidi = effIO bidi.biDiIO . A.networkContinueRequest (bidiRun bidi)

networkContinueResponse :: (e :> es) => BiDiEnv e -> ContinueResponse -> Eff es ()
networkContinueResponse bidi = effIO bidi.biDiIO . A.networkContinueResponse (bidiRun bidi)

networkContinueWithAuth :: (e :> es) => BiDiEnv e -> ContinueWithAuth -> Eff es ()
networkContinueWithAuth bidi = effIO bidi.biDiIO . A.networkContinueWithAuth (bidiRun bidi)

networkDisownData :: (e :> es) => BiDiEnv e -> DisownData -> Eff es ()
networkDisownData bidi = effIO bidi.biDiIO . A.networkDisownData (bidiRun bidi)

networkFailRequest :: (e :> es) => BiDiEnv e -> FailRequest -> Eff es ()
networkFailRequest bidi = effIO bidi.biDiIO . A.networkFailRequest (bidiRun bidi)

networkGetData :: (e :> es) => BiDiEnv e -> GetData -> Eff es GetDataResult
networkGetData bidi = effIO bidi.biDiIO . A.networkGetData (bidiRun bidi)

networkProvideResponse :: (e :> es) => BiDiEnv e -> ProvideResponse -> Eff es ()
networkProvideResponse bidi = effIO bidi.biDiIO . A.networkProvideResponse (bidiRun bidi)

networkRemoveDataCollector :: (e :> es) => BiDiEnv e -> RemoveDataCollector -> Eff es ()
networkRemoveDataCollector bidi = effIO bidi.biDiIO . A.networkRemoveDataCollector (bidiRun bidi)

networkRemoveIntercept :: (e :> es) => BiDiEnv e -> RemoveIntercept -> Eff es ()
networkRemoveIntercept bidi = effIO bidi.biDiIO . A.networkRemoveIntercept (bidiRun bidi)

networkSetCacheBehavior :: (e :> es) => BiDiEnv e -> SetCacheBehavior -> Eff es ()
networkSetCacheBehavior bidi = effIO bidi.biDiIO . A.networkSetCacheBehavior (bidiRun bidi)

networkSetExtraHeaders :: (e :> es) => BiDiEnv e -> SetExtraHeaders -> Eff es ()
networkSetExtraHeaders bidi = effIO bidi.biDiIO . A.networkSetExtraHeaders (bidiRun bidi)

-- ###########################################################################
-- ########################### Script Commands ###############################
-- ###########################################################################

scriptAddPreloadScript :: (e :> es) => BiDiEnv e -> AddPreloadScript -> Eff es AddPreloadScriptResult
scriptAddPreloadScript bidi = effIO bidi.biDiIO . A.scriptAddPreloadScript (bidiRun bidi)

scriptCallFunction :: (e :> es) => BiDiEnv e -> CallFunction -> Eff es EvaluateResult
scriptCallFunction bidi = effIO bidi.biDiIO . A.scriptCallFunction (bidiRun bidi)

scriptDisown :: (e :> es) => BiDiEnv e -> Disown -> Eff es ()
scriptDisown bidi = effIO bidi.biDiIO . A.scriptDisown (bidiRun bidi)

scriptEvaluate :: (e :> es) => BiDiEnv e -> Evaluate -> Eff es EvaluateResult
scriptEvaluate bidi = effIO bidi.biDiIO . A.scriptEvaluate (bidiRun bidi)

scriptGetRealms :: (e :> es) => BiDiEnv e -> GetRealms -> Eff es GetRealmsResult
scriptGetRealms bidi = effIO bidi.biDiIO . A.scriptGetRealms (bidiRun bidi)

scriptRemovePreloadScript :: (e :> es) => BiDiEnv e -> RemovePreloadScript -> Eff es ()
scriptRemovePreloadScript bidi = effIO bidi.biDiIO . A.scriptRemovePreloadScript (bidiRun bidi)

-- ###########################################################################
-- ########################### Storage Commands ##############################
-- ###########################################################################

storageDeleteCookies :: (e :> es) => BiDiEnv e -> DeleteCookies -> Eff es DeleteCookiesResult
storageDeleteCookies bidi = effIO bidi.biDiIO . A.storageDeleteCookies (bidiRun bidi)

storageGetCookies :: (e :> es) => BiDiEnv e -> GetCookies -> Eff es GetCookiesResult
storageGetCookies bidi = effIO bidi.biDiIO . A.storageGetCookies (bidiRun bidi)

storageSetCookie :: (e :> es) => BiDiEnv e -> SetCookie -> Eff es SetCookieResult
storageSetCookie bidi = effIO bidi.biDiIO . A.storageSetCookie (bidiRun bidi)

-- ###########################################################################
-- ######################### WebExtension Commands ###########################
-- ###########################################################################

webExtensionInstall :: (e :> es) => BiDiEnv e -> WebExtensionInstall -> Eff es WebExtensionResult
webExtensionInstall bidi = effIO bidi.biDiIO . A.webExtensionInstall (bidiRun bidi)

webExtensionUninstall :: (e :> es) => BiDiEnv e -> WebExtensionUninstall -> Eff es ()
webExtensionUninstall bidi = effIO bidi.biDiIO . A.webExtensionUninstall (bidiRun bidi)

-- ###########################################################################
-- ########################## Generic Command ################################
-- ###########################################################################

-- | Send any typed 'Command' through the BiDi runner.
sendCommand :: (e :> es, FromJSON r) => BiDiEnv e -> Command r -> Eff es r
sendCommand = run

-- | Send a typed 'Command' using a specific message ID.
sendCommand' :: (e :> es, FromJSON r) => BiDiEnv e -> JSUInt -> Command r -> Eff es r
sendCommand' bidi msgId cmd =
  effIO bidi.biDiIO $ (bidiRunWithId bidi) msgId cmd

-- | Send a typed 'Command' without waiting for a response.
sendCommandNoWait :: (e :> es) => BiDiEnv e -> Command r -> Eff es Request
sendCommandNoWait bidi cmd =
  effIO bidi.biDiIO $ Runner.runNoWait bidi.biDiRunner cmd

-- | Send an off-spec command with an explicit message ID.
sendOffSpecCommand' :: (e :> es) => BiDiEnv e -> JSUInt -> Text -> Object -> Eff es Object
sendOffSpecCommand' bidi msgId method params =
  effIO bidi.biDiIO $ bidi.biDiRunner.runOffSpecWithId msgId method params

-- | Send an off-spec command without waiting for a response.
sendOffSpecCommandNoWait :: (e :> es) => BiDiEnv e -> Text -> Object -> Eff es Request
sendOffSpecCommandNoWait bidi method params =
  effIO bidi.biDiIO $ Runner.runOffSpecNoWait bidi.biDiRunner method params

-- | Evaluate a script expression without waiting for the result.
scriptEvaluateNoWait :: (e :> es) => BiDiEnv e -> Evaluate -> Eff es Request
scriptEvaluateNoWait bidi cmd =
  effIO bidi.biDiIO $ Runner.runNoWait bidi.biDiRunner (mkCommand ScriptEvaluate cmd)

-- ###########################################################################
-- ######################### Subscription Helpers ############################
-- ###########################################################################

-- | Subscribe to multiple known event types (no context filters).
subscribeMany ::
  (e :> es) =>
  BiDiEnv e ->
  [KnownSubscriptionType] ->
  (Event -> IO ()) ->
  Eff es SubscriptionId
subscribeMany bidi = subscribeMany' bidi [] []

-- | Subscribe to multiple known event types with context filters.
subscribeMany' ::
  (e :> es) =>
  BiDiEnv e ->
  [BrowsingContext] ->
  [UserContext] ->
  [KnownSubscriptionType] ->
  (Event -> IO ()) ->
  Eff es SubscriptionId
subscribeMany' bidi bcs ucs sts handler =
  effIO bidi.biDiIO $
    A.subscribeMany' (mkSendSubMany' bidi.biDiRunner) sts bcs ucs handler

-- | Subscribe to unknown / off-spec event types (no context filters).
subscribeUnknownMany ::
  (e :> es) =>
  BiDiEnv e ->
  [OffSpecSubscriptionType] ->
  (Value -> IO ()) ->
  Eff es SubscriptionId
subscribeUnknownMany bidi = subscribeUnknownMany' bidi [] []

-- | Subscribe to unknown / off-spec event types with context filters.
subscribeUnknownMany' ::
  (e :> es) =>
  BiDiEnv e ->
  [BrowsingContext] ->
  [UserContext] ->
  [OffSpecSubscriptionType] ->
  (Value -> IO ()) ->
  Eff es SubscriptionId
subscribeUnknownMany' bidi bcs ucs sts handler =
  effIO bidi.biDiIO $
    A.subscribeOffSpecMany' (mkSendSubOffSpecMany' bidi.biDiRunner) sts bcs ucs handler

-- | Unsubscribe using a previously obtained 'SubscriptionId'.
unsubscribe :: (e :> es) => BiDiEnv e -> SubscriptionId -> Eff es ()
unsubscribe bidi subId =
  effIO bidi.biDiIO $
    Runner.unsubscribe
      bidi.biDiRunner.socketActions
      ((bidiRun bidi) . A.sessionUnsubscribe)
      (UnsubscribeById [subId])

-- ###########################################################################
-- ######################### Log Subscriptions ###############################
-- ###########################################################################

subscribeLogEntryAdded :: (e :> es) => BiDiEnv e -> (LogEntry -> IO ()) -> Eff es SubscriptionId
subscribeLogEntryAdded bidi = viaSub A.subscribeLogEntryAdded bidi

subscribeLogEntryAdded' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (LogEntry -> IO ()) -> Eff es SubscriptionId
subscribeLogEntryAdded' bidi = viaSub' A.subscribeLogEntryAdded' bidi

-- ###########################################################################
-- ################### BrowsingContext Subscriptions #########################
-- ###########################################################################

subscribeBrowsingContextCreated :: (e :> es) => BiDiEnv e -> (Info -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextCreated bidi = viaSub A.subscribeBrowsingContextCreated bidi

subscribeBrowsingContextCreated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextCreated' bidi = viaSub' A.subscribeBrowsingContextCreated' bidi

subscribeBrowsingContextDestroyed :: (e :> es) => BiDiEnv e -> (Info -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDestroyed bidi = viaSub A.subscribeBrowsingContextDestroyed bidi

subscribeBrowsingContextDestroyed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDestroyed' bidi = viaSub' A.subscribeBrowsingContextDestroyed' bidi

subscribeBrowsingContextNavigationStarted :: (e :> es) => BiDiEnv e -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationStarted bidi = viaSub A.subscribeBrowsingContextNavigationStarted bidi

subscribeBrowsingContextNavigationStarted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationStarted' bidi = viaSub' A.subscribeBrowsingContextNavigationStarted' bidi

subscribeBrowsingContextFragmentNavigated :: (e :> es) => BiDiEnv e -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextFragmentNavigated bidi = viaSub A.subscribeBrowsingContextFragmentNavigated bidi

subscribeBrowsingContextFragmentNavigated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextFragmentNavigated' bidi = viaSub' A.subscribeBrowsingContextFragmentNavigated' bidi

subscribeBrowsingContextHistoryUpdated :: (e :> es) => BiDiEnv e -> (HistoryUpdated -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextHistoryUpdated bidi = viaSub A.subscribeBrowsingContextHistoryUpdated bidi

subscribeBrowsingContextHistoryUpdated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextHistoryUpdated' bidi = viaSub' A.subscribeBrowsingContextHistoryUpdated' bidi

subscribeBrowsingContextDomContentLoaded :: (e :> es) => BiDiEnv e -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDomContentLoaded bidi = viaSub A.subscribeBrowsingContextDomContentLoaded bidi

subscribeBrowsingContextDomContentLoaded' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDomContentLoaded' bidi = viaSub' A.subscribeBrowsingContextDomContentLoaded' bidi

subscribeBrowsingContextLoad :: (e :> es) => BiDiEnv e -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextLoad bidi = viaSub A.subscribeBrowsingContextLoad bidi

subscribeBrowsingContextLoad' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextLoad' bidi = viaSub' A.subscribeBrowsingContextLoad' bidi

subscribeBrowsingContextDownloadWillBegin :: (e :> es) => BiDiEnv e -> (DownloadWillBegin -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadWillBegin bidi = viaSub A.subscribeBrowsingContextDownloadWillBegin bidi

subscribeBrowsingContextDownloadWillBegin' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadWillBegin' bidi = viaSub' A.subscribeBrowsingContextDownloadWillBegin' bidi

subscribeBrowsingContextDownloadEnd :: (e :> es) => BiDiEnv e -> (DownloadEnd -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadEnd bidi = viaSub A.subscribeBrowsingContextDownloadEnd bidi

subscribeBrowsingContextDownloadEnd' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (DownloadEnd -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadEnd' bidi = viaSub' A.subscribeBrowsingContextDownloadEnd' bidi

subscribeBrowsingContextNavigationAborted :: (e :> es) => BiDiEnv e -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationAborted bidi = viaSub A.subscribeBrowsingContextNavigationAborted bidi

subscribeBrowsingContextNavigationAborted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationAborted' bidi = viaSub' A.subscribeBrowsingContextNavigationAborted' bidi

subscribeBrowsingContextNavigationCommitted :: (e :> es) => BiDiEnv e -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationCommitted bidi = viaSub A.subscribeBrowsingContextNavigationCommitted bidi

subscribeBrowsingContextNavigationCommitted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationCommitted' bidi = viaSub' A.subscribeBrowsingContextNavigationCommitted' bidi

subscribeBrowsingContextNavigationFailed :: (e :> es) => BiDiEnv e -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationFailed bidi = viaSub A.subscribeBrowsingContextNavigationFailed bidi

subscribeBrowsingContextNavigationFailed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationFailed' bidi = viaSub' A.subscribeBrowsingContextNavigationFailed' bidi

subscribeBrowsingContextUserPromptClosed :: (e :> es) => BiDiEnv e -> (UserPromptClosed -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptClosed bidi = viaSub A.subscribeBrowsingContextUserPromptClosed bidi

subscribeBrowsingContextUserPromptClosed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptClosed' bidi = viaSub' A.subscribeBrowsingContextUserPromptClosed' bidi

subscribeBrowsingContextUserPromptOpened :: (e :> es) => BiDiEnv e -> (UserPromptOpened -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptOpened bidi = viaSub A.subscribeBrowsingContextUserPromptOpened bidi

subscribeBrowsingContextUserPromptOpened' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> IO ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptOpened' bidi = viaSub' A.subscribeBrowsingContextUserPromptOpened' bidi

-- ###########################################################################
-- ####################### Network Subscriptions #############################
-- ###########################################################################

subscribeNetworkAuthRequired :: (e :> es) => BiDiEnv e -> (AuthRequired -> IO ()) -> Eff es SubscriptionId
subscribeNetworkAuthRequired bidi = viaSub A.subscribeNetworkAuthRequired bidi

subscribeNetworkAuthRequired' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (AuthRequired -> IO ()) -> Eff es SubscriptionId
subscribeNetworkAuthRequired' bidi = viaSub' A.subscribeNetworkAuthRequired' bidi

subscribeNetworkBeforeRequestSent :: (e :> es) => BiDiEnv e -> (BeforeRequestSent -> IO ()) -> Eff es SubscriptionId
subscribeNetworkBeforeRequestSent bidi = viaSub A.subscribeNetworkBeforeRequestSent bidi

subscribeNetworkBeforeRequestSent' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> IO ()) -> Eff es SubscriptionId
subscribeNetworkBeforeRequestSent' bidi = viaSub' A.subscribeNetworkBeforeRequestSent' bidi

subscribeNetworkFetchError :: (e :> es) => BiDiEnv e -> (FetchError -> IO ()) -> Eff es SubscriptionId
subscribeNetworkFetchError bidi = viaSub A.subscribeNetworkFetchError bidi

subscribeNetworkFetchError' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (FetchError -> IO ()) -> Eff es SubscriptionId
subscribeNetworkFetchError' bidi = viaSub' A.subscribeNetworkFetchError' bidi

subscribeNetworkResponseCompleted :: (e :> es) => BiDiEnv e -> (ResponseCompleted -> IO ()) -> Eff es SubscriptionId
subscribeNetworkResponseCompleted bidi = viaSub A.subscribeNetworkResponseCompleted bidi

subscribeNetworkResponseCompleted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> IO ()) -> Eff es SubscriptionId
subscribeNetworkResponseCompleted' bidi = viaSub' A.subscribeNetworkResponseCompleted' bidi

subscribeNetworkResponseStarted :: (e :> es) => BiDiEnv e -> (ResponseStarted -> IO ()) -> Eff es SubscriptionId
subscribeNetworkResponseStarted bidi = viaSub A.subscribeNetworkResponseStarted bidi

subscribeNetworkResponseStarted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (ResponseStarted -> IO ()) -> Eff es SubscriptionId
subscribeNetworkResponseStarted' bidi = viaSub' A.subscribeNetworkResponseStarted' bidi

-- ###########################################################################
-- ####################### Script Subscriptions ##############################
-- ###########################################################################

subscribeScriptMessage :: (e :> es) => BiDiEnv e -> (Message -> IO ()) -> Eff es SubscriptionId
subscribeScriptMessage bidi = viaSub A.subscribeScriptMessage bidi

subscribeScriptMessage' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (Message -> IO ()) -> Eff es SubscriptionId
subscribeScriptMessage' bidi = viaSub' A.subscribeScriptMessage' bidi

subscribeScriptRealmCreated :: (e :> es) => BiDiEnv e -> (RealmInfo -> IO ()) -> Eff es SubscriptionId
subscribeScriptRealmCreated bidi = viaSub A.subscribeScriptRealmCreated bidi

subscribeScriptRealmCreated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (RealmInfo -> IO ()) -> Eff es SubscriptionId
subscribeScriptRealmCreated' bidi = viaSub' A.subscribeScriptRealmCreated' bidi

subscribeScriptRealmDestroyed :: (e :> es) => BiDiEnv e -> (RealmDestroyed -> IO ()) -> Eff es SubscriptionId
subscribeScriptRealmDestroyed bidi = viaSub A.subscribeScriptRealmDestroyed bidi

subscribeScriptRealmDestroyed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> IO ()) -> Eff es SubscriptionId
subscribeScriptRealmDestroyed' bidi = viaSub' A.subscribeScriptRealmDestroyed' bidi

-- ###########################################################################
-- ####################### Input Subscriptions ###############################
-- ###########################################################################

subscribeInputFileDialogOpened :: (e :> es) => BiDiEnv e -> (FileDialogOpened -> IO ()) -> Eff es SubscriptionId
subscribeInputFileDialogOpened bidi = viaSub A.subscribeInputFileDialogOpened bidi

subscribeInputFileDialogOpened' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> IO ()) -> Eff es SubscriptionId
subscribeInputFileDialogOpened' bidi = viaSub' A.subscribeInputFileDialogOpened' bidi
