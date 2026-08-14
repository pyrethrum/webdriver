-- |
-- Module: WebDriver.Bluefin.BiDi.Base.Actions
-- Description: Bluefin-style BiDi WebDriver action functions
--
-- Provides BiDi WebDriver actions for Bluefin.  All functions take an
-- explicit 'BiDiEnv' handle rather than using typeclass constraints.
--
-- Command functions delegate directly to the generic @A.xxx (run bidi)@
-- pattern: the 'run' helper already lifts the @IO@-based runner into 'Eff',
-- so no @effIO@ or @bidiRun@ plumbing is needed at each call site.
--
-- Subscription callbacks are @Eff es@ actions.  The underlying
-- 'BiDiRunner' dispatches events in @IO@, so handlers must be lowered back
-- to @IO@ at some point.  This is done via 'withEffToIO_', which is safe
-- because the enclosing 'withBiDiSession' bracket keeps the effect
-- environment alive.
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
    subscribeOffSpecMany,
    subscribeOffSpecMany',

    -- * Unsubscribe
    unsubscribe,
  )
where

import Data.Aeson (FromJSON, Object, Value)
import Data.Text (Text)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (effIO, withEffToIO_)
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
--
-- Handlers are lowered from @Eff es ()@ to @IO ()@ via 'withEffToIO_',
-- which is safe because the enclosing 'withBiDiSession' bracket keeps the
-- effect environment alive.
viaSub ::
  (e :> es) =>
  (A.SendSub IO a -> (a -> IO ()) -> IO SubscriptionId) ->
  BiDiEnv e ->
  (a -> Eff es ()) ->
  Eff es SubscriptionId
viaSub extFn bidi handler =
  withEffToIO_ bidi.biDiIO $ \toIO ->
    extFn (mkSendSub bidi.biDiRunner) (\a -> toIO (handler a))

-- | Subscribe with context filters, lifting result into 'Eff'.
--
-- Handlers are lowered from @Eff es ()@ to @IO ()@ via 'withEffToIO_'.
viaSub' ::
  (e :> es) =>
  (A.SendSub' IO a -> [BrowsingContext] -> [UserContext] -> (a -> IO ()) -> IO SubscriptionId) ->
  BiDiEnv e ->
  [BrowsingContext] ->
  [UserContext] ->
  (a -> Eff es ()) ->
  Eff es SubscriptionId
viaSub' extFn bidi bcs ucs handler =
  withEffToIO_ bidi.biDiIO $ \toIO ->
    extFn (mkSendSub' bidi.biDiRunner) bcs ucs (\a -> toIO (handler a))

-- ###########################################################################
-- ########################### Session Commands ##############################
-- ###########################################################################

sessionNew :: (e :> es) => BiDiEnv e -> Capabilities -> Eff es SessionNewResult
sessionNew bidi = A.sessionNew (run bidi)

sessionStatus :: (e :> es) => BiDiEnv e -> Eff es SessionStatusResult
sessionStatus bidi = A.sessionStatus (run bidi)

sessionEnd :: (e :> es) => BiDiEnv e -> Eff es ()
sessionEnd bidi = A.sessionEnd (run bidi)

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
browsingContextActivate bidi = A.browsingContextActivate (run bidi)

browsingContextCaptureScreenshot :: (e :> es) => BiDiEnv e -> CaptureScreenshot -> Eff es CaptureScreenshotResult
browsingContextCaptureScreenshot bidi = A.browsingContextCaptureScreenshot (run bidi)

browsingContextClose :: (e :> es) => BiDiEnv e -> Close -> Eff es ()
browsingContextClose bidi = A.browsingContextClose (run bidi)

browsingContextCreate :: (e :> es) => BiDiEnv e -> Create -> Eff es BrowsingContext
browsingContextCreate bidi = A.browsingContextCreate (run bidi)

browsingContextGetTree :: (e :> es) => BiDiEnv e -> GetTree -> Eff es GetTreeResult
browsingContextGetTree bidi = A.browsingContextGetTree (run bidi)

browsingContextHandleUserPrompt :: (e :> es) => BiDiEnv e -> HandleUserPrompt -> Eff es ()
browsingContextHandleUserPrompt bidi = A.browsingContextHandleUserPrompt (run bidi)

browsingContextLocateNodes :: (e :> es) => BiDiEnv e -> LocateNodes -> Eff es LocateNodesResult
browsingContextLocateNodes bidi = A.browsingContextLocateNodes (run bidi)

browsingContextNavigate :: (e :> es) => BiDiEnv e -> Navigate -> Eff es NavigateResult
browsingContextNavigate bidi = A.browsingContextNavigate (run bidi)

browsingContextPrint :: (e :> es) => BiDiEnv e -> Print -> Eff es PrintResult
browsingContextPrint bidi = A.browsingContextPrint (run bidi)

browsingContextReload :: (e :> es) => BiDiEnv e -> Reload -> Eff es ()
browsingContextReload bidi = A.browsingContextReload (run bidi)

browsingContextSetViewport :: (e :> es) => BiDiEnv e -> SetViewport -> Eff es ()
browsingContextSetViewport bidi = A.browsingContextSetViewport (run bidi)

browsingContextTraverseHistory :: (e :> es) => BiDiEnv e -> TraverseHistory -> Eff es ()
browsingContextTraverseHistory bidi = A.browsingContextTraverseHistory (run bidi)

-- ###########################################################################
-- ########################### Browser Commands ##############################
-- ###########################################################################

browserClose :: (e :> es) => BiDiEnv e -> Eff es ()
browserClose bidi = A.browserClose (run bidi)

browserCreateUserContext :: (e :> es) => BiDiEnv e -> CreateUserContext -> Eff es UserContext
browserCreateUserContext bidi = A.browserCreateUserContext (run bidi)

browserGetClientWindows :: (e :> es) => BiDiEnv e -> Eff es GetClientWindowsResult
browserGetClientWindows bidi = A.browserGetClientWindows (run bidi)

browserGetUserContexts :: (e :> es) => BiDiEnv e -> Eff es GetUserContextsResult
browserGetUserContexts bidi = A.browserGetUserContexts (run bidi)

browserRemoveUserContext :: (e :> es) => BiDiEnv e -> RemoveUserContext -> Eff es ()
browserRemoveUserContext bidi = A.browserRemoveUserContext (run bidi)

browserSetClientWindowState :: (e :> es) => BiDiEnv e -> SetClientWindowState -> Eff es ClientWindowInfo
browserSetClientWindowState bidi = A.browserSetClientWindowState (run bidi)

browserSetDownloadBehavior :: (e :> es) => BiDiEnv e -> SetDownloadBehavior -> Eff es ()
browserSetDownloadBehavior bidi = A.browserSetDownloadBehavior (run bidi)

-- ###########################################################################
-- ########################## Emulation Commands #############################
-- ###########################################################################

emulationSetForcedColorsModeThemeOverride :: (e :> es) => BiDiEnv e -> SetForcedColorsModeThemeOverride -> Eff es ()
emulationSetForcedColorsModeThemeOverride bidi = A.emulationSetForcedColorsModeThemeOverride (run bidi)

emulationSetGeolocationOverride :: (e :> es) => BiDiEnv e -> SetGeolocationOverride -> Eff es ()
emulationSetGeolocationOverride bidi = A.emulationSetGeolocationOverride (run bidi)

emulationSetLocaleOverride :: (e :> es) => BiDiEnv e -> SetLocaleOverride -> Eff es ()
emulationSetLocaleOverride bidi = A.emulationSetLocaleOverride (run bidi)

emulationSetNetworkConditions :: (e :> es) => BiDiEnv e -> SetNetworkConditions -> Eff es ()
emulationSetNetworkConditions bidi = A.emulationSetNetworkConditions (run bidi)

emulationSetScreenOrientationOverride :: (e :> es) => BiDiEnv e -> SetScreenOrientationOverride -> Eff es ()
emulationSetScreenOrientationOverride bidi = A.emulationSetScreenOrientationOverride (run bidi)

emulationSetScreenSettingsOverride :: (e :> es) => BiDiEnv e -> SetScreenSettingsOverride -> Eff es ()
emulationSetScreenSettingsOverride bidi = A.emulationSetScreenSettingsOverride (run bidi)

emulationSetScriptingEnabled :: (e :> es) => BiDiEnv e -> SetScriptingEnabled -> Eff es ()
emulationSetScriptingEnabled bidi = A.emulationSetScriptingEnabled (run bidi)

emulationSetTimezoneOverride :: (e :> es) => BiDiEnv e -> SetTimezoneOverride -> Eff es ()
emulationSetTimezoneOverride bidi = A.emulationSetTimezoneOverride (run bidi)

emulationSetTouchOverride :: (e :> es) => BiDiEnv e -> SetTouchOverride -> Eff es ()
emulationSetTouchOverride bidi = A.emulationSetTouchOverride (run bidi)

emulationSetUserAgentOverride :: (e :> es) => BiDiEnv e -> SetUserAgentOverride -> Eff es ()
emulationSetUserAgentOverride bidi = A.emulationSetUserAgentOverride (run bidi)

-- ###########################################################################
-- ############################ Input Commands ###############################
-- ###########################################################################

inputPerformActions :: (e :> es) => BiDiEnv e -> PerformActions -> Eff es ()
inputPerformActions bidi = A.inputPerformActions (run bidi)

inputReleaseActions :: (e :> es) => BiDiEnv e -> ReleaseActions -> Eff es ()
inputReleaseActions bidi = A.inputReleaseActions (run bidi)

inputSetFiles :: (e :> es) => BiDiEnv e -> SetFiles -> Eff es ()
inputSetFiles bidi = A.inputSetFiles (run bidi)

-- ###########################################################################
-- ########################### Network Commands ##############################
-- ###########################################################################

networkAddDataCollector :: (e :> es) => BiDiEnv e -> AddDataCollector -> Eff es AddDataCollectorResult
networkAddDataCollector bidi = A.networkAddDataCollector (run bidi)

networkAddIntercept :: (e :> es) => BiDiEnv e -> AddIntercept -> Eff es AddInterceptResult
networkAddIntercept bidi = A.networkAddIntercept (run bidi)

networkContinueRequest :: (e :> es) => BiDiEnv e -> ContinueRequest -> Eff es ()
networkContinueRequest bidi = A.networkContinueRequest (run bidi)

networkContinueResponse :: (e :> es) => BiDiEnv e -> ContinueResponse -> Eff es ()
networkContinueResponse bidi = A.networkContinueResponse (run bidi)

networkContinueWithAuth :: (e :> es) => BiDiEnv e -> ContinueWithAuth -> Eff es ()
networkContinueWithAuth bidi = A.networkContinueWithAuth (run bidi)

networkDisownData :: (e :> es) => BiDiEnv e -> DisownData -> Eff es ()
networkDisownData bidi = A.networkDisownData (run bidi)

networkFailRequest :: (e :> es) => BiDiEnv e -> FailRequest -> Eff es ()
networkFailRequest bidi = A.networkFailRequest (run bidi)

networkGetData :: (e :> es) => BiDiEnv e -> GetData -> Eff es GetDataResult
networkGetData bidi = A.networkGetData (run bidi)

networkProvideResponse :: (e :> es) => BiDiEnv e -> ProvideResponse -> Eff es ()
networkProvideResponse bidi = A.networkProvideResponse (run bidi)

networkRemoveDataCollector :: (e :> es) => BiDiEnv e -> RemoveDataCollector -> Eff es ()
networkRemoveDataCollector bidi = A.networkRemoveDataCollector (run bidi)

networkRemoveIntercept :: (e :> es) => BiDiEnv e -> RemoveIntercept -> Eff es ()
networkRemoveIntercept bidi = A.networkRemoveIntercept (run bidi)

networkSetCacheBehavior :: (e :> es) => BiDiEnv e -> SetCacheBehavior -> Eff es ()
networkSetCacheBehavior bidi = A.networkSetCacheBehavior (run bidi)

networkSetExtraHeaders :: (e :> es) => BiDiEnv e -> SetExtraHeaders -> Eff es ()
networkSetExtraHeaders bidi = A.networkSetExtraHeaders (run bidi)

-- ###########################################################################
-- ########################### Script Commands ###############################
-- ###########################################################################

scriptAddPreloadScript :: (e :> es) => BiDiEnv e -> AddPreloadScript -> Eff es AddPreloadScriptResult
scriptAddPreloadScript bidi = A.scriptAddPreloadScript (run bidi)

scriptCallFunction :: (e :> es) => BiDiEnv e -> CallFunction -> Eff es EvaluateResult
scriptCallFunction bidi = A.scriptCallFunction (run bidi)

scriptDisown :: (e :> es) => BiDiEnv e -> Disown -> Eff es ()
scriptDisown bidi = A.scriptDisown (run bidi)

scriptEvaluate :: (e :> es) => BiDiEnv e -> Evaluate -> Eff es EvaluateResult
scriptEvaluate bidi = A.scriptEvaluate (run bidi)

scriptGetRealms :: (e :> es) => BiDiEnv e -> GetRealms -> Eff es GetRealmsResult
scriptGetRealms bidi = A.scriptGetRealms (run bidi)

scriptRemovePreloadScript :: (e :> es) => BiDiEnv e -> RemovePreloadScript -> Eff es ()
scriptRemovePreloadScript bidi = A.scriptRemovePreloadScript (run bidi)

-- ###########################################################################
-- ########################### Storage Commands ##############################
-- ###########################################################################

storageDeleteCookies :: (e :> es) => BiDiEnv e -> DeleteCookies -> Eff es DeleteCookiesResult
storageDeleteCookies bidi = A.storageDeleteCookies (run bidi)

storageGetCookies :: (e :> es) => BiDiEnv e -> GetCookies -> Eff es GetCookiesResult
storageGetCookies bidi = A.storageGetCookies (run bidi)

storageSetCookie :: (e :> es) => BiDiEnv e -> SetCookie -> Eff es SetCookieResult
storageSetCookie bidi = A.storageSetCookie (run bidi)

-- ###########################################################################
-- ######################### WebExtension Commands ###########################
-- ###########################################################################

webExtensionInstall :: (e :> es) => BiDiEnv e -> WebExtensionInstall -> Eff es WebExtensionResult
webExtensionInstall bidi = A.webExtensionInstall (run bidi)

webExtensionUninstall :: (e :> es) => BiDiEnv e -> WebExtensionUninstall -> Eff es ()
webExtensionUninstall bidi = A.webExtensionUninstall (run bidi)

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
  (Event -> Eff es ()) ->
  Eff es SubscriptionId
subscribeMany bidi = subscribeMany' bidi [] []

-- | Subscribe to multiple known event types with context filters.
subscribeMany' ::
  (e :> es) =>
  BiDiEnv e ->
  [BrowsingContext] ->
  [UserContext] ->
  [KnownSubscriptionType] ->
  (Event -> Eff es ()) ->
  Eff es SubscriptionId
subscribeMany' bidi bcs ucs sts handler =
  withEffToIO_ bidi.biDiIO $ \toIO ->
    A.subscribeMany' (mkSendSubMany' bidi.biDiRunner) sts bcs ucs (\a -> toIO (handler a))

-- | Subscribe to unknown / off-spec event types (no context filters).
subscribeOffSpecMany ::
  (e :> es) =>
  BiDiEnv e ->
  [OffSpecSubscriptionType] ->
  (Value -> Eff es ()) ->
  Eff es SubscriptionId
subscribeOffSpecMany bidi = subscribeOffSpecMany' bidi [] []

-- | Subscribe to unknown / off-spec event types with context filters.
subscribeOffSpecMany' ::
  (e :> es) =>
  BiDiEnv e ->
  [BrowsingContext] ->
  [UserContext] ->
  [OffSpecSubscriptionType] ->
  (Value -> Eff es ()) ->
  Eff es SubscriptionId
subscribeOffSpecMany' bidi bcs ucs sts handler =
  withEffToIO_ bidi.biDiIO $ \toIO ->
    A.subscribeOffSpecMany' (mkSendSubOffSpecMany' bidi.biDiRunner) sts bcs ucs (\a -> toIO (handler a))

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

subscribeLogEntryAdded :: (e :> es) => BiDiEnv e -> (LogEntry -> Eff es ()) -> Eff es SubscriptionId
subscribeLogEntryAdded bidi = viaSub A.subscribeLogEntryAdded bidi

subscribeLogEntryAdded' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (LogEntry -> Eff es ()) -> Eff es SubscriptionId
subscribeLogEntryAdded' bidi = viaSub' A.subscribeLogEntryAdded' bidi

-- ###########################################################################
-- ################### BrowsingContext Subscriptions #########################
-- ###########################################################################

subscribeBrowsingContextCreated :: (e :> es) => BiDiEnv e -> (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextCreated bidi = viaSub A.subscribeBrowsingContextCreated bidi

subscribeBrowsingContextCreated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextCreated' bidi = viaSub' A.subscribeBrowsingContextCreated' bidi

subscribeBrowsingContextDestroyed :: (e :> es) => BiDiEnv e -> (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDestroyed bidi = viaSub A.subscribeBrowsingContextDestroyed bidi

subscribeBrowsingContextDestroyed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (Info -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDestroyed' bidi = viaSub' A.subscribeBrowsingContextDestroyed' bidi

subscribeBrowsingContextNavigationStarted :: (e :> es) => BiDiEnv e -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationStarted bidi = viaSub A.subscribeBrowsingContextNavigationStarted bidi

subscribeBrowsingContextNavigationStarted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationStarted' bidi = viaSub' A.subscribeBrowsingContextNavigationStarted' bidi

subscribeBrowsingContextFragmentNavigated :: (e :> es) => BiDiEnv e -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextFragmentNavigated bidi = viaSub A.subscribeBrowsingContextFragmentNavigated bidi

subscribeBrowsingContextFragmentNavigated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextFragmentNavigated' bidi = viaSub' A.subscribeBrowsingContextFragmentNavigated' bidi

subscribeBrowsingContextHistoryUpdated :: (e :> es) => BiDiEnv e -> (HistoryUpdated -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextHistoryUpdated bidi = viaSub A.subscribeBrowsingContextHistoryUpdated bidi

subscribeBrowsingContextHistoryUpdated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextHistoryUpdated' bidi = viaSub' A.subscribeBrowsingContextHistoryUpdated' bidi

subscribeBrowsingContextDomContentLoaded :: (e :> es) => BiDiEnv e -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDomContentLoaded bidi = viaSub A.subscribeBrowsingContextDomContentLoaded bidi

subscribeBrowsingContextDomContentLoaded' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDomContentLoaded' bidi = viaSub' A.subscribeBrowsingContextDomContentLoaded' bidi

subscribeBrowsingContextLoad :: (e :> es) => BiDiEnv e -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextLoad bidi = viaSub A.subscribeBrowsingContextLoad bidi

subscribeBrowsingContextLoad' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextLoad' bidi = viaSub' A.subscribeBrowsingContextLoad' bidi

subscribeBrowsingContextDownloadWillBegin :: (e :> es) => BiDiEnv e -> (DownloadWillBegin -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadWillBegin bidi = viaSub A.subscribeBrowsingContextDownloadWillBegin bidi

subscribeBrowsingContextDownloadWillBegin' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadWillBegin' bidi = viaSub' A.subscribeBrowsingContextDownloadWillBegin' bidi

subscribeBrowsingContextDownloadEnd :: (e :> es) => BiDiEnv e -> (DownloadEnd -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadEnd bidi = viaSub A.subscribeBrowsingContextDownloadEnd bidi

subscribeBrowsingContextDownloadEnd' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (DownloadEnd -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextDownloadEnd' bidi = viaSub' A.subscribeBrowsingContextDownloadEnd' bidi

subscribeBrowsingContextNavigationAborted :: (e :> es) => BiDiEnv e -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationAborted bidi = viaSub A.subscribeBrowsingContextNavigationAborted bidi

subscribeBrowsingContextNavigationAborted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationAborted' bidi = viaSub' A.subscribeBrowsingContextNavigationAborted' bidi

subscribeBrowsingContextNavigationCommitted :: (e :> es) => BiDiEnv e -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationCommitted bidi = viaSub A.subscribeBrowsingContextNavigationCommitted bidi

subscribeBrowsingContextNavigationCommitted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationCommitted' bidi = viaSub' A.subscribeBrowsingContextNavigationCommitted' bidi

subscribeBrowsingContextNavigationFailed :: (e :> es) => BiDiEnv e -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationFailed bidi = viaSub A.subscribeBrowsingContextNavigationFailed bidi

subscribeBrowsingContextNavigationFailed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextNavigationFailed' bidi = viaSub' A.subscribeBrowsingContextNavigationFailed' bidi

subscribeBrowsingContextUserPromptClosed :: (e :> es) => BiDiEnv e -> (UserPromptClosed -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptClosed bidi = viaSub A.subscribeBrowsingContextUserPromptClosed bidi

subscribeBrowsingContextUserPromptClosed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptClosed' bidi = viaSub' A.subscribeBrowsingContextUserPromptClosed' bidi

subscribeBrowsingContextUserPromptOpened :: (e :> es) => BiDiEnv e -> (UserPromptOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptOpened bidi = viaSub A.subscribeBrowsingContextUserPromptOpened bidi

subscribeBrowsingContextUserPromptOpened' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeBrowsingContextUserPromptOpened' bidi = viaSub' A.subscribeBrowsingContextUserPromptOpened' bidi

-- ###########################################################################
-- ####################### Network Subscriptions #############################
-- ###########################################################################

subscribeNetworkAuthRequired :: (e :> es) => BiDiEnv e -> (AuthRequired -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkAuthRequired bidi = viaSub A.subscribeNetworkAuthRequired bidi

subscribeNetworkAuthRequired' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (AuthRequired -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkAuthRequired' bidi = viaSub' A.subscribeNetworkAuthRequired' bidi

subscribeNetworkBeforeRequestSent :: (e :> es) => BiDiEnv e -> (BeforeRequestSent -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkBeforeRequestSent bidi = viaSub A.subscribeNetworkBeforeRequestSent bidi

subscribeNetworkBeforeRequestSent' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkBeforeRequestSent' bidi = viaSub' A.subscribeNetworkBeforeRequestSent' bidi

subscribeNetworkFetchError :: (e :> es) => BiDiEnv e -> (FetchError -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkFetchError bidi = viaSub A.subscribeNetworkFetchError bidi

subscribeNetworkFetchError' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (FetchError -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkFetchError' bidi = viaSub' A.subscribeNetworkFetchError' bidi

subscribeNetworkResponseCompleted :: (e :> es) => BiDiEnv e -> (ResponseCompleted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseCompleted bidi = viaSub A.subscribeNetworkResponseCompleted bidi

subscribeNetworkResponseCompleted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseCompleted' bidi = viaSub' A.subscribeNetworkResponseCompleted' bidi

subscribeNetworkResponseStarted :: (e :> es) => BiDiEnv e -> (ResponseStarted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseStarted bidi = viaSub A.subscribeNetworkResponseStarted bidi

subscribeNetworkResponseStarted' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (ResponseStarted -> Eff es ()) -> Eff es SubscriptionId
subscribeNetworkResponseStarted' bidi = viaSub' A.subscribeNetworkResponseStarted' bidi

-- ###########################################################################
-- ####################### Script Subscriptions ##############################
-- ###########################################################################

subscribeScriptMessage :: (e :> es) => BiDiEnv e -> (Message -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptMessage bidi = viaSub A.subscribeScriptMessage bidi

subscribeScriptMessage' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (Message -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptMessage' bidi = viaSub' A.subscribeScriptMessage' bidi

subscribeScriptRealmCreated :: (e :> es) => BiDiEnv e -> (RealmInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmCreated bidi = viaSub A.subscribeScriptRealmCreated bidi

subscribeScriptRealmCreated' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (RealmInfo -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmCreated' bidi = viaSub' A.subscribeScriptRealmCreated' bidi

subscribeScriptRealmDestroyed :: (e :> es) => BiDiEnv e -> (RealmDestroyed -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmDestroyed bidi = viaSub A.subscribeScriptRealmDestroyed bidi

subscribeScriptRealmDestroyed' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> Eff es ()) -> Eff es SubscriptionId
subscribeScriptRealmDestroyed' bidi = viaSub' A.subscribeScriptRealmDestroyed' bidi

-- ###########################################################################
-- ####################### Input Subscriptions ###############################
-- ###########################################################################

subscribeInputFileDialogOpened :: (e :> es) => BiDiEnv e -> (FileDialogOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeInputFileDialogOpened bidi = viaSub A.subscribeInputFileDialogOpened bidi

subscribeInputFileDialogOpened' :: (e :> es) => BiDiEnv e -> [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> Eff es ()) -> Eff es SubscriptionId
subscribeInputFileDialogOpened' bidi = viaSub' A.subscribeInputFileDialogOpened' bidi
