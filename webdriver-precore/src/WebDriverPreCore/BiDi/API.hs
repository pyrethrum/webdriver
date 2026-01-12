{-# LANGUAGE CPP #-}

module WebDriverPreCore.BiDi.API
  ( 
    -- | Type definitions for commands and subscriptions to events defined in the [WebDriver BiDi specification](BiDiSpecURL).
    --
    -- Not all commands and subscriptions will be supported by all [BiDi drivers yet](https://wpt.fyi/results/webdriver/tests/bidi?label=experimental&label=master&aligned), as the specification is [still evolving](https://www.w3.org/standards/history/webdriver-bidi/) rapidly.
    -- 
    -- See the demos in the [demos](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/README.md) for how this module can be used to delvelop a WebDriver client.
    --

    -- * Session Commands
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
    subscribeOffSpecMany,
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
    OffSpecSubscriptionType,
    UserContext,
    UserPromptClosed,
    UserPromptOpened,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
    emptyCommand,
    mkCommand,
    mkMultiSubscription,
    mkSubscription,
    mkOffSpecSubscription,
  )
import WebDriverPreCore.BiDi.Script (Message, RealmInfo)

--- ############## Commands ##############

---- Session ----

-- | Specification Entry: <BiDiSpecURL#command-session-new session.new>
--  
-- This function is not supported by many Bidi drivers yet. To start a BiDi session you need to create an HTTP session with a web socket port specified.
-- Creating a new BiDi session via this command may result in a driver error.
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-new 21 November 2024 - First Public Working Draft>
sessionNew :: Capabilities -> Command SessionNewResult
sessionNew = mkCommand SessionNew

-- | Specification Entry: <BiDiSpecURL#command-session-status session.status>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-status 21 November 2024 - First Public Working Draft>
sessionStatus :: Command SessionStatusResult
sessionStatus = emptyCommand SessionStatus

-- | Specification Entry: <BiDiSpecURL#command-session-end session.end>
--
-- Only sessions created via 'sessionNew' can be ended via this command.
-- If the BiDi session was created by other means (e.g. during HTTP session creation with a web socket port), it needs to be ended by ending the HTTP session.
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-end 21 November 2024 - First Public Working Draft>
sessionEnd :: Command ()
sessionEnd = emptyCommand SessionEnd

-- | Specification Entry: <BiDiSpecURL#command-session-subscribe session.subscribe>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-subscribe 21 November 2024 - First Public Working Draft>
sessionSubscribe :: SessionSubscibe -> Command SessionSubscribeResult
sessionSubscribe = mkCommand SessionSubscribe

-- | Specification Entry: <BiDiSpecURL#command-session-unsubscribe session.unsubscribe>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-unsubscribe 21 November 2024 - First Public Working Draft>
sessionUnsubscribe :: SessionUnsubscribe -> Command ()
sessionUnsubscribe = mkCommand SessionUnsubscribe

---- Browsing Context ----

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-activate browsingContext.activate>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-activate 21 November 2024 - First Public Working Draft>
browsingContextActivate :: Activate -> Command ()
browsingContextActivate = mkCommand BrowsingContextActivate

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-captureScreenshot browsingContext.captureScreenshot>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-captureScreenshot 21 November 2024 - First Public Working Draft>
browsingContextCaptureScreenshot :: CaptureScreenshot -> Command CaptureScreenshotResult
browsingContextCaptureScreenshot = mkCommand BrowsingContextCaptureScreenshot

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-close browsingContext.close>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-close 21 November 2024 - First Public Working Draft>
browsingContextClose :: Close -> Command ()
browsingContextClose = mkCommand BrowsingContextClose

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-create browsingContext.create>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-create 21 November 2024 - First Public Working Draft>
browsingContextCreate :: Create -> Command BrowsingContext
browsingContextCreate = mkCommand BrowsingContextCreate

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-getTree browsingContext.getTree>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-getTree 21 November 2024 - First Public Working Draft>
browsingContextGetTree :: GetTree -> Command GetTreeResult
browsingContextGetTree = mkCommand BrowsingContextGetTree

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-handleUserPrompt browsingContext.handleUserPrompt>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-handleUserPrompt 21 November 2024 - First Public Working Draft>
browsingContextHandleUserPrompt :: HandleUserPrompt -> Command ()
browsingContextHandleUserPrompt = mkCommand BrowsingContextHandleUserPrompt

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-locateNodes browsingContext.locateNodes>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-locateNodes 21 November 2024 - First Public Working Draft>
browsingContextLocateNodes :: LocateNodes -> Command LocateNodesResult
browsingContextLocateNodes = mkCommand BrowsingContextLocateNodes

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-navigate browsingContext.navigate>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-navigate 21 November 2024 - First Public Working Draft>
browsingContextNavigate :: Navigate -> Command NavigateResult
browsingContextNavigate = mkCommand BrowsingContextNavigate

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-print browsingContext.print>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-print 21 November 2024 - First Public Working Draft>
browsingContextPrint :: Print -> Command PrintResult
browsingContextPrint = mkCommand BrowsingContextPrint

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-reload browsingContext.reload>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-reload 21 November 2024 - First Public Working Draft>
browsingContextReload :: Reload -> Command ()
browsingContextReload = mkCommand BrowsingContextReload

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-setViewport browsingContext.setViewport>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-setViewport 21 November 2024 - First Public Working Draft>
browsingContextSetViewport :: SetViewport -> Command ()
browsingContextSetViewport = mkCommand BrowsingContextSetViewport

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-traverseHistory browsingContext.traverseHistory>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-traverseHistory 21 November 2024 - First Public Working Draft>
browsingContextTraverseHistory :: TraverseHistory -> Command ()
browsingContextTraverseHistory = mkCommand BrowsingContextTraverseHistory

---- Browser ----

-- | Specification Entry: <BiDiSpecURL#command-browser-close browser.close>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-close 21 November 2024 - First Public Working Draft>
browserClose :: Command ()
browserClose = emptyCommand BrowserClose

-- | Specification Entry: <BiDiSpecURL#command-browser-createUserContext browser.createUserContext>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-createUserContext 21 November 2024 - First Public Working Draft>
browserCreateUserContext :: CreateUserContext -> Command UserContext
browserCreateUserContext = mkCommand BrowserCreateUserContext

-- | Specification Entry: <BiDiSpecURL#command-browser-getClientWindows browser.getClientWindows>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-getClientWindows 21 November 2024 - First Public Working Draft>
browserGetClientWindows :: Command GetClientWindowsResult
browserGetClientWindows = emptyCommand BrowserGetClientWindows

-- | Specification Entry: <BiDiSpecURL#command-browser-getUserContexts browser.getUserContexts>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-getUserContexts 21 November 2024 - First Public Working Draft>
browserGetUserContexts :: Command GetUserContextsResult
browserGetUserContexts = emptyCommand BrowserGetUserContexts

-- | Specification Entry: <BiDiSpecURL#command-browser-removeUserContext browser.removeUserContext>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-removeUserContext 21 November 2024 - First Public Working Draft>
browserRemoveUserContext :: RemoveUserContext -> Command ()
browserRemoveUserContext = mkCommand BrowserRemoveUserContext

-- | Specification Entry: <BiDiSpecURL#command-browser-setClientWindowState browser.setClientWindowState>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-setClientWindowState 21 November 2024 - First Public Working Draft>
browserSetClientWindowState :: SetClientWindowState -> Command ClientWindowInfo
browserSetClientWindowState = mkCommand BrowserSetClientWindowState

-- since 18-09-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250918
-- | Specification Entry: <BiDiSpecURL#command-browser-setDownloadBehavior browser.setDownloadBehavior>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250917/#command-browser-setDownloadBehavior 17 September 2025>
browserSetDownloadBehavior :: SetDownloadBehavior -> Command ()
browserSetDownloadBehavior = mkCommand BrowserSetDownloadBehavior

---- Emulation ----

-- | Specification Entry: <BiDiSpecURL#command-emulation-setForcedColorsModeThemeOverride emulation.setForcedColorsModeThemeOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250729/#command-emulation-setForcedColorsModeThemeOverride 29 July 2025>
emulationSetForcedColorsModeThemeOverride :: SetForcedColorsModeThemeOverride -> Command ()
emulationSetForcedColorsModeThemeOverride = mkCommand EmulationSetForcedColorsModeThemeOverride

-- | Specification Entry: <BiDiSpecURL#command-emulation-setGeolocationOverride emulation.setGeolocationOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250321/#command-emulation-setGeolocationOverride 21 March 2025>
emulationSetGeolocationOverride :: SetGeolocationOverride -> Command ()
emulationSetGeolocationOverride = mkCommand EmulationSetGeolocationOverride

-- | Specification Entry: <BiDiSpecURL#command-emulation-setLocaleOverride emulation.setLocaleOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250626/#command-emulation-setLocaleOverride 26 June 2025>
emulationSetLocaleOverride :: SetLocaleOverride -> Command ()
emulationSetLocaleOverride = mkCommand EmulationSetLocaleOverride

-- since 07-10-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007
-- | Specification Entry: <BiDiSpecURL#command-emulation-setNetworkConditions emulation.setNetworkConditions>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007/#command-emulation-setNetworkConditions 07 October 2025>
emulationSetNetworkConditions :: SetNetworkConditions -> Command ()
emulationSetNetworkConditions = mkCommand EmulationSetNetworkConditions

-- | Specification Entry: <BiDiSpecURL#command-emulation-setScreenOrientationOverride emulation.setScreenOrientationOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250626/#command-emulation-setScreenOrientationOverride 26 June 2025>
emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> Command ()
emulationSetScreenOrientationOverride = mkCommand EmulationSetScreenOrientationOverride

-- since 20-11-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120
-- | Specification Entry: <BiDiSpecURL#command-emulation-setScreenSettingsOverride emulation.setScreenSettingsOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120/#command-emulation-setScreenSettingsOverride 20 November 2025>
emulationSetScreenSettingsOverride :: SetScreenSettingsOverride -> Command ()
emulationSetScreenSettingsOverride = mkCommand EmulationSetScreenSettingsOverride

-- since 11-08-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811
-- | Specification Entry: <BiDiSpecURL#command-emulation-setScriptingEnabled emulation.setScriptingEnabled>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811/#command-emulation-setScriptingEnabled 11 August 2025>
emulationSetScriptingEnabled :: SetScriptingEnabled -> Command ()
emulationSetScriptingEnabled = mkCommand EmulationSetScriptingEnabled

-- | Specification Entry: <BiDiSpecURL#command-emulation-setTimezoneOverride emulation.setTimezoneOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250718/#command-emulation-setTimezoneOverride 18 July 2025>
emulationSetTimezoneOverride :: SetTimezoneOverride -> Command ()
emulationSetTimezoneOverride = mkCommand EmulationSetTimezoneOverride

-- since 10-09-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250910
-- | Specification Entry: <BiDiSpecURL#command-emulation-setUserAgentOverride emulation.setUserAgentOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250910/#command-emulation-setUserAgentOverride 10 September 2025>
emulationSetUserAgentOverride :: SetUserAgentOverride -> Command ()
emulationSetUserAgentOverride = mkCommand EmulationSetUserAgentOverride

---- Input ----

-- | Specification Entry: <BiDiSpecURL#command-input-performActions input.performActions>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-input-performActions 21 November 2024 - First Public Working Draft>
inputPerformActions :: PerformActions -> Command ()
inputPerformActions = mkCommand InputPerformActions

-- | Specification Entry: <BiDiSpecURL#command-input-releaseActions input.releaseActions>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-input-releaseActions 21 November 2024 - First Public Working Draft>
inputReleaseActions :: ReleaseActions -> Command ()
inputReleaseActions = mkCommand InputReleaseActions

-- | Specification Entry: <BiDiSpecURL#command-input-setFiles input.setFiles>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-input-setFiles 21 November 2024 - First Public Working Draft>
inputSetFiles :: SetFiles -> Command ()
inputSetFiles = mkCommand InputSetFiles

---- Network ----

-- | Specification Entry: <BiDiSpecURL#command-network-addDataCollector network.addDataCollector>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-addDataCollector 20 June 2025>
networkAddDataCollector :: AddDataCollector -> Command AddDataCollectorResult
networkAddDataCollector = mkCommand NetworkAddDataCollector

-- | Specification Entry: <BiDiSpecURL#command-network-addIntercept network.addIntercept>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-addIntercept 21 November 2024 - First Public Working Draft>
networkAddIntercept :: AddIntercept -> Command AddInterceptResult
networkAddIntercept = mkCommand NetworkAddIntercept

-- | Specification Entry: <BiDiSpecURL#command-network-continueRequest network.continueRequest>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-continueRequest 21 November 2024 - First Public Working Draft>
networkContinueRequest :: ContinueRequest -> Command ()
networkContinueRequest = mkCommand NetworkContinueRequest

-- | Specification Entry: <BiDiSpecURL#command-network-continueResponse network.continueResponse>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-continueResponse 21 November 2024 - First Public Working Draft>
networkContinueResponse :: ContinueResponse -> Command ()
networkContinueResponse = mkCommand NetworkContinueResponse

-- | Specification Entry: <BiDiSpecURL#command-network-continueWithAuth network.continueWithAuth>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-continueWithAuth 21 November 2024 - First Public Working Draft>
networkContinueWithAuth :: ContinueWithAuth -> Command ()
networkContinueWithAuth = mkCommand NetworkContinueWithAuth

-- | Specification Entry: <BiDiSpecURL#command-network-disownData network.disownData>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-disownData 20 June 2025>
networkDisownData :: DisownData -> Command ()
networkDisownData = mkCommand NetworkDisownData

-- | Specification Entry: <BiDiSpecURL#command-network-failRequest network.failRequest>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-failRequest 21 November 2024 - First Public Working Draft>
networkFailRequest :: FailRequest -> Command ()
networkFailRequest = mkCommand NetworkFailRequest

-- | Specification Entry: <BiDiSpecURL#command-network-getData network.getData>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-getData 20 June 2025>
networkGetData :: GetData -> Command GetDataResult
networkGetData = mkCommand NetworkGetData

-- | Specification Entry: <BiDiSpecURL#command-network-provideResponse network.provideResponse>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-provideResponse 21 November 2024 - First Public Working Draft>
networkProvideResponse :: ProvideResponse -> Command ()
networkProvideResponse = mkCommand NetworkProvideResponse

-- | Specification Entry: <BiDiSpecURL#command-network-removeDataCollector network.removeDataCollector>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-removeDataCollector 20 June 2025>
networkRemoveDataCollector :: RemoveDataCollector -> Command ()
networkRemoveDataCollector = mkCommand NetworkRemoveDataCollector

-- | Specification Entry: <BiDiSpecURL#command-network-removeIntercept network.removeIntercept>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-removeIntercept 21 November 2024 - First Public Working Draft>
networkRemoveIntercept :: RemoveIntercept -> Command ()
networkRemoveIntercept = mkCommand NetworkRemoveIntercept

-- | Specification Entry: <BiDiSpecURL#command-network-setCacheBehavior network.setCacheBehavior>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-setCacheBehavior 21 November 2024 - First Public Working Draft>
networkSetCacheBehavior :: SetCacheBehavior -> Command ()
networkSetCacheBehavior = mkCommand NetworkSetCacheBehavior

-- | Specification Entry: <BiDiSpecURL#command-network-setExtraHeaders network.setExtraHeaders>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250728/#command-network-setExtraHeaders 28 July 2025>
networkSetExtraHeaders :: SetExtraHeaders -> Command ()
networkSetExtraHeaders = mkCommand NetworkSetExtraHeaders

---- Script ----

-- | Specification Entry: <BiDiSpecURL#command-script-addPreloadScript script.addPreloadScript>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-addPreloadScript 21 November 2024 - First Public Working Draft>
scriptAddPreloadScript :: AddPreloadScript -> Command AddPreloadScriptResult
scriptAddPreloadScript = mkCommand ScriptAddPreloadScript

-- | Specification Entry: <BiDiSpecURL#command-script-callFunction script.callFunction>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-callFunction 21 November 2024 - First Public Working Draft>
scriptCallFunction :: CallFunction -> Command EvaluateResult
scriptCallFunction = mkCommand ScriptCallFunction

-- | Specification Entry: <BiDiSpecURL#command-script-disown script.disown>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-disown 21 November 2024 - First Public Working Draft>
scriptDisown :: Disown -> Command ()
scriptDisown = mkCommand ScriptDisown

-- | Specification Entry: <BiDiSpecURL#type-script-EvaluateResult script.evaluate>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#type-script-EvaluateResult 21 November 2024 - First Public Working Draft>
scriptEvaluate :: Evaluate -> Command EvaluateResult
scriptEvaluate = mkCommand ScriptEvaluate

-- | Specification Entry: <BiDiSpecURL#command-script-getRealms script.getRealms>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-getRealms 21 November 2024 - First Public Working Draft>
scriptGetRealms :: GetRealms -> Command GetRealmsResult
scriptGetRealms = mkCommand ScriptGetRealms

-- | Specification Entry: <BiDiSpecURL#command-script-removePreloadScript script.removePreloadScript>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-removePreloadScript 21 November 2024 - First Public Working Draft>
scriptRemovePreloadScript :: RemovePreloadScript -> Command ()
scriptRemovePreloadScript = mkCommand ScriptRemovePreloadScript

---- Storage ----

-- | Specification Entry: <BiDiSpecURL#command-storage-deleteCookies storage.deleteCookies>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-storage-deleteCookies 21 November 2024 - First Public Working Draft>
storageDeleteCookies :: DeleteCookies -> Command DeleteCookiesResult
storageDeleteCookies = mkCommand StorageDeleteCookies

-- | Specification Entry: <BiDiSpecURL#command-storage-getCookies storage.getCookies>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-storage-getCookies 21 November 2024 - First Public Working Draft>
storageGetCookies :: GetCookies -> Command GetCookiesResult
storageGetCookies = mkCommand StorageGetCookies

-- | Specification Entry: <BiDiSpecURL#command-storage-setCookie storage.setCookie>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-storage-setCookie 21 November 2024 - First Public Working Draft>
storageSetCookie :: SetCookie -> Command SetCookieResult
storageSetCookie = mkCommand StorageSetCookie

---- WebExtension ----

-- | Specification Entry: <BiDiSpecURL#command-webExtension-install webExtension.install>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241202/#command-webExtension-install 02 December 2024>
webExtensionInstall :: WebExtensionInstall -> Command WebExtensionResult
webExtensionInstall = mkCommand WebExtensionInstall

-- | Specification Entry: <BiDiSpecURL#command-webExtension-uninstall webExtension.uninstall>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241202/#command-webExtension-uninstall 02 December 2024>
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

subscribeOffSpecMany ::
  [OffSpecSubscriptionType] ->
  [BrowsingContext] ->
  [UserContext] ->
  (Value -> m ()) ->
  Subscription m
subscribeOffSpecMany = mkOffSpecSubscription

---- BrowsingContext ----

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-contextCreated browsingContext.contextCreated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-contextCreated 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextCreated ::
  [BrowsingContext] ->
  [UserContext] ->
  (Info -> m ()) ->
  Subscription m
subscribeBrowsingContextCreated = mkSubscription BrowsingContextContextCreated

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-contextDestroyed browsingContext.contextDestroyed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-contextDestroyed 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextDestroyed ::
  [BrowsingContext] ->
  [UserContext] ->
  (Info -> m ()) ->
  Subscription m
subscribeBrowsingContextDestroyed = mkSubscription BrowsingContextContextDestroyed

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationStarted browsingContext.navigationStarted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-navigationStarted 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextNavigationStarted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationStarted = mkSubscription BrowsingContextNavigationStarted

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-fragmentNavigated browsingContext.fragmentNavigated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-fragmentNavigated 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextFragmentNavigated ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextFragmentNavigated = mkSubscription BrowsingContextFragmentNavigated

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-historyUpdated browsingContext.historyUpdated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-historyUpdated 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextHistoryUpdated ::
  [BrowsingContext] ->
  [UserContext] ->
  (HistoryUpdated -> m ()) ->
  Subscription m
subscribeBrowsingContextHistoryUpdated = mkSubscription BrowsingContextHistoryUpdated

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-domContentLoaded browsingContext.domContentLoaded>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-domContentLoaded 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextDomContentLoaded ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextDomContentLoaded = mkSubscription BrowsingContextDomContentLoaded

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-load browsingContext.load>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-load 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextLoad ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextLoad = mkSubscription BrowsingContextLoad

subscribeBrowsingContextDownloadWillBegin ::
  [BrowsingContext] ->
  [UserContext] ->
  (DownloadWillBegin -> m ()) ->
  Subscription m
subscribeBrowsingContextDownloadWillBegin = mkSubscription BrowsingContextDownloadWillBegin

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-downloadEnd browsingContext.downloadEnd>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250603/#event-browsingContext-downloadEnd 03 June 2025>
subscribeBrowsingContextDownloadEnd ::
  [BrowsingContext] ->
  [UserContext] ->
  (DownloadEnd -> m ()) ->
  Subscription m
subscribeBrowsingContextDownloadEnd = mkSubscription BrowsingContextDownloadEnd

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationAborted browsingContext.navigationAborted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-navigationAborted 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextNavigationAborted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationAborted = mkSubscription BrowsingContextNavigationAborted

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationCommitted browsingContext.navigationCommitted>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250131/#event-browsingContext-navigationCommitted 31 January 2025>
subscribeBrowsingContextNavigationCommitted ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationCommitted = mkSubscription BrowsingContextNavigationCommitted

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationFailed browsingContext.navigationFailed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-navigationFailed 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextNavigationFailed ::
  [BrowsingContext] ->
  [UserContext] ->
  (NavigationInfo -> m ()) ->
  Subscription m
subscribeBrowsingContextNavigationFailed = mkSubscription BrowsingContextNavigationFailed

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-userPromptClosed browsingContext.userPromptClosed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-userPromptClosed 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextUserPromptClosed ::
  [BrowsingContext] ->
  [UserContext] ->
  (UserPromptClosed -> m ()) ->
  Subscription m
subscribeBrowsingContextUserPromptClosed = mkSubscription BrowsingContextUserPromptClosed

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-userPromptOpened browsingContext.userPromptOpened>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-userPromptOpened 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextUserPromptOpened ::
  [BrowsingContext] ->
  [UserContext] ->
  (UserPromptOpened -> m ()) ->
  Subscription m
subscribeBrowsingContextUserPromptOpened = mkSubscription BrowsingContextUserPromptOpened

---- Log ----

-- | Specification Entry: <BiDiSpecURL#event-log-entryAdded log.entryAdded>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-log-entryAdded 21 November 2024 - First Public Working Draft>
subscribeLogEntryAdded ::
  [BrowsingContext] ->
  [UserContext] ->
  (LogEntry -> m ()) ->
  Subscription m
subscribeLogEntryAdded = mkSubscription LogEntryAdded

---- Network ----

-- | Specification Entry: <BiDiSpecURL#event-network-authRequired network.authRequired>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-authRequired 21 November 2024 - First Public Working Draft>
subscribeNetworkAuthRequired ::
  [BrowsingContext] ->
  [UserContext] ->
  (AuthRequired -> m ()) ->
  Subscription m
subscribeNetworkAuthRequired = mkSubscription NetworkAuthRequired

subscribeNetworkBeforeRequestSent ::
  [BrowsingContext] ->
  [UserContext] ->
  (BeforeRequestSent -> m ()) ->
  Subscription m
subscribeNetworkBeforeRequestSent = mkSubscription NetworkBeforeRequestSent

-- | Specification Entry: <BiDiSpecURL#event-network-fetchError network.fetchError>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-fetchError 21 November 2024 - First Public Working Draft>
subscribeNetworkFetchError ::
  [BrowsingContext] ->
  [UserContext] ->
  (FetchError -> m ()) ->
  Subscription m
subscribeNetworkFetchError = mkSubscription NetworkFetchError

-- | Specification Entry: <BiDiSpecURL#event-network-responseCompleted network.responseCompleted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-responseCompleted 21 November 2024 - First Public Working Draft>
subscribeNetworkResponseCompleted ::
  [BrowsingContext] ->
  [UserContext] ->
  (ResponseCompleted -> m ()) ->
  Subscription m
subscribeNetworkResponseCompleted = mkSubscription NetworkResponseCompleted

-- | Specification Entry: <BiDiSpecURL#event-network-responseStarted network.responseStarted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-responseStarted 21 November 2024 - First Public Working Draft>
subscribeNetworkResponseStarted ::
  [BrowsingContext] ->
  [UserContext] ->
  (ResponseStarted -> m ()) ->
  Subscription m
subscribeNetworkResponseStarted = mkSubscription NetworkResponseStarted

---- Script ----

-- | Specification Entry: <BiDiSpecURL#event-script-message script.message>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-script-message 21 November 2024 - First Public Working Draft>
subscribeScriptMessage ::
  [BrowsingContext] ->
  [UserContext] ->
  (Message -> m ()) ->
  Subscription m
subscribeScriptMessage = mkSubscription ScriptMessage

-- | Specification Entry: <BiDiSpecURL#event-script-realmCreated script.realmCreated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-script-realmCreated 21 November 2024 - First Public Working Draft>
subscribeScriptRealmCreated ::
  [BrowsingContext] ->
  [UserContext] ->
  (RealmInfo -> m ()) ->
  Subscription m
subscribeScriptRealmCreated = mkSubscription ScriptRealmCreated

-- | Specification Entry: <BiDiSpecURL#event-script-realmDestroyed script.realmDestroyed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-script-realmDestroyed 21 November 2024 - First Public Working Draft>
subscribeScriptRealmDestroyed ::
  [BrowsingContext] ->
  [UserContext] ->
  (RealmDestroyed -> m ()) ->
  Subscription m
subscribeScriptRealmDestroyed = mkSubscription ScriptRealmDestroyed

---- Input ----

-- | Specification Entry: <BiDiSpecURL#event-input-fileDialogOpened input.fileDialogOpened>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250305/#event-input-fileDialogOpened 05 March 2025>
subscribeInputFileDialogOpened ::
  [BrowsingContext] ->
  [UserContext] ->
  (FileDialogOpened -> m ()) ->
  Subscription m
subscribeInputFileDialogOpened = mkSubscription InputFileDialogOpened
