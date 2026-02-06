-- |
-- Module: WebDriverPreCore.Extended.Base.BiDi.Actions
-- Description: BiDi actions module
module WebDriverPreCore.Extended.Base.BiDi.Actions
  ( -- * Runner Type
    Runner,
    -- * Session Commands
    sessionNew,
    sessionStatus,
    sessionEnd,
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
    -- * Subscription Types
    SendSubMany,
    SendSubMany',
    SendSub,
    SendSub',
    SendSubOffSpecMany,
    SendSubOffSpecMany',
    -- * Subscription Functions
    subscribeMany,
    subscribeMany',
    -- ** BrowsingContext Events
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
    -- ** Log Events
    subscribeLogEntryAdded,
    subscribeLogEntryAdded',
    -- ** Network Events
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
    -- ** Script Events
    subscribeScriptMessage,
    subscribeScriptMessage',
    subscribeScriptRealmCreated,
    subscribeScriptRealmCreated',
    subscribeScriptRealmDestroyed,
    subscribeScriptRealmDestroyed',
    -- ** Input Events
    subscribeInputFileDialogOpened,
    subscribeInputFileDialogOpened',
    -- * Fallback / Utility Functions
    subscribeOffSpecMany,
    subscribeOffSpecMany',
    offSpecCommand,
  )
where

import Data.Aeson (Object, Value)
import Data.Text (Text)
import WebDriverPreCore.Extended.Base.BiDi.API qualified as API
import WebDriverPreCore.Extended.Base.BiDi.Protocol

type Runner m a = Command a -> m a

--- ############## Commands ##############

---- Session ----

-- | Specification Entry: <BiDiSpecURL#command-session-new session.new>
--
-- This function is not supported by many Bidi drivers yet. To start a BiDi session you need to create an HTTP session with a web socket port specified.
-- Creating a new BiDi session via this command may result in a driver error.
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-new 21 November 2024 - First Public Working Draft>
sessionNew :: forall m. Runner m SessionNewResult -> Capabilities -> m SessionNewResult
sessionNew r = r . API.sessionNew

-- | Specification Entry: <BiDiSpecURL#command-session-status session.status>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-status 21 November 2024 - First Public Working Draft>
sessionStatus :: forall m. Runner m SessionStatusResult -> m SessionStatusResult
sessionStatus r = r API.sessionStatus

-- | Specification Entry: <BiDiSpecURL#command-session-end session.end>
--
-- Only sessions created via 'sessionNew' can be ended via this command.
-- If the BiDi session was created by other means (e.g. during HTTP session creation with a web socket port), it needs to be ended by ending the HTTP session.
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-session-end 21 November 2024 - First Public Working Draft>
sessionEnd :: forall m. Runner m () -> m ()
sessionEnd r = r API.sessionEnd

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
browsingContextActivate :: forall m. Runner m () -> Activate -> m ()
browsingContextActivate r = r . API.browsingContextActivate

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-captureScreenshot browsingContext.captureScreenshot>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-captureScreenshot 21 November 2024 - First Public Working Draft>
browsingContextCaptureScreenshot :: forall m. Runner m CaptureScreenshotResult -> CaptureScreenshot -> m CaptureScreenshotResult
browsingContextCaptureScreenshot r = r . API.browsingContextCaptureScreenshot

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-close browsingContext.close>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-close 21 November 2024 - First Public Working Draft>
browsingContextClose :: forall m. Runner m () -> Close -> m ()
browsingContextClose r = r . API.browsingContextClose

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-create browsingContext.create>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-create 21 November 2024 - First Public Working Draft>
browsingContextCreate :: forall m. Runner m BrowsingContext -> Create -> m BrowsingContext
browsingContextCreate r = r . API.browsingContextCreate

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-getTree browsingContext.getTree>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-getTree 21 November 2024 - First Public Working Draft>
browsingContextGetTree :: forall m. Runner m GetTreeResult -> GetTree -> m GetTreeResult
browsingContextGetTree r = r . API.browsingContextGetTree

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-handleUserPrompt browsingContext.handleUserPrompt>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-handleUserPrompt 21 November 2024 - First Public Working Draft>
browsingContextHandleUserPrompt :: forall m. Runner m () -> HandleUserPrompt -> m ()
browsingContextHandleUserPrompt r = r . API.browsingContextHandleUserPrompt

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-locateNodes browsingContext.locateNodes>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-locateNodes 21 November 2024 - First Public Working Draft>
browsingContextLocateNodes :: forall m. Runner m LocateNodesResult -> LocateNodes -> m LocateNodesResult
browsingContextLocateNodes r = r . API.browsingContextLocateNodes

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-navigate browsingContext.navigate>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-navigate 21 November 2024 - First Public Working Draft>
browsingContextNavigate :: forall m. Runner m NavigateResult -> Navigate -> m NavigateResult
browsingContextNavigate r = r . API.browsingContextNavigate

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-print browsingContext.print>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-print 21 November 2024 - First Public Working Draft>
browsingContextPrint :: forall m. Runner m PrintResult -> Print -> m PrintResult
browsingContextPrint r = r . API.browsingContextPrint

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-reload browsingContext.reload>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-reload 21 November 2024 - First Public Working Draft>
browsingContextReload :: forall m. Runner m () -> Reload -> m ()
browsingContextReload r = r . API.browsingContextReload

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-setViewport browsingContext.setViewport>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-setViewport 21 November 2024 - First Public Working Draft>
browsingContextSetViewport :: forall m. Runner m () -> SetViewport -> m ()
browsingContextSetViewport r = r . API.browsingContextSetViewport

-- | Specification Entry: <BiDiSpecURL#command-browsingContext-traverseHistory browsingContext.traverseHistory>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browsingContext-traverseHistory 21 November 2024 - First Public Working Draft>
browsingContextTraverseHistory :: forall m. Runner m () -> TraverseHistory -> m ()
browsingContextTraverseHistory r = r . API.browsingContextTraverseHistory

---- Browser ----

-- | Specification Entry: <BiDiSpecURL#command-browser-close browser.close>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-close 21 November 2024 - First Public Working Draft>
browserClose :: forall m. Runner m () -> m ()
browserClose r = r API.browserClose

-- | Specification Entry: <BiDiSpecURL#command-browser-createUserContext browser.createUserContext>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-createUserContext 21 November 2024 - First Public Working Draft>
browserCreateUserContext :: forall m. Runner m UserContext -> CreateUserContext -> m UserContext
browserCreateUserContext r = r . API.browserCreateUserContext

-- | Specification Entry: <BiDiSpecURL#command-browser-getClientWindows browser.getClientWindows>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-getClientWindows 21 November 2024 - First Public Working Draft>
browserGetClientWindows :: forall m. Runner m GetClientWindowsResult -> m GetClientWindowsResult
browserGetClientWindows r = r API.browserGetClientWindows

-- | Specification Entry: <BiDiSpecURL#command-browser-getUserContexts browser.getUserContexts>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-getUserContexts 21 November 2024 - First Public Working Draft>
browserGetUserContexts :: forall m. Runner m GetUserContextsResult -> m GetUserContextsResult
browserGetUserContexts r = r API.browserGetUserContexts

-- | Specification Entry: <BiDiSpecURL#command-browser-removeUserContext browser.removeUserContext>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-removeUserContext 21 November 2024 - First Public Working Draft>
browserRemoveUserContext :: forall m. Runner m () -> RemoveUserContext -> m ()
browserRemoveUserContext r = r . API.browserRemoveUserContext

-- | Specification Entry: <BiDiSpecURL#command-browser-setClientWindowState browser.setClientWindowState>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-browser-setClientWindowState 21 November 2024 - First Public Working Draft>
browserSetClientWindowState :: forall m. Runner m ClientWindowInfo -> SetClientWindowState -> m ClientWindowInfo
browserSetClientWindowState r = r . API.browserSetClientWindowState

-- since 18-09-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250918

-- | Specification Entry: <BiDiSpecURL#command-browser-setDownloadBehavior browser.setDownloadBehavior>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250917/#command-browser-setDownloadBehavior 17 September 2025>
browserSetDownloadBehavior :: forall m. Runner m () -> SetDownloadBehavior -> m ()
browserSetDownloadBehavior r = r . API.browserSetDownloadBehavior

---- Emulation ----

-- | Specification Entry: <BiDiSpecURL#command-emulation-setForcedColorsModeThemeOverride emulation.setForcedColorsModeThemeOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250729/#command-emulation-setForcedColorsModeThemeOverride 29 July 2025>
emulationSetForcedColorsModeThemeOverride :: forall m. Runner m () -> SetForcedColorsModeThemeOverride -> m ()
emulationSetForcedColorsModeThemeOverride r = r . API.emulationSetForcedColorsModeThemeOverride

-- | Specification Entry: <BiDiSpecURL#command-emulation-setGeolocationOverride emulation.setGeolocationOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250321/#command-emulation-setGeolocationOverride 21 March 2025>
emulationSetGeolocationOverride :: forall m. Runner m () -> SetGeolocationOverride -> m ()
emulationSetGeolocationOverride r = r . API.emulationSetGeolocationOverride

-- | Specification Entry: <BiDiSpecURL#command-emulation-setLocaleOverride emulation.setLocaleOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250626/#command-emulation-setLocaleOverride 26 June 2025>
emulationSetLocaleOverride :: forall m. Runner m () -> SetLocaleOverride -> m ()
emulationSetLocaleOverride r = r . API.emulationSetLocaleOverride

-- since 07-10-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007

-- | Specification Entry: <BiDiSpecURL#command-emulation-setNetworkConditions emulation.setNetworkConditions>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007/#command-emulation-setNetworkConditions 07 October 2025>
emulationSetNetworkConditions :: forall m. Runner m () -> SetNetworkConditions -> m ()
emulationSetNetworkConditions r = r . API.emulationSetNetworkConditions

-- | Specification Entry: <BiDiSpecURL#command-emulation-setScreenOrientationOverride emulation.setScreenOrientationOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250626/#command-emulation-setScreenOrientationOverride 26 June 2025>
emulationSetScreenOrientationOverride :: forall m. Runner m () -> SetScreenOrientationOverride -> m ()
emulationSetScreenOrientationOverride r = r . API.emulationSetScreenOrientationOverride

-- since 20-11-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120

-- | Specification Entry: <BiDiSpecURL#command-emulation-setScreenSettingsOverride emulation.setScreenSettingsOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120/#command-emulation-setScreenSettingsOverride 20 November 2025>
emulationSetScreenSettingsOverride :: forall m. Runner m () -> SetScreenSettingsOverride -> m ()
emulationSetScreenSettingsOverride r = r . API.emulationSetScreenSettingsOverride

-- since 11-08-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811

-- | Specification Entry: <BiDiSpecURL#command-emulation-setScriptingEnabled emulation.setScriptingEnabled>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811/#command-emulation-setScriptingEnabled 11 August 2025>
emulationSetScriptingEnabled :: forall m. Runner m () -> SetScriptingEnabled -> m ()
emulationSetScriptingEnabled r = r . API.emulationSetScriptingEnabled

-- | Specification Entry: <BiDiSpecURL#command-emulation-setTimezoneOverride emulation.setTimezoneOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250718/#command-emulation-setTimezoneOverride 18 July 2025>
emulationSetTimezoneOverride :: forall m. Runner m () -> SetTimezoneOverride -> m ()
emulationSetTimezoneOverride r = r . API.emulationSetTimezoneOverride

-- since 09-01-2026 https://www.w3.org/TR/2026/WD-webdriver-bidi-20260109

-- | Specification Entry: <BiDiSpecURL#command-emulation-setTouchOverride emulation.setTouchOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2026/WD-webdriver-bidi-20260109/#command-emulation-setTouchOverride 09 January 2026>
emulationSetTouchOverride :: forall m. Runner m () -> SetTouchOverride -> m ()
emulationSetTouchOverride r = r . API.emulationSetTouchOverride

-- since 10-09-2025 https://www.w3.org/TR/2025/WD-webdriver-bidi-20250910

-- | Specification Entry: <BiDiSpecURL#command-emulation-setUserAgentOverride emulation.setUserAgentOverride>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250910/#command-emulation-setUserAgentOverride 10 September 2025>
emulationSetUserAgentOverride :: forall m. Runner m () -> SetUserAgentOverride -> m ()
emulationSetUserAgentOverride r = r . API.emulationSetUserAgentOverride

---- Input ----

-- | Specification Entry: <BiDiSpecURL#command-input-performActions input.performActions>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-input-performActions 21 November 2024 - First Public Working Draft>
inputPerformActions :: forall m. Runner m () -> PerformActions -> m ()
inputPerformActions r = r . API.inputPerformActions

-- | Specification Entry: <BiDiSpecURL#command-input-releaseActions input.releaseActions>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-input-releaseActions 21 November 2024 - First Public Working Draft>
inputReleaseActions :: forall m. Runner m () -> ReleaseActions -> m ()
inputReleaseActions r = r . API.inputReleaseActions

-- | Specification Entry: <BiDiSpecURL#command-input-setFiles input.setFiles>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-input-setFiles 21 November 2024 - First Public Working Draft>
inputSetFiles :: forall m. Runner m () -> SetFiles -> m ()
inputSetFiles r = r . API.inputSetFiles

---- Network ----

-- | Specification Entry: <BiDiSpecURL#command-network-addDataCollector network.addDataCollector>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-addDataCollector 20 June 2025>
networkAddDataCollector :: forall m. Runner m AddDataCollectorResult -> AddDataCollector -> m AddDataCollectorResult
networkAddDataCollector r = r . API.networkAddDataCollector

-- | Specification Entry: <BiDiSpecURL#command-network-addIntercept network.addIntercept>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-addIntercept 21 November 2024 - First Public Working Draft>
networkAddIntercept :: forall m. Runner m AddInterceptResult -> AddIntercept -> m AddInterceptResult
networkAddIntercept r = r . API.networkAddIntercept

-- | Specification Entry: <BiDiSpecURL#command-network-continueRequest network.continueRequest>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-continueRequest 21 November 2024 - First Public Working Draft>
networkContinueRequest :: forall m. Runner m () -> ContinueRequest -> m ()
networkContinueRequest r = r . API.networkContinueRequest

-- | Specification Entry: <BiDiSpecURL#command-network-continueResponse network.continueResponse>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-continueResponse 21 November 2024 - First Public Working Draft>
networkContinueResponse :: forall m. Runner m () -> ContinueResponse -> m ()
networkContinueResponse r = r . API.networkContinueResponse

-- | Specification Entry: <BiDiSpecURL#command-network-continueWithAuth network.continueWithAuth>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-continueWithAuth 21 November 2024 - First Public Working Draft>
networkContinueWithAuth :: forall m. Runner m () -> ContinueWithAuth -> m ()
networkContinueWithAuth r = r . API.networkContinueWithAuth

-- | Specification Entry: <BiDiSpecURL#command-network-disownData network.disownData>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-disownData 20 June 2025>
networkDisownData :: forall m. Runner m () -> DisownData -> m ()
networkDisownData r = r . API.networkDisownData

-- | Specification Entry: <BiDiSpecURL#command-network-failRequest network.failRequest>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-failRequest 21 November 2024 - First Public Working Draft>
networkFailRequest :: forall m. Runner m () -> FailRequest -> m ()
networkFailRequest r = r . API.networkFailRequest

-- | Specification Entry: <BiDiSpecURL#command-network-getData network.getData>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-getData 20 June 2025>
networkGetData :: forall m. Runner m GetDataResult -> GetData -> m GetDataResult
networkGetData r = r . API.networkGetData

-- | Specification Entry: <BiDiSpecURL#command-network-provideResponse network.provideResponse>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-provideResponse 21 November 2024 - First Public Working Draft>
networkProvideResponse :: forall m. Runner m () -> ProvideResponse -> m ()
networkProvideResponse r = r . API.networkProvideResponse

-- | Specification Entry: <BiDiSpecURL#command-network-removeDataCollector network.removeDataCollector>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250620/#command-network-removeDataCollector 20 June 2025>
networkRemoveDataCollector :: forall m. Runner m () -> RemoveDataCollector -> m ()
networkRemoveDataCollector r = r . API.networkRemoveDataCollector

-- | Specification Entry: <BiDiSpecURL#command-network-removeIntercept network.removeIntercept>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-removeIntercept 21 November 2024 - First Public Working Draft>
networkRemoveIntercept :: forall m. Runner m () -> RemoveIntercept -> m ()
networkRemoveIntercept r = r . API.networkRemoveIntercept

-- | Specification Entry: <BiDiSpecURL#command-network-setCacheBehavior network.setCacheBehavior>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-network-setCacheBehavior 21 November 2024 - First Public Working Draft>
networkSetCacheBehavior :: forall m. Runner m () -> SetCacheBehavior -> m ()
networkSetCacheBehavior r = r . API.networkSetCacheBehavior

-- | Specification Entry: <BiDiSpecURL#command-network-setExtraHeaders network.setExtraHeaders>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250728/#command-network-setExtraHeaders 28 July 2025>
networkSetExtraHeaders :: forall m. Runner m () -> SetExtraHeaders -> m ()
networkSetExtraHeaders r = r . API.networkSetExtraHeaders

---- Script ----

-- | Specification Entry: <BiDiSpecURL#command-script-addPreloadScript script.addPreloadScript>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-addPreloadScript 21 November 2024 - First Public Working Draft>
scriptAddPreloadScript :: forall m. Runner m AddPreloadScriptResult -> AddPreloadScript -> m AddPreloadScriptResult
scriptAddPreloadScript r = r . API.scriptAddPreloadScript

-- | Specification Entry: <BiDiSpecURL#command-script-callFunction script.callFunction>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-callFunction 21 November 2024 - First Public Working Draft>
scriptCallFunction :: forall m. Runner m EvaluateResult -> CallFunction -> m EvaluateResult
scriptCallFunction r = r . API.scriptCallFunction

-- | Specification Entry: <BiDiSpecURL#command-script-disown script.disown>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-disown 21 November 2024 - First Public Working Draft>
scriptDisown :: forall m. Runner m () -> Disown -> m ()
scriptDisown r = r . API.scriptDisown

-- | Specification Entry: <BiDiSpecURL#type-script-EvaluateResult script.evaluate>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#type-script-EvaluateResult 21 November 2024 - First Public Working Draft>
scriptEvaluate :: forall m. Runner m EvaluateResult -> Evaluate -> m EvaluateResult
scriptEvaluate r = r . API.scriptEvaluate

-- | Specification Entry: <BiDiSpecURL#command-script-getRealms script.getRealms>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-getRealms 21 November 2024 - First Public Working Draft>
scriptGetRealms :: forall m. Runner m GetRealmsResult -> GetRealms -> m GetRealmsResult
scriptGetRealms r = r . API.scriptGetRealms

-- | Specification Entry: <BiDiSpecURL#command-script-removePreloadScript script.removePreloadScript>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-script-removePreloadScript 21 November 2024 - First Public Working Draft>
scriptRemovePreloadScript :: forall m. Runner m () -> RemovePreloadScript -> m ()
scriptRemovePreloadScript r = r . API.scriptRemovePreloadScript

---- Storage ----

-- | Specification Entry: <BiDiSpecURL#command-storage-deleteCookies storage.deleteCookies>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-storage-deleteCookies 21 November 2024 - First Public Working Draft>
storageDeleteCookies :: forall m. Runner m DeleteCookiesResult -> DeleteCookies -> m DeleteCookiesResult
storageDeleteCookies r = r . API.storageDeleteCookies

-- | Specification Entry: <BiDiSpecURL#command-storage-getCookies storage.getCookies>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-storage-getCookies 21 November 2024 - First Public Working Draft>
storageGetCookies :: forall m. Runner m GetCookiesResult -> GetCookies -> m GetCookiesResult
storageGetCookies r = r . API.storageGetCookies

-- | Specification Entry: <BiDiSpecURL#command-storage-setCookie storage.setCookie>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#command-storage-setCookie 21 November 2024 - First Public Working Draft>
storageSetCookie :: forall m. Runner m SetCookieResult -> SetCookie -> m SetCookieResult
storageSetCookie r = r . API.storageSetCookie

---- WebExtension ----

-- | Specification Entry: <BiDiSpecURL#command-webExtension-install webExtension.install>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241202/#command-webExtension-install 02 December 2024>
webExtensionInstall :: forall m. Runner m WebExtensionResult -> WebExtensionInstall -> m WebExtensionResult
webExtensionInstall r = r . API.webExtensionInstall

-- | Specification Entry: <BiDiSpecURL#command-webExtension-uninstall webExtension.uninstall>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241202/#command-webExtension-uninstall 02 December 2024>
webExtensionUninstall :: forall m. Runner m () -> WebExtensionUninstall -> m ()
webExtensionUninstall r = r . API.webExtensionUninstall

-- ############## Subscriptions (Events) ##############

type SendSubMany m =
  ( [KnownSubscriptionType] ->
    [BrowsingContext] ->
    [UserContext] ->
    (Event -> m ()) ->
    Subscription m
  ) ->
  [KnownSubscriptionType] ->
  (Event -> m ()) ->
  m SubscriptionId

type SendSubMany' m =
  ( [KnownSubscriptionType] ->
    [BrowsingContext] ->
    [UserContext] ->
    (Event -> m ()) ->
    Subscription m
  ) ->
  [KnownSubscriptionType] ->
  [BrowsingContext] ->
  [UserContext] ->
  (Event -> m ()) ->
  m SubscriptionId

subscribeMany :: forall m. SendSubMany m -> [KnownSubscriptionType] -> (Event -> m ()) -> m SubscriptionId
subscribeMany sendSubMany = sendSubMany API.subscribeMany

subscribeMany' :: forall m. SendSubMany' m -> [KnownSubscriptionType] -> [BrowsingContext] -> [UserContext] -> (Event -> m ()) -> m SubscriptionId
subscribeMany' sendSubMany' = sendSubMany' API.subscribeMany

------- 

type SendSub m a =
  ( [BrowsingContext] ->
    [UserContext] ->
    (a -> m ()) ->
    Subscription m
  ) ->
  (a -> m ()) ->
  m SubscriptionId

type SendSub' m a =
  ( [BrowsingContext] ->
    [UserContext] ->
    (a -> m ()) ->
    Subscription m
  ) ->
  [BrowsingContext] ->
  [UserContext] ->
  (a -> m ()) ->
  m SubscriptionId

---- BrowsingContext ----

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-contextCreated browsingContext.contextCreated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-contextCreated 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextCreated :: forall m. SendSub m Info -> (Info -> m ()) -> m SubscriptionId
subscribeBrowsingContextCreated sendSub = sendSub API.subscribeBrowsingContextCreated

subscribeBrowsingContextCreated' :: forall m. SendSub' m Info -> [BrowsingContext] -> [UserContext] -> (Info -> m ()) -> m SubscriptionId
subscribeBrowsingContextCreated' sendSub' = sendSub' API.subscribeBrowsingContextCreated

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-contextDestroyed browsingContext.contextDestroyed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-contextDestroyed 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextDestroyed :: forall m. SendSub m Info -> (Info -> m ()) -> m SubscriptionId
subscribeBrowsingContextDestroyed sendSub = sendSub API.subscribeBrowsingContextDestroyed

subscribeBrowsingContextDestroyed' :: forall m. SendSub' m Info -> [BrowsingContext] -> [UserContext] -> (Info -> m ()) -> m SubscriptionId
subscribeBrowsingContextDestroyed' sendSub' = sendSub' API.subscribeBrowsingContextDestroyed

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationStarted browsingContext.navigationStarted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-navigationStarted 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextNavigationStarted :: forall m. SendSub m NavigationInfo -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationStarted sendSub = sendSub API.subscribeBrowsingContextNavigationStarted

subscribeBrowsingContextNavigationStarted' :: forall m. SendSub' m NavigationInfo -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationStarted' sendSub' = sendSub' API.subscribeBrowsingContextNavigationStarted

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-fragmentNavigated browsingContext.fragmentNavigated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-fragmentNavigated 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextFragmentNavigated :: forall m. SendSub m NavigationInfo -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextFragmentNavigated sendSub = sendSub API.subscribeBrowsingContextFragmentNavigated

subscribeBrowsingContextFragmentNavigated' :: forall m. SendSub' m NavigationInfo -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextFragmentNavigated' sendSub' = sendSub' API.subscribeBrowsingContextFragmentNavigated

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-historyUpdated browsingContext.historyUpdated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-historyUpdated 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextHistoryUpdated :: forall m. SendSub m HistoryUpdated -> (HistoryUpdated -> m ()) -> m SubscriptionId
subscribeBrowsingContextHistoryUpdated sendSub = sendSub API.subscribeBrowsingContextHistoryUpdated

subscribeBrowsingContextHistoryUpdated' :: forall m. SendSub' m HistoryUpdated -> [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> m ()) -> m SubscriptionId
subscribeBrowsingContextHistoryUpdated' sendSub' = sendSub' API.subscribeBrowsingContextHistoryUpdated

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-domContentLoaded browsingContext.domContentLoaded>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-domContentLoaded 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextDomContentLoaded :: forall m. SendSub m NavigationInfo -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextDomContentLoaded sendSub = sendSub API.subscribeBrowsingContextDomContentLoaded

subscribeBrowsingContextDomContentLoaded' :: forall m. SendSub' m NavigationInfo -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextDomContentLoaded' sendSub' = sendSub' API.subscribeBrowsingContextDomContentLoaded

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-load browsingContext.load>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-load 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextLoad :: forall m. SendSub m NavigationInfo -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextLoad sendSub = sendSub API.subscribeBrowsingContextLoad

subscribeBrowsingContextLoad' :: forall m. SendSub' m NavigationInfo -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextLoad' sendSub' = sendSub' API.subscribeBrowsingContextLoad

subscribeBrowsingContextDownloadWillBegin :: forall m. SendSub m DownloadWillBegin -> (DownloadWillBegin -> m ()) -> m SubscriptionId
subscribeBrowsingContextDownloadWillBegin sendSub = sendSub API.subscribeBrowsingContextDownloadWillBegin

subscribeBrowsingContextDownloadWillBegin' :: forall m. SendSub' m DownloadWillBegin -> [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> m ()) -> m SubscriptionId
subscribeBrowsingContextDownloadWillBegin' sendSub' = sendSub' API.subscribeBrowsingContextDownloadWillBegin

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-downloadEnd browsingContext.downloadEnd>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250603/#event-browsingContext-downloadEnd 03 June 2025>
subscribeBrowsingContextDownloadEnd :: forall m. SendSub m DownloadEnd -> (DownloadEnd -> m ()) -> m SubscriptionId
subscribeBrowsingContextDownloadEnd sendSub = sendSub API.subscribeBrowsingContextDownloadEnd

subscribeBrowsingContextDownloadEnd' :: forall m. SendSub' m DownloadEnd -> [BrowsingContext] -> [UserContext] -> (DownloadEnd -> m ()) -> m SubscriptionId
subscribeBrowsingContextDownloadEnd' sendSub' = sendSub' API.subscribeBrowsingContextDownloadEnd

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationAborted browsingContext.navigationAborted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-navigationAborted 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextNavigationAborted :: forall m. SendSub m NavigationInfo -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationAborted sendSub = sendSub API.subscribeBrowsingContextNavigationAborted

subscribeBrowsingContextNavigationAborted' :: forall m. SendSub' m NavigationInfo -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationAborted' sendSub' = sendSub' API.subscribeBrowsingContextNavigationAborted

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationCommitted browsingContext.navigationCommitted>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250131/#event-browsingContext-navigationCommitted 31 January 2025>
subscribeBrowsingContextNavigationCommitted :: forall m. SendSub m NavigationInfo -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationCommitted sendSub = sendSub API.subscribeBrowsingContextNavigationCommitted

subscribeBrowsingContextNavigationCommitted' :: forall m. SendSub' m NavigationInfo -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationCommitted' sendSub' = sendSub' API.subscribeBrowsingContextNavigationCommitted

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-navigationFailed browsingContext.navigationFailed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-navigationFailed 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextNavigationFailed :: forall m. SendSub m NavigationInfo -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationFailed sendSub = sendSub API.subscribeBrowsingContextNavigationFailed

subscribeBrowsingContextNavigationFailed' :: forall m. SendSub' m NavigationInfo -> [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> m SubscriptionId
subscribeBrowsingContextNavigationFailed' sendSub' = sendSub' API.subscribeBrowsingContextNavigationFailed

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-userPromptClosed browsingContext.userPromptClosed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-userPromptClosed 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextUserPromptClosed :: forall m. SendSub m UserPromptClosed -> (UserPromptClosed -> m ()) -> m SubscriptionId
subscribeBrowsingContextUserPromptClosed sendSub = sendSub API.subscribeBrowsingContextUserPromptClosed

subscribeBrowsingContextUserPromptClosed' :: forall m. SendSub' m UserPromptClosed -> [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> m ()) -> m SubscriptionId
subscribeBrowsingContextUserPromptClosed' sendSub' = sendSub' API.subscribeBrowsingContextUserPromptClosed

-- | Specification Entry: <BiDiSpecURL#event-browsingContext-userPromptOpened browsingContext.userPromptOpened>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-browsingContext-userPromptOpened 21 November 2024 - First Public Working Draft>
subscribeBrowsingContextUserPromptOpened :: forall m. SendSub m UserPromptOpened -> (UserPromptOpened -> m ()) -> m SubscriptionId
subscribeBrowsingContextUserPromptOpened sendSub = sendSub API.subscribeBrowsingContextUserPromptOpened

subscribeBrowsingContextUserPromptOpened' :: forall m. SendSub' m UserPromptOpened -> [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> m ()) -> m SubscriptionId
subscribeBrowsingContextUserPromptOpened' sendSub' = sendSub' API.subscribeBrowsingContextUserPromptOpened

---- Log ----

-- | Specification Entry: <BiDiSpecURL#event-log-entryAdded log.entryAdded>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-log-entryAdded 21 November 2024 - First Public Working Draft>
subscribeLogEntryAdded :: forall m. SendSub m LogEntry -> (LogEntry -> m ()) -> m SubscriptionId
subscribeLogEntryAdded sendSub = sendSub API.subscribeLogEntryAdded

subscribeLogEntryAdded' :: forall m. SendSub' m LogEntry -> [BrowsingContext] -> [UserContext] -> (LogEntry -> m ()) -> m SubscriptionId
subscribeLogEntryAdded' sendSub' = sendSub' API.subscribeLogEntryAdded

---- Network ----

-- | Specification Entry: <BiDiSpecURL#event-network-authRequired network.authRequired>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-authRequired 21 November 2024 - First Public Working Draft>
subscribeNetworkAuthRequired :: forall m. SendSub m AuthRequired -> (AuthRequired -> m ()) -> m SubscriptionId
subscribeNetworkAuthRequired sendSub = sendSub API.subscribeNetworkAuthRequired

subscribeNetworkAuthRequired' :: forall m. SendSub' m AuthRequired -> [BrowsingContext] -> [UserContext] -> (AuthRequired -> m ()) -> m SubscriptionId
subscribeNetworkAuthRequired' sendSub' = sendSub' API.subscribeNetworkAuthRequired

subscribeNetworkBeforeRequestSent :: forall m. SendSub m BeforeRequestSent -> (BeforeRequestSent -> m ()) -> m SubscriptionId
subscribeNetworkBeforeRequestSent sendSub = sendSub API.subscribeNetworkBeforeRequestSent

subscribeNetworkBeforeRequestSent' :: forall m. SendSub' m BeforeRequestSent -> [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> m ()) -> m SubscriptionId
subscribeNetworkBeforeRequestSent' sendSub' = sendSub' API.subscribeNetworkBeforeRequestSent

-- | Specification Entry: <BiDiSpecURL#event-network-fetchError network.fetchError>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-fetchError 21 November 2024 - First Public Working Draft>
subscribeNetworkFetchError :: forall m. SendSub m FetchError -> (FetchError -> m ()) -> m SubscriptionId
subscribeNetworkFetchError sendSub = sendSub API.subscribeNetworkFetchError

subscribeNetworkFetchError' :: forall m. SendSub' m FetchError -> [BrowsingContext] -> [UserContext] -> (FetchError -> m ()) -> m SubscriptionId
subscribeNetworkFetchError' sendSub' = sendSub' API.subscribeNetworkFetchError

-- | Specification Entry: <BiDiSpecURL#event-network-responseCompleted network.responseCompleted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-responseCompleted 21 November 2024 - First Public Working Draft>
subscribeNetworkResponseCompleted :: forall m. SendSub m ResponseCompleted -> (ResponseCompleted -> m ()) -> m SubscriptionId
subscribeNetworkResponseCompleted sendSub = sendSub API.subscribeNetworkResponseCompleted

subscribeNetworkResponseCompleted' :: forall m. SendSub' m ResponseCompleted -> [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> m ()) -> m SubscriptionId
subscribeNetworkResponseCompleted' sendSub' = sendSub' API.subscribeNetworkResponseCompleted

-- | Specification Entry: <BiDiSpecURL#event-network-responseStarted network.responseStarted>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-network-responseStarted 21 November 2024 - First Public Working Draft>
subscribeNetworkResponseStarted :: forall m. SendSub m ResponseStarted -> (ResponseStarted -> m ()) -> m SubscriptionId
subscribeNetworkResponseStarted sendSub = sendSub API.subscribeNetworkResponseStarted

subscribeNetworkResponseStarted' :: forall m. SendSub' m ResponseStarted -> [BrowsingContext] -> [UserContext] -> (ResponseStarted -> m ()) -> m SubscriptionId
subscribeNetworkResponseStarted' sendSub' = sendSub' API.subscribeNetworkResponseStarted

---- Script ----

-- | Specification Entry: <BiDiSpecURL#event-script-message script.message>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-script-message 21 November 2024 - First Public Working Draft>
subscribeScriptMessage :: forall m. SendSub m Message -> (Message -> m ()) -> m SubscriptionId
subscribeScriptMessage sendSub = sendSub API.subscribeScriptMessage

subscribeScriptMessage' :: forall m. SendSub' m Message -> [BrowsingContext] -> [UserContext] -> (Message -> m ()) -> m SubscriptionId
subscribeScriptMessage' sendSub' = sendSub' API.subscribeScriptMessage

-- | Specification Entry: <BiDiSpecURL#event-script-realmCreated script.realmCreated>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-script-realmCreated 21 November 2024 - First Public Working Draft>
subscribeScriptRealmCreated :: forall m. SendSub m RealmInfo -> (RealmInfo -> m ()) -> m SubscriptionId
subscribeScriptRealmCreated sendSub = sendSub API.subscribeScriptRealmCreated

subscribeScriptRealmCreated' :: forall m. SendSub' m RealmInfo -> [BrowsingContext] -> [UserContext] -> (RealmInfo -> m ()) -> m SubscriptionId
subscribeScriptRealmCreated' sendSub' = sendSub' API.subscribeScriptRealmCreated

-- | Specification Entry: <BiDiSpecURL#event-script-realmDestroyed script.realmDestroyed>
--
-- First added to Spec: <https://www.w3.org/TR/2024/WD-webdriver-bidi-20241121/#event-script-realmDestroyed 21 November 2024 - First Public Working Draft>
subscribeScriptRealmDestroyed :: forall m. SendSub m RealmDestroyed -> (RealmDestroyed -> m ()) -> m SubscriptionId
subscribeScriptRealmDestroyed sendSub = sendSub API.subscribeScriptRealmDestroyed

subscribeScriptRealmDestroyed' :: forall m. SendSub' m RealmDestroyed -> [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> m ()) -> m SubscriptionId
subscribeScriptRealmDestroyed' sendSub' = sendSub' API.subscribeScriptRealmDestroyed

---- Input ----

-- | Specification Entry: <BiDiSpecURL#event-input-fileDialogOpened input.filedblogOpened>
--
-- First added to Spec: <https://www.w3.org/TR/2025/WD-webdriver-bidi-20250305/#event-input-fileDialogOpened 05 March 2025>
subscribeInputFileDialogOpened :: forall m. SendSub m FileDialogOpened -> (FileDialogOpened -> m ()) -> m SubscriptionId
subscribeInputFileDialogOpened sendSub = sendSub API.subscribeInputFileDialogOpened

subscribeInputFileDialogOpened' :: forall m. SendSub' m FileDialogOpened -> [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> m ()) -> m SubscriptionId
subscribeInputFileDialogOpened' sendSub' = sendSub' API.subscribeInputFileDialogOpened

-- ############## Fallback / Utility Functions ##############

-- | Subscribe to off-specification event types.
--
-- Use this only as a fallback when a driver supports events not covered by
-- this library. Prefer using the standard subscription functions when available.
type SendSubOffSpecMany m =
  ( [OffSpecSubscriptionType] ->
    [BrowsingContext] ->
    [UserContext] ->
    (Value -> m ()) ->
    Subscription m
  ) ->
  [OffSpecSubscriptionType] ->
  (Value -> m ()) ->
  m SubscriptionId

type SendSubOffSpecMany' m =
  ( [OffSpecSubscriptionType] ->
    [BrowsingContext] ->
    [UserContext] ->
    (Value -> m ()) ->
    Subscription m
  ) ->
  [OffSpecSubscriptionType] ->
  [BrowsingContext] ->
  [UserContext] ->
  (Value -> m ()) ->
  m SubscriptionId

-- | Subscribe to off-specification event types (simplified version).
--
-- Use this only as a fallback when a driver supports events not covered by
-- this library. Prefer using the standard subscription functions when available.
--
-- This is the simplified version that subscribes to all browsing contexts and user contexts.
-- For more control, use 'subscribeOffSpecMany''.
subscribeOffSpecMany :: forall m. SendSubOffSpecMany m -> [OffSpecSubscriptionType] -> (Value -> m ()) -> m SubscriptionId
subscribeOffSpecMany sendSubOffSpecMany = sendSubOffSpecMany API.subscribeOffSpecMany

-- | Subscribe to off-specification event types with full control.
--
-- Use this only as a fallback when a driver supports events not covered by
-- this library. Prefer using the standard subscription functions when available.
--
-- This version allows you to specify the browsing contexts and user contexts to subscribe to.
subscribeOffSpecMany' :: forall m. SendSubOffSpecMany' m -> [OffSpecSubscriptionType] -> [BrowsingContext] -> [UserContext] -> (Value -> m ()) -> m SubscriptionId
subscribeOffSpecMany' sendSubOffSpecMany' = sendSubOffSpecMany' API.subscribeOffSpecMany

-- | Create an off-specification command.
--
-- Use this only as a fallback when a driver supports commands not covered by
-- this library. Prefer using the standard command functions when available.
--
-- The first argument is the command name (e.g., "cdp.sendCommand"), and the
-- second is the parameters as a JSON Object.
offSpecCommand :: forall m. Runner m Object -> Text -> Object -> m Object
offSpecCommand r method params = r (mkOffSpecCommand method params)
