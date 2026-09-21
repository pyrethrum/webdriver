-- |
-- Module: WebDriver.Effectful.BiDi.Base.Interpreter
-- Description: IO-backed interpreter for the 'WebDriverBiDi' effect
--
-- Provides 'runWebDriverBiDi', which interprets the 'WebDriverBiDi'
-- algebraic effect by dispatching commands and registering event
-- subscriptions via the 'BiDiRunner' held in 'BiDiInfo'.
module WebDriver.Effectful.BiDi.Base.Interpreter
  ( runWebDriverBiDi,
  )
where

import Data.Aeson (FromJSON)
import Effectful (Eff, IOE, Limit (..), Persistence (..), UnliftStrategy (..), (:>))
import Effectful.Dispatch.Dynamic (interpret, localUnlift)
import WebDriver.Effectful.BiDi.Base.Effect
  ( WebDriverBiDi (..),
    bidiRun,
    mkSendSub,
    mkSendSub',
    mkSendSubMany',
    mkSendSubOffSpecMany',
  )
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext,
    SessionUnsubscribe (..),
    SubscriptionId,
    UserContext,
    mkCommand,
  )
import WebDriverPreCore.BiDi.Protocol qualified as BP
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.BiDiRunner qualified as Runner
import WebDriverPreCore.Extended.BiDi.Base.Actions qualified as BA

-- ---------------------------------------------------------------------------
-- BiDi interpreter
-- ---------------------------------------------------------------------------

-- | Interpret the 'WebDriverBiDi' effect by dispatching commands and
-- registering subscriptions via the 'BiDiRunner' in 'BiDiInfo'.
--
-- The interpreter maps each effect constructor to the corresponding
-- @WebDriverPreCore.Extended.BiDi.Base.Actions@ function, using the same
-- subscription helper pattern as the Bluefin POC.
runWebDriverBiDi :: forall es a. (IOE :> es) => BiDiRunner (Eff es) -> Eff (WebDriverBiDi : es) a -> Eff es a
runWebDriverBiDi ioRunner = interpret $ \localEnv ->
  \case
    -- Session
    SessionNew caps -> run1 BA.sessionNew caps
    SessionStatus -> run BA.sessionStatus
    SessionEnd -> run BA.sessionEnd
    -- BrowsingContext
    BrowsingContextActivate p -> run1 BA.browsingContextActivate p
    BrowsingContextCaptureScreenshot p -> run1 BA.browsingContextCaptureScreenshot p
    BrowsingContextClose p -> run1 BA.browsingContextClose p
    BrowsingContextCreate p -> run1 BA.browsingContextCreate p
    BrowsingContextGetTree p -> run1 BA.browsingContextGetTree p
    BrowsingContextHandleUserPrompt p -> run1 BA.browsingContextHandleUserPrompt p
    BrowsingContextLocateNodes p -> run1 BA.browsingContextLocateNodes p
    BrowsingContextNavigate p -> run1 BA.browsingContextNavigate p
    BrowsingContextPrint p -> run1 BA.browsingContextPrint p
    BrowsingContextReload p -> run1 BA.browsingContextReload p
    BrowsingContextSetBypassCSP p -> run1 BA.browsingContextSetBypassCSP p
    BrowsingContextSetViewport p -> run1 BA.browsingContextSetViewport p
    BrowsingContextStartScreencast p -> run1 BA.browsingContextStartScreencast p
    BrowsingContextStopScreencast p -> run1 BA.browsingContextStopScreencast p
    BrowsingContextTraverseHistory p -> run1 BA.browsingContextTraverseHistory p
    -- Browser
    BrowserClose -> run BA.browserClose
    BrowserCreateUserContext p -> run1 BA.browserCreateUserContext p
    BrowserGetClientWindows -> run BA.browserGetClientWindows
    BrowserGetUserContexts -> run BA.browserGetUserContexts
    BrowserRemoveUserContext p -> run1 BA.browserRemoveUserContext p
    BrowserSetClientWindowState p -> run1 BA.browserSetClientWindowState p
    BrowserSetDownloadBehavior p -> run1 BA.browserSetDownloadBehavior p
    -- Emulation
    EmulationSetForcedColorsModeThemeOverride p -> run1 BA.emulationSetForcedColorsModeThemeOverride p
    EmulationSetGeolocationOverride p -> run1 BA.emulationSetGeolocationOverride p
    EmulationSetLocaleOverride p -> run1 BA.emulationSetLocaleOverride p
    EmulationSetNetworkConditions p -> run1 BA.emulationSetNetworkConditions p
    EmulationSetScreenOrientationOverride p -> run1 BA.emulationSetScreenOrientationOverride p
    EmulationSetScreenSettingsOverride p -> run1 BA.emulationSetScreenSettingsOverride p
    EmulationSetScriptingEnabled p -> run1 BA.emulationSetScriptingEnabled p
    EmulationSetScrollbarTypeOverride p -> run1 BA.emulationSetScrollbarTypeOverride p
    EmulationSetTimezoneOverride p -> run1 BA.emulationSetTimezoneOverride p
    EmulationSetTouchOverride p -> run1 BA.emulationSetTouchOverride p
    EmulationSetUserAgentOverride p -> run1 BA.emulationSetUserAgentOverride p
    -- Input
    InputPerformActions p -> run1 BA.inputPerformActions p
    InputReleaseActions p -> run1 BA.inputReleaseActions p
    InputSetFiles p -> run1 BA.inputSetFiles p
    -- Network
    NetworkAddDataCollector p -> run1 BA.networkAddDataCollector p
    NetworkAddIntercept p -> run1 BA.networkAddIntercept p
    NetworkContinueRequest p -> run1 BA.networkContinueRequest p
    NetworkContinueResponse p -> run1 BA.networkContinueResponse p
    NetworkContinueWithAuth p -> run1 BA.networkContinueWithAuth p
    NetworkDisownData p -> run1 BA.networkDisownData p
    NetworkFailRequest p -> run1 BA.networkFailRequest p
    NetworkGetData p -> run1 BA.networkGetData p
    NetworkProvideResponse p -> run1 BA.networkProvideResponse p
    NetworkRemoveDataCollector p -> run1 BA.networkRemoveDataCollector p
    NetworkRemoveIntercept p -> run1 BA.networkRemoveIntercept p
    NetworkSetCacheBehavior p -> run1 BA.networkSetCacheBehavior p
    NetworkSetExtraHeaders p -> run1 BA.networkSetExtraHeaders p
    -- Script
    ScriptAddPreloadScript p -> run1 BA.scriptAddPreloadScript p
    ScriptCallFunction p -> run1 BA.scriptCallFunction p
    ScriptDisown p -> run1 BA.scriptDisown p
    ScriptEvaluate p -> run1 BA.scriptEvaluate p
    ScriptEvaluateNoWait p -> Runner.runNoWait ioRunner (mkCommand BP.ScriptEvaluate p)
    ScriptGetRealms p -> run1 BA.scriptGetRealms p
    ScriptRemovePreloadScript p -> run1 BA.scriptRemovePreloadScript p
    -- Storage
    StorageDeleteCookies p -> run1 BA.storageDeleteCookies p
    StorageGetCookies p -> run1 BA.storageGetCookies p
    StorageSetCookie p -> run1 BA.storageSetCookie p
    -- WebExtension
    WebExtensionInstall p -> run1 BA.webExtensionInstall p
    WebExtensionUninstall p -> run1 BA.webExtensionUninstall p
    -- Generic escape hatches
    SendBiDiCmd cmd -> bidiRun ioRunner cmd
    SendBiDiCmdNoWait cmd -> Runner.runNoWait ioRunner cmd
    SendBiDiOffSpecCmd mid m ps -> ioRunner.runOffSpecWithId mid m ps
    SendBiDiOffSpecCmdNoWait m ps -> Runner.runOffSpecNoWait ioRunner m ps
    -- Unsubscribe
    Unsubscribe subId ->
      Runner.unsubscribe
        ioRunner.socketActions
        (run' . BA.sessionUnsubscribe)
        (UnsubscribeById {subscriptions = [subId]})
    SessionUnsubscribe unsub ->
      Runner.unsubscribe
        ioRunner.socketActions
        (run' . BA.sessionUnsubscribe)
        unsub
    -- Subscriptions
    subscription -> localUnlift localEnv (ConcUnlift Persistent Unlimited) $
      case subscription of
        -- Log subscriptions
        SubscribeLogEntryAdded s -> sub BA.subscribeLogEntryAdded s
        SubscribeLogEntryAdded' b u s -> subWithContexts BA.subscribeLogEntryAdded' b u s
        -- BrowsingContext subscriptions
        SubscribeBrowsingContextCreated s -> sub BA.subscribeBrowsingContextCreated s
        SubscribeBrowsingContextCreated' b u s -> subWithContexts BA.subscribeBrowsingContextCreated' b u s
        SubscribeBrowsingContextDestroyed s -> sub BA.subscribeBrowsingContextDestroyed s
        SubscribeBrowsingContextDestroyed' b u s -> subWithContexts BA.subscribeBrowsingContextDestroyed' b u s
        SubscribeBrowsingContextNavigationStarted s -> sub BA.subscribeBrowsingContextNavigationStarted s
        SubscribeBrowsingContextNavigationStarted' b u s -> subWithContexts BA.subscribeBrowsingContextNavigationStarted' b u s
        SubscribeBrowsingContextFragmentNavigated s -> sub BA.subscribeBrowsingContextFragmentNavigated s
        SubscribeBrowsingContextFragmentNavigated' b u s -> subWithContexts BA.subscribeBrowsingContextFragmentNavigated' b u s
        SubscribeBrowsingContextHistoryUpdated s -> sub BA.subscribeBrowsingContextHistoryUpdated s
        SubscribeBrowsingContextHistoryUpdated' b u s -> subWithContexts BA.subscribeBrowsingContextHistoryUpdated' b u s
        SubscribeBrowsingContextDomContentLoaded s -> sub BA.subscribeBrowsingContextDomContentLoaded s
        SubscribeBrowsingContextDomContentLoaded' b u s -> subWithContexts BA.subscribeBrowsingContextDomContentLoaded' b u s
        SubscribeBrowsingContextLoad s -> sub BA.subscribeBrowsingContextLoad s
        SubscribeBrowsingContextLoad' b u s -> subWithContexts BA.subscribeBrowsingContextLoad' b u s
        SubscribeBrowsingContextDownloadWillBegin s -> sub BA.subscribeBrowsingContextDownloadWillBegin s
        SubscribeBrowsingContextDownloadWillBegin' b u s -> subWithContexts BA.subscribeBrowsingContextDownloadWillBegin' b u s
        SubscribeBrowsingContextDownloadEnd s -> sub BA.subscribeBrowsingContextDownloadEnd s
        SubscribeBrowsingContextDownloadEnd' b u s -> subWithContexts BA.subscribeBrowsingContextDownloadEnd' b u s
        SubscribeBrowsingContextNavigationAborted s -> sub BA.subscribeBrowsingContextNavigationAborted s
        SubscribeBrowsingContextNavigationAborted' b u s -> subWithContexts BA.subscribeBrowsingContextNavigationAborted' b u s
        SubscribeBrowsingContextNavigationCommitted s -> sub BA.subscribeBrowsingContextNavigationCommitted s
        SubscribeBrowsingContextNavigationCommitted' b u s -> subWithContexts BA.subscribeBrowsingContextNavigationCommitted' b u s
        SubscribeBrowsingContextNavigationFailed s -> sub BA.subscribeBrowsingContextNavigationFailed s
        SubscribeBrowsingContextNavigationFailed' b u s -> subWithContexts BA.subscribeBrowsingContextNavigationFailed' b u s
        SubscribeBrowsingContextUserPromptClosed s -> sub BA.subscribeBrowsingContextUserPromptClosed s
        SubscribeBrowsingContextUserPromptClosed' b u s -> subWithContexts BA.subscribeBrowsingContextUserPromptClosed' b u s
        SubscribeBrowsingContextUserPromptOpened s -> sub BA.subscribeBrowsingContextUserPromptOpened s
        SubscribeBrowsingContextUserPromptOpened' b u s -> subWithContexts BA.subscribeBrowsingContextUserPromptOpened' b u s
        -- Network subscriptions
        SubscribeNetworkAuthRequired s -> sub BA.subscribeNetworkAuthRequired s
        SubscribeNetworkAuthRequired' b u s -> subWithContexts BA.subscribeNetworkAuthRequired' b u s
        SubscribeNetworkBeforeRequestSent s -> sub BA.subscribeNetworkBeforeRequestSent s
        SubscribeNetworkBeforeRequestSent' b u s -> subWithContexts BA.subscribeNetworkBeforeRequestSent' b u s
        SubscribeNetworkFetchError s -> sub BA.subscribeNetworkFetchError s
        SubscribeNetworkFetchError' b u s -> subWithContexts BA.subscribeNetworkFetchError' b u s
        SubscribeNetworkResponseCompleted s -> sub BA.subscribeNetworkResponseCompleted s
        SubscribeNetworkResponseCompleted' b u s -> subWithContexts BA.subscribeNetworkResponseCompleted' b u s
        SubscribeNetworkResponseStarted s -> sub BA.subscribeNetworkResponseStarted s
        SubscribeNetworkResponseStarted' b u s -> subWithContexts BA.subscribeNetworkResponseStarted' b u s
        -- Script subscriptions
        SubscribeScriptMessage s -> sub BA.subscribeScriptMessage s
        SubscribeScriptMessage' b u s -> subWithContexts BA.subscribeScriptMessage' b u s
        SubscribeScriptRealmCreated s -> sub BA.subscribeScriptRealmCreated s
        SubscribeScriptRealmCreated' b u s -> subWithContexts BA.subscribeScriptRealmCreated' b u s
        SubscribeScriptRealmDestroyed s -> sub BA.subscribeScriptRealmDestroyed s
        SubscribeScriptRealmDestroyed' b u s -> subWithContexts BA.subscribeScriptRealmDestroyed' b u s
        -- Input subscriptions
        SubscribeInputFileDialogOpened s -> sub BA.subscribeInputFileDialogOpened s
        -- SubscribeInputFileDialogOpened' b u s -> \unlift -> BA.subscribeInputFileDialogOpened' sendSub' b u (unlift . s)
        SubscribeInputFileDialogOpened' b u s -> subWithContexts BA.subscribeInputFileDialogOpened' b u s
        -- Multi-event subscriptions
        SubscribeMany sts s -> \unlift -> BA.subscribeMany' sendSubMany' sts [] [] (unlift . s)
        SubscribeMany' b u sts s -> \unlift -> BA.subscribeMany' sendSubMany' sts b u (unlift . s)
        SubscribeOffSpecMany sts s -> \unlift -> BA.subscribeOffSpecMany' sendSubOffSpecMany' sts [] [] (unlift . s)
        SubscribeOffSpecMany' b u sts s -> \unlift -> BA.subscribeOffSpecMany' sendSubOffSpecMany' sts b u (unlift . s)
  where
    run' :: forall r. (FromJSON r) => BA.Runner (Eff es) r
    run' = bidiRun ioRunner

    run :: forall r. (FromJSON r) => (BA.Runner (Eff es) r -> Eff es r) -> Eff es r
    run action = action run'

    run1 :: forall r p. (FromJSON r) => (BA.Runner (Eff es) r -> p -> Eff es r) -> p -> Eff es r
    run1 action p = action run' p

    sendSubMany' :: BA.SendSubMany' (Eff es)
    sendSubMany' = mkSendSubMany' ioRunner

    sendSubOffSpecMany' :: BA.SendSubOffSpecMany' (Eff es)
    sendSubOffSpecMany' = mkSendSubOffSpecMany' ioRunner

    sub ::
      forall ev localM.
      (BA.SendSub (Eff es) ev -> (ev -> Eff es ()) -> Eff es SubscriptionId) ->
      (ev -> localM ()) ->
      (forall r. localM r -> Eff es r) ->
      Eff es SubscriptionId
    sub sender s unlift = sender (mkSendSub ioRunner) (unlift . s)

    subWithContexts ::
      forall ev localM.
      (BA.SendSub' (Eff es) ev -> [BrowsingContext] -> [UserContext] -> (ev -> Eff es ()) -> Eff es SubscriptionId) ->
      [BrowsingContext] ->
      [UserContext] ->
      (ev -> localM ()) ->
      (forall r. localM r -> Eff es r) ->
      Eff es SubscriptionId
    subWithContexts sender b u s unlift = sender (mkSendSub' ioRunner) b u (unlift . s)
