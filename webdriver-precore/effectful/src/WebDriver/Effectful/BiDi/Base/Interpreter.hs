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
  ( SessionUnsubscribe (..),
    SubscriptionId,
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
        SubscribeLogEntryAdded s -> subDefault BA.subscribeLogEntryAdded s
        SubscribeLogEntryAdded' b u s -> \unlift -> BA.subscribeLogEntryAdded' sendSub' b u (unlift . s)
        -- BrowsingContext subscriptions
        SubscribeBrowsingContextCreated s -> subDefault BA.subscribeBrowsingContextCreated s
        SubscribeBrowsingContextCreated' b u s -> \unlift -> BA.subscribeBrowsingContextCreated' sendSub' b u (unlift . s)
        SubscribeBrowsingContextDestroyed s -> subDefault BA.subscribeBrowsingContextDestroyed s
        SubscribeBrowsingContextDestroyed' b u s -> \unlift -> BA.subscribeBrowsingContextDestroyed' sendSub' b u (unlift . s)
        SubscribeBrowsingContextNavigationStarted s -> subDefault BA.subscribeBrowsingContextNavigationStarted s
        SubscribeBrowsingContextNavigationStarted' b u s -> \unlift -> BA.subscribeBrowsingContextNavigationStarted' sendSub' b u (unlift . s)
        SubscribeBrowsingContextFragmentNavigated s -> subDefault BA.subscribeBrowsingContextFragmentNavigated s
        SubscribeBrowsingContextFragmentNavigated' b u s -> \unlift -> BA.subscribeBrowsingContextFragmentNavigated' sendSub' b u (unlift . s)
        SubscribeBrowsingContextHistoryUpdated s -> subDefault BA.subscribeBrowsingContextHistoryUpdated s
        SubscribeBrowsingContextHistoryUpdated' b u s -> \unlift -> BA.subscribeBrowsingContextHistoryUpdated' sendSub' b u (unlift . s)
        SubscribeBrowsingContextDomContentLoaded s -> subDefault BA.subscribeBrowsingContextDomContentLoaded s
        SubscribeBrowsingContextDomContentLoaded' b u s -> \unlift -> BA.subscribeBrowsingContextDomContentLoaded' sendSub' b u (unlift . s)
        SubscribeBrowsingContextLoad s -> subDefault BA.subscribeBrowsingContextLoad s
        SubscribeBrowsingContextLoad' b u s -> \unlift -> BA.subscribeBrowsingContextLoad' sendSub' b u (unlift . s)
        SubscribeBrowsingContextDownloadWillBegin s -> subDefault BA.subscribeBrowsingContextDownloadWillBegin s
        SubscribeBrowsingContextDownloadWillBegin' b u s -> \unlift -> BA.subscribeBrowsingContextDownloadWillBegin' sendSub' b u (unlift . s)
        SubscribeBrowsingContextDownloadEnd s -> subDefault BA.subscribeBrowsingContextDownloadEnd s
        SubscribeBrowsingContextDownloadEnd' b u s -> \unlift -> BA.subscribeBrowsingContextDownloadEnd' sendSub' b u (unlift . s)
        SubscribeBrowsingContextNavigationAborted s -> subDefault BA.subscribeBrowsingContextNavigationAborted s
        SubscribeBrowsingContextNavigationAborted' b u s -> \unlift -> BA.subscribeBrowsingContextNavigationAborted' sendSub' b u (unlift . s)
        SubscribeBrowsingContextNavigationCommitted s -> subDefault BA.subscribeBrowsingContextNavigationCommitted s
        SubscribeBrowsingContextNavigationCommitted' b u s -> \unlift -> BA.subscribeBrowsingContextNavigationCommitted' sendSub' b u (unlift . s)
        SubscribeBrowsingContextNavigationFailed s -> subDefault BA.subscribeBrowsingContextNavigationFailed s
        SubscribeBrowsingContextNavigationFailed' b u s -> \unlift -> BA.subscribeBrowsingContextNavigationFailed' sendSub' b u (unlift . s)
        SubscribeBrowsingContextUserPromptClosed s -> subDefault BA.subscribeBrowsingContextUserPromptClosed s
        SubscribeBrowsingContextUserPromptClosed' b u s -> \unlift -> BA.subscribeBrowsingContextUserPromptClosed' sendSub' b u (unlift . s)
        SubscribeBrowsingContextUserPromptOpened s -> subDefault BA.subscribeBrowsingContextUserPromptOpened s
        SubscribeBrowsingContextUserPromptOpened' b u s -> \unlift -> BA.subscribeBrowsingContextUserPromptOpened' sendSub' b u (unlift . s)
        -- Network subscriptions
        SubscribeNetworkAuthRequired s -> subDefault BA.subscribeNetworkAuthRequired s
        SubscribeNetworkAuthRequired' b u s -> \unlift -> BA.subscribeNetworkAuthRequired' sendSub' b u (unlift . s)
        SubscribeNetworkBeforeRequestSent s -> subDefault BA.subscribeNetworkBeforeRequestSent s
        SubscribeNetworkBeforeRequestSent' b u s -> \unlift -> BA.subscribeNetworkBeforeRequestSent' sendSub' b u (unlift . s)
        SubscribeNetworkFetchError s -> subDefault BA.subscribeNetworkFetchError s
        SubscribeNetworkFetchError' b u s -> \unlift -> BA.subscribeNetworkFetchError' sendSub' b u (unlift . s)
        SubscribeNetworkResponseCompleted s -> subDefault BA.subscribeNetworkResponseCompleted s
        SubscribeNetworkResponseCompleted' b u s -> \unlift -> BA.subscribeNetworkResponseCompleted' sendSub' b u (unlift . s)
        SubscribeNetworkResponseStarted s -> subDefault BA.subscribeNetworkResponseStarted s
        SubscribeNetworkResponseStarted' b u s -> \unlift -> BA.subscribeNetworkResponseStarted' sendSub' b u (unlift . s)
        -- Script subscriptions
        SubscribeScriptMessage s -> subDefault BA.subscribeScriptMessage s
        SubscribeScriptMessage' b u s -> \unlift -> BA.subscribeScriptMessage' sendSub' b u (unlift . s)
        SubscribeScriptRealmCreated s -> subDefault BA.subscribeScriptRealmCreated s
        SubscribeScriptRealmCreated' b u s -> \unlift -> BA.subscribeScriptRealmCreated' sendSub' b u (unlift . s)
        SubscribeScriptRealmDestroyed s -> subDefault BA.subscribeScriptRealmDestroyed s
        SubscribeScriptRealmDestroyed' b u s -> \unlift -> BA.subscribeScriptRealmDestroyed' sendSub' b u (unlift . s)
        -- Input subscriptions
        SubscribeInputFileDialogOpened s -> subDefault BA.subscribeInputFileDialogOpened s
        SubscribeInputFileDialogOpened' b u s -> \unlift -> BA.subscribeInputFileDialogOpened' sendSub' b u (unlift . s)
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

    sendSub :: forall c. BA.SendSub (Eff es) c
    sendSub = mkSendSub ioRunner

    sendSub' :: forall d. BA.SendSub' (Eff es) d
    sendSub' = mkSendSub' ioRunner

    sendSubMany' :: BA.SendSubMany' (Eff es)
    sendSubMany' = mkSendSubMany' ioRunner

    sendSubOffSpecMany' :: BA.SendSubOffSpecMany' (Eff es)
    sendSubOffSpecMany' = mkSendSubOffSpecMany' ioRunner

    subDefault :: forall ev localM.
      (BA.SendSub (Eff es) ev -> (ev -> Eff es ()) -> Eff es SubscriptionId)
      -> (ev -> localM ())
      -> (forall r. localM r -> Eff es r)
      -> Eff es SubscriptionId
    subDefault param s unlift = param sendSub (unlift . s)


