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
import Effectful (Eff, IOE, Limit (..), Persistence (..), UnliftStrategy (..), liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import WebDriver.Effectful.BiDi.Base.Effect
  ( BiDiInfo (..),
    WebDriverBiDi (..),
    bidiRun,
    mkSendSub,
    mkSendSub',
    mkSendSubMany',
    mkSendSubOffSpecMany',
  )
import WebDriverPreCore.BiDi.Protocol
  ( SessionUnsubscribe (..),
    mkCommand,
  )
import WebDriverPreCore.BiDi.Protocol qualified as BP
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
runWebDriverBiDi :: forall es a. IOE :> es => BiDiInfo -> Eff (WebDriverBiDi : es) a -> Eff es a
runWebDriverBiDi info = interpret $ \localEnv -> \case
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
  ScriptEvaluateNoWait p -> liftIO $ Runner.runNoWait info.biDiRunner (mkCommand BP.ScriptEvaluate p)
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
  SendBiDiCmd cmd -> liftIO $ bidiRun info cmd
  SendBiDiCmdNoWait cmd -> liftIO $ Runner.runNoWait info.biDiRunner cmd
  SendBiDiOffSpecCmd mid m ps -> liftIO $ info.biDiRunner.runOffSpecWithId mid m ps
  SendBiDiOffSpecCmdNoWait m ps -> liftIO $ Runner.runOffSpecNoWait info.biDiRunner m ps
  -- Log subscriptions
  SubscribeLogEntryAdded h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeLogEntryAdded sendSub (unlift . h)
  SubscribeLogEntryAdded' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeLogEntryAdded' sendSub' b u (unlift . h)
  -- BrowsingContext subscriptions
  SubscribeBrowsingContextCreated h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextCreated sendSub (unlift . h)
  SubscribeBrowsingContextCreated' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextCreated' sendSub' b u (unlift . h)
  SubscribeBrowsingContextDestroyed h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDestroyed sendSub (unlift . h)
  SubscribeBrowsingContextDestroyed' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDestroyed' sendSub' b u (unlift . h)
  SubscribeBrowsingContextNavigationStarted h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationStarted sendSub (unlift . h)
  SubscribeBrowsingContextNavigationStarted' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationStarted' sendSub' b u (unlift . h)
  SubscribeBrowsingContextFragmentNavigated h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextFragmentNavigated sendSub (unlift . h)
  SubscribeBrowsingContextFragmentNavigated' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextFragmentNavigated' sendSub' b u (unlift . h)
  SubscribeBrowsingContextHistoryUpdated h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextHistoryUpdated sendSub (unlift . h)
  SubscribeBrowsingContextHistoryUpdated' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextHistoryUpdated' sendSub' b u (unlift . h)
  SubscribeBrowsingContextDomContentLoaded h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDomContentLoaded sendSub (unlift . h)
  SubscribeBrowsingContextDomContentLoaded' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDomContentLoaded' sendSub' b u (unlift . h)
  SubscribeBrowsingContextLoad h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextLoad sendSub (unlift . h)
  SubscribeBrowsingContextLoad' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextLoad' sendSub' b u (unlift . h)
  SubscribeBrowsingContextDownloadWillBegin h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDownloadWillBegin sendSub (unlift . h)
  SubscribeBrowsingContextDownloadWillBegin' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDownloadWillBegin' sendSub' b u (unlift . h)
  SubscribeBrowsingContextDownloadEnd h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDownloadEnd sendSub (unlift . h)
  SubscribeBrowsingContextDownloadEnd' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextDownloadEnd' sendSub' b u (unlift . h)
  SubscribeBrowsingContextNavigationAborted h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationAborted sendSub (unlift . h)
  SubscribeBrowsingContextNavigationAborted' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationAborted' sendSub' b u (unlift . h)
  SubscribeBrowsingContextNavigationCommitted h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationCommitted sendSub (unlift . h)
  SubscribeBrowsingContextNavigationCommitted' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationCommitted' sendSub' b u (unlift . h)
  SubscribeBrowsingContextNavigationFailed h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationFailed sendSub (unlift . h)
  SubscribeBrowsingContextNavigationFailed' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextNavigationFailed' sendSub' b u (unlift . h)
  SubscribeBrowsingContextUserPromptClosed h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextUserPromptClosed sendSub (unlift . h)
  SubscribeBrowsingContextUserPromptClosed' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextUserPromptClosed' sendSub' b u (unlift . h)
  SubscribeBrowsingContextUserPromptOpened h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextUserPromptOpened sendSub (unlift . h)
  SubscribeBrowsingContextUserPromptOpened' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeBrowsingContextUserPromptOpened' sendSub' b u (unlift . h)
  -- Network subscriptions
  SubscribeNetworkAuthRequired h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkAuthRequired sendSub (unlift . h)
  SubscribeNetworkAuthRequired' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkAuthRequired' sendSub' b u (unlift . h)
  SubscribeNetworkBeforeRequestSent h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkBeforeRequestSent sendSub (unlift . h)
  SubscribeNetworkBeforeRequestSent' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkBeforeRequestSent' sendSub' b u (unlift . h)
  SubscribeNetworkFetchError h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkFetchError sendSub (unlift . h)
  SubscribeNetworkFetchError' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkFetchError' sendSub' b u (unlift . h)
  SubscribeNetworkResponseCompleted h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkResponseCompleted sendSub (unlift . h)
  SubscribeNetworkResponseCompleted' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkResponseCompleted' sendSub' b u (unlift . h)
  SubscribeNetworkResponseStarted h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkResponseStarted sendSub (unlift . h)
  SubscribeNetworkResponseStarted' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeNetworkResponseStarted' sendSub' b u (unlift . h)
  -- Script subscriptions
  SubscribeScriptMessage h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeScriptMessage sendSub (unlift . h)
  SubscribeScriptMessage' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeScriptMessage' sendSub' b u (unlift . h)
  SubscribeScriptRealmCreated h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeScriptRealmCreated sendSub (unlift . h)
  SubscribeScriptRealmCreated' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeScriptRealmCreated' sendSub' b u (unlift . h)
  SubscribeScriptRealmDestroyed h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeScriptRealmDestroyed sendSub (unlift . h)
  SubscribeScriptRealmDestroyed' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeScriptRealmDestroyed' sendSub' b u (unlift . h)
  -- Input subscriptions
  SubscribeInputFileDialogOpened h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeInputFileDialogOpened sendSub (unlift . h)
  SubscribeInputFileDialogOpened' b u h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeInputFileDialogOpened' sendSub' b u (unlift . h)
  -- Multi-event subscriptions
  SubscribeMany sts h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeMany' sendSubMany' sts [] [] (unlift . h)
  SubscribeMany' b u sts h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeMany' sendSubMany' sts b u (unlift . h)
  SubscribeOffSpecMany sts h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeOffSpecMany' sendSubOffSpecMany' sts [] [] (unlift . h)
  SubscribeOffSpecMany' b u sts h -> localUnliftIO localEnv unliftStrategy $ \unlift -> BA.subscribeOffSpecMany' sendSubOffSpecMany' sts b u (unlift . h)
  -- Unsubscribe
  Unsubscribe subId ->
    liftIO $
      Runner.unsubscribe
        info.biDiRunner.socketActions
        (bidiRun info . BA.sessionUnsubscribe)
        (UnsubscribeById {subscriptions = [subId]})
  SessionUnsubscribe unsub ->
    liftIO $
      Runner.unsubscribe
        info.biDiRunner.socketActions
        (bidiRun info . BA.sessionUnsubscribe)
        unsub
  where
    run' :: forall r. (FromJSON r) => BA.Runner IO r
    run' = bidiRun info

    run :: forall r. (FromJSON r) => (BA.Runner IO r -> IO r) -> Eff es r
    run action = liftIO $ action run'

    run1 :: forall r p. (FromJSON r) => (BA.Runner IO r -> p -> IO r) -> p -> Eff es r
    run1 action p = liftIO $ action run' p


    unliftStrategy :: UnliftStrategy
    unliftStrategy = ConcUnlift Persistent Unlimited

    sendSub :: forall c. BA.SendSub IO c
    sendSub = mkSendSub info.biDiRunner

    sendSub' :: forall d. BA.SendSub' IO d
    sendSub' = mkSendSub' info.biDiRunner

    sendSubMany' = mkSendSubMany' info.biDiRunner


    sendSubOffSpecMany' = mkSendSubOffSpecMany' info.biDiRunner



