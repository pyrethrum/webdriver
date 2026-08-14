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
  ( Command,
    SessionUnsubscribe (..),
    mkCommand,
  )
import WebDriverPreCore.BiDi.Protocol qualified as BP
import WebDriverPreCore.BiDiRunner qualified as Runner
import WebDriverPreCore.BiDiRunner (BiDiRunner)
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
runWebDriverBiDi :: forall es a. (IOE :> es, FromJSON r) => BiDiInfo -> Eff (WebDriverBiDi : es) a -> Eff es a
runWebDriverBiDi info = interpret $ \localEnv -> \case
  -- Session
  SessionNew caps -> liftIO $ BA.sessionNew run' caps
  SessionStatus -> run BA.sessionStatus
  SessionEnd -> run BA.sessionEnd
  -- BrowsingContext
  BrowsingContextActivate p -> liftIO $ BA.browsingContextActivate run' p
  BrowsingContextCaptureScreenshot p -> liftIO $ BA.browsingContextCaptureScreenshot run' p
  BrowsingContextClose p -> liftIO $ BA.browsingContextClose run' p
  BrowsingContextCreate p -> liftIO $ BA.browsingContextCreate run' p
  BrowsingContextGetTree p -> liftIO $ BA.browsingContextGetTree run' p
  BrowsingContextHandleUserPrompt p -> liftIO $ BA.browsingContextHandleUserPrompt run' p
  BrowsingContextLocateNodes p -> liftIO $ BA.browsingContextLocateNodes run' p
  BrowsingContextNavigate p -> liftIO $ BA.browsingContextNavigate run' p
  BrowsingContextPrint p -> liftIO $ BA.browsingContextPrint run' p
  BrowsingContextReload p -> liftIO $ BA.browsingContextReload run' p
  BrowsingContextSetBypassCSP p -> liftIO $ BA.browsingContextSetBypassCSP run' p
  BrowsingContextSetViewport p -> liftIO $ BA.browsingContextSetViewport run' p
  BrowsingContextStartScreencast p -> liftIO $ BA.browsingContextStartScreencast run' p
  BrowsingContextStopScreencast p -> liftIO $ BA.browsingContextStopScreencast run' p
  BrowsingContextTraverseHistory p -> liftIO $ BA.browsingContextTraverseHistory run' p
  -- Browser
  BrowserClose -> run BA.browserClose
  BrowserCreateUserContext p -> liftIO $ BA.browserCreateUserContext run' p
  BrowserGetClientWindows -> run BA.browserGetClientWindows
  BrowserGetUserContexts -> run BA.browserGetUserContexts
  BrowserRemoveUserContext p -> liftIO $ BA.browserRemoveUserContext run' p
  BrowserSetClientWindowState p -> liftIO $ BA.browserSetClientWindowState run' p
  BrowserSetDownloadBehavior p -> liftIO $ BA.browserSetDownloadBehavior run' p
  -- Emulation
  EmulationSetForcedColorsModeThemeOverride p -> liftIO $ BA.emulationSetForcedColorsModeThemeOverride run' p
  EmulationSetGeolocationOverride p -> liftIO $ BA.emulationSetGeolocationOverride run' p
  EmulationSetLocaleOverride p -> liftIO $ BA.emulationSetLocaleOverride run' p
  EmulationSetNetworkConditions p -> liftIO $ BA.emulationSetNetworkConditions run' p
  EmulationSetScreenOrientationOverride p -> liftIO $ BA.emulationSetScreenOrientationOverride run' p
  EmulationSetScreenSettingsOverride p -> liftIO $ BA.emulationSetScreenSettingsOverride run' p
  EmulationSetScriptingEnabled p -> liftIO $ BA.emulationSetScriptingEnabled run' p
  EmulationSetScrollbarTypeOverride p -> liftIO $ BA.emulationSetScrollbarTypeOverride run' p
  EmulationSetTimezoneOverride p -> liftIO $ BA.emulationSetTimezoneOverride run' p
  EmulationSetTouchOverride p -> liftIO $ BA.emulationSetTouchOverride run' p
  EmulationSetUserAgentOverride p -> liftIO $ BA.emulationSetUserAgentOverride run' p
  -- Input
  InputPerformActions p -> liftIO $ BA.inputPerformActions run' p
  InputReleaseActions p -> liftIO $ BA.inputReleaseActions run' p
  InputSetFiles p -> liftIO $ BA.inputSetFiles run' p
  -- Network
  NetworkAddDataCollector p -> liftIO $ BA.networkAddDataCollector run' p
  NetworkAddIntercept p -> liftIO $ BA.networkAddIntercept run' p
  NetworkContinueRequest p -> liftIO $ BA.networkContinueRequest run' p
  NetworkContinueResponse p -> liftIO $ BA.networkContinueResponse run' p
  NetworkContinueWithAuth p -> liftIO $ BA.networkContinueWithAuth run' p
  NetworkDisownData p -> liftIO $ BA.networkDisownData run' p
  NetworkFailRequest p -> liftIO $ BA.networkFailRequest run' p
  NetworkGetData p -> liftIO $ BA.networkGetData run' p
  NetworkProvideResponse p -> liftIO $ BA.networkProvideResponse run' p
  NetworkRemoveDataCollector p -> liftIO $ BA.networkRemoveDataCollector run' p
  NetworkRemoveIntercept p -> liftIO $ BA.networkRemoveIntercept run' p
  NetworkSetCacheBehavior p -> liftIO $ BA.networkSetCacheBehavior run' p
  NetworkSetExtraHeaders p -> liftIO $ BA.networkSetExtraHeaders run' p
  -- Script
  ScriptAddPreloadScript p -> liftIO $ BA.scriptAddPreloadScript run' p
  ScriptCallFunction p -> liftIO $ BA.scriptCallFunction run' p
  ScriptDisown p -> liftIO $ BA.scriptDisown run' p
  ScriptEvaluate p -> liftIO $ BA.scriptEvaluate run' p
  ScriptEvaluateNoWait p -> liftIO $ Runner.runNoWait info.biDiRunner (mkCommand BP.ScriptEvaluate p)
  ScriptGetRealms p -> liftIO $ BA.scriptGetRealms run' p
  ScriptRemovePreloadScript p -> liftIO $ BA.scriptRemovePreloadScript run' p
  -- Storage
  StorageDeleteCookies p -> liftIO $ BA.storageDeleteCookies run' p
  StorageGetCookies p -> liftIO $ BA.storageGetCookies run' p
  StorageSetCookie p -> liftIO $ BA.storageSetCookie run' p
  -- WebExtension
  WebExtensionInstall p -> liftIO $ BA.webExtensionInstall run' p
  WebExtensionUninstall p -> liftIO $ BA.webExtensionUninstall run' p
  -- Generic escape hatches
  SendBiDiCmd cmd -> liftIO $ bidiRun info cmd
  SendBiDiCmdNoWait cmd -> liftIO $ Runner.runNoWait info.biDiRunner cmd
  SendBiDiOffSpecCmd mid m ps -> liftIO $ info.biDiRunner.runOffSpecWithId mid m ps
  SendBiDiOffSpecCmdNoWait m ps -> liftIO $ Runner.runOffSpecNoWait info.biDiRunner m ps
  -- Log subscriptions
  SubscribeLogEntryAdded h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeLogEntryAdded (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeLogEntryAdded' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeLogEntryAdded' (mkSendSub' info.biDiRunner) b u (unlift . h)
  -- BrowsingContext subscriptions
  SubscribeBrowsingContextCreated h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextCreated (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextCreated' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextCreated' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextDestroyed h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDestroyed (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextDestroyed' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDestroyed' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextNavigationStarted h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationStarted (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextNavigationStarted' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationStarted' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextFragmentNavigated h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextFragmentNavigated (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextFragmentNavigated' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextFragmentNavigated' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextHistoryUpdated h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextHistoryUpdated (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextHistoryUpdated' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextHistoryUpdated' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextDomContentLoaded h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDomContentLoaded (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextDomContentLoaded' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDomContentLoaded' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextLoad h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextLoad (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextLoad' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextLoad' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextDownloadWillBegin h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDownloadWillBegin (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextDownloadWillBegin' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDownloadWillBegin' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextDownloadEnd h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDownloadEnd (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextDownloadEnd' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextDownloadEnd' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextNavigationAborted h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationAborted (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextNavigationAborted' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationAborted' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextNavigationCommitted h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationCommitted (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextNavigationCommitted' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationCommitted' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextNavigationFailed h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationFailed (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextNavigationFailed' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextNavigationFailed' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextUserPromptClosed h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextUserPromptClosed (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextUserPromptClosed' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextUserPromptClosed' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeBrowsingContextUserPromptOpened h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextUserPromptOpened (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeBrowsingContextUserPromptOpened' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeBrowsingContextUserPromptOpened' (mkSendSub' info.biDiRunner) b u (unlift . h)
  -- Network subscriptions
  SubscribeNetworkAuthRequired h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkAuthRequired (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeNetworkAuthRequired' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkAuthRequired' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeNetworkBeforeRequestSent h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkBeforeRequestSent (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeNetworkBeforeRequestSent' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkBeforeRequestSent' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeNetworkFetchError h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkFetchError (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeNetworkFetchError' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkFetchError' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeNetworkResponseCompleted h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkResponseCompleted (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeNetworkResponseCompleted' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkResponseCompleted' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeNetworkResponseStarted h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkResponseStarted (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeNetworkResponseStarted' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeNetworkResponseStarted' (mkSendSub' info.biDiRunner) b u (unlift . h)
  -- Script subscriptions
  SubscribeScriptMessage h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeScriptMessage (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeScriptMessage' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeScriptMessage' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeScriptRealmCreated h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeScriptRealmCreated (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeScriptRealmCreated' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeScriptRealmCreated' (mkSendSub' info.biDiRunner) b u (unlift . h)
  SubscribeScriptRealmDestroyed h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeScriptRealmDestroyed (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeScriptRealmDestroyed' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeScriptRealmDestroyed' (mkSendSub' info.biDiRunner) b u (unlift . h)
  -- Input subscriptions
  SubscribeInputFileDialogOpened h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeInputFileDialogOpened (mkSendSub info.biDiRunner) (unlift . h)
  SubscribeInputFileDialogOpened' b u h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeInputFileDialogOpened' (mkSendSub' info.biDiRunner) b u (unlift . h)
  -- Multi-event subscriptions
  SubscribeMany sts h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeMany' (mkSendSubMany' info.biDiRunner) sts [] [] (unlift . h)
  SubscribeMany' b u sts h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeMany' (mkSendSubMany' info.biDiRunner) sts b u (unlift . h)
  SubscribeOffSpecMany sts h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeOffSpecMany' (mkSendSubOffSpecMany' info.biDiRunner) sts [] [] (unlift . h)
  SubscribeOffSpecMany' b u sts h -> localUnliftIO localEnv (ConcUnlift Persistent Unlimited) $ \unlift -> BA.subscribeOffSpecMany' (mkSendSubOffSpecMany' info.biDiRunner) sts b u (unlift . h)
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
    
    run2 :: forall r p1 p2. (FromJSON r) => (BA.Runner IO r -> p1 -> p2 -> IO r) -> p1 -> p2 -> Eff es r
    run2 action p1 p2 = liftIO $ action run' p1 p2
