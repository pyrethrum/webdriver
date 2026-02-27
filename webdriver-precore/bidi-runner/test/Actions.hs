{-|
BiDi Actions module for webdriver-precore-bidi-runner tests

This module provides a Actions interface that wraps the BiDiRunner,
making it compatible with demos migrated from the test suite.
-}
module Actions
  ( Actions (..),
    mkActions,
  )
where

import Data.Aeson (FromJSON, Object, Value, toJSON)
import Data.Coerce (coerce)
import Data.Text (Text)
import WebDriverPreCore.BiDiRunnerBase.Types (Request)
import WebDriverPreCore.BiDiRunnerBase.Socket qualified as Socket
import WebDriverPreCore.BiDiRunnerBase qualified as Base
import WebDriverPreCore.BiDi.Protocol
  ( AddDataCollector,
    AddDataCollectorResult,
    AddIntercept,
    AddInterceptResult,
    AddPreloadScript,
    AddPreloadScriptResult,
    Activate,
    AuthRequired,
    BeforeRequestSent,
    CommandMethod (..),
    OffSpecCommand (..),
    Subscription,
    knownCommandToText,
    BrowsingContext,
    CallFunction,
    CaptureScreenshot,
    CaptureScreenshotResult,
    ClientWindowInfo,
    Close,
    Command (..),
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
    GetCookies,
    GetCookiesResult,
    GetData,
    GetDataResult,
    GetRealms,
    GetRealmsResult,
    GetTree,
    GetTreeResult,
    GetUserContextsResult,
    GetClientWindowsResult,
    HandleUserPrompt,
    HistoryUpdated,
    Info,
    JSUInt (..),
    KnownSubscriptionType (..),
    LocateNodes,
    LocateNodesResult,
    LogEntry,
    Message,
    Navigate,
    NavigateResult,
    OffSpecSubscriptionType,
    NavigationInfo,
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
    SessionSubscribeResult (..),
    SubscriptionId (..),
    TraverseHistory,
    UserContext,
    UserPromptClosed,
    UserPromptOpened,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
    Capabilities,
  )
import WebDriverPreCore.BiDi.API qualified as API
import WebDriverPreCore.BiDiRunner (BiDiRunner (..))
import WebDriverPreCore.BiDiRunner qualified as Runner

-- | Extract subscription ID from result
extractSubscription :: SessionSubscribeResult -> SubscriptionId
extractSubscription (MkSessionSubscribeResult {subscription}) = subscription

-- | BiDi actions interface - wraps BiDiRunner with named functions for each command
data Actions = MkActions
  { -- Session commands
    sessionNew :: Capabilities -> IO SessionNewResult,
    sessionStatus :: IO SessionStatusResult,
    sessionEnd :: IO (),
    sessionSubscribe :: SessionSubscibe -> IO SubscriptionId,
    sessionUnsubscribe :: SessionUnsubscribe -> IO (),
    -- BrowsingContext commands
    browsingContextActivate :: Activate -> IO (),
    browsingContextCaptureScreenshot :: CaptureScreenshot -> IO CaptureScreenshotResult,
    browsingContextClose :: Close -> IO (),
    browsingContextCreate :: Create -> IO BrowsingContext,
    browsingContextGetTree :: GetTree -> IO GetTreeResult,
    browsingContextHandleUserPrompt :: HandleUserPrompt -> IO (),
    browsingContextLocateNodes :: LocateNodes -> IO LocateNodesResult,
    browsingContextNavigate :: Navigate -> IO NavigateResult,
    browsingContextPrint :: Print -> IO PrintResult,
    browsingContextReload :: Reload -> IO (),
    browsingContextSetViewport :: SetViewport -> IO (),
    browsingContextTraverseHistory :: TraverseHistory -> IO (),
    -- Browser commands
    browserClose :: IO (),
    browserCreateUserContext :: CreateUserContext -> IO UserContext,
    browserGetClientWindows :: IO GetClientWindowsResult,
    browserGetUserContexts :: IO GetUserContextsResult,
    browserRemoveUserContext :: RemoveUserContext -> IO (),
    browserSetClientWindowState :: SetClientWindowState -> IO ClientWindowInfo,
    browserSetDownloadBehavior :: SetDownloadBehavior -> IO (),
    -- Emulation commands
    emulationSetForcedColorsModeThemeOverride :: SetForcedColorsModeThemeOverride -> IO (),
    emulationSetGeolocationOverride :: SetGeolocationOverride -> IO (),
    emulationSetLocaleOverride :: SetLocaleOverride -> IO (),
    emulationSetNetworkConditions :: SetNetworkConditions -> IO (),
    emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> IO (),
    emulationSetScreenSettingsOverride :: SetScreenSettingsOverride -> IO (),
    emulationSetScriptingEnabled :: SetScriptingEnabled -> IO (),
    emulationSetTimezoneOverride :: SetTimezoneOverride -> IO (),
    emulationSetTouchOverride :: SetTouchOverride -> IO (),
    emulationSetUserAgentOverride :: SetUserAgentOverride -> IO (),
    -- Input commands
    inputPerformActions :: PerformActions -> IO (),
    inputReleaseActions :: ReleaseActions -> IO (),
    inputSetFiles :: SetFiles -> IO (),
    -- Network commands
    networkAddDataCollector :: AddDataCollector -> IO AddDataCollectorResult,
    networkAddIntercept :: AddIntercept -> IO AddInterceptResult,
    networkContinueRequest :: ContinueRequest -> IO (),
    networkContinueResponse :: ContinueResponse -> IO (),
    networkContinueWithAuth :: ContinueWithAuth -> IO (),
    networkDisownData :: DisownData -> IO (),
    networkFailRequest :: FailRequest -> IO (),
    networkGetData :: GetData -> IO GetDataResult,
    networkProvideResponse :: ProvideResponse -> IO (),
    networkRemoveDataCollector :: RemoveDataCollector -> IO (),
    networkRemoveIntercept :: RemoveIntercept -> IO (),
    networkSetCacheBehavior :: SetCacheBehavior -> IO (),
    networkSetExtraHeaders :: SetExtraHeaders -> IO (),
    -- Script commands
    scriptAddPreloadScript :: AddPreloadScript -> IO AddPreloadScriptResult,
    scriptCallFunction :: CallFunction -> IO EvaluateResult,
    scriptDisown :: Disown -> IO (),
    scriptEvaluate :: Evaluate -> IO EvaluateResult,
    scriptEvaluateNoWait :: Evaluate -> IO Request,
    scriptGetRealms :: GetRealms -> IO GetRealmsResult,
    scriptRemovePreloadScript :: RemovePreloadScript -> IO (),
    -- Storage commands
    storageDeleteCookies :: DeleteCookies -> IO DeleteCookiesResult,
    storageGetCookies :: GetCookies -> IO GetCookiesResult,
    storageSetCookie :: SetCookie -> IO SetCookieResult,
    -- WebExtension commands
    webExtensionInstall :: WebExtensionInstall -> IO WebExtensionResult,
    webExtensionUninstall :: WebExtensionUninstall -> IO (),
    -- Subscription methods
    subscribeMany :: [KnownSubscriptionType] -> (Event -> IO ()) -> IO SubscriptionId,
    subscribeMany' :: [BrowsingContext] -> [UserContext] -> [KnownSubscriptionType] -> (Event -> IO ()) -> IO SubscriptionId,
    -- Log
    subscribeLogEntryAdded :: (LogEntry -> IO ()) -> IO SubscriptionId,
    subscribeLogEntryAdded' :: [BrowsingContext] -> [UserContext] -> (LogEntry -> IO ()) -> IO SubscriptionId,
    -- BrowsingContext
    subscribeBrowsingContextCreated :: (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextCreated' :: [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDestroyed :: (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDestroyed' :: [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationStarted :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationStarted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextFragmentNavigated :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextFragmentNavigated' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextHistoryUpdated :: (HistoryUpdated -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextHistoryUpdated' :: [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDomContentLoaded :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDomContentLoaded' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextLoad :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextLoad' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadWillBegin :: (DownloadWillBegin -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadWillBegin' :: [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadEnd :: (DownloadEnd -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextDownloadEnd' :: [BrowsingContext] -> [UserContext] -> (DownloadEnd -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationAborted :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationAborted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationCommitted :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationCommitted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationFailed :: (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextNavigationFailed' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptClosed :: (UserPromptClosed -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptClosed' :: [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptOpened :: (UserPromptOpened -> IO ()) -> IO SubscriptionId,
    subscribeBrowsingContextUserPromptOpened' :: [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> IO ()) -> IO SubscriptionId,
    -- Network
    subscribeNetworkAuthRequired :: (AuthRequired -> IO ()) -> IO SubscriptionId,
    subscribeNetworkAuthRequired' :: [BrowsingContext] -> [UserContext] -> (AuthRequired -> IO ()) -> IO SubscriptionId,
    subscribeNetworkBeforeRequestSent :: (BeforeRequestSent -> IO ()) -> IO SubscriptionId,
    subscribeNetworkBeforeRequestSent' :: [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> IO ()) -> IO SubscriptionId,
    subscribeNetworkFetchError :: (FetchError -> IO ()) -> IO SubscriptionId,
    subscribeNetworkFetchError' :: [BrowsingContext] -> [UserContext] -> (FetchError -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseCompleted :: (ResponseCompleted -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseCompleted' :: [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseStarted :: (ResponseStarted -> IO ()) -> IO SubscriptionId,
    subscribeNetworkResponseStarted' :: [BrowsingContext] -> [UserContext] -> (ResponseStarted -> IO ()) -> IO SubscriptionId,
    -- Script
    subscribeScriptMessage :: (Message -> IO ()) -> IO SubscriptionId,
    subscribeScriptMessage' :: [BrowsingContext] -> [UserContext] -> (Message -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmCreated :: (RealmInfo -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmCreated' :: [BrowsingContext] -> [UserContext] -> (RealmInfo -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmDestroyed :: (RealmDestroyed -> IO ()) -> IO SubscriptionId,
    subscribeScriptRealmDestroyed' :: [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> IO ()) -> IO SubscriptionId,
    -- Input
    subscribeInputFileDialogOpened :: (FileDialogOpened -> IO ()) -> IO SubscriptionId,
    subscribeInputFileDialogOpened' :: [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> IO ()) -> IO SubscriptionId,
    -- Unsubscribe
    unsubscribe :: SubscriptionId -> IO (),
    -- Generic and low-level command methods
    sendCommand :: forall r. (FromJSON r) => Command r -> IO r,
    sendCommand' :: forall r. (FromJSON r) => JSUInt -> Command r -> IO r,
    sendCommandNoWait :: forall r. Command r -> IO Request,
    sendOffSpecCommand' :: JSUInt -> Text -> Object -> IO Object,
    sendOffSpecCommandNoWait :: Text -> Object -> IO Request,
    -- fallback subscriptions
    subscribeUnknownMany ::
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId,
    subscribeUnknownMany' ::
      [BrowsingContext] ->
      [UserContext] ->
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId
  }

-- | Create Actions from a BiDiRunner
mkActions :: BiDiRunner IO -> Actions
mkActions (MkBiDiRunner {run, socketActions}) =
  MkActions
    { -- Session commands
      sessionNew = run . API.sessionNew,
      sessionStatus = run API.sessionStatus,
      sessionEnd = run API.sessionEnd,
      sessionSubscribe = fmap extractSubscription . sessionSubscribe',
      sessionUnsubscribe = sessionUnsubscribe',
      -- BrowsingContext commands
      browsingContextActivate = run . API.browsingContextActivate,
      browsingContextCaptureScreenshot = run . API.browsingContextCaptureScreenshot,
      browsingContextClose = run . API.browsingContextClose,
      browsingContextCreate = run . API.browsingContextCreate,
      browsingContextGetTree = run . API.browsingContextGetTree,
      browsingContextHandleUserPrompt = run . API.browsingContextHandleUserPrompt,
      browsingContextLocateNodes = run . API.browsingContextLocateNodes,
      browsingContextNavigate = run . API.browsingContextNavigate,
      browsingContextPrint = run . API.browsingContextPrint,
      browsingContextReload = run . API.browsingContextReload,
      browsingContextSetViewport = run . API.browsingContextSetViewport,
      browsingContextTraverseHistory = run . API.browsingContextTraverseHistory,
      -- Browser commands
      browserClose = run API.browserClose,
      browserCreateUserContext = run . API.browserCreateUserContext,
      browserGetClientWindows = run API.browserGetClientWindows,
      browserGetUserContexts = run API.browserGetUserContexts,
      browserRemoveUserContext = run . API.browserRemoveUserContext,
      browserSetClientWindowState = run . API.browserSetClientWindowState,
      browserSetDownloadBehavior = run . API.browserSetDownloadBehavior,
      -- Emulation commands
      emulationSetForcedColorsModeThemeOverride = run . API.emulationSetForcedColorsModeThemeOverride,
      emulationSetGeolocationOverride = run . API.emulationSetGeolocationOverride,
      emulationSetLocaleOverride = run . API.emulationSetLocaleOverride,
      emulationSetNetworkConditions = run . API.emulationSetNetworkConditions,
      emulationSetScreenOrientationOverride = run . API.emulationSetScreenOrientationOverride,
      emulationSetScreenSettingsOverride = run . API.emulationSetScreenSettingsOverride,
      emulationSetScriptingEnabled = run . API.emulationSetScriptingEnabled,
      emulationSetTimezoneOverride = run . API.emulationSetTimezoneOverride,
      emulationSetTouchOverride = run . API.emulationSetTouchOverride,
      emulationSetUserAgentOverride = run . API.emulationSetUserAgentOverride,
      -- Input commands
      inputPerformActions = run . API.inputPerformActions,
      inputReleaseActions = run . API.inputReleaseActions,
      inputSetFiles = run . API.inputSetFiles,
      -- Network commands
      networkAddDataCollector = run . API.networkAddDataCollector,
      networkAddIntercept = run . API.networkAddIntercept,
      networkContinueRequest = run . API.networkContinueRequest,
      networkContinueResponse = run . API.networkContinueResponse,
      networkContinueWithAuth = run . API.networkContinueWithAuth,
      networkDisownData = run . API.networkDisownData,
      networkFailRequest = run . API.networkFailRequest,
      networkGetData = run . API.networkGetData,
      networkProvideResponse = run . API.networkProvideResponse,
      networkRemoveDataCollector = run . API.networkRemoveDataCollector,
      networkRemoveIntercept = run . API.networkRemoveIntercept,
      networkSetCacheBehavior = run . API.networkSetCacheBehavior,
      networkSetExtraHeaders = run . API.networkSetExtraHeaders,
      -- Script commands
      scriptAddPreloadScript = run . API.scriptAddPreloadScript,
      scriptCallFunction = run . API.scriptCallFunction,
      scriptDisown = run . API.scriptDisown,
      scriptEvaluate = run . API.scriptEvaluate,
      scriptEvaluateNoWait = sendCommandNoWait . API.scriptEvaluate,
      scriptGetRealms = run . API.scriptGetRealms,
      scriptRemovePreloadScript = run . API.scriptRemovePreloadScript,
      -- Storage commands
      storageDeleteCookies = run . API.storageDeleteCookies,
      storageGetCookies = run . API.storageGetCookies,
      storageSetCookie = run . API.storageSetCookie,
      -- WebExtension commands
      webExtensionInstall = run . API.webExtensionInstall,
      webExtensionUninstall = run . API.webExtensionUninstall,
      -- Subscription methods
      subscribeMany = \sts -> subscribeMany' [] [] sts,
      subscribeMany',
      -- Log
      subscribeLogEntryAdded = sendSub API.subscribeLogEntryAdded,
      subscribeLogEntryAdded' = sendSub' API.subscribeLogEntryAdded,
      -- BrowsingContext
      subscribeBrowsingContextCreated = sendSub API.subscribeBrowsingContextCreated,
      subscribeBrowsingContextCreated' = sendSub' API.subscribeBrowsingContextCreated,
      subscribeBrowsingContextDestroyed = sendSub API.subscribeBrowsingContextDestroyed,
      subscribeBrowsingContextDestroyed' = sendSub' API.subscribeBrowsingContextDestroyed,
      subscribeBrowsingContextNavigationStarted = sendSub API.subscribeBrowsingContextNavigationStarted,
      subscribeBrowsingContextNavigationStarted' = sendSub' API.subscribeBrowsingContextNavigationStarted,
      subscribeBrowsingContextFragmentNavigated = sendSub API.subscribeBrowsingContextFragmentNavigated,
      subscribeBrowsingContextFragmentNavigated' = sendSub' API.subscribeBrowsingContextFragmentNavigated,
      subscribeBrowsingContextHistoryUpdated = sendSub API.subscribeBrowsingContextHistoryUpdated,
      subscribeBrowsingContextHistoryUpdated' = sendSub' API.subscribeBrowsingContextHistoryUpdated,
      subscribeBrowsingContextDomContentLoaded = sendSub API.subscribeBrowsingContextDomContentLoaded,
      subscribeBrowsingContextDomContentLoaded' = sendSub' API.subscribeBrowsingContextDomContentLoaded,
      subscribeBrowsingContextLoad = sendSub API.subscribeBrowsingContextLoad,
      subscribeBrowsingContextLoad' = sendSub' API.subscribeBrowsingContextLoad,
      subscribeBrowsingContextDownloadWillBegin = sendSub API.subscribeBrowsingContextDownloadWillBegin,
      subscribeBrowsingContextDownloadWillBegin' = sendSub' API.subscribeBrowsingContextDownloadWillBegin,
      subscribeBrowsingContextDownloadEnd = sendSub API.subscribeBrowsingContextDownloadEnd,
      subscribeBrowsingContextDownloadEnd' = sendSub' API.subscribeBrowsingContextDownloadEnd,
      subscribeBrowsingContextNavigationAborted = sendSub API.subscribeBrowsingContextNavigationAborted,
      subscribeBrowsingContextNavigationAborted' = sendSub' API.subscribeBrowsingContextNavigationAborted,
      subscribeBrowsingContextNavigationCommitted = sendSub API.subscribeBrowsingContextNavigationCommitted,
      subscribeBrowsingContextNavigationCommitted' = sendSub' API.subscribeBrowsingContextNavigationCommitted,
      subscribeBrowsingContextNavigationFailed = sendSub API.subscribeBrowsingContextNavigationFailed,
      subscribeBrowsingContextNavigationFailed' = sendSub' API.subscribeBrowsingContextNavigationFailed,
      subscribeBrowsingContextUserPromptClosed = sendSub API.subscribeBrowsingContextUserPromptClosed,
      subscribeBrowsingContextUserPromptClosed' = sendSub' API.subscribeBrowsingContextUserPromptClosed,
      subscribeBrowsingContextUserPromptOpened = sendSub API.subscribeBrowsingContextUserPromptOpened,
      subscribeBrowsingContextUserPromptOpened' = sendSub' API.subscribeBrowsingContextUserPromptOpened,
      -- Network
      subscribeNetworkAuthRequired = sendSub API.subscribeNetworkAuthRequired,
      subscribeNetworkAuthRequired' = sendSub' API.subscribeNetworkAuthRequired,
      subscribeNetworkBeforeRequestSent = sendSub API.subscribeNetworkBeforeRequestSent,
      subscribeNetworkBeforeRequestSent' = sendSub' API.subscribeNetworkBeforeRequestSent,
      subscribeNetworkFetchError = sendSub API.subscribeNetworkFetchError,
      subscribeNetworkFetchError' = sendSub' API.subscribeNetworkFetchError,
      subscribeNetworkResponseCompleted = sendSub API.subscribeNetworkResponseCompleted,
      subscribeNetworkResponseCompleted' = sendSub' API.subscribeNetworkResponseCompleted,
      subscribeNetworkResponseStarted = sendSub API.subscribeNetworkResponseStarted,
      subscribeNetworkResponseStarted' = sendSub' API.subscribeNetworkResponseStarted,
      -- Script
      subscribeScriptMessage = sendSub API.subscribeScriptMessage,
      subscribeScriptMessage' = sendSub' API.subscribeScriptMessage,
      subscribeScriptRealmCreated = sendSub API.subscribeScriptRealmCreated,
      subscribeScriptRealmCreated' = sendSub' API.subscribeScriptRealmCreated,
      subscribeScriptRealmDestroyed = sendSub API.subscribeScriptRealmDestroyed,
      subscribeScriptRealmDestroyed' = sendSub' API.subscribeScriptRealmDestroyed,
      -- Input
      subscribeInputFileDialogOpened = sendSub API.subscribeInputFileDialogOpened,
      subscribeInputFileDialogOpened' = sendSub' API.subscribeInputFileDialogOpened,
      --
      unsubscribe,
      -- Generic and low-level command methods
      sendCommand = run,
      sendCommand',
      sendCommandNoWait,
      sendOffSpecCommand',
      sendOffSpecCommandNoWait,
      -- Fallback subscriptions
      subscribeUnknownMany,
      subscribeUnknownMany'
    }
  where
    -- Helper to convert Command to SocketCommand
    commandToSocketCommand :: Command r -> Base.SocketCommand Text r
    commandToSocketCommand cmd = Base.MkSocketCommand
      { method = toCommandText cmd.method,
        params = toJSON cmd.params
      }
      where
        toCommandText = \case
          KnownCommand k -> knownCommandToText k
          OffSpecCommand (MkOffSpecCommand cmdText) -> cmdText

    -- Send a command without waiting for response
    sendCommandNoWait :: forall r. Command r -> IO Request
    sendCommandNoWait = Socket.sendCommandNoWait (coerce socketActions) . commandToSocketCommand

    -- Send a command with specific ID
    sendCommand' :: forall r. (FromJSON r) => JSUInt -> Command r -> IO r
    sendCommand' (MkJSUInt id') = Socket.sendCommand' (coerce socketActions) (Base.MkJSUInt id') . commandToSocketCommand

    -- Send off-spec command with specific ID
    sendOffSpecCommand' :: JSUInt -> Text -> Object -> IO Object
    sendOffSpecCommand' (MkJSUInt id') method params =
      Socket.sendCommand' (coerce socketActions) (Base.MkJSUInt id') $ Base.MkSocketCommand method (toJSON params)

    -- Send off-spec command without waiting
    sendOffSpecCommandNoWait :: Text -> Object -> IO Request
    sendOffSpecCommandNoWait method params =
      Socket.sendCommandNoWait (coerce socketActions) $ Base.MkSocketCommand method (toJSON params)

    -- Session subscribe/unsubscribe helpers
    sessionSubscribe' :: SessionSubscibe -> IO SessionSubscribeResult
    sessionSubscribe' = run . API.sessionSubscribe

    sessionUnsubscribe' :: SessionUnsubscribe -> IO ()
    sessionUnsubscribe' unsub = Runner.unsubscribe socketActions sessionUnsubscribe'' unsub
      where
        sessionUnsubscribe'' = run . API.sessionUnsubscribe

    -- Subscription helpers
    subscribeMany' ::
      [BrowsingContext] ->
      [UserContext] ->
      [KnownSubscriptionType] ->
      (Event -> IO ()) ->
      IO SubscriptionId
    subscribeMany' bcs ucs sts = Runner.subscribe socketActions sessionSubscribe' . API.subscribeMany sts bcs ucs

    subscribeUnknownMany ::
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId
    subscribeUnknownMany sts = Runner.subscribe socketActions sessionSubscribe' . API.subscribeOffSpecMany sts [] []

    subscribeUnknownMany' ::
      [BrowsingContext] ->
      [UserContext] ->
      [OffSpecSubscriptionType] ->
      (Value -> IO ()) ->
      IO SubscriptionId
    subscribeUnknownMany' bcs ucs sts = Runner.subscribe socketActions sessionSubscribe' . API.subscribeOffSpecMany sts bcs ucs

    sendSub ::
      ( [BrowsingContext] ->
        [UserContext] ->
        (a -> IO ()) ->
        Subscription IO
      ) ->
      (a -> IO ()) ->
      IO SubscriptionId
    sendSub mkSubscription =
      Runner.subscribe socketActions sessionSubscribe' . mkSubscription [] []

    sendSub' ::
      ( [BrowsingContext] ->
        [UserContext] ->
        (a -> IO ()) ->
        Subscription IO
      ) ->
      [BrowsingContext] ->
      [UserContext] ->
      (a -> IO ()) ->
      IO SubscriptionId
    sendSub' mkSubscription bcs ucs =
      Runner.subscribe socketActions sessionSubscribe' . mkSubscription bcs ucs

    unsubscribe :: SubscriptionId -> IO ()
    unsubscribe subId = Runner.unsubscribe socketActions (run . API.sessionUnsubscribe) (UnsubscribeById [subId])
