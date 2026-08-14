-- |
-- Module: WebDriver.Effectful.BiDi.Base.Effect
-- Description: BiDi WebDriver algebraic effect definition
--
-- Defines the 'WebDriverBiDi' algebraic effect together with the
-- configuration type 'BiDiInfo' and the internal subscription helpers used
-- by the interpreter.
--
-- Smart constructors for each operation are in
-- "WebDriver.Effectful.BiDi.Base.Actions".
-- The IO-backed interpreter is in
-- "WebDriver.Effectful.BiDi.Base.Interpreter".
module WebDriver.Effectful.BiDi.Base.Effect
  ( -- * Configuration type
    BiDiInfo (..),

    -- * Internal helpers
    bidiRun,
    mkSendSub,
    mkSendSub',
    mkSendSubMany',
    mkSendSubOffSpecMany',

    -- * BiDi Effect
    WebDriverBiDi (..),
  )
where

import Data.Aeson (FromJSON, Object, Value)
import Data.Text (Text)
import Effectful (Dispatch (..), DispatchOf, Effect)
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
    SessionUnsubscribe,
    SetCacheBehavior,
    SetBypassCSP,
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
    SetScrollbarTypeOverride,
    SetTimezoneOverride,
    SetTouchOverride,
    SetUserAgentOverride,
    SetViewport,
    StartScreencast,
    StartScreencastResult,
    StopScreencast,
    StopScreencastResult,
    SubscriptionId (..),
    TraverseHistory,
    UserContext,
    UserPromptClosed,
    UserPromptOpened,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
  )
import WebDriverPreCore.BiDiRunner (BiDiRunner (..), Request)
import WebDriverPreCore.BiDiRunner qualified as Runner
import WebDriverPreCore.Extended.BiDi.Base.Actions qualified as BA
import WebDriverPreCore.Utils.Timeout (Timeout)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | BiDi driver environment holding the async WebSocket runner.
data BiDiInfo = MkBiDiInfo
  { biDiRunner :: BiDiRunner IO,
    -- | Duration to sleep on each 'pause' call.
    pauseDuration :: Timeout
  }

-- ---------------------------------------------------------------------------
-- Internal subscription helpers
-- ---------------------------------------------------------------------------

-- | Extract the rank-2 polymorphic @run@ function from a 'BiDiRunner'.
bidiRun :: (FromJSON r) => BiDiInfo -> Command r -> IO r
bidiRun (MkBiDiInfo {biDiRunner = MkBiDiRunner {run = r}}) = r

mkSendSub :: BiDiRunner IO -> BA.SendSub IO a
mkSendSub MkBiDiRunner {run = r, socketActions} mkSub handler =
  Runner.subscribe socketActions (r . BA.sessionSubscribe) (mkSub [] [] handler)

mkSendSub' :: BiDiRunner IO -> BA.SendSub' IO a
mkSendSub' MkBiDiRunner {run = r, socketActions} mkSub bcs ucs handler =
  Runner.subscribe socketActions (r . BA.sessionSubscribe) (mkSub bcs ucs handler)

mkSendSubMany' :: BiDiRunner IO -> BA.SendSubMany' IO
mkSendSubMany' MkBiDiRunner {run = r, socketActions} mkSub sts bcs ucs handler =
  Runner.subscribe socketActions (r . BA.sessionSubscribe) (mkSub sts bcs ucs handler)

mkSendSubOffSpecMany' :: BiDiRunner IO -> BA.SendSubOffSpecMany' IO
mkSendSubOffSpecMany' MkBiDiRunner {run = r, socketActions} mkSub sts bcs ucs handler =
  Runner.subscribe socketActions (r . BA.sessionSubscribe) (mkSub sts bcs ucs handler)

-- ---------------------------------------------------------------------------
-- BiDi effect
-- ---------------------------------------------------------------------------

-- | Algebraic effect encoding all BiDi WebDriver commands and event
-- subscriptions.
--
-- Command constructors (e.g. 'BrowsingContextNavigate') invoke a BiDi
-- command and return its response.  Subscription constructors
-- (e.g. 'SubscribeBrowsingContextDomContentLoaded') accept an @m ()@ callback
-- so callers can use any effectful action in the handler.  The interpreter
-- uses 'Effectful.Dispatch.Dynamic.localUnliftIO' with @ConcUnlift Persistent
-- Unlimited@ to convert the @Eff@ callback to @IO@ before handing it off to
-- the underlying WebSocket runner, which invokes callbacks from a separate thread.
--
-- For idiomatic use of subscriptions in tests, use 'liftIO' inside the
-- callback to write to a 'Control.Concurrent.STM.TMVar', then call
-- @liftIO atomically@ in the main @Eff@ stack to wait for the event.
--
-- Smart constructors are in "WebDriver.Effectful.BiDi.Base.Actions".
data WebDriverBiDi :: Effect where
  -- Session
  SessionNew :: Capabilities -> WebDriverBiDi m SessionNewResult
  SessionStatus :: WebDriverBiDi m SessionStatusResult
  SessionEnd :: WebDriverBiDi m ()
  -- BrowsingContext
  BrowsingContextActivate :: Activate -> WebDriverBiDi m ()
  BrowsingContextCaptureScreenshot :: CaptureScreenshot -> WebDriverBiDi m CaptureScreenshotResult
  BrowsingContextClose :: Close -> WebDriverBiDi m ()
  BrowsingContextCreate :: Create -> WebDriverBiDi m BrowsingContext
  BrowsingContextGetTree :: GetTree -> WebDriverBiDi m GetTreeResult
  BrowsingContextHandleUserPrompt :: HandleUserPrompt -> WebDriverBiDi m ()
  BrowsingContextLocateNodes :: LocateNodes -> WebDriverBiDi m LocateNodesResult
  BrowsingContextNavigate :: Navigate -> WebDriverBiDi m NavigateResult
  BrowsingContextPrint :: Print -> WebDriverBiDi m PrintResult
  BrowsingContextReload :: Reload -> WebDriverBiDi m ()
  BrowsingContextSetBypassCSP :: SetBypassCSP -> WebDriverBiDi m ()
  BrowsingContextSetViewport :: SetViewport -> WebDriverBiDi m ()
  BrowsingContextStartScreencast :: StartScreencast -> WebDriverBiDi m StartScreencastResult
  BrowsingContextStopScreencast :: StopScreencast -> WebDriverBiDi m StopScreencastResult
  BrowsingContextTraverseHistory :: TraverseHistory -> WebDriverBiDi m ()
  -- Browser
  BrowserClose :: WebDriverBiDi m ()
  BrowserCreateUserContext :: CreateUserContext -> WebDriverBiDi m UserContext
  BrowserGetClientWindows :: WebDriverBiDi m GetClientWindowsResult
  BrowserGetUserContexts :: WebDriverBiDi m GetUserContextsResult
  BrowserRemoveUserContext :: RemoveUserContext -> WebDriverBiDi m ()
  BrowserSetClientWindowState :: SetClientWindowState -> WebDriverBiDi m ClientWindowInfo
  BrowserSetDownloadBehavior :: SetDownloadBehavior -> WebDriverBiDi m ()
  -- Emulation
  EmulationSetForcedColorsModeThemeOverride :: SetForcedColorsModeThemeOverride -> WebDriverBiDi m ()
  EmulationSetGeolocationOverride :: SetGeolocationOverride -> WebDriverBiDi m ()
  EmulationSetLocaleOverride :: SetLocaleOverride -> WebDriverBiDi m ()
  EmulationSetNetworkConditions :: SetNetworkConditions -> WebDriverBiDi m ()
  EmulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> WebDriverBiDi m ()
  EmulationSetScreenSettingsOverride :: SetScreenSettingsOverride -> WebDriverBiDi m ()
  EmulationSetScriptingEnabled :: SetScriptingEnabled -> WebDriverBiDi m ()
  EmulationSetScrollbarTypeOverride :: SetScrollbarTypeOverride -> WebDriverBiDi m ()
  EmulationSetTimezoneOverride :: SetTimezoneOverride -> WebDriverBiDi m ()
  EmulationSetTouchOverride :: SetTouchOverride -> WebDriverBiDi m ()
  EmulationSetUserAgentOverride :: SetUserAgentOverride -> WebDriverBiDi m ()
  -- Input
  InputPerformActions :: PerformActions -> WebDriverBiDi m ()
  InputReleaseActions :: ReleaseActions -> WebDriverBiDi m ()
  InputSetFiles :: SetFiles -> WebDriverBiDi m ()
  -- Network
  NetworkAddDataCollector :: AddDataCollector -> WebDriverBiDi m AddDataCollectorResult
  NetworkAddIntercept :: AddIntercept -> WebDriverBiDi m AddInterceptResult
  NetworkContinueRequest :: ContinueRequest -> WebDriverBiDi m ()
  NetworkContinueResponse :: ContinueResponse -> WebDriverBiDi m ()
  NetworkContinueWithAuth :: ContinueWithAuth -> WebDriverBiDi m ()
  NetworkDisownData :: DisownData -> WebDriverBiDi m ()
  NetworkFailRequest :: FailRequest -> WebDriverBiDi m ()
  NetworkGetData :: GetData -> WebDriverBiDi m GetDataResult
  NetworkProvideResponse :: ProvideResponse -> WebDriverBiDi m ()
  NetworkRemoveDataCollector :: RemoveDataCollector -> WebDriverBiDi m ()
  NetworkRemoveIntercept :: RemoveIntercept -> WebDriverBiDi m ()
  NetworkSetCacheBehavior :: SetCacheBehavior -> WebDriverBiDi m ()
  NetworkSetExtraHeaders :: SetExtraHeaders -> WebDriverBiDi m ()
  -- Script
  ScriptAddPreloadScript :: AddPreloadScript -> WebDriverBiDi m AddPreloadScriptResult
  ScriptCallFunction :: CallFunction -> WebDriverBiDi m EvaluateResult
  ScriptDisown :: Disown -> WebDriverBiDi m ()
  ScriptEvaluate :: Evaluate -> WebDriverBiDi m EvaluateResult
  ScriptEvaluateNoWait :: Evaluate -> WebDriverBiDi m Request
  ScriptGetRealms :: GetRealms -> WebDriverBiDi m GetRealmsResult
  ScriptRemovePreloadScript :: RemovePreloadScript -> WebDriverBiDi m ()
  -- Storage
  StorageDeleteCookies :: DeleteCookies -> WebDriverBiDi m DeleteCookiesResult
  StorageGetCookies :: GetCookies -> WebDriverBiDi m GetCookiesResult
  StorageSetCookie :: SetCookie -> WebDriverBiDi m SetCookieResult
  -- WebExtension
  WebExtensionInstall :: WebExtensionInstall -> WebDriverBiDi m WebExtensionResult
  WebExtensionUninstall :: WebExtensionUninstall -> WebDriverBiDi m ()
  -- Generic (escape hatch)
  SendBiDiCmd :: (FromJSON r) => Command r -> WebDriverBiDi m r
  SendBiDiCmdNoWait :: Command r -> WebDriverBiDi m Request
  SendBiDiOffSpecCmd :: JSUInt -> Text -> Object -> WebDriverBiDi m Object
  SendBiDiOffSpecCmdNoWait :: Text -> Object -> WebDriverBiDi m Request
  -- Log subscriptions
  SubscribeLogEntryAdded :: (LogEntry -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeLogEntryAdded' :: [BrowsingContext] -> [UserContext] -> (LogEntry -> m ()) -> WebDriverBiDi m SubscriptionId
  -- BrowsingContext subscriptions
  SubscribeBrowsingContextCreated :: (Info -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextCreated' :: [BrowsingContext] -> [UserContext] -> (Info -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDestroyed :: (Info -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDestroyed' :: [BrowsingContext] -> [UserContext] -> (Info -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationStarted :: (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationStarted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextFragmentNavigated :: (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextFragmentNavigated' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextHistoryUpdated :: (HistoryUpdated -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextHistoryUpdated' :: [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDomContentLoaded :: (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDomContentLoaded' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextLoad :: (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextLoad' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadWillBegin :: (DownloadWillBegin -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadWillBegin' :: [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadEnd :: (DownloadEnd -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadEnd' :: [BrowsingContext] -> [UserContext] -> (DownloadEnd -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationAborted :: (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationAborted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationCommitted :: (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationCommitted' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationFailed :: (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationFailed' :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptClosed :: (UserPromptClosed -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptClosed' :: [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptOpened :: (UserPromptOpened -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptOpened' :: [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> m ()) -> WebDriverBiDi m SubscriptionId
  -- Network subscriptions
  SubscribeNetworkAuthRequired :: (AuthRequired -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkAuthRequired' :: [BrowsingContext] -> [UserContext] -> (AuthRequired -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkBeforeRequestSent :: (BeforeRequestSent -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkBeforeRequestSent' :: [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkFetchError :: (FetchError -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkFetchError' :: [BrowsingContext] -> [UserContext] -> (FetchError -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseCompleted :: (ResponseCompleted -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseCompleted' :: [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseStarted :: (ResponseStarted -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseStarted' :: [BrowsingContext] -> [UserContext] -> (ResponseStarted -> m ()) -> WebDriverBiDi m SubscriptionId
  -- Script subscriptions
  SubscribeScriptMessage :: (Message -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptMessage' :: [BrowsingContext] -> [UserContext] -> (Message -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmCreated :: (RealmInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmCreated' :: [BrowsingContext] -> [UserContext] -> (RealmInfo -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmDestroyed :: (RealmDestroyed -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmDestroyed' :: [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> m ()) -> WebDriverBiDi m SubscriptionId
  -- Input subscriptions
  SubscribeInputFileDialogOpened :: (FileDialogOpened -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeInputFileDialogOpened' :: [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> m ()) -> WebDriverBiDi m SubscriptionId
  -- Multi-event subscriptions
  SubscribeMany :: [KnownSubscriptionType] -> (Event -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeMany' :: [BrowsingContext] -> [UserContext] -> [KnownSubscriptionType] -> (Event -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeOffSpecMany :: [OffSpecSubscriptionType] -> (Value -> m ()) -> WebDriverBiDi m SubscriptionId
  SubscribeOffSpecMany' :: [BrowsingContext] -> [UserContext] -> [OffSpecSubscriptionType] -> (Value -> m ()) -> WebDriverBiDi m SubscriptionId
  -- Unsubscribe
  Unsubscribe :: SubscriptionId -> WebDriverBiDi m ()
  -- SessionUnsubscribe
  SessionUnsubscribe :: SessionUnsubscribe -> WebDriverBiDi m ()

type instance DispatchOf WebDriverBiDi = Dynamic
