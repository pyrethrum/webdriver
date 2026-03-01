-- |
-- Module: WebDriver.Effectful.HTTP.Core
-- Description: Core types and effects for Effectful WebDriver
--
-- Defines the types and algebraic effects used throughout the Effectful POC:
--
-- * 'HttpDriverInfo'   — HTTP connection configuration
-- * 'HttpSessionInfo'  — driver info + session + pause duration
-- * 'BiDiInfo'         — BiDi runner + pause duration
-- * 'WebDriverHttp'    — Dynamic effect encoding all HTTP session operations
-- * 'WebDriverBiDi'    — Dynamic effect encoding all BiDi commands + subscriptions
--
-- The effects are dispatched dynamically: 'runWebDriverHttp' and
-- 'runWebDriverBiDi' provide the @IO@-backed interpreters.  The separation
-- of "what" (effect algebra) from "how" (interpreter) means you can add
-- alternative interpreters (e.g. pure test doubles) without changing
-- call-site code.
--
-- This mirrors "WebDriver.Bluefin.HTTP.Core" but uses Effectful algebraic
-- effects instead of explicit Bluefin compound handles.
module WebDriver.Effectful.HTTP.Core
  ( -- * Types
    HttpDriverInfo (..),
    HttpSessionInfo (..),
    BiDiInfo (..),
    defaultDriverInfo,

    -- * HTTP Effect
    WebDriverHttp (..),

    -- * BiDi Effect
    WebDriverBiDi (..),

    -- * HTTP Interpreter
    runWebDriverHttp,

    -- * BiDi Interpreter
    runWebDriverBiDi,

    -- * Internal helpers (re-exported for App module)
    mkSessionRunner,
  )
where

import Data.Aeson (FromJSON, Object, Value)
import Data.Text (Text)
import Effectful (Effect, Dispatch (..), DispatchOf, Eff, IOE, (:>), liftIO)
import Effectful.Dispatch.Dynamic (interpret_)
import UnliftIO (throwIO)
import WebDriverPreCore.BiDi.Protocol qualified as BP
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
    KnownCommand,
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
    SessionUnsubscribe (..),
    SubscriptionId (..),
    TraverseHistory,
    UserContext,
    UserPromptClosed,
    UserPromptOpened,
    WebExtensionInstall,
    WebExtensionResult,
    WebExtensionUninstall,
    mkCommand,
  )
import WebDriverPreCore.BiDiRunner (BiDiRunner (..), Request)
import WebDriverPreCore.BiDiRunner qualified as Runner
import WebDriverPreCore.Error (parseFailToWDException)
import WebDriverPreCore.Extended.BiDi.Base.Actions qualified as BA
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.Extended.HTTP.Base.Protocol
  ( Actions,
    Cookie,
    ElementId,
    FrameReference,
    Handle,
    Script,
    Selector,
    Session (..),
    ShadowRootElementId,
    Timeouts,
    URL,
    WindowHandleSpec,
    WindowRect,
  )
import WebDriverPreCore.HttpRunner (HttpEndpoint (..), callWebDriver)
import WebDriverPreCore.Utils.Timeout (Timeout)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Configuration for an HTTP WebDriver connection.
data HttpDriverInfo = MkHttpDriverInfo
  { httpEndpoint :: HttpEndpoint,
    -- | When 'Just', each driver request\/response is logged via this function.
    driverLogFn :: Maybe (Text -> IO ())
  }

-- | Default driver info targeting localhost:4444 with logging disabled.
defaultDriverInfo :: HttpDriverInfo
defaultDriverInfo =
  MkHttpDriverInfo
    { httpEndpoint = MkHttpEndpoint {host = "127.0.0.1", port = 4444},
      driverLogFn = Nothing
    }

-- | Session-scoped HTTP driver configuration.
data HttpSessionInfo = MkHttpSessionInfo
  { driverInfo    :: HttpDriverInfo,
    -- | The active WebDriver session identifier.
    session       :: Session,
    -- | Duration to sleep on each 'pause' call.
    pauseDuration :: Timeout
  }

-- | BiDi driver environment holding the async WebSocket runner.
data BiDiInfo = MkBiDiInfo
  { biDiRunner    :: BiDiRunner IO,
    -- | Duration to sleep on each 'pause' call.
    pauseDuration :: Timeout
  }

-- ---------------------------------------------------------------------------
-- Runner helpers (internal)
-- ---------------------------------------------------------------------------

-- | Build a @Command a -> IO a@ runner from an 'HttpSessionInfo'.
mkSessionRunner :: (FromJSON a) => HttpSessionInfo -> HA.Runner IO a
mkSessionRunner info cmd =
  callWebDriver info.driverInfo.httpEndpoint info.driverInfo.driverLogFn cmd
    >>= either (throwIO . parseFailToWDException) pure

-- | Extract the rank-2 polymorphic @run@ function from a 'BiDiRunner'.
bidiRun :: (FromJSON r) => BiDiInfo -> Command r -> IO r
bidiRun (MkBiDiInfo {biDiRunner = MkBiDiRunner {run = r}}) = r

-- Subscription helper builders (mirror the Bluefin BiDi Actions helpers).

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
-- HTTP effect
-- ---------------------------------------------------------------------------

-- | Algebraic effect encoding all HTTP WebDriver session-level operations.
--
-- Each constructor corresponds to one WebDriver HTTP command.  The effect is
-- interpreted by 'runWebDriverHttp', which delegates to the underlying HTTP
-- runner.  Alternative interpreters (e.g. pure test doubles) can be provided
-- without changing call-site code.
--
-- Smart constructors for each operation are in
-- "WebDriver.Effectful.HTTP.Base.Actions".
data WebDriverHttp :: Effect where
  -- Session management
  DeleteSession   :: WebDriverHttp m ()
  GetTimeouts     :: WebDriverHttp m Timeouts
  SetTimeouts     :: Timeouts -> WebDriverHttp m ()
  -- Navigation
  NavigateTo      :: URL -> WebDriverHttp m ()
  GetCurrentUrl   :: WebDriverHttp m URL
  Back            :: WebDriverHttp m ()
  Forward         :: WebDriverHttp m ()
  Refresh         :: WebDriverHttp m ()
  GetTitle        :: WebDriverHttp m Text
  -- Windows
  GetWindowHandle     :: WebDriverHttp m Handle
  GetWindowHandles    :: WebDriverHttp m [Handle]
  NewWindow           :: WebDriverHttp m WindowHandleSpec
  CloseWindow         :: WebDriverHttp m [Handle]
  SwitchToWindow      :: Handle -> WebDriverHttp m ()
  GetWindowRect       :: WebDriverHttp m WindowRect
  SetWindowRect       :: WindowRect -> WebDriverHttp m WindowRect
  MaximizeWindow      :: WebDriverHttp m WindowRect
  MinimizeWindow      :: WebDriverHttp m WindowRect
  FullScreenWindow    :: WebDriverHttp m WindowRect
  -- Frames
  SwitchToFrame       :: FrameReference -> WebDriverHttp m ()
  SwitchToParentFrame :: WebDriverHttp m ()
  -- Page / Script
  GetPageSource       :: WebDriverHttp m Text
  ExecuteScript       :: Script -> WebDriverHttp m Value
  ExecuteScriptAsync  :: Script -> WebDriverHttp m Value
  -- Cookies
  AddCookie           :: Cookie -> WebDriverHttp m ()
  GetAllCookies       :: WebDriverHttp m [Cookie]
  GetNamedCookie      :: Text -> WebDriverHttp m Cookie
  DeleteCookie        :: Text -> WebDriverHttp m ()
  DeleteAllCookies    :: WebDriverHttp m ()
  -- Actions / Prompts
  PerformActions  :: Actions -> WebDriverHttp m ()
  ReleaseActions  :: WebDriverHttp m ()
  DismissAlert    :: WebDriverHttp m ()
  AcceptAlert     :: WebDriverHttp m ()
  GetAlertText    :: WebDriverHttp m Text
  SendAlertText   :: Text -> WebDriverHttp m ()
  -- Screenshots / Print
  TakeScreenshot  :: WebDriverHttp m Text
  PrintPage       :: WebDriverHttp m Text
  -- Elements
  GetActiveElement    :: WebDriverHttp m ElementId
  FindElement         :: Selector -> WebDriverHttp m ElementId
  FindElements        :: Selector -> WebDriverHttp m [ElementId]
  -- Element sub-finders
  FindElementFromElement          :: ElementId -> Selector -> WebDriverHttp m ElementId
  FindElementsFromElement         :: ElementId -> Selector -> WebDriverHttp m [ElementId]
  FindElementFromShadowRoot      :: ShadowRootElementId -> Selector -> WebDriverHttp m ElementId
  FindElementsFromShadowRoot     :: ShadowRootElementId -> Selector -> WebDriverHttp m [ElementId]
  -- Element state
  IsElementSelected       :: ElementId -> WebDriverHttp m Bool
  GetElementAttribute     :: ElementId -> Text -> WebDriverHttp m Text
  GetElementProperty      :: ElementId -> Text -> WebDriverHttp m Value
  GetElementCssValue      :: ElementId -> Text -> WebDriverHttp m Text
  GetElementShadowRoot    :: ElementId -> WebDriverHttp m ShadowRootElementId
  GetElementText          :: ElementId -> WebDriverHttp m Text
  GetElementTagName       :: ElementId -> WebDriverHttp m Text
  GetElementRect          :: ElementId -> WebDriverHttp m WindowRect
  IsElementEnabled        :: ElementId -> WebDriverHttp m Bool
  GetElementComputedRole  :: ElementId -> WebDriverHttp m Text
  GetElementComputedLabel :: ElementId -> WebDriverHttp m Text
  -- Element actions
  ElementClick          :: ElementId -> WebDriverHttp m ()
  ElementClear          :: ElementId -> WebDriverHttp m ()
  ElementSendKeys       :: ElementId -> Text -> WebDriverHttp m ()
  TakeElementScreenshot :: ElementId -> WebDriverHttp m Text

type instance DispatchOf WebDriverHttp = Dynamic

-- ---------------------------------------------------------------------------
-- BiDi effect
-- ---------------------------------------------------------------------------

-- | Algebraic effect encoding all BiDi WebDriver commands and event
-- subscriptions.
--
-- Command constructors (e.g. 'BrowsingContextNavigate') invoke a BiDi
-- command and return its response.  Subscription constructors
-- (e.g. 'SubscribeBrowsingContextDomContentLoaded') register an IO callback
-- that the BiDi runner will invoke asynchronously when the event fires.
-- Callbacks are @IO@-based because the underlying WebSocket reader loop is
-- @IO@-based; the subscription call itself remains an effect operation.
--
-- For idiomatic use of subscriptions in tests, pass a callback that writes
-- to a 'Control.Concurrent.STM.TMVar' and then use
-- 'Effectful.Concurrent.STM.atomically' to wait for the event in the main
-- effect stack (see the Effectful test suite for an example).
--
-- Smart constructors are in "WebDriver.Effectful.BiDi.Base.Actions".
data WebDriverBiDi :: Effect where
  -- Session
  BiDiSessionNew    :: Capabilities -> WebDriverBiDi m SessionNewResult
  BiDiSessionStatus :: WebDriverBiDi m SessionStatusResult
  BiDiSessionEnd    :: WebDriverBiDi m ()
  -- BrowsingContext
  BrowsingContextActivate          :: Activate -> WebDriverBiDi m ()
  BrowsingContextCaptureScreenshot :: CaptureScreenshot -> WebDriverBiDi m CaptureScreenshotResult
  BrowsingContextClose             :: Close -> WebDriverBiDi m ()
  BrowsingContextCreate            :: Create -> WebDriverBiDi m BrowsingContext
  BrowsingContextGetTree           :: GetTree -> WebDriverBiDi m GetTreeResult
  BrowsingContextHandleUserPrompt  :: HandleUserPrompt -> WebDriverBiDi m ()
  BrowsingContextLocateNodes       :: LocateNodes -> WebDriverBiDi m LocateNodesResult
  BrowsingContextNavigate          :: Navigate -> WebDriverBiDi m NavigateResult
  BrowsingContextPrint             :: Print -> WebDriverBiDi m PrintResult
  BrowsingContextReload            :: Reload -> WebDriverBiDi m ()
  BrowsingContextSetViewport       :: SetViewport -> WebDriverBiDi m ()
  BrowsingContextTraverseHistory   :: TraverseHistory -> WebDriverBiDi m ()
  -- Browser
  BrowserClose               :: WebDriverBiDi m ()
  BrowserCreateUserContext   :: CreateUserContext -> WebDriverBiDi m UserContext
  BrowserGetClientWindows    :: WebDriverBiDi m GetClientWindowsResult
  BrowserGetUserContexts     :: WebDriverBiDi m GetUserContextsResult
  BrowserRemoveUserContext   :: RemoveUserContext -> WebDriverBiDi m ()
  BrowserSetClientWindowState :: SetClientWindowState -> WebDriverBiDi m ClientWindowInfo
  BrowserSetDownloadBehavior :: SetDownloadBehavior -> WebDriverBiDi m ()
  -- Emulation
  EmulationSetForcedColorsModeThemeOverride :: SetForcedColorsModeThemeOverride -> WebDriverBiDi m ()
  EmulationSetGeolocationOverride           :: SetGeolocationOverride -> WebDriverBiDi m ()
  EmulationSetLocaleOverride                :: SetLocaleOverride -> WebDriverBiDi m ()
  EmulationSetNetworkConditions             :: SetNetworkConditions -> WebDriverBiDi m ()
  EmulationSetScreenOrientationOverride     :: SetScreenOrientationOverride -> WebDriverBiDi m ()
  EmulationSetScreenSettingsOverride        :: SetScreenSettingsOverride -> WebDriverBiDi m ()
  EmulationSetScriptingEnabled              :: SetScriptingEnabled -> WebDriverBiDi m ()
  EmulationSetTimezoneOverride              :: SetTimezoneOverride -> WebDriverBiDi m ()
  EmulationSetTouchOverride                 :: SetTouchOverride -> WebDriverBiDi m ()
  EmulationSetUserAgentOverride             :: SetUserAgentOverride -> WebDriverBiDi m ()
  -- Input
  InputPerformActions :: PerformActions -> WebDriverBiDi m ()
  InputReleaseActions :: ReleaseActions -> WebDriverBiDi m ()
  InputSetFiles       :: SetFiles -> WebDriverBiDi m ()
  -- Network
  NetworkAddDataCollector    :: AddDataCollector -> WebDriverBiDi m AddDataCollectorResult
  NetworkAddIntercept        :: AddIntercept -> WebDriverBiDi m AddInterceptResult
  NetworkContinueRequest     :: ContinueRequest -> WebDriverBiDi m ()
  NetworkContinueResponse    :: ContinueResponse -> WebDriverBiDi m ()
  NetworkContinueWithAuth    :: ContinueWithAuth -> WebDriverBiDi m ()
  NetworkDisownData          :: DisownData -> WebDriverBiDi m ()
  NetworkFailRequest         :: FailRequest -> WebDriverBiDi m ()
  NetworkGetData             :: GetData -> WebDriverBiDi m GetDataResult
  NetworkProvideResponse     :: ProvideResponse -> WebDriverBiDi m ()
  NetworkRemoveDataCollector :: RemoveDataCollector -> WebDriverBiDi m ()
  NetworkRemoveIntercept     :: RemoveIntercept -> WebDriverBiDi m ()
  NetworkSetCacheBehavior    :: SetCacheBehavior -> WebDriverBiDi m ()
  NetworkSetExtraHeaders     :: SetExtraHeaders -> WebDriverBiDi m ()
  -- Script
  ScriptAddPreloadScript    :: AddPreloadScript -> WebDriverBiDi m AddPreloadScriptResult
  ScriptCallFunction        :: CallFunction -> WebDriverBiDi m EvaluateResult
  ScriptDisown              :: Disown -> WebDriverBiDi m ()
  ScriptEvaluate            :: Evaluate -> WebDriverBiDi m EvaluateResult
  ScriptEvaluateNoWait      :: Evaluate -> WebDriverBiDi m Request
  ScriptGetRealms           :: GetRealms -> WebDriverBiDi m GetRealmsResult
  ScriptRemovePreloadScript :: RemovePreloadScript -> WebDriverBiDi m ()
  -- Storage
  StorageDeleteCookies :: DeleteCookies -> WebDriverBiDi m DeleteCookiesResult
  StorageGetCookies    :: GetCookies -> WebDriverBiDi m GetCookiesResult
  StorageSetCookie     :: SetCookie -> WebDriverBiDi m SetCookieResult
  -- WebExtension
  WebExtensionInstall   :: WebExtensionInstall -> WebDriverBiDi m WebExtensionResult
  WebExtensionUninstall :: WebExtensionUninstall -> WebDriverBiDi m ()
  -- Generic (escape hatch)
  SendBiDiCmd             :: (FromJSON r) => Command r -> WebDriverBiDi m r
  SendBiDiCmdNoWait       :: Command r -> WebDriverBiDi m Request
  SendBiDiOffSpecCmd      :: JSUInt -> Text -> Object -> WebDriverBiDi m Object
  SendBiDiOffSpecCmdNoWait :: Text -> Object -> WebDriverBiDi m Request
  -- Log subscriptions
  SubscribeLogEntryAdded  :: (LogEntry -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeLogEntryAdded' :: [BrowsingContext] -> [UserContext] -> (LogEntry -> IO ()) -> WebDriverBiDi m SubscriptionId
  -- BrowsingContext subscriptions
  SubscribeBrowsingContextCreated               :: (Info -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextCreated'              :: [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDestroyed             :: (Info -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDestroyed'            :: [BrowsingContext] -> [UserContext] -> (Info -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationStarted     :: (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationStarted'    :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextFragmentNavigated     :: (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextFragmentNavigated'    :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextHistoryUpdated        :: (HistoryUpdated -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextHistoryUpdated'       :: [BrowsingContext] -> [UserContext] -> (HistoryUpdated -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDomContentLoaded      :: (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDomContentLoaded'     :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextLoad                  :: (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextLoad'                 :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadWillBegin     :: (DownloadWillBegin -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadWillBegin'    :: [BrowsingContext] -> [UserContext] -> (DownloadWillBegin -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadEnd           :: (DownloadEnd -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextDownloadEnd'          :: [BrowsingContext] -> [UserContext] -> (DownloadEnd -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationAborted     :: (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationAborted'    :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationCommitted   :: (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationCommitted'  :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationFailed      :: (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextNavigationFailed'     :: [BrowsingContext] -> [UserContext] -> (NavigationInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptClosed      :: (UserPromptClosed -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptClosed'     :: [BrowsingContext] -> [UserContext] -> (UserPromptClosed -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptOpened      :: (UserPromptOpened -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeBrowsingContextUserPromptOpened'     :: [BrowsingContext] -> [UserContext] -> (UserPromptOpened -> IO ()) -> WebDriverBiDi m SubscriptionId
  -- Network subscriptions
  SubscribeNetworkAuthRequired        :: (AuthRequired -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkAuthRequired'       :: [BrowsingContext] -> [UserContext] -> (AuthRequired -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkBeforeRequestSent   :: (BeforeRequestSent -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkBeforeRequestSent'  :: [BrowsingContext] -> [UserContext] -> (BeforeRequestSent -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkFetchError          :: (FetchError -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkFetchError'         :: [BrowsingContext] -> [UserContext] -> (FetchError -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseCompleted   :: (ResponseCompleted -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseCompleted'  :: [BrowsingContext] -> [UserContext] -> (ResponseCompleted -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseStarted     :: (ResponseStarted -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeNetworkResponseStarted'    :: [BrowsingContext] -> [UserContext] -> (ResponseStarted -> IO ()) -> WebDriverBiDi m SubscriptionId
  -- Script subscriptions
  SubscribeScriptMessage        :: (Message -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptMessage'       :: [BrowsingContext] -> [UserContext] -> (Message -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmCreated   :: (RealmInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmCreated'  :: [BrowsingContext] -> [UserContext] -> (RealmInfo -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmDestroyed :: (RealmDestroyed -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeScriptRealmDestroyed' :: [BrowsingContext] -> [UserContext] -> (RealmDestroyed -> IO ()) -> WebDriverBiDi m SubscriptionId
  -- Input subscriptions
  SubscribeInputFileDialogOpened  :: (FileDialogOpened -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeInputFileDialogOpened' :: [BrowsingContext] -> [UserContext] -> (FileDialogOpened -> IO ()) -> WebDriverBiDi m SubscriptionId
  -- Multi-event subscriptions
  SubscribeMany         :: [KnownSubscriptionType] -> (Event -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeMany'        :: [BrowsingContext] -> [UserContext] -> [KnownSubscriptionType] -> (Event -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeUnknownMany  :: [OffSpecSubscriptionType] -> (Value -> IO ()) -> WebDriverBiDi m SubscriptionId
  SubscribeUnknownMany' :: [BrowsingContext] -> [UserContext] -> [OffSpecSubscriptionType] -> (Value -> IO ()) -> WebDriverBiDi m SubscriptionId
  -- Unsubscribe
  Unsubscribe :: SubscriptionId -> WebDriverBiDi m ()

type instance DispatchOf WebDriverBiDi = Dynamic

-- ---------------------------------------------------------------------------
-- HTTP interpreter
-- ---------------------------------------------------------------------------

-- | Interpret the 'WebDriverHttp' effect by running HTTP WebDriver commands
-- against the session described by 'HttpSessionInfo'.
--
-- The interpreter maps each effect constructor to the corresponding
-- @WebDriverPreCore.Extended.HTTP.Base.Actions@ function.
runWebDriverHttp :: (IOE :> es) => HttpSessionInfo -> Eff (WebDriverHttp : es) a -> Eff es a
runWebDriverHttp info = interpret_ $ \case
  DeleteSession             -> liftIO $ HA.deleteSession       runner sess
  GetTimeouts               -> liftIO $ HA.getTimeouts         runner sess
  SetTimeouts ts            -> liftIO $ HA.setTimeouts         runner sess ts
  NavigateTo url            -> liftIO $ HA.navigateTo          runner sess url
  GetCurrentUrl             -> liftIO $ HA.getCurrentUrl       runner sess
  Back                      -> liftIO $ HA.back                runner sess
  Forward                   -> liftIO $ HA.forward             runner sess
  Refresh                   -> liftIO $ HA.refresh             runner sess
  GetTitle                  -> liftIO $ HA.getTitle            runner sess
  GetWindowHandle           -> liftIO $ HA.getWindowHandle     runner sess
  GetWindowHandles          -> liftIO $ HA.getWindowHandles    runner sess
  NewWindow                 -> liftIO $ HA.newWindow           runner sess
  CloseWindow               -> liftIO $ HA.closeWindow        runner sess
  SwitchToWindow wh         -> liftIO $ HA.switchToWindow      runner sess wh
  GetWindowRect             -> liftIO $ HA.getWindowRect       runner sess
  SetWindowRect wr          -> liftIO $ HA.setWindowRect       runner sess wr
  MaximizeWindow            -> liftIO $ HA.maximizeWindow      runner sess
  MinimizeWindow            -> liftIO $ HA.minimizeWindow      runner sess
  FullScreenWindow          -> liftIO $ HA.fullScreenWindow    runner sess
  SwitchToFrame fr          -> liftIO $ HA.switchToFrame       runner sess fr
  SwitchToParentFrame       -> liftIO $ HA.switchToParentFrame runner sess
  GetPageSource             -> liftIO $ HA.getPageSource       runner sess
  ExecuteScript sc          -> liftIO $ HA.executeScript       runner sess sc
  ExecuteScriptAsync sc     -> liftIO $ HA.executeScriptAsync  runner sess sc
  AddCookie ck              -> liftIO $ HA.addCookie           runner sess ck
  GetAllCookies             -> liftIO $ HA.getAllCookies        runner sess
  GetNamedCookie n          -> liftIO $ HA.getNamedCookie      runner sess n
  DeleteCookie n            -> liftIO $ HA.deleteCookie        runner sess n
  DeleteAllCookies          -> liftIO $ HA.deleteAllCookies    runner sess
  PerformActions ac         -> liftIO $ HA.performActions      runner sess ac
  ReleaseActions            -> liftIO $ HA.releaseActions      runner sess
  DismissAlert              -> liftIO $ HA.dismissAlert        runner sess
  AcceptAlert               -> liftIO $ HA.acceptAlert         runner sess
  GetAlertText              -> liftIO $ HA.getAlertText        runner sess
  SendAlertText t           -> liftIO $ HA.sendAlertText       runner sess t
  TakeScreenshot            -> liftIO $ HA.takeScreenshot      runner sess
  PrintPage                 -> liftIO $ HA.printPage           runner sess
  GetActiveElement          -> liftIO $ HA.getActiveElement    runner sess
  FindElement sel           -> liftIO $ HA.findElement         runner sess sel
  FindElements sel          -> liftIO $ HA.findElements        runner sess sel
  FindElementFromElement el sel     -> liftIO $ HA.findElementFromElement  runner sess el sel
  FindElementsFromElement el sel    -> liftIO $ HA.findElementsFromElement runner sess el sel
  FindElementFromShadowRoot sr sel  -> liftIO $ HA.findElementFromShadowRoot  runner sess sr sel
  FindElementsFromShadowRoot sr sel -> liftIO $ HA.findElementsFromShadowRoot runner sess sr sel
  IsElementSelected el      -> liftIO $ HA.isElementSelected      runner sess el
  GetElementAttribute el n  -> liftIO $ HA.getElementAttribute    runner sess el n
  GetElementProperty el n   -> liftIO $ HA.getElementProperty     runner sess el n
  GetElementCssValue el n   -> liftIO $ HA.getElementCssValue     runner sess el n
  GetElementShadowRoot el   -> liftIO $ HA.getElementShadowRoot   runner sess el
  GetElementText el         -> liftIO $ HA.getElementText         runner sess el
  GetElementTagName el      -> liftIO $ HA.getElementTagName      runner sess el
  GetElementRect el         -> liftIO $ HA.getElementRect         runner sess el
  IsElementEnabled el       -> liftIO $ HA.isElementEnabled       runner sess el
  GetElementComputedRole el -> liftIO $ HA.getElementComputedRole runner sess el
  GetElementComputedLabel el -> liftIO $ HA.getElementComputedLabel runner sess el
  ElementClick el           -> liftIO $ HA.elementClick           runner sess el
  ElementClear el           -> liftIO $ HA.elementClear           runner sess el
  ElementSendKeys el t      -> liftIO $ HA.elementSendKeys        runner sess el t
  TakeElementScreenshot el  -> liftIO $ HA.takeElementScreenshot  runner sess el
  where
    runner :: forall r. (FromJSON r) => HA.Runner IO r
    runner = mkSessionRunner info
    sess   = info.session

-- ---------------------------------------------------------------------------
-- BiDi interpreter
-- ---------------------------------------------------------------------------

-- | Interpret the 'WebDriverBiDi' effect by dispatching commands and
-- registering subscriptions via the 'BiDiRunner' in 'BiDiInfo'.
--
-- The interpreter maps each effect constructor to the corresponding
-- @WebDriverPreCore.Extended.BiDi.Base.Actions@ function, using the same
-- subscription helper pattern as the Bluefin POC.
runWebDriverBiDi :: (IOE :> es) => BiDiInfo -> Eff (WebDriverBiDi : es) a -> Eff es a
runWebDriverBiDi info = interpret_ $ \case
  -- Session
  BiDiSessionNew caps   -> liftIO $ BA.sessionNew    run' caps
  BiDiSessionStatus     -> liftIO $ BA.sessionStatus run'
  BiDiSessionEnd        -> liftIO $ BA.sessionEnd    run'
  -- BrowsingContext
  BrowsingContextActivate           p -> liftIO $ BA.browsingContextActivate         run' p
  BrowsingContextCaptureScreenshot  p -> liftIO $ BA.browsingContextCaptureScreenshot run' p
  BrowsingContextClose              p -> liftIO $ BA.browsingContextClose             run' p
  BrowsingContextCreate             p -> liftIO $ BA.browsingContextCreate            run' p
  BrowsingContextGetTree            p -> liftIO $ BA.browsingContextGetTree           run' p
  BrowsingContextHandleUserPrompt   p -> liftIO $ BA.browsingContextHandleUserPrompt  run' p
  BrowsingContextLocateNodes        p -> liftIO $ BA.browsingContextLocateNodes       run' p
  BrowsingContextNavigate           p -> liftIO $ BA.browsingContextNavigate          run' p
  BrowsingContextPrint              p -> liftIO $ BA.browsingContextPrint             run' p
  BrowsingContextReload             p -> liftIO $ BA.browsingContextReload            run' p
  BrowsingContextSetViewport        p -> liftIO $ BA.browsingContextSetViewport       run' p
  BrowsingContextTraverseHistory    p -> liftIO $ BA.browsingContextTraverseHistory   run' p
  -- Browser
  BrowserClose                   -> liftIO $ BA.browserClose            run'
  BrowserCreateUserContext     p -> liftIO $ BA.browserCreateUserContext run' p
  BrowserGetClientWindows        -> liftIO $ BA.browserGetClientWindows  run'
  BrowserGetUserContexts         -> liftIO $ BA.browserGetUserContexts   run'
  BrowserRemoveUserContext     p -> liftIO $ BA.browserRemoveUserContext  run' p
  BrowserSetClientWindowState  p -> liftIO $ BA.browserSetClientWindowState run' p
  BrowserSetDownloadBehavior   p -> liftIO $ BA.browserSetDownloadBehavior  run' p
  -- Emulation
  EmulationSetForcedColorsModeThemeOverride p -> liftIO $ BA.emulationSetForcedColorsModeThemeOverride run' p
  EmulationSetGeolocationOverride           p -> liftIO $ BA.emulationSetGeolocationOverride run' p
  EmulationSetLocaleOverride                p -> liftIO $ BA.emulationSetLocaleOverride       run' p
  EmulationSetNetworkConditions             p -> liftIO $ BA.emulationSetNetworkConditions    run' p
  EmulationSetScreenOrientationOverride     p -> liftIO $ BA.emulationSetScreenOrientationOverride run' p
  EmulationSetScreenSettingsOverride        p -> liftIO $ BA.emulationSetScreenSettingsOverride    run' p
  EmulationSetScriptingEnabled              p -> liftIO $ BA.emulationSetScriptingEnabled     run' p
  EmulationSetTimezoneOverride              p -> liftIO $ BA.emulationSetTimezoneOverride     run' p
  EmulationSetTouchOverride                 p -> liftIO $ BA.emulationSetTouchOverride        run' p
  EmulationSetUserAgentOverride             p -> liftIO $ BA.emulationSetUserAgentOverride    run' p
  -- Input
  InputPerformActions p -> liftIO $ BA.inputPerformActions run' p
  InputReleaseActions p -> liftIO $ BA.inputReleaseActions run' p
  InputSetFiles       p -> liftIO $ BA.inputSetFiles       run' p
  -- Network
  NetworkAddDataCollector    p -> liftIO $ BA.networkAddDataCollector    run' p
  NetworkAddIntercept        p -> liftIO $ BA.networkAddIntercept        run' p
  NetworkContinueRequest     p -> liftIO $ BA.networkContinueRequest     run' p
  NetworkContinueResponse    p -> liftIO $ BA.networkContinueResponse    run' p
  NetworkContinueWithAuth    p -> liftIO $ BA.networkContinueWithAuth    run' p
  NetworkDisownData          p -> liftIO $ BA.networkDisownData          run' p
  NetworkFailRequest         p -> liftIO $ BA.networkFailRequest         run' p
  NetworkGetData             p -> liftIO $ BA.networkGetData             run' p
  NetworkProvideResponse     p -> liftIO $ BA.networkProvideResponse     run' p
  NetworkRemoveDataCollector p -> liftIO $ BA.networkRemoveDataCollector run' p
  NetworkRemoveIntercept     p -> liftIO $ BA.networkRemoveIntercept     run' p
  NetworkSetCacheBehavior    p -> liftIO $ BA.networkSetCacheBehavior    run' p
  NetworkSetExtraHeaders     p -> liftIO $ BA.networkSetExtraHeaders     run' p
  -- Script
  ScriptAddPreloadScript    p -> liftIO $ BA.scriptAddPreloadScript    run' p
  ScriptCallFunction        p -> liftIO $ BA.scriptCallFunction        run' p
  ScriptDisown              p -> liftIO $ BA.scriptDisown              run' p
  ScriptEvaluate            p -> liftIO $ BA.scriptEvaluate            run' p
  ScriptEvaluateNoWait      p -> liftIO $ Runner.runNoWait info.biDiRunner (mkCommand BP.ScriptEvaluate p)
  ScriptGetRealms           p -> liftIO $ BA.scriptGetRealms           run' p
  ScriptRemovePreloadScript p -> liftIO $ BA.scriptRemovePreloadScript run' p
  -- Storage
  StorageDeleteCookies p -> liftIO $ BA.storageDeleteCookies run' p
  StorageGetCookies    p -> liftIO $ BA.storageGetCookies    run' p
  StorageSetCookie     p -> liftIO $ BA.storageSetCookie     run' p
  -- WebExtension
  WebExtensionInstall   p -> liftIO $ BA.webExtensionInstall   run' p
  WebExtensionUninstall p -> liftIO $ BA.webExtensionUninstall run' p
  -- Generic escape hatches
  SendBiDiCmd cmd             -> liftIO $ bidiRun info cmd
  SendBiDiCmdNoWait cmd       -> liftIO $ Runner.runNoWait info.biDiRunner cmd
  SendBiDiOffSpecCmd mid m ps -> liftIO $ info.biDiRunner.runOffSpecWithId mid m ps
  SendBiDiOffSpecCmdNoWait m ps -> liftIO $ Runner.runOffSpecNoWait info.biDiRunner m ps
  -- Log subscriptions
  SubscribeLogEntryAdded  h    -> liftIO $ BA.subscribeLogEntryAdded  (mkSendSub  info.biDiRunner) h
  SubscribeLogEntryAdded' b u h -> liftIO $ BA.subscribeLogEntryAdded' (mkSendSub' info.biDiRunner) b u h
  -- BrowsingContext subscriptions
  SubscribeBrowsingContextCreated              h     -> liftIO $ BA.subscribeBrowsingContextCreated             (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextCreated'             b u h -> liftIO $ BA.subscribeBrowsingContextCreated'            (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextDestroyed            h     -> liftIO $ BA.subscribeBrowsingContextDestroyed           (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextDestroyed'           b u h -> liftIO $ BA.subscribeBrowsingContextDestroyed'          (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextNavigationStarted    h     -> liftIO $ BA.subscribeBrowsingContextNavigationStarted   (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextNavigationStarted'   b u h -> liftIO $ BA.subscribeBrowsingContextNavigationStarted'  (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextFragmentNavigated    h     -> liftIO $ BA.subscribeBrowsingContextFragmentNavigated   (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextFragmentNavigated'   b u h -> liftIO $ BA.subscribeBrowsingContextFragmentNavigated'  (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextHistoryUpdated       h     -> liftIO $ BA.subscribeBrowsingContextHistoryUpdated      (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextHistoryUpdated'      b u h -> liftIO $ BA.subscribeBrowsingContextHistoryUpdated'     (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextDomContentLoaded     h     -> liftIO $ BA.subscribeBrowsingContextDomContentLoaded    (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextDomContentLoaded'    b u h -> liftIO $ BA.subscribeBrowsingContextDomContentLoaded'   (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextLoad                 h     -> liftIO $ BA.subscribeBrowsingContextLoad                (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextLoad'                b u h -> liftIO $ BA.subscribeBrowsingContextLoad'               (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextDownloadWillBegin    h     -> liftIO $ BA.subscribeBrowsingContextDownloadWillBegin   (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextDownloadWillBegin'   b u h -> liftIO $ BA.subscribeBrowsingContextDownloadWillBegin'  (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextDownloadEnd          h     -> liftIO $ BA.subscribeBrowsingContextDownloadEnd         (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextDownloadEnd'         b u h -> liftIO $ BA.subscribeBrowsingContextDownloadEnd'        (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextNavigationAborted    h     -> liftIO $ BA.subscribeBrowsingContextNavigationAborted   (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextNavigationAborted'   b u h -> liftIO $ BA.subscribeBrowsingContextNavigationAborted'  (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextNavigationCommitted  h     -> liftIO $ BA.subscribeBrowsingContextNavigationCommitted (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextNavigationCommitted' b u h -> liftIO $ BA.subscribeBrowsingContextNavigationCommitted' (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextNavigationFailed     h     -> liftIO $ BA.subscribeBrowsingContextNavigationFailed    (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextNavigationFailed'    b u h -> liftIO $ BA.subscribeBrowsingContextNavigationFailed'   (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextUserPromptClosed     h     -> liftIO $ BA.subscribeBrowsingContextUserPromptClosed    (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextUserPromptClosed'    b u h -> liftIO $ BA.subscribeBrowsingContextUserPromptClosed'   (mkSendSub' info.biDiRunner) b u h
  SubscribeBrowsingContextUserPromptOpened     h     -> liftIO $ BA.subscribeBrowsingContextUserPromptOpened    (mkSendSub  info.biDiRunner) h
  SubscribeBrowsingContextUserPromptOpened'    b u h -> liftIO $ BA.subscribeBrowsingContextUserPromptOpened'   (mkSendSub' info.biDiRunner) b u h
  -- Network subscriptions
  SubscribeNetworkAuthRequired       h     -> liftIO $ BA.subscribeNetworkAuthRequired      (mkSendSub  info.biDiRunner) h
  SubscribeNetworkAuthRequired'      b u h -> liftIO $ BA.subscribeNetworkAuthRequired'     (mkSendSub' info.biDiRunner) b u h
  SubscribeNetworkBeforeRequestSent  h     -> liftIO $ BA.subscribeNetworkBeforeRequestSent (mkSendSub  info.biDiRunner) h
  SubscribeNetworkBeforeRequestSent' b u h -> liftIO $ BA.subscribeNetworkBeforeRequestSent' (mkSendSub' info.biDiRunner) b u h
  SubscribeNetworkFetchError         h     -> liftIO $ BA.subscribeNetworkFetchError        (mkSendSub  info.biDiRunner) h
  SubscribeNetworkFetchError'        b u h -> liftIO $ BA.subscribeNetworkFetchError'       (mkSendSub' info.biDiRunner) b u h
  SubscribeNetworkResponseCompleted  h     -> liftIO $ BA.subscribeNetworkResponseCompleted (mkSendSub  info.biDiRunner) h
  SubscribeNetworkResponseCompleted' b u h -> liftIO $ BA.subscribeNetworkResponseCompleted' (mkSendSub' info.biDiRunner) b u h
  SubscribeNetworkResponseStarted    h     -> liftIO $ BA.subscribeNetworkResponseStarted   (mkSendSub  info.biDiRunner) h
  SubscribeNetworkResponseStarted'   b u h -> liftIO $ BA.subscribeNetworkResponseStarted'  (mkSendSub' info.biDiRunner) b u h
  -- Script subscriptions
  SubscribeScriptMessage         h     -> liftIO $ BA.subscribeScriptMessage        (mkSendSub  info.biDiRunner) h
  SubscribeScriptMessage'        b u h -> liftIO $ BA.subscribeScriptMessage'       (mkSendSub' info.biDiRunner) b u h
  SubscribeScriptRealmCreated    h     -> liftIO $ BA.subscribeScriptRealmCreated   (mkSendSub  info.biDiRunner) h
  SubscribeScriptRealmCreated'   b u h -> liftIO $ BA.subscribeScriptRealmCreated'  (mkSendSub' info.biDiRunner) b u h
  SubscribeScriptRealmDestroyed  h     -> liftIO $ BA.subscribeScriptRealmDestroyed (mkSendSub  info.biDiRunner) h
  SubscribeScriptRealmDestroyed' b u h -> liftIO $ BA.subscribeScriptRealmDestroyed' (mkSendSub' info.biDiRunner) b u h
  -- Input subscriptions
  SubscribeInputFileDialogOpened  h     -> liftIO $ BA.subscribeInputFileDialogOpened  (mkSendSub  info.biDiRunner) h
  SubscribeInputFileDialogOpened' b u h -> liftIO $ BA.subscribeInputFileDialogOpened' (mkSendSub' info.biDiRunner) b u h
  -- Multi-event subscriptions
  SubscribeMany         sts h      -> liftIO $ BA.subscribeMany' (mkSendSubMany'        info.biDiRunner) sts [] [] h
  SubscribeMany'        b u sts h  -> liftIO $ BA.subscribeMany' (mkSendSubMany'        info.biDiRunner) sts b  u  h
  SubscribeUnknownMany  sts h      -> liftIO $ BA.subscribeOffSpecMany' (mkSendSubOffSpecMany' info.biDiRunner) sts [] [] h
  SubscribeUnknownMany' b u sts h  -> liftIO $ BA.subscribeOffSpecMany' (mkSendSubOffSpecMany' info.biDiRunner) sts b  u  h
  -- Unsubscribe
  Unsubscribe subId ->
    liftIO $
      Runner.unsubscribe
        info.biDiRunner.socketActions
        (bidiRun info . BA.sessionUnsubscribe)
        (UnsubscribeById {subscriptions = [subId]})
  where
    run' :: forall r. FromJSON r => Command r -> IO r
    run' = bidiRun info
