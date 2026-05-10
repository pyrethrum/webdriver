-- |
-- Module: WebDriver.Effectful.HTTP.Base.Effect
-- Description: HTTP WebDriver algebraic effect definition
--
-- Defines the 'WebDriverHttp' algebraic effect together with the
-- configuration types 'HttpDriverInfo', 'HttpSessionInfo', and
-- 'defaultDriverInfo' used to configure the interpreter.
--
-- Smart constructors for each operation are in
-- "WebDriver.Effectful.HTTP.Base.Actions".
-- The IO-backed interpreter is in
-- "WebDriver.Effectful.HTTP.Base.Interpreter".
module WebDriver.Effectful.HTTP.Base.Effect
  ( -- * Configuration types
    HttpDriverInfo (..),
    HttpSessionInfo (..),
    defaultDriverInfo,

    -- * Internal helpers
    mkSessionRunner,

    -- * HTTP Effect
    WebDriverHttp (..),
  )
where

import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import Effectful (Dispatch (..), DispatchOf, Effect)
import UnliftIO (throwIO)
import WebDriverPreCore.Error (parseFailToWDException)
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
  { driverInfo :: HttpDriverInfo,
    -- | The active WebDriver session identifier.
    session :: Session,
    -- | Duration to sleep on each 'pause' call.
    pauseDuration :: Timeout
  }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Build a @Command a -> IO a@ runner from an 'HttpSessionInfo'.
mkSessionRunner :: (FromJSON a) => HttpSessionInfo -> HA.Runner IO a
mkSessionRunner info cmd =
  callWebDriver info.driverInfo.httpEndpoint info.driverInfo.driverLogFn cmd
    >>= either (throwIO . parseFailToWDException) pure

-- ---------------------------------------------------------------------------
-- HTTP effect
-- ---------------------------------------------------------------------------

-- | Algebraic effect encoding all HTTP WebDriver session-level operations.
--
-- Each constructor corresponds to one WebDriver HTTP command.  The effect is
-- interpreted by 'WebDriver.Effectful.HTTP.Base.Interpreter.runWebDriverHttp',
-- which delegates to the underlying HTTP runner.  Alternative interpreters
-- (e.g. pure test doubles) can be provided without changing call-site code.
--
-- Smart constructors for each operation are in
-- "WebDriver.Effectful.HTTP.Base.Actions".
data WebDriverHttp :: Effect where
  -- Session management
  -- TODO: not sure delete session belongs here
  DeleteSession :: WebDriverHttp m ()
  GetTimeouts :: WebDriverHttp m Timeouts
  SetTimeouts :: Timeouts -> WebDriverHttp m ()
  -- Navigation
  NavigateTo :: URL -> WebDriverHttp m ()
  GetCurrentUrl :: WebDriverHttp m URL
  Back :: WebDriverHttp m ()
  Forward :: WebDriverHttp m ()
  Refresh :: WebDriverHttp m ()
  GetTitle :: WebDriverHttp m Text
  -- Windows
  GetWindowHandle :: WebDriverHttp m Handle
  GetWindowHandles :: WebDriverHttp m [Handle]
  NewWindow :: WebDriverHttp m WindowHandleSpec
  CloseWindow :: WebDriverHttp m [Handle]
  SwitchToWindow :: Handle -> WebDriverHttp m ()
  GetWindowRect :: WebDriverHttp m WindowRect
  SetWindowRect :: WindowRect -> WebDriverHttp m WindowRect
  MaximizeWindow :: WebDriverHttp m WindowRect
  MinimizeWindow :: WebDriverHttp m WindowRect
  FullScreenWindow :: WebDriverHttp m WindowRect
  -- Frames
  SwitchToFrame :: FrameReference -> WebDriverHttp m ()
  SwitchToParentFrame :: WebDriverHttp m ()
  -- Page / Script
  GetPageSource :: WebDriverHttp m Text
  ExecuteScript :: Script -> WebDriverHttp m Value
  ExecuteScriptAsync :: Script -> WebDriverHttp m Value
  -- Cookies
  AddCookie :: Cookie -> WebDriverHttp m ()
  GetAllCookies :: WebDriverHttp m [Cookie]
  GetNamedCookie :: Text -> WebDriverHttp m Cookie
  DeleteCookie :: Text -> WebDriverHttp m ()
  DeleteAllCookies :: WebDriverHttp m ()
  -- Actions / Prompts
  PerformActions :: Actions -> WebDriverHttp m ()
  ReleaseActions :: WebDriverHttp m ()
  DismissAlert :: WebDriverHttp m ()
  AcceptAlert :: WebDriverHttp m ()
  GetAlertText :: WebDriverHttp m Text
  SendAlertText :: Text -> WebDriverHttp m ()
  -- Screenshots / Print
  TakeScreenshot :: WebDriverHttp m Text
  PrintPage :: WebDriverHttp m Text
  -- Elements
  GetActiveElement :: WebDriverHttp m ElementId
  FindElement :: Selector -> WebDriverHttp m ElementId
  FindElements :: Selector -> WebDriverHttp m [ElementId]
  -- Element sub-finders
  FindElementFromElement :: ElementId -> Selector -> WebDriverHttp m ElementId
  FindElementsFromElement :: ElementId -> Selector -> WebDriverHttp m [ElementId]
  FindElementFromShadowRoot :: ShadowRootElementId -> Selector -> WebDriverHttp m ElementId
  FindElementsFromShadowRoot :: ShadowRootElementId -> Selector -> WebDriverHttp m [ElementId]
  -- Element state
  IsElementSelected :: ElementId -> WebDriverHttp m Bool
  GetElementAttribute :: ElementId -> Text -> WebDriverHttp m (Maybe Text)
  GetElementProperty :: ElementId -> Text -> WebDriverHttp m (Maybe Value)
  GetElementCssValue :: ElementId -> Text -> WebDriverHttp m Text
  GetElementShadowRoot :: ElementId -> WebDriverHttp m ShadowRootElementId
  GetElementText :: ElementId -> WebDriverHttp m Text
  GetElementTagName :: ElementId -> WebDriverHttp m Text
  GetElementRect :: ElementId -> WebDriverHttp m WindowRect
  IsElementEnabled :: ElementId -> WebDriverHttp m Bool
  GetElementComputedRole :: ElementId -> WebDriverHttp m Text
  GetElementComputedLabel :: ElementId -> WebDriverHttp m Text
  -- Element actions
  ElementClick :: ElementId -> WebDriverHttp m ()
  ElementClear :: ElementId -> WebDriverHttp m ()
  ElementSendKeys :: ElementId -> Text -> WebDriverHttp m ()
  TakeElementScreenshot :: ElementId -> WebDriverHttp m Text

type instance DispatchOf WebDriverHttp = Dynamic
