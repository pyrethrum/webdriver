-- |
-- Module: WebDriver.Effectful.HTTP.Base.Effect
-- Description: HTTP WebDriver  effect definition
module WebDriver.Effectful.HTTP.Base.Effect
  ( 
    -- * HTTP Effect
    WebDriver (..),
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful (Dispatch (..), DispatchOf, Effect)
import WebDriverPreCore.HTTP.Protocol
    ( Actions,
      Cookie,
      ElementId,
      FrameReference,
      Handle,
      Script,
      Selector,
      ShadowRootElementId,
      Status,
      Timeouts,
      WindowHandleSpec,
      WindowRect )
import WebDriverPreCore.Common.Protocol (URL)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

--  DELETE ME
-- defaultHttpEndpoint :: HttpEndpoint
-- defaultHttpEndpoint = MkHttpEndpoint {host = "127.0.0.1", port = 4444}

-- noOpLogger :: Text -> IO ()
-- noOpLogger _ = pure ()

-- -- | Session-scoped HTTP driver configuration.
-- data HttpSessionInfo = MkHttpSessionInfo
--   { endpoint :: HttpEndpoint,
--     -- | A logger for internal Webdriver operations.
--     logger :: Text -> IO (),
--     -- | Full response
--     sessionResponse :: HttpSessionResponse
--   }

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
data WebDriver :: Effect where
  -- Root operations (no session required)
  Status :: WebDriver m Status
  -- Session operations (require active session managed by withHttpSession)
  GetTimeouts :: WebDriver m Timeouts
  SetTimeouts :: Timeouts -> WebDriver m ()
  -- Navigation
  NavigateTo :: URL -> WebDriver m ()
  GetCurrentUrl :: WebDriver m URL
  Back :: WebDriver m ()
  Forward :: WebDriver m ()
  Refresh :: WebDriver m ()
  GetTitle :: WebDriver m Text
  -- Windows
  GetWindowHandle :: WebDriver m Handle
  GetWindowHandles :: WebDriver m [Handle]
  NewWindow :: WebDriver m WindowHandleSpec
  CloseWindow :: WebDriver m [Handle]
  SwitchToWindow :: Handle -> WebDriver m ()
  GetWindowRect :: WebDriver m WindowRect
  SetWindowRect :: WindowRect -> WebDriver m WindowRect
  MaximizeWindow :: WebDriver m WindowRect
  MinimizeWindow :: WebDriver m WindowRect
  FullScreenWindow :: WebDriver m WindowRect
  -- Frames
  SwitchToFrame :: FrameReference -> WebDriver m ()
  SwitchToParentFrame :: WebDriver m ()
  -- Page / Script
  GetPageSource :: WebDriver m Text
  ExecuteScript :: Script -> WebDriver m Value
  ExecuteScriptAsync :: Script -> WebDriver m Value
  -- Cookies
  AddCookie :: Cookie -> WebDriver m ()
  GetAllCookies :: WebDriver m [Cookie]
  GetNamedCookie :: Text -> WebDriver m Cookie
  DeleteCookie :: Text -> WebDriver m ()
  DeleteAllCookies :: WebDriver m ()
  -- Actions / Prompts
  PerformActions :: Actions -> WebDriver m ()
  ReleaseActions :: WebDriver m ()
  DismissAlert :: WebDriver m ()
  AcceptAlert :: WebDriver m ()
  GetAlertText :: WebDriver m Text
  SendAlertText :: Text -> WebDriver m ()
  -- Screenshots / Print
  TakeScreenshot :: WebDriver m Text
  PrintPage :: WebDriver m Text
  -- Elements
  GetActiveElement :: WebDriver m ElementId
  FindElement :: Selector -> WebDriver m ElementId
  FindElements :: Selector -> WebDriver m [ElementId]
  -- Element sub-finders
  FindElementFromElement :: ElementId -> Selector -> WebDriver m ElementId
  FindElementsFromElement :: ElementId -> Selector -> WebDriver m [ElementId]
  FindElementFromShadowRoot :: ShadowRootElementId -> Selector -> WebDriver m ElementId
  FindElementsFromShadowRoot :: ShadowRootElementId -> Selector -> WebDriver m [ElementId]
  -- Element state
  IsElementSelected :: ElementId -> WebDriver m Bool
  GetElementAttribute :: ElementId -> Text -> WebDriver m (Maybe Text)
  GetElementProperty :: ElementId -> Text -> WebDriver m (Maybe Value)
  GetElementCssValue :: ElementId -> Text -> WebDriver m Text
  GetElementShadowRoot :: ElementId -> WebDriver m ShadowRootElementId
  GetElementText :: ElementId -> WebDriver m Text
  GetElementTagName :: ElementId -> WebDriver m Text
  GetElementRect :: ElementId -> WebDriver m WindowRect
  IsElementEnabled :: ElementId -> WebDriver m Bool
  GetElementComputedRole :: ElementId -> WebDriver m Text
  GetElementComputedLabel :: ElementId -> WebDriver m Text
  -- Element actions
  ElementClick :: ElementId -> WebDriver m ()
  ElementClear :: ElementId -> WebDriver m ()
  ElementSendKeys :: ElementId -> Text -> WebDriver m ()
  TakeElementScreenshot :: ElementId -> WebDriver m Text

type instance DispatchOf WebDriver = Dynamic
