-- |
-- Module: WebDriver.Effectful.HTTP.Base.Interpreter
-- Description: IO-backed interpreter for the 'WebDriverHttp' effect
--
-- Provides 'runWebDriverHttp', which interprets the 'WebDriverHttp'
-- algebraic effect by dispatching each constructor to the corresponding
-- function in "WebDriverPreCore.Extended.HTTP.Base.Actions".
module WebDriver.Effectful.HTTP.Base.Interpreter
  ( runWebDriverHttp,
  )
where

import Data.Aeson (FromJSON)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import UnliftIO (throwIO)
import WebDriver.Effectful.HTTP.Base.Effect
  ( WebDriverHttp (..),
  )
import WebDriver.Effectful.Logger.Effect (Logger, logDebug)
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as A
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.Extended.HTTP.Base.Protocol (parseFailToWDException)
import WebDriverPreCore.Extended.Protocol (Session)
import WebDriverPreCore.HttpRunner qualified as HR

data HttpParams = MkHttpParams
  { session :: Session,
    endpoint :: HR.HttpEndpoint
  }

-- ---------------------------------------------------------------------------
-- HTTP interpreter
-- ---------------------------------------------------------------------------

runWebDriverHttp :: forall es a. (IOE :> es, Logger :> es) => HttpParams -> Eff (WebDriverHttp : es) a -> Eff es a
runWebDriverHttp MkHttpParams {session = sess, endpoint} = interpret $ \_localEnv -> \case
  Status -> runRoot A.status
  GetTimeouts -> run A.getTimeouts
  SetTimeouts ts -> run1 A.setTimeouts ts
  NavigateTo url -> run1 A.navigateTo url
  GetCurrentUrl -> run A.getCurrentUrl
  Back -> run A.back
  Forward -> run A.forward
  Refresh -> run A.refresh
  GetTitle -> run A.getTitle
  GetWindowHandle -> run A.getWindowHandle
  GetWindowHandles -> run A.getWindowHandles
  NewWindow -> run A.newWindow
  CloseWindow -> run A.closeWindow
  SwitchToWindow wh -> run1 A.switchToWindow wh
  GetWindowRect -> run A.getWindowRect
  SetWindowRect wr -> run1 A.setWindowRect wr
  MaximizeWindow -> run A.maximizeWindow
  MinimizeWindow -> run A.minimizeWindow
  FullScreenWindow -> run A.fullScreenWindow
  SwitchToFrame fr -> run1 A.switchToFrame fr
  SwitchToParentFrame -> run A.switchToParentFrame
  GetPageSource -> run A.getPageSource
  ExecuteScript sc -> run1 A.executeScript sc
  ExecuteScriptAsync sc -> run1 A.executeScriptAsync sc
  AddCookie ck -> run1 A.addCookie ck
  GetAllCookies -> run A.getAllCookies
  GetNamedCookie n -> run1 A.getNamedCookie n
  DeleteCookie n -> run1 A.deleteCookie n
  DeleteAllCookies -> run A.deleteAllCookies
  PerformActions ac -> run1 A.performActions ac
  ReleaseActions -> run A.releaseActions
  DismissAlert -> run A.dismissAlert
  AcceptAlert -> run A.acceptAlert
  GetAlertText -> run A.getAlertText
  SendAlertText t -> run1 A.sendAlertText t
  TakeScreenshot -> run A.takeScreenshot
  PrintPage -> run A.printPage
  GetActiveElement -> run A.getActiveElement
  FindElement sel -> run1 A.findElement sel
  FindElements sel -> run1 A.findElements sel
  FindElementFromElement el sel -> run2 A.findElementFromElement el sel
  FindElementsFromElement el sel -> run2 A.findElementsFromElement el sel
  FindElementFromShadowRoot sr sel -> run2 A.findElementFromShadowRoot sr sel
  FindElementsFromShadowRoot sr sel -> run2 A.findElementsFromShadowRoot sr sel
  IsElementSelected el -> run1 A.isElementSelected el
  GetElementAttribute el n -> run2 A.getElementAttribute el n
  GetElementProperty el n -> run2 A.getElementProperty el n
  GetElementCssValue el n -> run2 A.getElementCssValue el n
  GetElementShadowRoot el -> run1 A.getElementShadowRoot el
  GetElementText el -> run1 A.getElementText el
  GetElementTagName el -> run1 A.getElementTagName el
  GetElementRect el -> run1 A.getElementRect el
  IsElementEnabled el -> run1 A.isElementEnabled el
  GetElementComputedRole el -> run1 A.getElementComputedRole el
  GetElementComputedLabel el -> run1 A.getElementComputedLabel el
  ElementClick el -> run1 A.elementClick el
  ElementClear el -> run1 A.elementClear el
  ElementSendKeys el t -> run2 A.elementSendKeys el t
  TakeElementScreenshot el -> run1 A.takeElementScreenshot el
  where
    runner :: forall r. (FromJSON r) => A.Runner (Eff es) r
    runner = callWebDriver endpoint

    runRoot :: forall r. (FromJSON r) => (A.Runner (Eff es) r ->  (Eff es)  r) -> Eff es r
    runRoot action = action runner

    run :: forall r. (FromJSON r) => (A.Runner (Eff es) r -> Session -> (Eff es) r) -> Eff es r
    run action = action runner sess

    run1 :: forall r p. (FromJSON r) => (A.Runner (Eff es) r -> Session -> p -> (Eff es) r) -> p -> Eff es r
    run1 action p = action runner sess p

    run2 :: forall r p1 p2. (FromJSON r) => (A.Runner (Eff es) r -> Session -> p1 -> p2 -> (Eff es) r) -> p1 -> p2 -> Eff es r
    run2 action p1 p2 = action runner sess p1 p2

callWebDriver :: forall a es. (IOE :> es, Logger :> es, FromJSON a) => HR.HttpEndpoint -> HA.Runner (Eff es) a
callWebDriver endpoint cmd =
  HR.callWebDriver endpoint logDebug cmd >>= either (throwIO . parseFailToWDException) pure
