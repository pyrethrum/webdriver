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
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import WebDriver.Effectful.HTTP.Base.Effect
  ( HttpSessionInfo (..),
    WebDriverHttp (..),
    mkSessionRunner,
  )
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.Extended.Protocol (Session)

-- ---------------------------------------------------------------------------
-- HTTP interpreter
-- ---------------------------------------------------------------------------

-- | Interpret the 'WebDriverHttp' effect by running HTTP WebDriver commands
-- against the session described by 'HttpSessionInfo'.
--
-- The interpreter maps each effect constructor to the corresponding
-- @WebDriverPreCore.Extended.HTTP.Base.Actions@ function.
runWebDriverHttp :: forall es a. (IOE :> es) => HttpSessionInfo -> Eff (WebDriverHttp : es) a -> Eff es a
runWebDriverHttp info = interpret $ \_localEnv -> \case
  DeleteSession -> run HA.deleteSession
  GetTimeouts -> run HA.getTimeouts
  SetTimeouts ts -> run1 HA.setTimeouts ts
  NavigateTo url -> run1 HA.navigateTo url
  GetCurrentUrl -> run HA.getCurrentUrl
  Back -> run HA.back
  Forward -> run HA.forward
  Refresh -> run HA.refresh
  GetTitle -> run HA.getTitle
  GetWindowHandle -> run HA.getWindowHandle
  GetWindowHandles -> run HA.getWindowHandles
  NewWindow -> run HA.newWindow
  CloseWindow -> run HA.closeWindow
  SwitchToWindow wh -> run1 HA.switchToWindow wh
  GetWindowRect -> run HA.getWindowRect
  SetWindowRect wr -> run1 HA.setWindowRect wr
  MaximizeWindow -> run HA.maximizeWindow
  MinimizeWindow -> run HA.minimizeWindow
  FullScreenWindow -> run HA.fullScreenWindow
  SwitchToFrame fr -> run1 HA.switchToFrame fr
  SwitchToParentFrame -> run HA.switchToParentFrame
  GetPageSource -> run HA.getPageSource
  ExecuteScript sc -> run1 HA.executeScript sc
  ExecuteScriptAsync sc -> run1 HA.executeScriptAsync sc
  AddCookie ck -> run1 HA.addCookie ck
  GetAllCookies -> run HA.getAllCookies
  GetNamedCookie n -> run1 HA.getNamedCookie n
  DeleteCookie n -> run1 HA.deleteCookie n
  DeleteAllCookies -> run HA.deleteAllCookies
  PerformActions ac -> run1 HA.performActions ac
  ReleaseActions -> run HA.releaseActions
  DismissAlert -> run HA.dismissAlert
  AcceptAlert -> run HA.acceptAlert
  GetAlertText -> run HA.getAlertText
  SendAlertText t -> run1 HA.sendAlertText t
  TakeScreenshot -> run HA.takeScreenshot
  PrintPage -> run HA.printPage
  GetActiveElement -> run HA.getActiveElement
  FindElement sel -> run1 HA.findElement sel
  FindElements sel -> run1 HA.findElements sel
  FindElementFromElement el sel -> run2 HA.findElementFromElement el sel
  FindElementsFromElement el sel -> run2 HA.findElementsFromElement el sel
  FindElementFromShadowRoot sr sel -> run2 HA.findElementFromShadowRoot sr sel
  FindElementsFromShadowRoot sr sel -> run2 HA.findElementsFromShadowRoot sr sel
  IsElementSelected el -> run1 HA.isElementSelected el
  GetElementAttribute el n -> run2 HA.getElementAttribute el n
  GetElementProperty el n -> run2 HA.getElementProperty el n
  GetElementCssValue el n -> run2 HA.getElementCssValue el n
  GetElementShadowRoot el -> run1 HA.getElementShadowRoot el
  GetElementText el -> run1 HA.getElementText el
  GetElementTagName el -> run1 HA.getElementTagName el
  GetElementRect el -> run1 HA.getElementRect el
  IsElementEnabled el -> run1 HA.isElementEnabled el
  GetElementComputedRole el -> run1 HA.getElementComputedRole el
  GetElementComputedLabel el -> run1 HA.getElementComputedLabel el
  ElementClick el -> run1 HA.elementClick el
  ElementClear el -> run1 HA.elementClear el
  ElementSendKeys el t -> run2 HA.elementSendKeys el t
  TakeElementScreenshot el -> run1 HA.takeElementScreenshot el
  where
    runner :: forall r. (FromJSON r) => HA.Runner IO r
    runner = mkSessionRunner info

    sess :: Session
    sess = info.session

    run :: forall r. (FromJSON r) => (HA.Runner IO r -> Session -> IO r) -> Eff es r
    run action = liftIO $ action runner sess

    run1 :: forall r p. (FromJSON r) => (HA.Runner IO r -> Session -> p -> IO r) -> p -> Eff es r
    run1 action p = liftIO $ action runner sess p
    
    run2 :: forall r p1 p2. (FromJSON r) => (HA.Runner IO r -> Session -> p1 -> p2 -> IO r) -> p1 -> p2 -> Eff es r
    run2 action p1 p2 = liftIO $ action runner sess p1 p2
