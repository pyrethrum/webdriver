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

-- ---------------------------------------------------------------------------
-- HTTP interpreter
-- ---------------------------------------------------------------------------

-- | Interpret the 'WebDriverHttp' effect by running HTTP WebDriver commands
-- against the session described by 'HttpSessionInfo'.
--
-- The interpreter maps each effect constructor to the corresponding
-- @WebDriverPreCore.Extended.HTTP.Base.Actions@ function.
runWebDriverHttp :: (IOE :> es) => HttpSessionInfo -> Eff (WebDriverHttp : es) a -> Eff es a
runWebDriverHttp info = interpret $ \_localEnv -> \case
  DeleteSession -> liftIO $ HA.deleteSession runner sess
  GetTimeouts -> liftIO $ HA.getTimeouts runner sess
  SetTimeouts ts -> liftIO $ HA.setTimeouts runner sess ts
  NavigateTo url -> liftIO $ HA.navigateTo runner sess url
  GetCurrentUrl -> liftIO $ HA.getCurrentUrl runner sess
  Back -> liftIO $ HA.back runner sess
  Forward -> liftIO $ HA.forward runner sess
  Refresh -> liftIO $ HA.refresh runner sess
  GetTitle -> liftIO $ HA.getTitle runner sess
  GetWindowHandle -> liftIO $ HA.getWindowHandle runner sess
  GetWindowHandles -> liftIO $ HA.getWindowHandles runner sess
  NewWindow -> liftIO $ HA.newWindow runner sess
  CloseWindow -> liftIO $ HA.closeWindow runner sess
  SwitchToWindow wh -> liftIO $ HA.switchToWindow runner sess wh
  GetWindowRect -> liftIO $ HA.getWindowRect runner sess
  SetWindowRect wr -> liftIO $ HA.setWindowRect runner sess wr
  MaximizeWindow -> liftIO $ HA.maximizeWindow runner sess
  MinimizeWindow -> liftIO $ HA.minimizeWindow runner sess
  FullScreenWindow -> liftIO $ HA.fullScreenWindow runner sess
  SwitchToFrame fr -> liftIO $ HA.switchToFrame runner sess fr
  SwitchToParentFrame -> liftIO $ HA.switchToParentFrame runner sess
  GetPageSource -> liftIO $ HA.getPageSource runner sess
  ExecuteScript sc -> liftIO $ HA.executeScript runner sess sc
  ExecuteScriptAsync sc -> liftIO $ HA.executeScriptAsync runner sess sc
  AddCookie ck -> liftIO $ HA.addCookie runner sess ck
  GetAllCookies -> liftIO $ HA.getAllCookies runner sess
  GetNamedCookie n -> liftIO $ HA.getNamedCookie runner sess n
  DeleteCookie n -> liftIO $ HA.deleteCookie runner sess n
  DeleteAllCookies -> liftIO $ HA.deleteAllCookies runner sess
  PerformActions ac -> liftIO $ HA.performActions runner sess ac
  ReleaseActions -> liftIO $ HA.releaseActions runner sess
  DismissAlert -> liftIO $ HA.dismissAlert runner sess
  AcceptAlert -> liftIO $ HA.acceptAlert runner sess
  GetAlertText -> liftIO $ HA.getAlertText runner sess
  SendAlertText t -> liftIO $ HA.sendAlertText runner sess t
  TakeScreenshot -> liftIO $ HA.takeScreenshot runner sess
  PrintPage -> liftIO $ HA.printPage runner sess
  GetActiveElement -> liftIO $ HA.getActiveElement runner sess
  FindElement sel -> liftIO $ HA.findElement runner sess sel
  FindElements sel -> liftIO $ HA.findElements runner sess sel
  FindElementFromElement el sel -> liftIO $ HA.findElementFromElement runner sess el sel
  FindElementsFromElement el sel -> liftIO $ HA.findElementsFromElement runner sess el sel
  FindElementFromShadowRoot sr sel -> liftIO $ HA.findElementFromShadowRoot runner sess sr sel
  FindElementsFromShadowRoot sr sel -> liftIO $ HA.findElementsFromShadowRoot runner sess sr sel
  IsElementSelected el -> liftIO $ HA.isElementSelected runner sess el
  GetElementAttribute el n -> liftIO $ HA.getElementAttribute runner sess el n
  GetElementProperty el n -> liftIO $ HA.getElementProperty runner sess el n
  GetElementCssValue el n -> liftIO $ HA.getElementCssValue runner sess el n
  GetElementShadowRoot el -> liftIO $ HA.getElementShadowRoot runner sess el
  GetElementText el -> liftIO $ HA.getElementText runner sess el
  GetElementTagName el -> liftIO $ HA.getElementTagName runner sess el
  GetElementRect el -> liftIO $ HA.getElementRect runner sess el
  IsElementEnabled el -> liftIO $ HA.isElementEnabled runner sess el
  GetElementComputedRole el -> liftIO $ HA.getElementComputedRole runner sess el
  GetElementComputedLabel el -> liftIO $ HA.getElementComputedLabel runner sess el
  ElementClick el -> liftIO $ HA.elementClick runner sess el
  ElementClear el -> liftIO $ HA.elementClear runner sess el
  ElementSendKeys el t -> liftIO $ HA.elementSendKeys runner sess el t
  TakeElementScreenshot el -> liftIO $ HA.takeElementScreenshot runner sess el
  where
    runner :: forall r. (FromJSON r) => HA.Runner IO r
    runner = mkSessionRunner info
    sess = info.session
