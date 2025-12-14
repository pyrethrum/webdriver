module HTTP.HttpActionsDeprecated
  ( 
    mkDeprecatedActions,
  )
where

import HTTP.HttpRunnerDeprecated (HttpRunnerDeprecated (..))

import  WebDriverPreCore.HTTP.Protocol qualified as P
import WebDriverPreCore.HTTP.SpecDefinition qualified as W
import HTTP.HttpActions (HttpActions (..))
import WebDriverPreCore.Http qualified as Legacy

mkDeprecatedActions :: HttpRunnerDeprecated -> HttpActions
mkDeprecatedActions MkHttpRunnerDeprecated {run} =
  MkHttpActions
    { -- Root methods
      status = run W.status,
      newSession = run . W.newSession,
      -- Session methods
      deleteSession = run . W.deleteSession,
      getTimeouts = run . W.getTimeouts,
      setTimeouts = sessRun W.setTimeouts,
      navigateTo = sessRun W.navigateTo,
      getCurrentUrl = run . W.getCurrentUrl,
      back = run . W.back,
      forward = run . W.forward,
      refresh = run . W.refresh,
      getTitle = run . W.getTitle,
      getWindowHandle = run . W.getWindowHandle,
      newWindow = run . W.newWindow,
      closeWindow = run . W.closeWindow,
      switchToWindow = sessRun W.switchToWindow,
      switchToFrame = sessRun W.switchToFrame,
      getPageSource = run . W.getPageSource,
      executeScript = sessRun W.executeScript,
      executeScriptAsync = sessRun W.executeScriptAsync,
      addCookie = sessRun W.addCookie,
      getAllCookies = run . W.getAllCookies,
      getNamedCookie = sessRun W.getNamedCookie,
      deleteCookie = sessRun W.deleteCookie,
      deleteAllCookies = run . W.deleteAllCookies,
      performActions = sessRun W.performActions,
      releaseActions = run . W.releaseActions,
      dismissAlert = run . W.dismissAlert,
      acceptAlert = run . W.acceptAlert,
      getAlertText = run . W.getAlertText,
      sendAlertText = sessRun W.sendAlertText,
      takeScreenshot = run . W.takeScreenshot,
      printPage = run . W.printPage,
      -- Window methods
      getWindowHandles = run . W.getWindowHandles,
      getWindowRect = run . W.getWindowRect,
      setWindowRect = sessRun W.setWindowRect,
      maximizeWindow = run . W.maximizeWindow,
      minimizeWindow = run . W.minimizeWindow,
      fullScreenWindow = run . W.fullscreenWindow,
      -- Frame methods
      switchToParentFrame = run . W.switchToParentFrame,
      -- Element(s) methods
      getActiveElement = run . W.getActiveElement,
      findElement = sessRun W.findElement,
      findElements = sessRun W.findElements,
      -- Element instance methods
      findElementFromElement = sessRun2 W.findElementFromElement,
      findElementsFromElement = sessRun2 W.findElementsFromElement,
      isElementSelected = sessRun W.isElementSelected,
      getElementAttribute = sessRun2 W.getElementAttribute,
      getElementProperty = sessRun2 W.getElementProperty,
      getElementCssValue = sessRun2 W.getElementCssValue,
      getElementShadowRoot = sessRun W.getElementShadowRoot,
      getElementText = sessRun W.getElementText,
      getElementTagName = sessRun W.getElementTagName,
      getElementRect = sessRun W.getElementRect,
      isElementEnabled = sessRun W.isElementEnabled,
      getElementComputedRole = sessRun W.getElementComputedRole,
      getElementComputedLabel = sessRun W.getElementComputedLabel,
      elementClick = sessRun W.elementClick,
      elementClear = sessRun W.elementClear,
      elementSendKeys = sessRun2 W.elementSendKeys,
      takeElementScreenshot = sessRun W.takeElementScreenshot,
      -- Shadow DOM methods
      findElementFromShadowRoot = sessRun2 W.findElementFromShadowRoot,
      findElementsFromShadowRoot = sessRun2 W.findElementsFromShadowRoot,
      -- stubs for new methods that will not be implemented withthsi API
      runCommand = \_ -> error "runCommand not implemented in legacy actions",
      runCommand' = \_ -> error "runCommand not implemented in legacy actions"
    }
  where
    sessRun :: forall a r. (Show r) => (P.SessionId -> a -> Legacy.HttpSpec r) -> P.SessionId -> a -> IO r
    sessRun f s = run . f s

    sessRun2 :: forall a b r. (Show r) => (P.SessionId -> a -> b -> Legacy.HttpSpec r) -> P.SessionId -> a -> b -> IO r
    sessRun2 f s a = run . f s a
