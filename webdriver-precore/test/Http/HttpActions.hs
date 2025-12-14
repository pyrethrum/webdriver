module Http.HttpActions
  ( HttpActions (..),
    mkActions,
  )
where

import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import Http.HttpRunner (HttpRunner (..))
import WebDriverPreCore.HTTP.API qualified as API
import WebDriverPreCore.HTTP.Protocol
  ( Actions (..),
    Cookie (..),
    ElementId (..),
    FrameReference (..),
    FullCapabilities (..),
    Handle (..),
    Script (..),
    Selector (..),
    SessionId (..),
    SessionResponse (..),
    ShadowRootElementId,
    Status (..),
    Timeouts (..),
    URL,
    WindowHandleSpec (..),
    WindowRect (..),
    Command
  )

_for_implementation_see_mkActions :: HttpRunner -> HttpActions
_for_implementation_see_mkActions = mkActions

data HttpActions = MkHttpActions
  { -- Root methods
    status :: IO Status,
    newSession :: FullCapabilities -> IO SessionResponse,
    -- Session methods
    deleteSession :: SessionId -> IO (),
    getTimeouts :: SessionId -> IO Timeouts,
    setTimeouts :: SessionId -> Timeouts -> IO (),
    navigateTo :: SessionId -> URL -> IO (),
    getCurrentUrl :: SessionId -> IO URL,
    back :: SessionId -> IO (),
    forward :: SessionId -> IO (),
    refresh :: SessionId -> IO (),
    getTitle :: SessionId -> IO Text,
    getWindowHandle :: SessionId -> IO Handle,
    newWindow :: SessionId -> IO WindowHandleSpec,
    closeWindow :: SessionId -> IO [Handle],
    switchToWindow :: SessionId -> Handle -> IO (),
    switchToFrame :: SessionId -> FrameReference -> IO (),
    getPageSource :: SessionId -> IO Text,
    executeScript :: SessionId -> Script -> IO Value,
    executeScriptAsync :: SessionId -> Script -> IO Value,
    addCookie :: SessionId -> Cookie -> IO (),
    getAllCookies :: SessionId -> IO [Cookie],
    getNamedCookie :: SessionId -> Text -> IO Cookie,
    deleteCookie :: SessionId -> Text -> IO (),
    deleteAllCookies :: SessionId -> IO (),
    performActions :: SessionId -> Actions -> IO (),
    releaseActions :: SessionId -> IO (),
    dismissAlert :: SessionId -> IO (),
    acceptAlert :: SessionId -> IO (),
    getAlertText :: SessionId -> IO Text,
    sendAlertText :: SessionId -> Text -> IO (),
    takeScreenshot :: SessionId -> IO Text,
    printPage :: SessionId -> IO Text,
    -- Window methods
    getWindowHandles :: SessionId -> IO [Handle],
    getWindowRect :: SessionId -> IO WindowRect,
    setWindowRect :: SessionId -> WindowRect -> IO WindowRect,
    maximizeWindow :: SessionId -> IO WindowRect,
    minimizeWindow :: SessionId -> IO WindowRect,
    fullScreenWindow :: SessionId -> IO WindowRect,
    -- Frame methods
    switchToParentFrame :: SessionId -> IO (),
    -- Element(s) methods
    getActiveElement :: SessionId -> IO ElementId,
    findElement :: SessionId -> Selector -> IO ElementId,
    findElements :: SessionId -> Selector -> IO [ElementId],
    -- Element instance methods
    findElementFromElement :: SessionId -> ElementId -> Selector -> IO ElementId,
    findElementsFromElement :: SessionId -> ElementId -> Selector -> IO [ElementId],
    isElementSelected :: SessionId -> ElementId -> IO Bool,
    getElementAttribute :: SessionId -> ElementId -> Text -> IO Text,
    getElementProperty :: SessionId -> ElementId -> Text -> IO Value,
    getElementCssValue :: SessionId -> ElementId -> Text -> IO Text,
    getElementShadowRoot :: SessionId -> ElementId -> IO ShadowRootElementId,
    getElementText :: SessionId -> ElementId -> IO Text,
    getElementTagName :: SessionId -> ElementId -> IO Text,
    getElementRect :: SessionId -> ElementId -> IO WindowRect,
    isElementEnabled :: SessionId -> ElementId -> IO Bool,
    getElementComputedRole :: SessionId -> ElementId -> IO Text,
    getElementComputedLabel :: SessionId -> ElementId -> IO Text,
    elementClick :: SessionId -> ElementId -> IO (),
    elementClear :: SessionId -> ElementId -> IO (),
    elementSendKeys :: SessionId -> ElementId -> Text -> IO (),
    takeElementScreenshot :: SessionId -> ElementId -> IO Text,
    -- Shadow DOM methods
    findElementFromShadowRoot :: SessionId -> ShadowRootElementId -> Selector -> IO ElementId,
    findElementsFromShadowRoot :: SessionId -> ShadowRootElementId -> Selector -> IO [ElementId],
    -- Fallback methods
    runCommand :: forall a. (FromJSON a) => Command a -> IO a,
    runCommand' ::forall a. (FromJSON a) => Command a -> IO Value
  }

mkActions :: HttpRunner -> HttpActions
mkActions MkHttpRunner {run,  fullResponse} =
  MkHttpActions
    { -- Root methods
      status = run API.status,
      newSession = run . API.newSession,
 
      -- Session methods
      deleteSession = run . API.deleteSession,
      getTimeouts = run . API.getTimeouts,
      setTimeouts = sessRun_ API.setTimeouts,
      navigateTo = sessRun_ API.navigateTo,
      getCurrentUrl = run . API.getCurrentUrl,
      back = run . API.back,
      forward = run . API.forward,
      refresh = run . API.refresh,
      getTitle = run . API.getTitle,
      getWindowHandle = run . API.getWindowHandle,
      newWindow = run . API.newWindow,
      closeWindow = run . API.closeWindow,
      switchToWindow = sessRun_ API.switchToWindow,
      switchToFrame = sessRun_ API.switchToFrame,
      getPageSource = run . API.getPageSource,
      executeScript = sessRun API.executeScript,
      executeScriptAsync = sessRun API.executeScriptAsync,
      addCookie = sessRun_ API.addCookie,
      getAllCookies = run . API.getAllCookies,
      getNamedCookie = sessRun API.getNamedCookie,
      deleteCookie = sessRun_ API.deleteCookie,
      deleteAllCookies = run . API.deleteAllCookies,
      performActions = sessRun_ API.performActions,
      releaseActions = run . API.releaseActions,
      dismissAlert = run . API.dismissAlert,
      acceptAlert = run . API.acceptAlert,
      getAlertText = run . API.getAlertText,
      sendAlertText = sessRun_ API.sendAlertText,
      takeScreenshot = run . API.takeScreenshot,
      printPage = run . API.printPage,
      -- Window methods
      getWindowHandles = run . API.getWindowHandles,
      getWindowRect = run . API.getWindowRect,
      setWindowRect = sessRun API.setWindowRect,
      maximizeWindow = run . API.maximizeWindow,
      minimizeWindow = run . API.minimizeWindow,
      fullScreenWindow = run . API.fullScreenWindow,
      -- Frame methods
      switchToParentFrame = run . API.switchToParentFrame,
      -- Element(s) methods
      getActiveElement = run . API.getActiveElement,
      findElement = sessRun API.findElement,
      findElements = sessRun API.findElements,
      -- Element instance methods
      findElementFromElement = sessRun2 API.findElementFromElement,
      findElementsFromElement = sessRun2 API.findElementsFromElement,
      isElementSelected = sessRun API.isElementSelected,
      getElementAttribute = sessRun2 API.getElementAttribute,
      getElementProperty = sessRun2 API.getElementProperty,
      getElementCssValue = sessRun2 API.getElementCssValue,
      getElementShadowRoot = sessRun API.getElementShadowRoot,
      getElementText = sessRun API.getElementText,
      getElementTagName = sessRun API.getElementTagName,
      getElementRect = sessRun API.getElementRect,
      isElementEnabled = sessRun API.isElementEnabled,
      getElementComputedRole = sessRun API.getElementComputedRole,
      getElementComputedLabel = sessRun API.getElementComputedLabel,
      elementClick = sessRun_ API.elementClick,
      elementClear = sessRun_ API.elementClear,
      elementSendKeys = sessRun2_ API.elementSendKeys,
      takeElementScreenshot = sessRun API.takeElementScreenshot,
      -- Shadow DOM methods
      findElementFromShadowRoot = sessRun2 API.findElementFromShadowRoot,
      findElementsFromShadowRoot = sessRun2 API.findElementsFromShadowRoot,
      --
      runCommand = run,
      runCommand' = fullResponse
    }
  where
    sessRun :: forall a r. (FromJSON r) => (SessionId -> a -> Command r) -> SessionId -> a -> IO r
    sessRun f s = run . f s

    sessRun_ :: forall a. (SessionId -> a -> Command ()) -> SessionId -> a -> IO ()
    sessRun_ f s = run . f s

    sessRun2 :: forall a b r. (FromJSON r) => (SessionId -> a -> b -> Command r) -> SessionId -> a -> b -> IO r
    sessRun2 f s a = run . f s a

    sessRun2_ :: forall a b. (SessionId -> a -> b -> Command ()) -> SessionId -> a -> b -> IO ()
    sessRun2_ f s a = run . f s a
