module HTTP.HttpActions
  ( HttpActions (..),
    mkActions,
  )
where

import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import HTTP.HttpRunner (HttpRunner (..))
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
    Session (..),
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
    deleteSession :: Session -> IO (),
    getTimeouts :: Session -> IO Timeouts,
    setTimeouts :: Session -> Timeouts -> IO (),
    navigateTo :: Session -> URL -> IO (),
    getCurrentUrl :: Session -> IO URL,
    back :: Session -> IO (),
    forward :: Session -> IO (),
    refresh :: Session -> IO (),
    getTitle :: Session -> IO Text,
    getWindowHandle :: Session -> IO Handle,
    newWindow :: Session -> IO WindowHandleSpec,
    closeWindow :: Session -> IO [Handle],
    switchToWindow :: Session -> Handle -> IO (),
    switchToFrame :: Session -> FrameReference -> IO (),
    getPageSource :: Session -> IO Text,
    executeScript :: Session -> Script -> IO Value,
    executeScriptAsync :: Session -> Script -> IO Value,
    addCookie :: Session -> Cookie -> IO (),
    getAllCookies :: Session -> IO [Cookie],
    getNamedCookie :: Session -> Text -> IO Cookie,
    deleteCookie :: Session -> Text -> IO (),
    deleteAllCookies :: Session -> IO (),
    performActions :: Session -> Actions -> IO (),
    releaseActions :: Session -> IO (),
    dismissAlert :: Session -> IO (),
    acceptAlert :: Session -> IO (),
    getAlertText :: Session -> IO Text,
    sendAlertText :: Session -> Text -> IO (),
    takeScreenshot :: Session -> IO Text,
    printPage :: Session -> IO Text,
    -- Window methods
    getWindowHandles :: Session -> IO [Handle],
    getWindowRect :: Session -> IO WindowRect,
    setWindowRect :: Session -> WindowRect -> IO WindowRect,
    maximizeWindow :: Session -> IO WindowRect,
    minimizeWindow :: Session -> IO WindowRect,
    fullScreenWindow :: Session -> IO WindowRect,
    -- Frame methods
    switchToParentFrame :: Session -> IO (),
    -- Element(s) methods
    getActiveElement :: Session -> IO ElementId,
    findElement :: Session -> Selector -> IO ElementId,
    findElements :: Session -> Selector -> IO [ElementId],
    -- Element instance methods
    findElementFromElement :: Session -> ElementId -> Selector -> IO ElementId,
    findElementsFromElement :: Session -> ElementId -> Selector -> IO [ElementId],
    isElementSelected :: Session -> ElementId -> IO Bool,
    getElementAttribute :: Session -> ElementId -> Text -> IO Text,
    getElementProperty :: Session -> ElementId -> Text -> IO Value,
    getElementCssValue :: Session -> ElementId -> Text -> IO Text,
    getElementShadowRoot :: Session -> ElementId -> IO ShadowRootElementId,
    getElementText :: Session -> ElementId -> IO Text,
    getElementTagName :: Session -> ElementId -> IO Text,
    getElementRect :: Session -> ElementId -> IO WindowRect,
    isElementEnabled :: Session -> ElementId -> IO Bool,
    getElementComputedRole :: Session -> ElementId -> IO Text,
    getElementComputedLabel :: Session -> ElementId -> IO Text,
    elementClick :: Session -> ElementId -> IO (),
    elementClear :: Session -> ElementId -> IO (),
    elementSendKeys :: Session -> ElementId -> Text -> IO (),
    takeElementScreenshot :: Session -> ElementId -> IO Text,
    -- Shadow DOM methods
    findElementFromShadowRoot :: Session -> ShadowRootElementId -> Selector -> IO ElementId,
    findElementsFromShadowRoot :: Session -> ShadowRootElementId -> Selector -> IO [ElementId],
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
    sessRun :: forall a r. (FromJSON r) => (Session -> a -> Command r) -> Session -> a -> IO r
    sessRun f s = run . f s

    sessRun_ :: forall a. (Session -> a -> Command ()) -> Session -> a -> IO ()
    sessRun_ f s = run . f s

    sessRun2 :: forall a b r. (FromJSON r) => (Session -> a -> b -> Command r) -> Session -> a -> b -> IO r
    sessRun2 f s a = run . f s a

    sessRun2_ :: forall a b. (Session -> a -> b -> Command ()) -> Session -> a -> b -> IO ()
    sessRun2_ f s a = run . f s a
