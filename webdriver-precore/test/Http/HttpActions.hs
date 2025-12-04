module Http.HttpActions
  ( HttpActions (..),
    mkActions,
  )
where

import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import Http.HttpRunner (Extended, HttpRunner (..))
import WebDriverPreCore.Http.API qualified as P
import WebDriverPreCore.Http.Command
import WebDriverPreCore.Http.Protocol
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
  )

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
    runCommand :: forall a. (FromJSON a) => Command a -> IO (Extended a),
    runCommand' :: Command () -> IO Value
  }

mkActions :: HttpRunner -> HttpActions
mkActions MkHttpRunner {run, run', run_, fullResponse} =
  MkHttpActions
    { -- Root methods
      status = run P.status,
      newSession = run . P.newSession,
 
      -- Session methods
      deleteSession = run_ . P.deleteSession,
      getTimeouts = run . P.getTimeouts,
      setTimeouts = sessRun_ P.setTimeouts,
      navigateTo = sessRun_ P.navigateTo,
      getCurrentUrl = run . P.getCurrentUrl,
      back = run_ . P.back,
      forward = run_ . P.forward,
      refresh = run_ . P.refresh,
      getTitle = run . P.getTitle,
      getWindowHandle = run . P.getWindowHandle,
      newWindow = run . P.newWindow,
      closeWindow = run . P.closeWindow,
      switchToWindow = sessRun_ P.switchToWindow,
      switchToFrame = sessRun_ P.switchToFrame,
      getPageSource = run . P.getPageSource,
      executeScript = sessRun P.executeScript,
      executeScriptAsync = sessRun P.executeScriptAsync,
      addCookie = sessRun_ P.addCookie,
      getAllCookies = run . P.getAllCookies,
      getNamedCookie = sessRun P.getNamedCookie,
      deleteCookie = sessRun_ P.deleteCookie,
      deleteAllCookies = run_ . P.deleteAllCookies,
      performActions = sessRun_ P.performActions,
      releaseActions = run_ . P.releaseActions,
      dismissAlert = run_ . P.dismissAlert,
      acceptAlert = run_ . P.acceptAlert,
      getAlertText = run . P.getAlertText,
      sendAlertText = sessRun_ P.sendAlertText,
      takeScreenshot = run . P.takeScreenshot,
      printPage = run . P.printPage,
      -- Window methods
      getWindowHandles = run . P.getWindowHandles,
      getWindowRect = run . P.getWindowRect,
      setWindowRect = sessRun P.setWindowRect,
      maximizeWindow = run . P.maximizeWindow,
      minimizeWindow = run . P.minimizeWindow,
      fullScreenWindow = run . P.fullScreenWindow,
      -- Frame methods
      switchToParentFrame = run_ . P.switchToParentFrame,
      -- Element(s) methods
      getActiveElement = run . P.getActiveElement,
      findElement = sessRun P.findElement,
      findElements = sessRun P.findElements,
      -- Element instance methods
      findElementFromElement = sessRun2 P.findElementFromElement,
      findElementsFromElement = sessRun2 P.findElementsFromElement,
      isElementSelected = sessRun P.isElementSelected,
      getElementAttribute = sessRun2 P.getElementAttribute,
      getElementProperty = sessRun2 P.getElementProperty,
      getElementCssValue = sessRun2 P.getElementCssValue,
      getElementShadowRoot = sessRun P.getElementShadowRoot,
      getElementText = sessRun P.getElementText,
      getElementTagName = sessRun P.getElementTagName,
      getElementRect = sessRun P.getElementRect,
      isElementEnabled = sessRun P.isElementEnabled,
      getElementComputedRole = sessRun P.getElementComputedRole,
      getElementComputedLabel = sessRun P.getElementComputedLabel,
      elementClick = sessRun_ P.elementClick,
      elementClear = sessRun_ P.elementClear,
      elementSendKeys = sessRun2_ P.elementSendKeys,
      takeElementScreenshot = sessRun P.takeElementScreenshot,
      -- Shadow DOM methods
      findElementFromShadowRoot = sessRun2 P.findElementFromShadowRoot,
      findElementsFromShadowRoot = sessRun2 P.findElementsFromShadowRoot,
      --
      runCommand = run',
      runCommand' = fullResponse
    }
  where
    sessRun :: forall a r. (FromJSON r) => (SessionId -> a -> Command r) -> SessionId -> a -> IO r
    sessRun f s = run . f s

    sessRun_ :: forall a. (SessionId -> a -> Command ()) -> SessionId -> a -> IO ()
    sessRun_ f s = run_ . f s

    sessRun2 :: forall a b r. (FromJSON r) => (SessionId -> a -> b -> Command r) -> SessionId -> a -> b -> IO r
    sessRun2 f s a = run . f s a

    sessRun2_ :: forall a b. (SessionId -> a -> b -> Command ()) -> SessionId -> a -> b -> IO ()
    sessRun2_ f s a = run . f s a
