-- |
-- Module: WebDriver.RIO.HTTP.Base.Actions
-- Description: RIO-based HTTP WebDriver action functions
--
-- Provides monadic WebDriver actions for RIO. Static functions like 'status'
-- and 'newSession' require 'HasHttpRunner'. Session-scoped functions like
-- 'navigateTo' and 'newWindow' additionally require 'HasHttpSession'.
module WebDriver.RIO.HTTP.Base.Actions
  ( -- * Root Methods
    status,
    newSession,

    -- * Session Methods
    deleteSession,
    getTimeouts,
    setTimeouts,
    navigateTo,
    getCurrentUrl,
    back,
    forward,
    refresh,
    getTitle,
    getWindowHandle,
    newWindow,
    closeWindow,
    switchToWindow,
    switchToFrame,
    getPageSource,
    executeScript,
    executeScriptAsync,
    addCookie,
    getAllCookies,
    getNamedCookie,
    deleteCookie,
    deleteAllCookies,
    performActions,
    releaseActions,
    dismissAlert,
    acceptAlert,
    getAlertText,
    sendAlertText,
    takeScreenshot,
    printPage,

    -- * Window Methods
    getWindowHandles,
    getWindowRect,
    setWindowRect,
    maximizeWindow,
    minimizeWindow,
    fullScreenWindow,

    -- * Frame Methods
    switchToParentFrame,

    -- * Element(s) Methods
    getActiveElement,
    findElement,
    findElements,

    -- * Element Instance Methods
    findElementFromElement,
    findElementsFromElement,
    isElementSelected,
    getElementAttribute,
    getElementProperty,
    getElementCssValue,
    getElementShadowRoot,
    getElementText,
    getElementTagName,
    getElementRect,
    isElementEnabled,
    getElementComputedRole,
    getElementComputedLabel,
    elementClick,
    elementClear,
    elementSendKeys,
    takeElementScreenshot,

    -- * Shadow DOM Methods
    findElementFromShadowRoot,
    findElementsFromShadowRoot,
  )
where

import Data.Aeson (FromJSON, Value)
import RIO (RIO, Text, ask, asks, liftIO, view)
import WebDriver.RIO.Env
  ( HasHttpRunner (..),
    HasHttpSession (..),
  )
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as A
import WebDriverPreCore.Extended.HTTP.Base.Protocol
  ( Actions,
    Command,
    Cookie,
    ElementId,
    FrameReference,
    FullCapabilities,
    Handle,
    Script,
    Selector,
    Session (..),
    SessionResponse,
    ShadowRootElementId,
    Status,
    Timeouts,
    URL,
    WindowHandleSpec,
    WindowRect,
  )
import WebDriverPreCore.HttpRunner (HttpRunner (..))

-- ######################################################################
-- ########################### Helpers ##################################
-- ######################################################################

-- | Lift a session action through the RIO environment.
getRunner :: (HasHttpRunner env, FromJSON a) => RIO env (Command a -> IO a)
getRunner = view httpRunnerL >>= \MkHttpRunner {run = r} -> pure r

-- | Lift a session action through the RIO environment.
withSession :: (HasHttpRunner env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> IO a) -> RIO env a
withSession sesFunc =
  sesFunc
    <$> getRunner
    <*> (MkSession <$> asks getHttpSessionId)
    >>= liftIO

-- | Lift a session action with one extra argument.
withSession1 :: (HasHttpRunner env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> b -> IO a) -> b -> RIO env a
withSession1 f b = withSession (\r s -> f r s b)

-- | Lift a session action with two extra arguments.
withSession2 :: (HasHttpRunner env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> b -> c -> IO a) -> b -> c -> RIO env a
withSession2 f b c = withSession (\r s -> f r s b c)

-- ######################################################################
-- ########################### Root Methods #############################
-- ######################################################################

status :: (HasHttpRunner env) => RIO env Status
status = getRunner >>= liftIO . A.status

newSession :: (HasHttpRunner env) => FullCapabilities -> RIO env SessionResponse
newSession caps = getRunner >>= liftIO . flip A.newSession caps

-- ######################################################################
-- ########################### Session Methods ##########################
-- ######################################################################

deleteSession :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
deleteSession = withSession A.deleteSession

getTimeouts :: (HasHttpRunner env, HasHttpSession env) => RIO env Timeouts
getTimeouts = withSession A.getTimeouts

setTimeouts :: (HasHttpRunner env, HasHttpSession env) => Timeouts -> RIO env ()
setTimeouts = withSession1 A.setTimeouts

navigateTo :: (HasHttpRunner env, HasHttpSession env) => URL -> RIO env ()
navigateTo = withSession1 A.navigateTo

getCurrentUrl :: (HasHttpRunner env, HasHttpSession env) => RIO env URL
getCurrentUrl = withSession A.getCurrentUrl

back :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
back = withSession A.back

forward :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
forward = withSession A.forward

refresh :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
refresh = withSession A.refresh

getTitle :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
getTitle = withSession A.getTitle

getWindowHandle :: (HasHttpRunner env, HasHttpSession env) => RIO env Handle
getWindowHandle = withSession A.getWindowHandle

newWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowHandleSpec
newWindow = withSession A.newWindow

closeWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env [Handle]
closeWindow = withSession A.closeWindow

switchToWindow :: (HasHttpRunner env, HasHttpSession env) => Handle -> RIO env ()
switchToWindow = withSession1 A.switchToWindow

switchToFrame :: (HasHttpRunner env, HasHttpSession env) => FrameReference -> RIO env ()
switchToFrame = withSession1 A.switchToFrame

getPageSource :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
getPageSource = withSession A.getPageSource

executeScript :: (HasHttpRunner env, HasHttpSession env) => Script -> RIO env Value
executeScript = withSession1 A.executeScript

executeScriptAsync :: (HasHttpRunner env, HasHttpSession env) => Script -> RIO env Value
executeScriptAsync = withSession1 A.executeScriptAsync

addCookie :: (HasHttpRunner env, HasHttpSession env) => Cookie -> RIO env ()
addCookie = withSession1 A.addCookie

getAllCookies :: (HasHttpRunner env, HasHttpSession env) => RIO env [Cookie]
getAllCookies = withSession A.getAllCookies

getNamedCookie :: (HasHttpRunner env, HasHttpSession env) => Text -> RIO env Cookie
getNamedCookie = withSession1 A.getNamedCookie

deleteCookie :: (HasHttpRunner env, HasHttpSession env) => Text -> RIO env ()
deleteCookie = withSession1 A.deleteCookie

deleteAllCookies :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
deleteAllCookies = withSession A.deleteAllCookies

performActions :: (HasHttpRunner env, HasHttpSession env) => Actions -> RIO env ()
performActions = withSession1 A.performActions

releaseActions :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
releaseActions = withSession A.releaseActions

dismissAlert :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
dismissAlert = withSession A.dismissAlert

acceptAlert :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
acceptAlert = withSession A.acceptAlert

getAlertText :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
getAlertText = withSession A.getAlertText

sendAlertText :: (HasHttpRunner env, HasHttpSession env) => Text -> RIO env ()
sendAlertText = withSession1 A.sendAlertText

takeScreenshot :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
takeScreenshot = withSession A.takeScreenshot

printPage :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
printPage = withSession A.printPage

-- ######################################################################
-- ########################### Window Methods ###########################
-- ######################################################################

getWindowHandles :: (HasHttpRunner env, HasHttpSession env) => RIO env [Handle]
getWindowHandles = withSession A.getWindowHandles

getWindowRect :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
getWindowRect = withSession A.getWindowRect

setWindowRect :: (HasHttpRunner env, HasHttpSession env) => WindowRect -> RIO env WindowRect
setWindowRect = withSession1 A.setWindowRect

maximizeWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
maximizeWindow = withSession A.maximizeWindow

minimizeWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
minimizeWindow = withSession A.minimizeWindow

fullScreenWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
fullScreenWindow = withSession A.fullScreenWindow

-- ######################################################################
-- ########################### Frame Methods ############################
-- ######################################################################

switchToParentFrame :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
switchToParentFrame = withSession A.switchToParentFrame

-- ######################################################################
-- ########################## Element(s) Methods ########################
-- ######################################################################

getActiveElement :: (HasHttpRunner env, HasHttpSession env) => RIO env ElementId
getActiveElement = withSession A.getActiveElement

findElement :: (HasHttpRunner env, HasHttpSession env) => Selector -> RIO env ElementId
findElement = withSession1 A.findElement

findElements :: (HasHttpRunner env, HasHttpSession env) => Selector -> RIO env [ElementId]
findElements = withSession1 A.findElements

-- ######################################################################
-- ##################### Element Instance Methods #######################
-- ######################################################################

findElementFromElement :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Selector -> RIO env ElementId
findElementFromElement = withSession2 A.findElementFromElement

findElementsFromElement :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Selector -> RIO env [ElementId]
findElementsFromElement = withSession2 A.findElementsFromElement

isElementSelected :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Bool
isElementSelected = withSession1 A.isElementSelected

getElementAttribute :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env Text
getElementAttribute = withSession2 A.getElementAttribute

getElementProperty :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env Value
getElementProperty = withSession2 A.getElementProperty

getElementCssValue :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env Text
getElementCssValue = withSession2 A.getElementCssValue

getElementShadowRoot :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env ShadowRootElementId
getElementShadowRoot = withSession1 A.getElementShadowRoot

getElementText :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementText = withSession1 A.getElementText

getElementTagName :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementTagName = withSession1 A.getElementTagName

getElementRect :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env WindowRect
getElementRect = withSession1 A.getElementRect

isElementEnabled :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Bool
isElementEnabled = withSession1 A.isElementEnabled

getElementComputedRole :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementComputedRole = withSession1 A.getElementComputedRole

getElementComputedLabel :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementComputedLabel = withSession1 A.getElementComputedLabel

elementClick :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env ()
elementClick = withSession1 A.elementClick

elementClear :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env ()
elementClear = withSession1 A.elementClear

elementSendKeys :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env ()
elementSendKeys = withSession2 A.elementSendKeys

takeElementScreenshot :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
takeElementScreenshot = withSession1 A.takeElementScreenshot

-- ######################################################################
-- ######################### Shadow DOM Methods #########################
-- ######################################################################

findElementFromShadowRoot :: (HasHttpRunner env, HasHttpSession env) => ShadowRootElementId -> Selector -> RIO env ElementId
findElementFromShadowRoot = withSession2 A.findElementFromShadowRoot

findElementsFromShadowRoot :: (HasHttpRunner env, HasHttpSession env) => ShadowRootElementId -> Selector -> RIO env [ElementId]
findElementsFromShadowRoot = withSession2 A.findElementsFromShadowRoot
