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
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as A
import WebDriverPreCore.Extended.HTTP.Base.Protocol
  ( Actions,
    Command,
    Cookie,
    ElementId,
    FrameReference,
    Handle,
    Script,
    Selector,
    Session (..),
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
getRunner = view httpRunnerL >>= \MkHttpRunner {run} -> pure run

-- | Lift a session action through the RIO environment.
viaSession :: (HasHttpRunner env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> IO a) -> RIO env a
viaSession sesFunc =
  sesFunc
    <$> getRunner
    <*> asks getHttpSession
    >>= liftIO

-- | Lift a session action with one extra argument.
viaSession1 :: (HasHttpRunner env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> b -> IO a) -> b -> RIO env a
viaSession1 f b = viaSession (\r s -> f r s b)

-- | Lift a session action with two extra arguments.
viaSession2 :: (HasHttpRunner env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> b -> c -> IO a) -> b -> c -> RIO env a
viaSession2 f b c = viaSession (\r s -> f r s b c)

-- ######################################################################
-- ########################### Root Methods #############################
-- ######################################################################

viaRunner :: ((Command a -> IO a) -> IO b) -> RIO env b
viaRunner f = (getRunner >>= liftIO . f)

status :: (HasHttpRunner env) => RIO env Status
status = viaRunner A.status

-- NOTE USES Extended Capabilities types
newSessionResponse :: (HasHttpRunner env) => EC.HttpCapabilities -> RIO env EC.HttpSessionResponse
newSessionResponse caps = viaRunner (flip EC.newHttpSessionResponse caps)

newSession :: (HasHttpRunner env) => EC.HttpCapabilities -> RIO env Session
newSession caps = viaRunner (flip EC.newHttpSession caps)

-- ######################################################################
-- ########################### Session Methods ##########################
-- ######################################################################

deleteSession :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
deleteSession = viaSession A.deleteSession

getTimeouts :: (HasHttpRunner env, HasHttpSession env) => RIO env Timeouts
getTimeouts = viaSession A.getTimeouts

setTimeouts :: (HasHttpRunner env, HasHttpSession env) => Timeouts -> RIO env ()
setTimeouts = viaSession1 A.setTimeouts

navigateTo :: (HasHttpRunner env, HasHttpSession env) => URL -> RIO env ()
navigateTo = viaSession1 A.navigateTo

getCurrentUrl :: (HasHttpRunner env, HasHttpSession env) => RIO env URL
getCurrentUrl = viaSession A.getCurrentUrl

back :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
back = viaSession A.back

forward :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
forward = viaSession A.forward

refresh :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
refresh = viaSession A.refresh

getTitle :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
getTitle = viaSession A.getTitle

getWindowHandle :: (HasHttpRunner env, HasHttpSession env) => RIO env Handle
getWindowHandle = viaSession A.getWindowHandle

newWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowHandleSpec
newWindow = viaSession A.newWindow

closeWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env [Handle]
closeWindow = viaSession A.closeWindow

switchToWindow :: (HasHttpRunner env, HasHttpSession env) => Handle -> RIO env ()
switchToWindow = viaSession1 A.switchToWindow

switchToFrame :: (HasHttpRunner env, HasHttpSession env) => FrameReference -> RIO env ()
switchToFrame = viaSession1 A.switchToFrame

getPageSource :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
getPageSource = viaSession A.getPageSource

executeScript :: (HasHttpRunner env, HasHttpSession env) => Script -> RIO env Value
executeScript = viaSession1 A.executeScript

executeScriptAsync :: (HasHttpRunner env, HasHttpSession env) => Script -> RIO env Value
executeScriptAsync = viaSession1 A.executeScriptAsync

addCookie :: (HasHttpRunner env, HasHttpSession env) => Cookie -> RIO env ()
addCookie = viaSession1 A.addCookie

getAllCookies :: (HasHttpRunner env, HasHttpSession env) => RIO env [Cookie]
getAllCookies = viaSession A.getAllCookies

getNamedCookie :: (HasHttpRunner env, HasHttpSession env) => Text -> RIO env Cookie
getNamedCookie = viaSession1 A.getNamedCookie

deleteCookie :: (HasHttpRunner env, HasHttpSession env) => Text -> RIO env ()
deleteCookie = viaSession1 A.deleteCookie

deleteAllCookies :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
deleteAllCookies = viaSession A.deleteAllCookies

performActions :: (HasHttpRunner env, HasHttpSession env) => Actions -> RIO env ()
performActions = viaSession1 A.performActions

releaseActions :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
releaseActions = viaSession A.releaseActions

dismissAlert :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
dismissAlert = viaSession A.dismissAlert

acceptAlert :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
acceptAlert = viaSession A.acceptAlert

getAlertText :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
getAlertText = viaSession A.getAlertText

sendAlertText :: (HasHttpRunner env, HasHttpSession env) => Text -> RIO env ()
sendAlertText = viaSession1 A.sendAlertText

takeScreenshot :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
takeScreenshot = viaSession A.takeScreenshot

printPage :: (HasHttpRunner env, HasHttpSession env) => RIO env Text
printPage = viaSession A.printPage

-- ######################################################################
-- ########################### Window Methods ###########################
-- ######################################################################

getWindowHandles :: (HasHttpRunner env, HasHttpSession env) => RIO env [Handle]
getWindowHandles = viaSession A.getWindowHandles

getWindowRect :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
getWindowRect = viaSession A.getWindowRect

setWindowRect :: (HasHttpRunner env, HasHttpSession env) => WindowRect -> RIO env WindowRect
setWindowRect = viaSession1 A.setWindowRect

maximizeWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
maximizeWindow = viaSession A.maximizeWindow

minimizeWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
minimizeWindow = viaSession A.minimizeWindow

fullScreenWindow :: (HasHttpRunner env, HasHttpSession env) => RIO env WindowRect
fullScreenWindow = viaSession A.fullScreenWindow

-- ######################################################################
-- ########################### Frame Methods ############################
-- ######################################################################

switchToParentFrame :: (HasHttpRunner env, HasHttpSession env) => RIO env ()
switchToParentFrame = viaSession A.switchToParentFrame

-- ######################################################################
-- ########################## Element(s) Methods ########################
-- ######################################################################

getActiveElement :: (HasHttpRunner env, HasHttpSession env) => RIO env ElementId
getActiveElement = viaSession A.getActiveElement

findElement :: (HasHttpRunner env, HasHttpSession env) => Selector -> RIO env ElementId
findElement = viaSession1 A.findElement

findElements :: (HasHttpRunner env, HasHttpSession env) => Selector -> RIO env [ElementId]
findElements = viaSession1 A.findElements

-- ######################################################################
-- ##################### Element Instance Methods #######################
-- ######################################################################

findElementFromElement :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Selector -> RIO env ElementId
findElementFromElement = viaSession2 A.findElementFromElement

findElementsFromElement :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Selector -> RIO env [ElementId]
findElementsFromElement = viaSession2 A.findElementsFromElement

isElementSelected :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Bool
isElementSelected = viaSession1 A.isElementSelected

getElementAttribute :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env Text
getElementAttribute = viaSession2 A.getElementAttribute

getElementProperty :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env Value
getElementProperty = viaSession2 A.getElementProperty

getElementCssValue :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env Text
getElementCssValue = viaSession2 A.getElementCssValue

getElementShadowRoot :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env ShadowRootElementId
getElementShadowRoot = viaSession1 A.getElementShadowRoot

getElementText :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementText = viaSession1 A.getElementText

getElementTagName :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementTagName = viaSession1 A.getElementTagName

getElementRect :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env WindowRect
getElementRect = viaSession1 A.getElementRect

isElementEnabled :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Bool
isElementEnabled = viaSession1 A.isElementEnabled

getElementComputedRole :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementComputedRole = viaSession1 A.getElementComputedRole

getElementComputedLabel :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
getElementComputedLabel = viaSession1 A.getElementComputedLabel

elementClick :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env ()
elementClick = viaSession1 A.elementClick

elementClear :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env ()
elementClear = viaSession1 A.elementClear

elementSendKeys :: (HasHttpRunner env, HasHttpSession env) => ElementId -> Text -> RIO env ()
elementSendKeys = viaSession2 A.elementSendKeys

takeElementScreenshot :: (HasHttpRunner env, HasHttpSession env) => ElementId -> RIO env Text
takeElementScreenshot = viaSession1 A.takeElementScreenshot

-- ######################################################################
-- ######################### Shadow DOM Methods #########################
-- ######################################################################

findElementFromShadowRoot :: (HasHttpRunner env, HasHttpSession env) => ShadowRootElementId -> Selector -> RIO env ElementId
findElementFromShadowRoot = viaSession2 A.findElementFromShadowRoot

findElementsFromShadowRoot :: (HasHttpRunner env, HasHttpSession env) => ShadowRootElementId -> Selector -> RIO env [ElementId]
findElementsFromShadowRoot = viaSession2 A.findElementsFromShadowRoot
