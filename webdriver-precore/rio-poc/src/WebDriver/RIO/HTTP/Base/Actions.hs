-- |
-- Module: WebDriver.RIO.HTTP.Base.Actions
-- Description: RIO-based HTTP WebDriver action functions
--
-- Provides monadic WebDriver actions for RIO. Static functions like 'status'
-- and 'newSession' require 'HasHttpDriverInfo'. Session-scoped functions like
-- 'navigateTo' and 'newWindow' additionally require 'HasHttpSession'.
module WebDriver.RIO.HTTP.Base.Actions
  (

   -- * Root Methods
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
  newSessionResponse,

  )
where

import Data.Aeson (FromJSON, Value)
import RIO (HasLogFunc (..), RIO, Text, ask, asks, liftIO, runRIO)
import WebDriver.RIO.Env
  ( HasHttpDriverInfo (..),
    HasHttpSession (..),
    runCommand,
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
-- ######################################################################
-- ########################### Helpers ##################################
-- ######################################################################

-- | Lift a session action through the RIO environment.
viaSession :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> IO a) -> RIO env a
viaSession sesFunc = do
  env <- ask
  session <- asks getHttpSession
  liftIO $ sesFunc (runRIO env . runCommand) session

-- | Lift a session action with one extra argument.
viaSession1 :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> b -> IO a) -> b -> RIO env a
viaSession1 f b = viaSession (\r s -> f r s b)

-- | Lift a session action with two extra arguments.
viaSession2 :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env, FromJSON a) => (A.Runner IO a -> Session -> b -> c -> IO a) -> b -> c -> RIO env a
viaSession2 f b c = viaSession (\r s -> f r s b c)

-- ######################################################################
-- ########################### Root Methods #############################
-- ######################################################################

viaRunner :: (HasHttpDriverInfo env, HasLogFunc env, FromJSON a) => ((Command a -> IO a) -> IO b) -> RIO env b
viaRunner f = do
  env <- ask
  liftIO $ f (runRIO env . runCommand)

status :: (HasHttpDriverInfo env, HasLogFunc env) => RIO env Status
status = viaRunner A.status


-- NOTE USES Extended Capabilities types
newSessionResponse :: (HasHttpDriverInfo env, HasLogFunc env) => EC.HttpCapabilities -> RIO env EC.HttpSessionResponse
newSessionResponse caps = viaRunner (flip EC.newHttpSessionResponse caps)

newSession :: (HasHttpDriverInfo env, HasLogFunc env) => EC.HttpCapabilities -> RIO env Session
newSession caps = viaRunner (flip EC.newHttpSession caps)

-- ######################################################################
-- ########################### Session Methods ##########################
-- ######################################################################

deleteSession :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
deleteSession = viaSession A.deleteSession

getTimeouts :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env Timeouts
getTimeouts = viaSession A.getTimeouts

setTimeouts :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Timeouts -> RIO env ()
setTimeouts = viaSession1 A.setTimeouts

navigateTo :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => URL -> RIO env ()
navigateTo = viaSession1 A.navigateTo

getCurrentUrl :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env URL
getCurrentUrl = viaSession A.getCurrentUrl

back :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
back = viaSession A.back

forward :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
forward = viaSession A.forward

refresh :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
refresh = viaSession A.refresh

getTitle :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env Text
getTitle = viaSession A.getTitle

getWindowHandle :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env Handle
getWindowHandle = viaSession A.getWindowHandle

newWindow :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env WindowHandleSpec
newWindow = viaSession A.newWindow

closeWindow :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env [Handle]
closeWindow = viaSession A.closeWindow

switchToWindow :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Handle -> RIO env ()
switchToWindow = viaSession1 A.switchToWindow

switchToFrame :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => FrameReference -> RIO env ()
switchToFrame = viaSession1 A.switchToFrame

getPageSource :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env Text
getPageSource = viaSession A.getPageSource

executeScript :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Script -> RIO env Value
executeScript = viaSession1 A.executeScript

executeScriptAsync :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Script -> RIO env Value
executeScriptAsync = viaSession1 A.executeScriptAsync

addCookie :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Cookie -> RIO env ()
addCookie = viaSession1 A.addCookie

getAllCookies :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env [Cookie]
getAllCookies = viaSession A.getAllCookies

getNamedCookie :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Text -> RIO env Cookie
getNamedCookie = viaSession1 A.getNamedCookie

deleteCookie :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Text -> RIO env ()
deleteCookie = viaSession1 A.deleteCookie

deleteAllCookies :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
deleteAllCookies = viaSession A.deleteAllCookies

performActions :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Actions -> RIO env ()
performActions = viaSession1 A.performActions

releaseActions :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
releaseActions = viaSession A.releaseActions

dismissAlert :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
dismissAlert = viaSession A.dismissAlert

acceptAlert :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
acceptAlert = viaSession A.acceptAlert

getAlertText :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env Text
getAlertText = viaSession A.getAlertText

sendAlertText :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Text -> RIO env ()
sendAlertText = viaSession1 A.sendAlertText

takeScreenshot :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env Text
takeScreenshot = viaSession A.takeScreenshot

printPage :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env Text
printPage = viaSession A.printPage

-- ######################################################################
-- ########################### Window Methods ###########################
-- ######################################################################{-

getWindowHandles :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env [Handle]
getWindowHandles = viaSession A.getWindowHandles

getWindowRect :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env WindowRect
getWindowRect = viaSession A.getWindowRect

setWindowRect :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => WindowRect -> RIO env WindowRect
setWindowRect = viaSession1 A.setWindowRect

maximizeWindow :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env WindowRect
maximizeWindow = viaSession A.maximizeWindow

minimizeWindow :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env WindowRect
minimizeWindow = viaSession A.minimizeWindow

fullScreenWindow :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env WindowRect
fullScreenWindow = viaSession A.fullScreenWindow

-- ######################################################################
-- ########################### Frame Methods ############################
-- ######################################################################

switchToParentFrame :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ()
switchToParentFrame = viaSession A.switchToParentFrame

-- ######################################################################
-- ########################## Element(s) Methods ########################
-- ######################################################################

getActiveElement :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => RIO env ElementId
getActiveElement = viaSession A.getActiveElement

findElement :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Selector -> RIO env ElementId
findElement = viaSession1 A.findElement

findElements :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => Selector -> RIO env [ElementId]
findElements = viaSession1 A.findElements

-- ######################################################################
-- ##################### Element Instance Methods #######################
-- ######################################################################

findElementFromElement :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> Selector -> RIO env ElementId
findElementFromElement = viaSession2 A.findElementFromElement

findElementsFromElement :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> Selector -> RIO env [ElementId]
findElementsFromElement = viaSession2 A.findElementsFromElement

isElementSelected :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env Bool
isElementSelected = viaSession1 A.isElementSelected

getElementAttribute :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> Text -> RIO env Text
getElementAttribute = viaSession2 A.getElementAttribute

getElementProperty :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> Text -> RIO env Value
getElementProperty = viaSession2 A.getElementProperty

getElementCssValue :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> Text -> RIO env Text 
getElementCssValue = viaSession2 A.getElementCssValue

getElementShadowRoot :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env ShadowRootElementId
getElementShadowRoot = viaSession1 A.getElementShadowRoot

getElementText :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env Text
getElementText = viaSession1 A.getElementText

getElementTagName :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env Text
getElementTagName = viaSession1 A.getElementTagName

getElementRect :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env WindowRect
getElementRect = viaSession1 A.getElementRect

isElementEnabled :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env Bool
isElementEnabled = viaSession1 A.isElementEnabled

getElementComputedRole :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env Text
getElementComputedRole = viaSession1 A.getElementComputedRole

getElementComputedLabel :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env Text
getElementComputedLabel = viaSession1 A.getElementComputedLabel

elementClick :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env ()
elementClick = viaSession1 A.elementClick

elementClear :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env ()
elementClear = viaSession1 A.elementClear

elementSendKeys :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> Text -> RIO env ()
elementSendKeys = viaSession2 A.elementSendKeys

takeElementScreenshot :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ElementId -> RIO env Text
takeElementScreenshot = viaSession1 A.takeElementScreenshot

-- ######################################################################
-- ######################### Shadow DOM Methods #########################
-- ######################################################################
 
findElementFromShadowRoot :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ShadowRootElementId -> Selector -> RIO env ElementId
findElementFromShadowRoot = viaSession2 A.findElementFromShadowRoot

findElementsFromShadowRoot :: (HasHttpDriverInfo env, HasLogFunc env, HasHttpSession env) => ShadowRootElementId -> Selector -> RIO env [ElementId]
findElementsFromShadowRoot = viaSession2 A.findElementsFromShadowRoot

