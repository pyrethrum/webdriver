-- |
-- Module: WebDriver.Bluefin.HTTP.Base.Actions
-- Description: Bluefin-style HTTP WebDriver action functions
--
-- Provides monadic WebDriver actions for Bluefin.  Root methods (e.g.
-- 'status', 'newSession') take an 'HttpEnv' handle.  Session-scoped methods
-- (e.g. 'navigateTo', 'findElement') take an 'HttpSessionEnv' handle.
--
-- This mirrors 'WebDriver.RIO.HTTP.Base.Actions' but uses explicit handle
-- arguments instead of implicit typeclass constraints.
module WebDriver.Bluefin.HTTP.Base.Actions
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
import Data.Text (Text)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (effIO)
import WebDriver.Bluefin.HTTP.Core
  ( HttpEnv (..),
    HttpSessionEnv (..),
    mkEnvRunner,
    mkSessionRunner,
  )
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as A
import WebDriverPreCore.Extended.HTTP.Base.Protocol
  ( Actions,
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

-- | Lift a session action into Bluefin 'Eff'.
viaSession :: (e :> es, FromJSON a) => (A.Runner IO a -> Session -> IO a) -> HttpSessionEnv e -> Eff es a
viaSession sesFunc sess =
  effIO sess.envIO $ sesFunc (mkSessionRunner sess) sess.httpSession

viaSession1 :: (e :> es, FromJSON a) => (A.Runner IO a -> Session -> b -> IO a) -> HttpSessionEnv e -> b -> Eff es a
viaSession1 f sess b = viaSession (\r s -> f r s b) sess

viaSession2 :: (e :> es, FromJSON a) => (A.Runner IO a -> Session -> b -> c -> IO a) -> HttpSessionEnv e -> b -> c -> Eff es a
viaSession2 f sess b c = viaSession (\r s -> f r s b c) sess

-- | Lift an env-level (no session) action into Bluefin 'Eff'.
viaEnv :: (e :> es, FromJSON a) => (A.Runner IO a -> IO a) -> HttpEnv e -> Eff es a
viaEnv f env = effIO env.envIO (f (mkEnvRunner env))

-- ######################################################################
-- ########################### Root Methods #############################
-- ######################################################################

status :: (e :> es) => HttpEnv e -> Eff es Status
status env = viaEnv A.status env

newSession :: (e :> es) => HttpEnv e -> EC.HttpCapabilities -> Eff es EC.HttpSessionResponse
newSession env caps = effIO env.envIO $ EC.newHttpSession (mkEnvRunner env) caps

-- ######################################################################
-- ########################### Session Methods ##########################
-- ######################################################################

deleteSession :: (e :> es) => HttpSessionEnv e -> Eff es ()
deleteSession sess = viaSession A.deleteSession sess

getTimeouts :: (e :> es) => HttpSessionEnv e -> Eff es Timeouts
getTimeouts sess = viaSession A.getTimeouts sess

setTimeouts :: (e :> es) => HttpSessionEnv e -> Timeouts -> Eff es ()
setTimeouts sess = viaSession1 A.setTimeouts sess

navigateTo :: (e :> es) => HttpSessionEnv e -> URL -> Eff es ()
navigateTo sess = viaSession1 A.navigateTo sess

getCurrentUrl :: (e :> es) => HttpSessionEnv e -> Eff es URL
getCurrentUrl sess = viaSession A.getCurrentUrl sess

back :: (e :> es) => HttpSessionEnv e -> Eff es ()
back sess = viaSession A.back sess

forward :: (e :> es) => HttpSessionEnv e -> Eff es ()
forward sess = viaSession A.forward sess

refresh :: (e :> es) => HttpSessionEnv e -> Eff es ()
refresh sess = viaSession A.refresh sess

getTitle :: (e :> es) => HttpSessionEnv e -> Eff es Text
getTitle sess = viaSession A.getTitle sess

getWindowHandle :: (e :> es) => HttpSessionEnv e -> Eff es Handle
getWindowHandle sess = viaSession A.getWindowHandle sess

newWindow :: (e :> es) => HttpSessionEnv e -> Eff es WindowHandleSpec
newWindow sess = viaSession A.newWindow sess

closeWindow :: (e :> es) => HttpSessionEnv e -> Eff es [Handle]
closeWindow sess = viaSession A.closeWindow sess

switchToWindow :: (e :> es) => HttpSessionEnv e -> Handle -> Eff es ()
switchToWindow sess = viaSession1 A.switchToWindow sess

switchToFrame :: (e :> es) => HttpSessionEnv e -> FrameReference -> Eff es ()
switchToFrame sess = viaSession1 A.switchToFrame sess

getPageSource :: (e :> es) => HttpSessionEnv e -> Eff es Text
getPageSource sess = viaSession A.getPageSource sess

executeScript :: (e :> es) => HttpSessionEnv e -> Script -> Eff es Value
executeScript sess = viaSession1 A.executeScript sess

executeScriptAsync :: (e :> es) => HttpSessionEnv e -> Script -> Eff es Value
executeScriptAsync sess = viaSession1 A.executeScriptAsync sess

addCookie :: (e :> es) => HttpSessionEnv e -> Cookie -> Eff es ()
addCookie sess = viaSession1 A.addCookie sess

getAllCookies :: (e :> es) => HttpSessionEnv e -> Eff es [Cookie]
getAllCookies sess = viaSession A.getAllCookies sess

getNamedCookie :: (e :> es) => HttpSessionEnv e -> Text -> Eff es Cookie
getNamedCookie sess = viaSession1 A.getNamedCookie sess

deleteCookie :: (e :> es) => HttpSessionEnv e -> Text -> Eff es ()
deleteCookie sess = viaSession1 A.deleteCookie sess

deleteAllCookies :: (e :> es) => HttpSessionEnv e -> Eff es ()
deleteAllCookies sess = viaSession A.deleteAllCookies sess

performActions :: (e :> es) => HttpSessionEnv e -> Actions -> Eff es ()
performActions sess = viaSession1 A.performActions sess

releaseActions :: (e :> es) => HttpSessionEnv e -> Eff es ()
releaseActions sess = viaSession A.releaseActions sess

dismissAlert :: (e :> es) => HttpSessionEnv e -> Eff es ()
dismissAlert sess = viaSession A.dismissAlert sess

acceptAlert :: (e :> es) => HttpSessionEnv e -> Eff es ()
acceptAlert sess = viaSession A.acceptAlert sess

getAlertText :: (e :> es) => HttpSessionEnv e -> Eff es Text
getAlertText sess = viaSession A.getAlertText sess

sendAlertText :: (e :> es) => HttpSessionEnv e -> Text -> Eff es ()
sendAlertText sess = viaSession1 A.sendAlertText sess

takeScreenshot :: (e :> es) => HttpSessionEnv e -> Eff es Text
takeScreenshot sess = viaSession A.takeScreenshot sess

printPage :: (e :> es) => HttpSessionEnv e -> Eff es Text
printPage sess = viaSession A.printPage sess

-- ######################################################################
-- ########################### Window Methods ###########################
-- ######################################################################

getWindowHandles :: (e :> es) => HttpSessionEnv e -> Eff es [Handle]
getWindowHandles sess = viaSession A.getWindowHandles sess

getWindowRect :: (e :> es) => HttpSessionEnv e -> Eff es WindowRect
getWindowRect sess = viaSession A.getWindowRect sess

setWindowRect :: (e :> es) => HttpSessionEnv e -> WindowRect -> Eff es WindowRect
setWindowRect sess = viaSession1 A.setWindowRect sess

maximizeWindow :: (e :> es) => HttpSessionEnv e -> Eff es WindowRect
maximizeWindow sess = viaSession A.maximizeWindow sess

minimizeWindow :: (e :> es) => HttpSessionEnv e -> Eff es WindowRect
minimizeWindow sess = viaSession A.minimizeWindow sess

fullScreenWindow :: (e :> es) => HttpSessionEnv e -> Eff es WindowRect
fullScreenWindow sess = viaSession A.fullScreenWindow sess

-- ######################################################################
-- ########################### Frame Methods ############################
-- ######################################################################

switchToParentFrame :: (e :> es) => HttpSessionEnv e -> Eff es ()
switchToParentFrame sess = viaSession A.switchToParentFrame sess

-- ######################################################################
-- ########################## Element(s) Methods #######################
-- ######################################################################

getActiveElement :: (e :> es) => HttpSessionEnv e -> Eff es ElementId
getActiveElement sess = viaSession A.getActiveElement sess

findElement :: (e :> es) => HttpSessionEnv e -> Selector -> Eff es ElementId
findElement sess = viaSession1 A.findElement sess

findElements :: (e :> es) => HttpSessionEnv e -> Selector -> Eff es [ElementId]
findElements sess = viaSession1 A.findElements sess

-- ######################################################################
-- ##################### Element Instance Methods #######################
-- ######################################################################

findElementFromElement :: (e :> es) => HttpSessionEnv e -> ElementId -> Selector -> Eff es ElementId
findElementFromElement sess = viaSession2 A.findElementFromElement sess

findElementsFromElement :: (e :> es) => HttpSessionEnv e -> ElementId -> Selector -> Eff es [ElementId]
findElementsFromElement sess = viaSession2 A.findElementsFromElement sess

isElementSelected :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es Bool
isElementSelected sess = viaSession1 A.isElementSelected sess

getElementAttribute :: (e :> es) => HttpSessionEnv e -> ElementId -> Text -> Eff es (Maybe Text)
getElementAttribute sess = viaSession2 A.getElementAttribute sess

getElementProperty :: (e :> es) => HttpSessionEnv e -> ElementId -> Text -> Eff es (Maybe Value)
getElementProperty sess = viaSession2 A.getElementProperty sess

getElementCssValue :: (e :> es) => HttpSessionEnv e -> ElementId -> Text -> Eff es Text
getElementCssValue sess = viaSession2 A.getElementCssValue sess

getElementShadowRoot :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es ShadowRootElementId
getElementShadowRoot sess = viaSession1 A.getElementShadowRoot sess

getElementText :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es Text
getElementText sess = viaSession1 A.getElementText sess

getElementTagName :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es Text
getElementTagName sess = viaSession1 A.getElementTagName sess

getElementRect :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es WindowRect
getElementRect sess = viaSession1 A.getElementRect sess

isElementEnabled :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es Bool
isElementEnabled sess = viaSession1 A.isElementEnabled sess

getElementComputedRole :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es Text
getElementComputedRole sess = viaSession1 A.getElementComputedRole sess

getElementComputedLabel :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es Text
getElementComputedLabel sess = viaSession1 A.getElementComputedLabel sess

elementClick :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es ()
elementClick sess = viaSession1 A.elementClick sess

elementClear :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es ()
elementClear sess = viaSession1 A.elementClear sess

elementSendKeys :: (e :> es) => HttpSessionEnv e -> ElementId -> Text -> Eff es ()
elementSendKeys sess = viaSession2 A.elementSendKeys sess

takeElementScreenshot :: (e :> es) => HttpSessionEnv e -> ElementId -> Eff es Text
takeElementScreenshot sess = viaSession1 A.takeElementScreenshot sess

-- ######################################################################
-- ######################### Shadow DOM Methods #########################
-- ######################################################################

findElementFromShadowRoot :: (e :> es) => HttpSessionEnv e -> ShadowRootElementId -> Selector -> Eff es ElementId
findElementFromShadowRoot sess = viaSession2 A.findElementFromShadowRoot sess

findElementsFromShadowRoot :: (e :> es) => HttpSessionEnv e -> ShadowRootElementId -> Selector -> Eff es [ElementId]
findElementsFromShadowRoot sess = viaSession2 A.findElementsFromShadowRoot sess
