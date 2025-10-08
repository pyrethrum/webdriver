module Http.HttpAPI
  ( 
    W.Cookie (..),
    W.Capabilities(..),
    W.FullCapabilities(..), 
    W.VendorSpecific(..),
    W.DriverStatus(..),
    W.Timeouts (..),
    W.WindowHandleSpec (..),
    W.WindowHandle(..),
    W.SameSite (..),
    W.Selector (..),
    W.SessionId (..),
    W.SessionResponse (..),
    W.FrameReference (..),
    W.WindowRect (..),
    W.PointerOrigin (..),
    W.Action (..),
    W.Actions (..),
    W.KeyAction (..),
    W.Pointer (..),
    W.PointerAction (..),
    W.WheelAction (..),
    W.errorCodeToErrorType,
    W.minFirefoxCapabilities,
    W.minCapabilities,
    W.BrowserName(..),
    encodeFileToBase64,
    status,
    findElementFromElement,
    findElementsFromElement,
    findElements,
    getTimeouts,
    setTimeouts,
    back,
    forward,
    getActiveElement,
    refresh,
    getCurrentUrl,
    getElementAttribute,
    getElementShadowRoot,
    findElementFromShadowRoot,
    findElementsFromShadowRoot,
    getTitle,
    getWindowHandles,
    isElementSelected,
    maximizeWindow,
    minimizeWindow,
    fullScreenWindow,
    getWindowHandle,
    getWindowRect,
    closeWindow,
    newWindow,
    newSession,
    newSessionFull,
    newSessionFull',
    minFirefoxSession,
    performActions,
    releaseActions,
    deleteSession,
    navigateTo,
    findElement,
    elementClick,
    getElementText,
    setWindowRect,
    sleep,
    switchToWindow,
    switchToFrame,
    switchToParentFrame,
    getElementProperty,
    getElementCssValue,
    getElementTagName,
    getElementRect,
    isElementEnabled,
    getElementComputedRole,
    getElementComputedLabel,
    elementClear,
    elementSendKeys,
    printPage,
    getPageSource,
    takeScreenshot,
    takeElementScreenshot,
    executeScript,
    executeScriptAsync,
    getAllCookies,
    getNamedCookie,
    addCookie,
    deleteCookie,
    deleteAllCookies,
    dismissAlert,
    acceptAlert,
    getAlertText,
    sendAlertText
  )
where

import Data.Aeson (Value, ToJSON)

import Data.Text  as T (Text)
import WebDriverPreCore.Http (DriverStatus, ElementId, Selector, SessionId, SessionResponse(..))
import WebDriverPreCore.Http qualified as W
import Prelude hiding (log)
import IOUtils (sleep, encodeFileToBase64)
import Http.HttpRunner (run)

-- ############# API #############

status :: IO DriverStatus
status = run W.status

newSession :: W.FullCapabilities -> IO SessionId
newSession = fmap (.sessionId) .  newSessionFull

newSessionFull :: W.FullCapabilities -> IO SessionResponse
newSessionFull c = run $ W.newSession c

newSessionFull' :: (ToJSON a) => a -> IO SessionResponse
newSessionFull' = run . W.newSession' 

getTimeouts :: SessionId -> IO W.Timeouts
getTimeouts = run . W.getTimeouts

setTimeouts :: SessionId -> W.Timeouts -> IO ()
setTimeouts s = run . W.setTimeouts s

getCurrentUrl :: SessionId -> IO Text
getCurrentUrl = run . W.getCurrentUrl

getTitle :: SessionId -> IO Text
getTitle = run . W.getTitle

maximizeWindow :: SessionId -> IO W.WindowRect
maximizeWindow = run . W.maximizeWindow

minimizeWindow :: SessionId -> IO W.WindowRect
minimizeWindow = run . W.minimizeWindow

fullScreenWindow :: SessionId -> IO W.WindowRect
fullScreenWindow = run . W.fullscreenWindow

getWindowHandle :: SessionId -> IO W.WindowHandle
getWindowHandle = run . W.getWindowHandle

getWindowRect :: SessionId -> IO W.WindowRect
getWindowRect = run . W.getWindowRect

getWindowHandles :: SessionId -> IO [W.WindowHandle]
getWindowHandles = run . W.getWindowHandles

newWindow :: SessionId -> IO W.WindowHandleSpec
newWindow = run . W.newWindow

switchToWindow :: SessionId -> W.WindowHandle -> IO ()
switchToWindow s = run . W.switchToWindow s

switchToFrame :: SessionId -> W.FrameReference -> IO ()
switchToFrame s = run . W.switchToFrame s

switchToParentFrame :: SessionId -> IO ()
switchToParentFrame = run . W.switchToParentFrame

closeWindow :: SessionId -> IO ()
closeWindow = run . W.closeWindow

back :: SessionId -> IO ()
back = run . W.back

forward :: SessionId -> IO ()
forward = run . W.forward

refresh :: SessionId -> IO ()
refresh = run . W.refresh

setWindowRect :: SessionId -> W.WindowRect -> IO W.WindowRect
setWindowRect s = run . W.setWindowRect s

minFirefoxSession :: IO SessionId
minFirefoxSession = newSession W.minFirefoxCapabilities

deleteSession :: SessionId -> IO ()
deleteSession = run . W.deleteSession

navigateTo :: SessionId -> Text -> IO ()
navigateTo s = run . W.navigateTo s

findElement :: SessionId -> Selector -> IO ElementId
findElement s = run . W.findElement s

findElementFromElement :: SessionId -> ElementId -> Selector -> IO ElementId
findElementFromElement s eid = run . W.findElementFromElement s eid

findElementsFromElement :: SessionId -> ElementId -> Selector -> IO [ElementId]
findElementsFromElement s eid = run . W.findElementsFromElement s eid

getActiveElement :: SessionId -> IO ElementId
getActiveElement = run . W.getActiveElement

isElementSelected :: SessionId -> ElementId -> IO Bool
isElementSelected s = run . W.isElementSelected s

getElementShadowRoot :: SessionId -> ElementId -> IO ElementId
getElementShadowRoot s = run . W.getElementShadowRoot s

findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> IO ElementId
findElementFromShadowRoot s e = run . W.findElementFromShadowRoot s e

getElementTagName :: SessionId -> ElementId -> IO Text
getElementTagName s = run . W.getElementTagName s

getElementRect :: SessionId -> ElementId -> IO W.WindowRect
getElementRect s = run . W.getElementRect s

isElementEnabled :: SessionId -> ElementId -> IO Bool
isElementEnabled s = run . W.isElementEnabled s

getElementComputedRole :: SessionId -> ElementId -> IO Text
getElementComputedRole s = run . W.getElementComputedRole s

getElementComputedLabel :: SessionId -> ElementId -> IO Text
getElementComputedLabel s = run . W.getElementComputedLabel s

findElements :: SessionId -> Selector -> IO [ElementId]
findElements s = run . W.findElements s

findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> IO [ElementId]
findElementsFromShadowRoot s e = run . W.findElementsFromShadowRoot s e

elementClick :: SessionId -> ElementId -> IO ()
elementClick s = run . W.elementClick s

getElementText :: SessionId -> ElementId -> IO Text
getElementText s = run . W.getElementText s

getElementProperty :: SessionId -> ElementId -> Text -> IO Value
getElementProperty s eid = run . W.getElementProperty s eid

getElementAttribute :: SessionId -> ElementId -> Text -> IO Text
getElementAttribute s eid = run . W.getElementAttribute s eid

getElementCssValue :: SessionId -> ElementId -> Text -> IO Text
getElementCssValue s eid = run . W.getElementCssValue s eid

elementClear :: SessionId -> ElementId -> IO ()
elementClear s = run . W.elementClear s

elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
elementSendKeys s eid = run . W.elementSendKeys s eid

getPageSource :: SessionId -> IO Text
getPageSource = run . W.getPageSource

takeScreenshot :: SessionId -> IO Text
takeScreenshot = run . W.takeScreenshot

takeElementScreenshot :: SessionId -> ElementId -> IO Text
takeElementScreenshot s = run . W.takeElementScreenshot s

printPage :: SessionId -> IO Text
printPage = run . W.printPage

executeScript :: SessionId -> Text -> [Value] -> IO Value
executeScript ses script = run . W.executeScript ses script

executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
executeScriptAsync ses script = run . W.executeScriptAsync ses script

getAllCookies :: SessionId -> IO [W.Cookie]
getAllCookies = run . W.getAllCookies

getNamedCookie :: SessionId -> Text -> IO W.Cookie
getNamedCookie s = run . W.getNamedCookie s

addCookie :: SessionId -> W.Cookie -> IO ()
addCookie s = run . W.addCookie s

deleteCookie :: SessionId -> Text -> IO ()
deleteCookie s = run . W.deleteCookie s

deleteAllCookies :: SessionId -> IO ()
deleteAllCookies = run . W.deleteAllCookies

dismissAlert :: SessionId -> IO ()
dismissAlert = run . W.dismissAlert

acceptAlert :: SessionId -> IO ()
acceptAlert = run . W.acceptAlert

getAlertText :: SessionId -> IO Text
getAlertText = run . W.getAlertText

sendAlertText :: SessionId -> Text -> IO ()
sendAlertText s = run . W.sendAlertText s

performActions :: SessionId -> W.Actions -> IO ()
performActions s = run . W.performActions s

releaseActions :: SessionId -> IO ()
releaseActions = run . W.releaseActions
