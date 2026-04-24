-- |
-- Module: WebDriver.Effectful.HTTP.Base.Actions
-- Description: Effectful-style HTTP WebDriver action functions
--
-- Provides smart constructors for the 'WebDriverHttp' algebraic effect.
-- Each function simply invokes the corresponding constructor via 'send',
-- giving you a clean, typed API that mirrors
-- "WebDriver.Bluefin.HTTP.Base.Actions" but threads constraints implicitly
-- through the effect stack rather than via an explicit handle.
--
-- Typical usage:
--
-- @
-- myTest :: (WebDriverHttp ':>' es, Logger ':>' es, LogPause ':>' es, IOE ':>' es)
--        => Eff es ()
-- myTest = do
--   log "navigate"
--   navigateTo (MkUrl "https://example.com")
--   title <- getTitle
--   log title
-- @
module WebDriver.Effectful.HTTP.Base.Actions
  ( -- * Session Management
    deleteSession,
    getTimeouts,
    setTimeouts,

    -- * Navigation
    navigateTo,
    getCurrentUrl,
    back,
    forward,
    refresh,
    getTitle,

    -- * Windows
    getWindowHandle,
    getWindowHandles,
    newWindow,
    closeWindow,
    switchToWindow,
    getWindowRect,
    setWindowRect,
    maximizeWindow,
    minimizeWindow,
    fullScreenWindow,

    -- * Frames
    switchToFrame,
    switchToParentFrame,

    -- * Page / Script
    getPageSource,
    executeScript,
    executeScriptAsync,

    -- * Cookies
    addCookie,
    getAllCookies,
    getNamedCookie,
    deleteCookie,
    deleteAllCookies,

    -- * User Actions / Prompts
    performActions,
    releaseActions,
    dismissAlert,
    acceptAlert,
    getAlertText,
    sendAlertText,

    -- * Screenshots / Print
    takeScreenshot,
    printPage,

    -- * Element Finders
    getActiveElement,
    findElement,
    findElements,

    -- * Element Sub-finders
    findElementFromElement,
    findElementsFromElement,
    findElementFromShadowRoot,
    findElementsFromShadowRoot,

    -- * Element State
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

    -- * Element Actions
    elementClick,
    elementClear,
    elementSendKeys,
    takeElementScreenshot,
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (send)
import WebDriver.Effectful.HTTP.Base.Effect
  ( WebDriverHttp (..),
  )
import WebDriverPreCore.Extended.HTTP.Base.Protocol
  ( Actions,
    Cookie,
    ElementId,
    FrameReference,
    Handle,
    Script,
    Selector,
    ShadowRootElementId,
    Timeouts,
    URL,
    WindowHandleSpec,
    WindowRect,
  )

-- ---------------------------------------------------------------------------
-- Session management
-- ---------------------------------------------------------------------------

deleteSession :: (WebDriverHttp :> es) => Eff es ()
deleteSession = send DeleteSession

getTimeouts :: (WebDriverHttp :> es) => Eff es Timeouts
getTimeouts = send GetTimeouts

setTimeouts :: (WebDriverHttp :> es) => Timeouts -> Eff es ()
setTimeouts = send . SetTimeouts

-- ---------------------------------------------------------------------------
-- Navigation
-- ---------------------------------------------------------------------------

navigateTo :: (WebDriverHttp :> es) => URL -> Eff es ()
navigateTo = send . NavigateTo

getCurrentUrl :: (WebDriverHttp :> es) => Eff es URL
getCurrentUrl = send GetCurrentUrl

back :: (WebDriverHttp :> es) => Eff es ()
back = send Back

forward :: (WebDriverHttp :> es) => Eff es ()
forward = send Forward

refresh :: (WebDriverHttp :> es) => Eff es ()
refresh = send Refresh

getTitle :: (WebDriverHttp :> es) => Eff es Text
getTitle = send GetTitle

-- ---------------------------------------------------------------------------
-- Windows
-- ---------------------------------------------------------------------------

getWindowHandle :: (WebDriverHttp :> es) => Eff es Handle
getWindowHandle = send GetWindowHandle

getWindowHandles :: (WebDriverHttp :> es) => Eff es [Handle]
getWindowHandles = send GetWindowHandles

newWindow :: (WebDriverHttp :> es) => Eff es WindowHandleSpec
newWindow = send NewWindow

closeWindow :: (WebDriverHttp :> es) => Eff es [Handle]
closeWindow = send CloseWindow

switchToWindow :: (WebDriverHttp :> es) => Handle -> Eff es ()
switchToWindow = send . SwitchToWindow

getWindowRect :: (WebDriverHttp :> es) => Eff es WindowRect
getWindowRect = send GetWindowRect

setWindowRect :: (WebDriverHttp :> es) => WindowRect -> Eff es WindowRect
setWindowRect = send . SetWindowRect

maximizeWindow :: (WebDriverHttp :> es) => Eff es WindowRect
maximizeWindow = send MaximizeWindow

minimizeWindow :: (WebDriverHttp :> es) => Eff es WindowRect
minimizeWindow = send MinimizeWindow

fullScreenWindow :: (WebDriverHttp :> es) => Eff es WindowRect
fullScreenWindow = send FullScreenWindow

-- ---------------------------------------------------------------------------
-- Frames
-- ---------------------------------------------------------------------------

switchToFrame :: (WebDriverHttp :> es) => FrameReference -> Eff es ()
switchToFrame = send . SwitchToFrame

switchToParentFrame :: (WebDriverHttp :> es) => Eff es ()
switchToParentFrame = send SwitchToParentFrame

-- ---------------------------------------------------------------------------
-- Page / Script
-- ---------------------------------------------------------------------------

getPageSource :: (WebDriverHttp :> es) => Eff es Text
getPageSource = send GetPageSource

executeScript :: (WebDriverHttp :> es) => Script -> Eff es Value
executeScript = send . ExecuteScript

executeScriptAsync :: (WebDriverHttp :> es) => Script -> Eff es Value
executeScriptAsync = send . ExecuteScriptAsync

-- ---------------------------------------------------------------------------
-- Cookies
-- ---------------------------------------------------------------------------

addCookie :: (WebDriverHttp :> es) => Cookie -> Eff es ()
addCookie = send . AddCookie

getAllCookies :: (WebDriverHttp :> es) => Eff es [Cookie]
getAllCookies = send GetAllCookies

getNamedCookie :: (WebDriverHttp :> es) => Text -> Eff es Cookie
getNamedCookie = send . GetNamedCookie

deleteCookie :: (WebDriverHttp :> es) => Text -> Eff es ()
deleteCookie = send . DeleteCookie

deleteAllCookies :: (WebDriverHttp :> es) => Eff es ()
deleteAllCookies = send DeleteAllCookies

-- ---------------------------------------------------------------------------
-- User actions / Prompts
-- ---------------------------------------------------------------------------

performActions :: (WebDriverHttp :> es) => Actions -> Eff es ()
performActions = send . PerformActions

releaseActions :: (WebDriverHttp :> es) => Eff es ()
releaseActions = send ReleaseActions

dismissAlert :: (WebDriverHttp :> es) => Eff es ()
dismissAlert = send DismissAlert

acceptAlert :: (WebDriverHttp :> es) => Eff es ()
acceptAlert = send AcceptAlert

getAlertText :: (WebDriverHttp :> es) => Eff es Text
getAlertText = send GetAlertText

sendAlertText :: (WebDriverHttp :> es) => Text -> Eff es ()
sendAlertText = send . SendAlertText

-- ---------------------------------------------------------------------------
-- Screenshots / Print
-- ---------------------------------------------------------------------------

takeScreenshot :: (WebDriverHttp :> es) => Eff es Text
takeScreenshot = send TakeScreenshot

printPage :: (WebDriverHttp :> es) => Eff es Text
printPage = send PrintPage

-- ---------------------------------------------------------------------------
-- Element finders
-- ---------------------------------------------------------------------------

getActiveElement :: (WebDriverHttp :> es) => Eff es ElementId
getActiveElement = send GetActiveElement

findElement :: (WebDriverHttp :> es) => Selector -> Eff es ElementId
findElement = send . FindElement

findElements :: (WebDriverHttp :> es) => Selector -> Eff es [ElementId]
findElements = send . FindElements

-- ---------------------------------------------------------------------------
-- Element sub-finders
-- ---------------------------------------------------------------------------

findElementFromElement :: (WebDriverHttp :> es) => ElementId -> Selector -> Eff es ElementId
findElementFromElement el = send . FindElementFromElement el

findElementsFromElement :: (WebDriverHttp :> es) => ElementId -> Selector -> Eff es [ElementId]
findElementsFromElement el = send . FindElementsFromElement el

findElementFromShadowRoot :: (WebDriverHttp :> es) => ShadowRootElementId -> Selector -> Eff es ElementId
findElementFromShadowRoot sr = send . FindElementFromShadowRoot sr

findElementsFromShadowRoot :: (WebDriverHttp :> es) => ShadowRootElementId -> Selector -> Eff es [ElementId]
findElementsFromShadowRoot sr = send . FindElementsFromShadowRoot sr

-- ---------------------------------------------------------------------------
-- Element state
-- ---------------------------------------------------------------------------

isElementSelected :: (WebDriverHttp :> es) => ElementId -> Eff es Bool
isElementSelected = send . IsElementSelected

getElementAttribute :: (WebDriverHttp :> es) => ElementId -> Text -> Eff es (Maybe Text)
getElementAttribute el = send . GetElementAttribute el

getElementProperty :: (WebDriverHttp :> es) => ElementId -> Text -> Eff es (Maybe Value)
getElementProperty el = send . GetElementProperty el

getElementCssValue :: (WebDriverHttp :> es) => ElementId -> Text -> Eff es Text
getElementCssValue el = send . GetElementCssValue el

getElementShadowRoot :: (WebDriverHttp :> es) => ElementId -> Eff es ShadowRootElementId
getElementShadowRoot = send . GetElementShadowRoot

getElementText :: (WebDriverHttp :> es) => ElementId -> Eff es Text
getElementText = send . GetElementText

getElementTagName :: (WebDriverHttp :> es) => ElementId -> Eff es Text
getElementTagName = send . GetElementTagName

getElementRect :: (WebDriverHttp :> es) => ElementId -> Eff es WindowRect
getElementRect = send . GetElementRect

isElementEnabled :: (WebDriverHttp :> es) => ElementId -> Eff es Bool
isElementEnabled = send . IsElementEnabled

getElementComputedRole :: (WebDriverHttp :> es) => ElementId -> Eff es Text
getElementComputedRole = send . GetElementComputedRole

getElementComputedLabel :: (WebDriverHttp :> es) => ElementId -> Eff es Text
getElementComputedLabel = send . GetElementComputedLabel

-- ---------------------------------------------------------------------------
-- Element actions
-- ---------------------------------------------------------------------------

elementClick :: (WebDriverHttp :> es) => ElementId -> Eff es ()
elementClick = send . ElementClick

elementClear :: (WebDriverHttp :> es) => ElementId -> Eff es ()
elementClear = send . ElementClear

elementSendKeys :: (WebDriverHttp :> es) => ElementId -> Text -> Eff es ()
elementSendKeys el = send . ElementSendKeys el

takeElementScreenshot :: (WebDriverHttp :> es) => ElementId -> Eff es Text
takeElementScreenshot = send . TakeElementScreenshot
