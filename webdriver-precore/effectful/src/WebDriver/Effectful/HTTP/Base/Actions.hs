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
  ( -- * Root Operations
    status,

    -- * Session Management
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
  ( WebDriver (..),
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
    Status,
    Timeouts,
    WindowHandleSpec,
    WindowRect,
  )
import WebDriverPreCore.Common.Protocol (URL)

-- ---------------------------------------------------------------------------
-- Root operations
-- ---------------------------------------------------------------------------

status :: (WebDriver :> es) => Eff es Status
status = send Status

-- ---------------------------------------------------------------------------
-- Session management
-- ---------------------------------------------------------------------------

getTimeouts :: (WebDriver :> es) => Eff es Timeouts
getTimeouts = send GetTimeouts

setTimeouts :: (WebDriver :> es) => Timeouts -> Eff es ()
setTimeouts = send . SetTimeouts

-- ---------------------------------------------------------------------------
-- Navigation
-- ---------------------------------------------------------------------------

navigateTo :: (WebDriver :> es) => URL -> Eff es ()
navigateTo = send . NavigateTo

getCurrentUrl :: (WebDriver :> es) => Eff es URL
getCurrentUrl = send GetCurrentUrl

back :: (WebDriver :> es) => Eff es ()
back = send Back

forward :: (WebDriver :> es) => Eff es ()
forward = send Forward

refresh :: (WebDriver :> es) => Eff es ()
refresh = send Refresh

getTitle :: (WebDriver :> es) => Eff es Text
getTitle = send GetTitle

-- ---------------------------------------------------------------------------
-- Windows
-- ---------------------------------------------------------------------------

getWindowHandle :: (WebDriver :> es) => Eff es Handle
getWindowHandle = send GetWindowHandle

getWindowHandles :: (WebDriver :> es) => Eff es [Handle]
getWindowHandles = send GetWindowHandles

newWindow :: (WebDriver :> es) => Eff es WindowHandleSpec
newWindow = send NewWindow

closeWindow :: (WebDriver :> es) => Eff es [Handle]
closeWindow = send CloseWindow

switchToWindow :: (WebDriver :> es) => Handle -> Eff es ()
switchToWindow = send . SwitchToWindow

getWindowRect :: (WebDriver :> es) => Eff es WindowRect
getWindowRect = send GetWindowRect

setWindowRect :: (WebDriver :> es) => WindowRect -> Eff es WindowRect
setWindowRect = send . SetWindowRect

maximizeWindow :: (WebDriver :> es) => Eff es WindowRect
maximizeWindow = send MaximizeWindow

minimizeWindow :: (WebDriver :> es) => Eff es WindowRect
minimizeWindow = send MinimizeWindow

fullScreenWindow :: (WebDriver :> es) => Eff es WindowRect
fullScreenWindow = send FullScreenWindow

-- ---------------------------------------------------------------------------
-- Frames
-- ---------------------------------------------------------------------------

switchToFrame :: (WebDriver :> es) => FrameReference -> Eff es ()
switchToFrame = send . SwitchToFrame

switchToParentFrame :: (WebDriver :> es) => Eff es ()
switchToParentFrame = send SwitchToParentFrame

-- ---------------------------------------------------------------------------
-- Page / Script
-- ---------------------------------------------------------------------------

getPageSource :: (WebDriver :> es) => Eff es Text
getPageSource = send GetPageSource

executeScript :: (WebDriver :> es) => Script -> Eff es Value
executeScript = send . ExecuteScript

executeScriptAsync :: (WebDriver :> es) => Script -> Eff es Value
executeScriptAsync = send . ExecuteScriptAsync

-- ---------------------------------------------------------------------------
-- Cookies
-- ---------------------------------------------------------------------------

addCookie :: (WebDriver :> es) => Cookie -> Eff es ()
addCookie = send . AddCookie

getAllCookies :: (WebDriver :> es) => Eff es [Cookie]
getAllCookies = send GetAllCookies

getNamedCookie :: (WebDriver :> es) => Text -> Eff es Cookie
getNamedCookie = send . GetNamedCookie

deleteCookie :: (WebDriver :> es) => Text -> Eff es ()
deleteCookie = send . DeleteCookie

deleteAllCookies :: (WebDriver :> es) => Eff es ()
deleteAllCookies = send DeleteAllCookies

-- ---------------------------------------------------------------------------
-- User actions / Prompts
-- ---------------------------------------------------------------------------

performActions :: (WebDriver :> es) => Actions -> Eff es ()
performActions = send . PerformActions

releaseActions :: (WebDriver :> es) => Eff es ()
releaseActions = send ReleaseActions

dismissAlert :: (WebDriver :> es) => Eff es ()
dismissAlert = send DismissAlert

acceptAlert :: (WebDriver :> es) => Eff es ()
acceptAlert = send AcceptAlert

getAlertText :: (WebDriver :> es) => Eff es Text
getAlertText = send GetAlertText

sendAlertText :: (WebDriver :> es) => Text -> Eff es ()
sendAlertText = send . SendAlertText

-- ---------------------------------------------------------------------------
-- Screenshots / Print
-- ---------------------------------------------------------------------------

takeScreenshot :: (WebDriver :> es) => Eff es Text
takeScreenshot = send TakeScreenshot

printPage :: (WebDriver :> es) => Eff es Text
printPage = send PrintPage

-- ---------------------------------------------------------------------------
-- Element finders
-- ---------------------------------------------------------------------------

getActiveElement :: (WebDriver :> es) => Eff es ElementId
getActiveElement = send GetActiveElement

findElement :: (WebDriver :> es) => Selector -> Eff es ElementId
findElement = send . FindElement

findElements :: (WebDriver :> es) => Selector -> Eff es [ElementId]
findElements = send . FindElements

-- ---------------------------------------------------------------------------
-- Element sub-finders
-- ---------------------------------------------------------------------------

findElementFromElement :: (WebDriver :> es) => ElementId -> Selector -> Eff es ElementId
findElementFromElement el = send . FindElementFromElement el

findElementsFromElement :: (WebDriver :> es) => ElementId -> Selector -> Eff es [ElementId]
findElementsFromElement el = send . FindElementsFromElement el

findElementFromShadowRoot :: (WebDriver :> es) => ShadowRootElementId -> Selector -> Eff es ElementId
findElementFromShadowRoot sr = send . FindElementFromShadowRoot sr

findElementsFromShadowRoot :: (WebDriver :> es) => ShadowRootElementId -> Selector -> Eff es [ElementId]
findElementsFromShadowRoot sr = send . FindElementsFromShadowRoot sr

-- ---------------------------------------------------------------------------
-- Element state
-- ---------------------------------------------------------------------------

isElementSelected :: (WebDriver :> es) => ElementId -> Eff es Bool
isElementSelected = send . IsElementSelected

getElementAttribute :: (WebDriver :> es) => ElementId -> Text -> Eff es (Maybe Text)
getElementAttribute el = send . GetElementAttribute el

getElementProperty :: (WebDriver :> es) => ElementId -> Text -> Eff es (Maybe Value)
getElementProperty el = send . GetElementProperty el

getElementCssValue :: (WebDriver :> es) => ElementId -> Text -> Eff es Text
getElementCssValue el = send . GetElementCssValue el

getElementShadowRoot :: (WebDriver :> es) => ElementId -> Eff es ShadowRootElementId
getElementShadowRoot = send . GetElementShadowRoot

getElementText :: (WebDriver :> es) => ElementId -> Eff es Text
getElementText = send . GetElementText

getElementTagName :: (WebDriver :> es) => ElementId -> Eff es Text
getElementTagName = send . GetElementTagName

getElementRect :: (WebDriver :> es) => ElementId -> Eff es WindowRect
getElementRect = send . GetElementRect

isElementEnabled :: (WebDriver :> es) => ElementId -> Eff es Bool
isElementEnabled = send . IsElementEnabled

getElementComputedRole :: (WebDriver :> es) => ElementId -> Eff es Text
getElementComputedRole = send . GetElementComputedRole

getElementComputedLabel :: (WebDriver :> es) => ElementId -> Eff es Text
getElementComputedLabel = send . GetElementComputedLabel

-- ---------------------------------------------------------------------------
-- Element actions
-- ---------------------------------------------------------------------------

elementClick :: (WebDriver :> es) => ElementId -> Eff es ()
elementClick = send . ElementClick

elementClear :: (WebDriver :> es) => ElementId -> Eff es ()
elementClear = send . ElementClear

elementSendKeys :: (WebDriver :> es) => ElementId -> Text -> Eff es ()
elementSendKeys el = send . ElementSendKeys el

takeElementScreenshot :: (WebDriver :> es) => ElementId -> Eff es Text
takeElementScreenshot = send . TakeElementScreenshot
