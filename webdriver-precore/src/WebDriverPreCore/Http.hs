{-|
Description : Deprecated in favour of "WebDriverPreCore.HTTP.Protocol"

-}


module WebDriverPreCore.Http  {-# DEPRECATED "WebDriverPreCore.Http - will be removed in a future release ~ 2027-02-01 (USE WebDriverPreCore.HTTP.Protocol instead). See ChangeLog.md for upgrade instructions" #-} 
  ( 
    -- ** The HttpSpec Type
    module WC3Spec,

    -- ** Root Methods
    module RootMethods,

    -- ** Session Methods
    -- | /See also 'newSession' and 'newSession''/
    module SessionMethods,

    -- ** Window Methods
    module WindowMethods,

    -- ** Frame Methods
    module FrameMethods,

    -- ** Element(s) Methods
    module ElementMethods,

    -- ** Element Instance Methods
    module ElementInstanceMethods,

    -- ** Shadow DOM Methods
    module ShadowDOMMethods,

    -- * HTTP Response
    module WebDriverPreCore.HTTP.HttpResponse,

    -- * Capabilities
    module CoreCapabilities,

    -- * Errors
    module WebDriverPreCore.Error,

    -- * Action Types
    -- module ActionTypes,

    -- * Auxiliary Spec Types
    -- module AuxTypes,
  )
where
import WebDriverPreCore.HTTP.SpecDefinition as WC3Spec (HttpSpec (..))
import WebDriverPreCore.HTTP.SpecDefinition as RootMethods (newSession, newSession', status)
import WebDriverPreCore.HTTP.SpecDefinition as SessionMethods
  ( acceptAlert,
    addCookie,
    back,
    closeWindow,
    deleteAllCookies,
    deleteCookie,
    deleteSession,
    dismissAlert,
    executeScript,
    executeScriptAsync,
    forward,
    fullscreenWindow,
    getAlertText,
    getAllCookies,
    getCurrentUrl,
    getNamedCookie,
    getPageSource,
    getTimeouts,
    getTitle,
    getWindowHandle,
    getWindowHandles,
    getWindowRect,
    maximizeWindow,
    minimizeWindow,
    navigateTo,
    newWindow,
    performActions,
    printPage,
    refresh,
    releaseActions,
    sendAlertText,
    setTimeouts,
    setWindowRect,
    switchToFrame,
    switchToWindow,
    takeScreenshot,
  )
import WebDriverPreCore.HTTP.SpecDefinition as WindowMethods
  ( closeWindow,
    fullscreenWindow,
    getWindowHandles,
    getWindowRect,
    maximizeWindow,
    minimizeWindow,
    newWindow,
    setWindowRect,
    switchToWindow,
  )
import WebDriverPreCore.HTTP.SpecDefinition as FrameMethods (switchToParentFrame)
import WebDriverPreCore.HTTP.SpecDefinition as ElementMethods
  ( findElement,
    findElements,
    getActiveElement,
  )
import WebDriverPreCore.HTTP.SpecDefinition as ElementInstanceMethods
  ( elementClear,
    elementClick,
    elementSendKeys,
    findElementFromElement,
    findElementsFromElement,
    getElementAttribute,
    getElementComputedLabel,
    getElementComputedRole,
    getElementCssValue,
    getElementProperty,
    getElementRect,
    getElementShadowRoot,
    getElementTagName,
    getElementText,
    isElementEnabled,
    isElementSelected,
    takeElementScreenshot,
  )
import WebDriverPreCore.HTTP.SpecDefinition as ShadowDOMMethods (findElementFromShadowRoot, findElementsFromShadowRoot)
import WebDriverPreCore.HTTP.Protocol as CoreCapabilities (FullCapabilities(..), Capabilities(..) )
import WebDriverPreCore.HTTP.Protocol
import WebDriverPreCore.Error
import WebDriverPreCore.HTTP.HttpResponse
-- import WebDriverPreCore.HTTP.SpecDefinition as ActionTypes
--   ( Action (..),
--     Actions (..),
--     KeyAction (..),
--     Pointer (..),
--     PointerAction (..),
--     PointerOrigin (..),
--     WheelAction (..),
--   )
-- import WebDriverPreCore.HTTP.SpecDefinition as AuxTypes
--   ( Cookie (..),
--     DriverStatus (..),
--     ElementId (..),
--     FrameReference (..),
--     HttpResponse (..),
--     SameSite (..),
--     Selector (..),
--     SessionId (..),
--     SessionResponse (..),
--     Timeouts (..),
--     UrlPath (..),
--     Handle (..),
--     WindowHandleSpec (..),
--     WindowRect (..),
--   )