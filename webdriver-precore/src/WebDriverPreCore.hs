module WebDriverPreCore
  ( 
    -- ** The W3Spec Type
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
    module WebDriverPreCore.HttpResponse,

    -- * Capabilities
    module CoreCapabilities,
    module WebDriverPreCore.Capabilities,

    -- * Errors
    module WebDriverPreCore.Error,

    -- * Action Types
    module ActionTypes,

    -- * Auxiliary Spec Types
    module AuxTypes,
  )
where

import WebDriverPreCore.Capabilities as CoreCapabilities (FullCapabilities(..), Capabilities(..) )
import WebDriverPreCore.Capabilities
import WebDriverPreCore.Error
import WebDriverPreCore.HttpResponse
import WebDriverPreCore.SpecDefinition as ActionTypes
  ( Action (..),
    Actions (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    WheelAction (..),
  )
import WebDriverPreCore.SpecDefinition as AuxTypes
  ( Cookie (..),
    DriverStatus (..),
    ElementId (..),
    FrameReference (..),
    HttpResponse (..),
    SameSite (..),
    Selector (..),
    SessionId (..),
    Timeouts (..),
    UrlPath (..),
    WindowHandle (..),
    WindowHandleSpec (..),
    WindowRect (..),
  )
import WebDriverPreCore.SpecDefinition as ElementInstanceMethods
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
import WebDriverPreCore.SpecDefinition as ElementMethods
  ( findElement,
    findElements,
    getActiveElement,
  )
import WebDriverPreCore.SpecDefinition as FrameMethods (switchToParentFrame)
import WebDriverPreCore.SpecDefinition as RootMethods (newSession, newSession', status)
import WebDriverPreCore.SpecDefinition as SessionMethods
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
import WebDriverPreCore.SpecDefinition as ShadowDOMMethods (findElementFromShadowRoot, findElementsFromShadowRoot)
import WebDriverPreCore.SpecDefinition as WC3Spec (W3Spec (..))
import WebDriverPreCore.SpecDefinition as WindowMethods
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
