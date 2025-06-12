module WebDriverPreCore.Http
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
    module WebDriverPreCore.Http.HttpResponse,

    -- * Capabilities
    module CoreCapabilities,
    module WebDriverPreCore.Http.Capabilities,

    -- * Errors
    module WebDriverPreCore.Http.Error,

    -- * Action Types
    module ActionTypes,

    -- * Auxiliary Spec Types
    module AuxTypes,
  )
where
import WebDriverPreCore.Http.SpecDefinition as WC3Spec (HttpSpec (..))
import WebDriverPreCore.Http.SpecDefinition as RootMethods (newSession, newSession', status)
import WebDriverPreCore.Http.SpecDefinition as SessionMethods
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
import WebDriverPreCore.Http.SpecDefinition as WindowMethods
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
import WebDriverPreCore.Http.SpecDefinition as FrameMethods (switchToParentFrame)
import WebDriverPreCore.Http.SpecDefinition as ElementMethods
  ( findElement,
    findElements,
    getActiveElement,
  )
import WebDriverPreCore.Http.SpecDefinition as ElementInstanceMethods
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
import WebDriverPreCore.Http.SpecDefinition as ShadowDOMMethods (findElementFromShadowRoot, findElementsFromShadowRoot)
import WebDriverPreCore.Http.Capabilities as CoreCapabilities (FullCapabilities(..), Capabilities(..) )
import WebDriverPreCore.Http.Capabilities
import WebDriverPreCore.Http.Error
import WebDriverPreCore.Http.HttpResponse
import WebDriverPreCore.Http.SpecDefinition as ActionTypes
  ( Action (..),
    Actions (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    WheelAction (..),
  )
import WebDriverPreCore.Http.SpecDefinition as AuxTypes
  ( Cookie (..),
    DriverStatus (..),
    ElementId (..),
    FrameReference (..),
    HttpResponse (..),
    SameSite (..),
    Selector (..),
    SessionId (..),
    SessionResponse (..),
    Timeouts (..),
    UrlPath (..),
    WindowHandle (..),
    WindowHandleSpec (..),
    WindowRect (..),
  )