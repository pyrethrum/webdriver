module WebDriverPreCore.Spec (
   -- * The W3Spec Type
    S.W3Spec (..),
    -- * The API
    -- ** Root Methods
    S.newSession,
    S.newSession',
    S.status,

    -- ** Session Methods
    S.deleteSession,
    S.getTimeouts,
    S.setTimeouts,
    S.navigateTo,
    S.getCurrentUrl,
    S.back,
    S.forward,
    S.refresh,
    S.getTitle,
    S.getWindowHandle,
    S.newWindow,
    S.closeWindow,
    S.switchToWindow,
    S.switchToFrame,
    S.getPageSource,
    S.executeScript,
    S.executeScriptAsync,
    S.getAllCookies,
    S.getNamedCookie,
    S.addCookie,
    S.deleteCookie,
    S.deleteAllCookies,
    S.performActions,
    S.releaseActions,
    S.dismissAlert,
    S.acceptAlert,
    S.getAlertText,
    S.sendAlertText,
    S.takeScreenshot,
    S.printPage,
    
    -- ** Window Methods
    S.getWindowHandles,
    S.getWindowRect,
    S.setWindowRect,
    S.maximizeWindow,
    S.minimizeWindow,
    S.fullscreenWindow,

    -- ** Frame Methods
    S.switchToParentFrame,

    -- ** Element(s) Methods
    S.getActiveElement,
    S.findElement,
    S.findElements,

    -- ** Element Instance Methods
    S.getElementShadowRoot,
    S.findElementFromElement,
    S.findElementsFromElement,
    S.isElementSelected,
    S.getElementAttribute,
    S.getElementProperty,
    S.getElementCssValue,
    S.getElementText,
    S.getElementTagName,
    S.getElementRect,
    S.isElementEnabled,
    S.getElementComputedRole,
    S.getElementComputedLabel,
    S.elementClick,
    S.elementClear,
    S.elementSendKeys,
    S.takeElementScreenshot,
    
    -- ** Shadow DOM Methods
    S.findElementFromShadowRoot,
    S.findElementsFromShadowRoot,
    
    -- * Auxilary Spec Types
    S.Cookie (..),
    S.DriverStatus (..),
    S.ElementId (..),
    S.FrameReference (..),
    S.HttpResponse (..),
    S.SameSite (..),
    S.Selector (..),
    S.SessionId (..),
    S.Timeouts (..),
    S.WindowHandle (..),
    S.WindowHandleSpec (..),
    S.WindowRect (..),
    S.UrlPath (..),

    -- ** Action Types
    S.Action (..),
    S.Actions (..),
    S.KeyAction (..),
    S.Pointer (..),
    S.PointerAction (..),
    S.PointerOrigin (..),
    S.WheelAction (..),

    -- * HTTP Response
    module WebDriverPreCore.Spec.HttpResponse,

    -- * Capabilities
    module WebDriverPreCore.Spec.Capabilities,

     -- * Errors
    module WebDriverPreCore.Spec.Error,

) where

import WebDriverPreCore.Spec.SpecDefinition qualified as S



import WebDriverPreCore.Spec.HttpResponse
import WebDriverPreCore.Spec.Capabilities
import WebDriverPreCore.Spec.Error