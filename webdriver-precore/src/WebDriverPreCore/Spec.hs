module WebDriverPreCore.Spec (
    -- * Introduction
    -- $whyThisLibrary

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

-- $whyThisLibrary
--
-- == What is This Library?
-- This library provides a miniaml abstration over the [WebDriver W3C Protocol](https://www.w3.org/TR/2025/WD-webdriver2-20250210/).
-- It has low dependencies, is unopinionated, and doesn't do very much. 
--
-- It provides a description the W3C API as a type but does not provide any implementation. 
-- This is left to dependent libraries to implement. 
-- 
-- == Why This Library?
-- There are a number of libraries that provide WebDriver bindings for Haskell. However, the libraries we have found to be one or more of:
--
-- 1. Based on the use of Selenium and the older /JSON Wire Protocol/
--
-- 2. Unmaintained
--
-- 3. Part of larger opnionated testing frameworks
-- 
-- This library is intended to be a well maintained, simple, low-dependency, unopinionated base on which other libraries can be built.
--

-- $usage
-- == How to Use This Library?
-- 