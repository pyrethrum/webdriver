

module BiDi.BiDiAPI where

import WebDriverPreCore.Http qualified as W


--   ( 
--     B.Cookie (..),
--     B.Capabilities(..),
--     B.FullCapabilities(..), 
--     B.VendorSpecific(..),
--     B.DriverStatus(..),
--     B.Timeouts (..),
--     B.WindowHandleSpec (..),
--     B.WindowHandle(..),
--     B.SameSite (..),
--     B.Selector (..),
--     B.SessionId (..),
--     B.FrameReference (..),
--     B.WindowRect (..),
--     B.PointerOrigin (..),
--     B.Action (..),
--     B.Actions (..),
--     B.KeyAction (..),
--     B.Pointer (..),
--     B.PointerAction (..),
--     B.WheelAction (..),
--     B.errorCodeToErrorType,
--     B.minFirefoxCapabilities,
--     B.minCapabilities,
--     B.BrowserName(..),
--     encodeFileToBase64,
--     status,
--     findElementFromElement,
--     findElementsFromElement,
--     findElements,
--     getTimeouts,
--     setTimeouts,
--     back,
--     forward,
--     getActiveElement,
--     refresh,
--     getCurrentUrl,
--     getElementAttribute,
--     getElementShadowRoot,
--     findElementFromShadowRoot,
--     findElementsFromShadowRoot,
--     getTitle,
--     getWindowHandles,
--     isElementSelected,
--     maximizeWindow,
--     minimizeWindow,
--     fullScreenWindow,
--     getWindowHandle,
--     getWindowRect,
--     closeWindow,
--     newWindow,
--     newSession,
--     minFirefoxSession,
-- )
-- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- elementSendKeys s eid = run . B.elementSendKeys s eid

-- getPageSource :: SessionId -> IO Text
-- getPageSource = run . B.getPageSource

-- takeScreenshot :: SessionId -> IO Text
-- takeScreenshot = run . B.takeScreenshot

-- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- takeElementScreenshot s = run . B.takeElementScreenshot s

-- printPage :: SessionId -> IO Text
-- printPage = run . B.printPage



-- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- executeScript ses script = run . B.executeScript ses script

-- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- getAllCookies :: SessionId -> IO [B.Cookie]
-- getAllCookies = run . B.getAllCookies

-- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- getNamedCookie s = run . B.getNamedCookie s

-- addCookie :: SessionId -> B.Cookie -> IO ()
-- addCookie s = run . B.addCookie s

-- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- elementSendKeys s eid = run . B.elementSendKeys s eid

-- getPageSource :: SessionId -> IO Text
-- getPageSource = run . B.getPageSource

-- takeScreenshot :: SessionId -> IO Text
-- takeScreenshot = run . B.takeScreenshot

-- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- takeElementScreenshot s = run . B.takeElementScreenshot s

-- printPage :: SessionId -> IO Text
-- printPage = run . B.printPage



-- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- executeScript ses script = run . B.executeScript ses script

-- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- getAllCookies :: SessionId -> IO [B.Cookie]
-- getAllCookies = run . B.getAllCookies

-- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- getNamedCookie s = run . B.getNamedCookie s

-- addCookie :: SessionId -> B.Cookie -> IO ()
-- addCookie s = run . B.addCookie s

-- deleteCookie :: SessionId -> Text -> IO ()
-- deleteCookie s = run . B.deleteCookie s

-- deleteAllCookies :: SessionId -> IO ()

-- dismissAlert = run . B.dismissAlert

-- acceptAlert :: SessionId -> IO ()
-- acceptAlert = run . B.acceptAlert

-- getAlertText :: SessionId -> IO Text
-- getAlertText = run . B.getAlertText

-- sendAlertText :: SessionId -> Text -> IO ()
-- sendAlertText s = run . B.sendAlertText s

-- performActions :: SessionId -> B.Actions -> IO ()
-- performActions s = run . B.performActions s

-- releaseActions :: SessionId -> IO ()
-- releaseActions = run . B.releaseActions


-- deleteCookie :: SessionId -> Text -> IO ()
-- deleteCookie s = run . B.deleteCookie s

-- deleteAllCookies :: SessionId -> IO ()

-- dismissAlert = run . B.dismissAlert

-- acceptAlert :: SessionId -> IO ()
-- acceptAlert = run . B.acceptAlert

-- getAlertText :: SessionId -> IO Text
-- getAlertText = run . B.getAlertText

-- sendAlertText :: SessionId -> Text -> IO ()
-- sendAlertText s = run . B.sendAlertText s

-- performActions :: SessionId -> B.Actions -> IO ()
-- performActions s = run . B.performActions s

-- releaseActions :: SessionId -> IO ()
-- releaseActions = run . B.releaseActions

-- --     performActions,
-- --     releaseActions,
-- --     deleteSession,
-- --     navigateTo,
-- --     findElement,
-- --     elementClick,
-- --     getElementText,
-- --     setWindowRect,
-- --     sleepMs,
-- --     switchToWindow,
-- --     switchToFrame,
-- --     switchToParentFrame,
-- --     getElementProperty,
-- --     getElementCssValue,
-- --     getElementTagName,
-- --     getElementRect,
-- --     isElementEnabled,
-- --     getElementComputedRole,
-- --     getElementComputedLabel,
-- --     elementClear,
-- --     elementSendKeys,
-- --     printPage,
-- --     getPageSource,
-- --     takeScreenshot,
-- --     takeElementScreenshot,
-- --     executeScript,
-- --     executeScriptAsync,
-- --     getAllCookies,
-- --     getNamedCookie,
-- --     addCookie,
-- --     deleteCookie,
-- --     deleteAllCookies,
-- --     dismissAlert,
-- --     acceptAlert,
-- --     getAlertText,
-- --     sendAlertText
-- --   )
-- -- where

-- -- import Data.Aeson (Value)

-- -- import Data.Text  as T (Text)
-- -- import Prelude hiding (log)
-- -- import Http.IOUtils (sleepMs, encodeFileToBase64)
-- -- import Http.HttpRunner (run)

-- -- -- ############# API #############

-- -- status :: IO DriverStatus
-- -- status = run B.status

-- -- newSession :: B.FullCapabilities -> IO SessionId
-- -- newSession = run . B.newSession

-- -- getTimeouts :: SessionId -> IO B.Timeouts
-- -- getTimeouts = run . B.getTimeouts

-- -- setTimeouts :: SessionId -> B.Timeouts -> IO ()
-- -- setTimeouts s = run . B.setTimeouts s

-- -- getCurrentUrl :: SessionId -> IO Text
-- -- getCurrentUrl = run . B.getCurrentUrl

-- -- getTitle :: SessionId -> IO Text
-- -- getTitle = run . B.getTitle

-- -- maximizeWindow :: SessionId -> IO B.WindowRect
-- -- maximizeWindow = run . B.maximizeWindow

-- -- minimizeWindow :: SessionId -> IO B.WindowRect
-- -- minimizeWindow = run . B.minimizeWindow

-- -- fullScreenWindow :: SessionId -> IO B.WindowRect
-- -- fullScreenWindow = run . B.fullscreenWindow

-- -- getWindowHandle :: SessionId -> IO B.WindowHandle
-- -- getWindowHandle = run . B.getWindowHandle

-- -- getWindowRect :: SessionId -> IO B.WindowRect
-- -- getWindowRect = run . B.getWindowRect

-- -- getWindowHandles :: SessionId -> IO [B.WindowHandle]
-- -- getWindowHandles = run . B.getWindowHandles

-- -- newWindow :: SessionId -> IO B.WindowHandleSpec
-- -- newWindow = run . B.newWindow

-- -- switchToWindow :: SessionId -> B.WindowHandle -> IO ()
-- -- switchToWindow s = run . B.switchToWindow s

-- -- switchToFrame :: SessionId -> B.FrameReference -> IO ()
-- -- switchToFrame s = run . B.switchToFrame s

-- -- switchToParentFrame :: SessionId -> IO ()
-- -- switchToParentFrame = run . B.switchToParentFrame

-- -- closeWindow :: SessionId -> IO ()
-- -- closeWindow = run . B.closeWindow

-- -- back :: SessionId -> IO ()
-- -- back = run . B.back

-- -- forward :: SessionId -> IO ()
-- -- forward = run . B.forward

-- -- refresh :: SessionId -> IO ()
-- -- refresh = run . B.refresh

-- -- setWindowRect :: SessionId -> B.WindowRect -> IO B.WindowRect
-- -- setWindowRect s = run . B.setWindowRect s

-- -- minFirefoxSession :: IO SessionId
-- -- minFirefoxSession = newSession B.minFirefoxCapabilities

-- -- deleteSession :: SessionId -> IO ()
-- -- deleteSession = run . B.deleteSession

-- -- navigateTo :: SessionId -> Text -> IO ()
-- -- navigateTo s = run . B.navigateTo s

-- -- findElement :: SessionId -> Selector -> IO ElementId
-- -- findElement s = run . B.findElement s

-- -- findElementFromElement :: SessionId -> ElementId -> Selector -> IO ElementId
-- -- findElementFromElement s eid = run . B.findElementFromElement s eid

-- -- findElementsFromElement :: SessionId -> ElementId -> Selector -> IO [ElementId]
-- -- findElementsFromElement s eid = run . B.findElementsFromElement s eid

-- -- getActiveElement :: SessionId -> IO ElementId
-- -- getActiveElement = run . B.getActiveElement

-- -- isElementSelected :: SessionId -> ElementId -> IO Bool
-- -- isElementSelected s = run . B.isElementSelected s

-- -- getElementShadowRoot :: SessionId -> ElementId -> IO ElementId
-- -- getElementShadowRoot s = run . B.getElementShadowRoot s

-- -- findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> IO ElementId
-- -- findElementFromShadowRoot s e = run . B.findElementFromShadowRoot s e

-- -- getElementTagName :: SessionId -> ElementId -> IO Text
-- -- getElementTagName s = run . B.getElementTagName s

-- -- getElementRect :: SessionId -> ElementId -> IO B.WindowRect
-- -- getElementRect s = run . B.getElementRect s

-- -- isElementEnabled :: SessionId -> ElementId -> IO Bool
-- -- isElementEnabled s = run . B.isElementEnabled s

-- -- getElementComputedRole :: SessionId -> ElementId -> IO Text
-- -- getElementComputedRole s = run . B.getElementComputedRole s

-- -- getElementComputedLabel :: SessionId -> ElementId -> IO Text
-- -- getElementComputedLabel s = run . B.getElementComputedLabel s

-- -- findElements :: SessionId -> Selector -> IO [ElementId]
-- -- findElements s = run . B.findElements s

-- -- findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> IO [ElementId]
-- -- findElementsFromShadowRoot s e = run . B.findElementsFromShadowRoot s e

-- -- elementClick :: SessionId -> ElementId -> IO ()
-- -- elementClick s = run . B.elementClick s

-- -- getElementText :: SessionId -> ElementId -> IO Text
-- -- getElementText s = run . B.getElementText s

-- -- getElementProperty :: SessionId -> ElementId -> Text -> IO Value
-- -- getElementProperty s eid = run . B.getElementProperty s eid

-- -- getElementAttribute :: SessionId -> ElementId -> Text -> IO Text
-- -- getElementAttribute s eid = run . B.getElementAttribute s eid

-- -- getElementCssValue :: SessionId -> ElementId -> Text -> IO Text
-- -- getElementCssValue s eid = run . B.getElementCssValue s eid)
-- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- elementSendKeys s eid = run . B.elementSendKeys s eid

-- getPageSource :: SessionId -> IO Text
-- getPageSource = run . B.getPageSource

-- takeScreenshot :: SessionId -> IO Text
-- takeScreenshot = run . B.takeScreenshot

-- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- takeElementScreenshot s = run . B.takeElementScreenshot s

-- printPage :: SessionId -> IO Text
-- printPage = run . B.printPage



-- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- executeScript ses script = run . B.executeScript ses script

-- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- getAllCookies :: SessionId -> IO [B.Cookie]
-- getAllCookies = run . B.getAllCookies

-- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- getNamedCookie s = run . B.getNamedCookie s

-- addCookie :: SessionId -> B.Cookie -> IO ()
-- addCookie s = run . B.addCookie s

-- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- elementSendKeys s eid = run . B.elementSendKeys s eid

-- getPageSource :: SessionId -> IO Text
-- getPageSource = run . B.getPageSource

-- takeScreenshot :: SessionId -> IO Text
-- takeScreenshot = run . B.takeScreenshot

-- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- takeElementScreenshot s = run . B.takeElementScreenshot s

-- printPage :: SessionId -> IO Text
-- printPage = run . B.printPage



-- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- executeScript ses script = run . B.executeScript ses script

-- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- getAllCookies :: SessionId -> IO [B.Cookie]
-- getAllCookies = run . B.getAllCookies

-- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- getNamedCookie s = run . B.getNamedCookie s

-- addCookie :: SessionId -> B.Cookie -> IO ()
-- addCookie s = run . B.addCookie s

-- deleteCookie :: SessionId -> Text -> IO ()
-- deleteCookie s = run . B.deleteCookie s

-- deleteAllCookies :: SessionId -> IO ()

-- dismissAlert = run . B.dismissAlert

-- acceptAlert :: SessionId -> IO ()
-- acceptAlert = run . B.acceptAlert

-- getAlertText :: SessionId -> IO Text
-- getAlertText = run . B.getAlertText

-- sendAlertText :: SessionId -> Text -> IO ()
-- sendAlertText s = run . B.sendAlertText s

-- performActions :: SessionId -> B.Actions -> IO ()
-- performActions s = run . B.performActions s

-- releaseActions :: SessionId -> IO ()
-- releaseActions = run . B.releaseActions


-- deleteCookie :: SessionId -> Text -> IO ()
-- deleteCookie s = run . B.deleteCookie s

-- deleteAllCookies :: SessionId -> IO ()

-- dismissAlert = run . B.dismissAlert

-- acceptAlert :: SessionId -> IO ()
-- acceptAlert = run . B.acceptAlert

-- getAlertText :: SessionId -> IO Text
-- getAlertText = run . B.getAlertText

-- sendAlertText :: SessionId -> Text -> IO ()
-- sendAlertText s = run . B.sendAlertText s

-- performActions :: SessionId -> B.Actions -> IO ()
-- performActions s = run . B.performActions s

-- releaseActions :: SessionId -> IO ()
-- releaseActions = run . B.releaseActions

-- --     performActions,
-- --     releaseActions,
-- --     deleteSession,
-- --     navigateTo,
-- --     findElement,
-- --     elementClick,
-- --     getElementText,
-- --     setWindowRect,
-- --     sleepMs,
-- --     switchToWindow,
-- --     switchToFrame,
-- --     switchToParentFrame,
-- --     getElementProperty,
-- --     getElementCssValue,
-- --     getElementTagName,
-- --     getElementRect,
-- --     isElementEnabled,
-- --     getElementComputedRole,
-- --     getElementComputedLabel,
-- --     elementClear,
-- --     elementSendKeys,
-- --     printPage,
-- --     getPageSource,
-- --     takeScreenshot,
-- --     takeElementScreenshot,
-- --     executeScript,
-- --     executeScriptAsync,
-- --     getAllCookies,
-- --     getNamedCookie,
-- --     addCookie,
-- --     deleteCookie,
-- --     deleteAllCookies,
-- --     dismissAlert,
-- --     acceptAlert,
-- --     getAlertText,
-- --     sendAlertText
-- --   )
-- -- where

-- -- import Data.Aeson (Value)

-- -- import Data.Text  as T (Text)
-- -- import Prelude hiding (log)
-- -- import Http.IOUtils (sleepMs, encodeFileToBase64)
-- -- import Http.HttpRunner (run)

-- -- -- ############# API #############

-- -- status :: IO DriverStatus
-- -- status = run B.status

-- -- newSession :: B.FullCapabilities -> IO SessionId
-- -- newSession = run . B.newSession

-- -- getTimeouts :: SessionId -> IO B.Timeouts
-- -- getTimeouts = run . B.getTimeouts

-- -- setTimeouts :: SessionId -> B.Timeouts -> IO ()
-- -- setTimeouts s = run . B.setTimeouts s

-- -- getCurrentUrl :: SessionId -> IO Text
-- -- getCurrentUrl = run . B.getCurrentUrl

-- -- getTitle :: SessionId -> IO Text
-- -- getTitle = run . B.getTitle

-- -- maximizeWindow :: SessionId -> IO B.WindowRect
-- -- maximizeWindow = run . B.maximizeWindow

-- -- minimizeWindow :: SessionId -> IO B.WindowRect
-- -- minimizeWindow = run . B.minimizeWindow

-- -- fullScreenWindow :: SessionId -> IO B.WindowRect
-- -- fullScreenWindow = run . B.fullscreenWindow

-- -- getWindowHandle :: SessionId -> IO B.WindowHandle
-- -- getWindowHandle = run . B.getWindowHandle

-- -- getWindowRect :: SessionId -> IO B.WindowRect
-- -- getWindowRect = run . B.getWindowRect

-- -- getWindowHandles :: SessionId -> IO [B.WindowHandle]
-- -- getWindowHandles = run . B.getWindowHandles

-- -- newWindow :: SessionId -> IO B.WindowHandleSpec
-- -- newWindow = run . B.newWindow

-- -- switchToWindow :: SessionId -> B.WindowHandle -> IO ()
-- -- switchToWindow s = run . B.switchToWindow s

-- -- switchToFrame :: SessionId -> B.FrameReference -> IO ()
-- -- switchToFrame s = run . B.switchToFrame s

-- -- switchToParentFrame :: SessionId -> IO ()
-- -- switchToParentFrame = run . B.switchToParentFrame

-- -- closeWindow :: SessionId -> IO ()
-- -- closeWindow = run . B.closeWindow

-- -- back :: SessionId -> IO ()
-- -- back = run . B.back

-- -- forward :: SessionId -> IO ()
-- -- forward = run . B.forward

-- -- refresh :: SessionId -> IO ()
-- -- refresh = run . B.refresh

-- -- setWindowRect :: SessionId -> B.WindowRect -> IO B.WindowRect
-- -- setWindowRect s = run . B.setWindowRect s

-- -- minFirefoxSession :: IO SessionId
-- -- minFirefoxSession = newSession B.minFirefoxCapabilities

-- -- deleteSession :: SessionId -> IO ()
-- -- deleteSession = run . B.deleteSession

-- -- navigateTo :: SessionId -> Text -> IO ()
-- -- navigateTo s = run . B.navigateTo s

-- -- findElement :: SessionId -> Selector -> IO ElementId
-- -- findElement s = run . B.findElement s

-- -- findElementFromElement :: SessionId -> ElementId -> Selector -> IO ElementId
-- -- findElementFromElement s eid = run . B.findElementFromElement s eid

-- -- findElementsFromElement :: SessionId -> ElementId -> Selector -> IO [ElementId]
-- -- findElementsFromElement s eid = run . B.findElementsFromElement s eid

-- -- getActiveElement :: SessionId -> IO ElementId
-- -- getActiveElement = run . B.getActiveElement

-- -- isElementSelected :: SessionId -> ElementId -> IO Bool
-- -- isElementSelected s = run . B.isElementSelected s

-- -- getElementShadowRoot :: SessionId -> ElementId -> IO ElementId
-- -- getElementShadowRoot s = run . B.getElementShadowRoot s

-- -- findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> IO ElementId
-- -- findElementFromShadowRoot s e = run . B.findElementFromShadowRoot s e

-- -- getElementTagName :: SessionId -> ElementId -> IO Text
-- -- getElementTagName s = run . B.getElementTagName s

-- -- getElementRect :: SessionId -> ElementId -> IO B.WindowRect
-- -- getElementRect s = run . B.getElementRect s

-- -- isElementEnabled :: SessionId -> ElementId -> IO Bool
-- -- isElementEnabled s = run . B.isElementEnabled s

-- -- getElementComputedRole :: SessionId -> ElementId -> IO Text
-- -- getElementComputedRole s = run . B.getElementComputedRole s

-- -- getElementComputedLabel :: SessionId -> ElementId -> IO Text
-- -- getElementComputedLabel s = run . B.getElementComputedLabel s

-- -- findElements :: SessionId -> Selector -> IO [ElementId]
-- -- findElements s = run . B.findElements s

-- -- findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> IO [ElementId]
-- -- findElementsFromShadowRoot s e = run . B.findElementsFromShadowRoot s e

-- -- elementClick :: SessionId -> ElementId -> IO ()
-- -- elementClick s = run . B.elementClick s

-- -- getElementText :: SessionId -> ElementId -> IO Text
-- -- getElementText s = run . B.getElementText s

-- -- getElementProperty :: SessionId -> ElementId -> Text -> IO Value
-- -- getElementProperty s eid = run . B.getElementProperty s eid

-- -- getElementAttribute :: SessionId -> ElementId -> Text -> IO Text
-- -- getElementAttribute s eid = run . B.getElementAttribute s eid

-- -- getElementCssValue :: SessionId -> ElementId -> Text -> IO Text
-- -- getElementCssValue s eid = run . B.getElementCssValue s eid

-- -- elementClear :: SessionId -> ElementId -> IO ()
-- -- elementClear s = run . B.elementClear s


-- -- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- -- elementSendKeys s eid = run . B.elementSendKeys s eid

-- -- getPageSource :: SessionId -> IO Text
-- -- getPageSource = run . B.getPageSource

-- -- takeScreenshot :: SessionId -> IO Text
-- -- takeScreenshot = run . B.takeScreenshot

-- -- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- -- takeElementScreenshot s = run . B.takeElementScreenshot s

-- -- printPage :: SessionId -> IO Text
-- -- printPage = run . B.printPage



-- -- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- -- executeScript ses script = run . B.executeScript ses script

-- -- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- -- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- -- getAllCookies :: SessionId -> IO [B.Cookie])
-- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- elementSendKeys s eid = run . B.elementSendKeys s eid

-- getPageSource :: SessionId -> IO Text
-- getPageSource = run . B.getPageSource

-- takeScreenshot :: SessionId -> IO Text
-- takeScreenshot = run . B.takeScreenshot

-- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- takeElementScreenshot s = run . B.takeElementScreenshot s

-- printPage :: SessionId -> IO Text
-- printPage = run . B.printPage



-- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- executeScript ses script = run . B.executeScript ses script

-- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- getAllCookies :: SessionId -> IO [B.Cookie]
-- getAllCookies = run . B.getAllCookies

-- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- getNamedCookie s = run . B.getNamedCookie s

-- addCookie :: SessionId -> B.Cookie -> IO ()
-- addCookie s = run . B.addCookie s

-- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- elementSendKeys s eid = run . B.elementSendKeys s eid

-- getPageSource :: SessionId -> IO Text
-- getPageSource = run . B.getPageSource

-- takeScreenshot :: SessionId -> IO Text
-- takeScreenshot = run . B.takeScreenshot

-- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- takeElementScreenshot s = run . B.takeElementScreenshot s

-- printPage :: SessionId -> IO Text
-- printPage = run . B.printPage



-- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- executeScript ses script = run . B.executeScript ses script

-- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- getAllCookies :: SessionId -> IO [B.Cookie]
-- getAllCookies = run . B.getAllCookies

-- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- getNamedCookie s = run . B.getNamedCookie s

-- addCookie :: SessionId -> B.Cookie -> IO ()
-- addCookie s = run . B.addCookie s

-- deleteCookie :: SessionId -> Text -> IO ()
-- deleteCookie s = run . B.deleteCookie s

-- deleteAllCookies :: SessionId -> IO ()

-- dismissAlert = run . B.dismissAlert

-- acceptAlert :: SessionId -> IO ()
-- acceptAlert = run . B.acceptAlert

-- getAlertText :: SessionId -> IO Text
-- getAlertText = run . B.getAlertText

-- sendAlertText :: SessionId -> Text -> IO ()
-- sendAlertText s = run . B.sendAlertText s

-- performActions :: SessionId -> B.Actions -> IO ()
-- performActions s = run . B.performActions s

-- releaseActions :: SessionId -> IO ()
-- releaseActions = run . B.releaseActions


-- deleteCookie :: SessionId -> Text -> IO ()
-- deleteCookie s = run . B.deleteCookie s

-- deleteAllCookies :: SessionId -> IO ()

-- dismissAlert = run . B.dismissAlert

-- acceptAlert :: SessionId -> IO ()
-- acceptAlert = run . B.acceptAlert

-- getAlertText :: SessionId -> IO Text
-- getAlertText = run . B.getAlertText

-- sendAlertText :: SessionId -> Text -> IO ()
-- sendAlertText s = run . B.sendAlertText s

-- performActions :: SessionId -> B.Actions -> IO ()
-- performActions s = run . B.performActions s

-- releaseActions :: SessionId -> IO ()
-- releaseActions = run . B.releaseActions

-- --     performActions,
-- --     releaseActions,
-- --     deleteSession,
-- --     navigateTo,
-- --     findElement,
-- --     elementClick,
-- --     getElementText,
-- --     setWindowRect,
-- --     sleepMs,
-- --     switchToWindow,
-- --     switchToFrame,
-- --     switchToParentFrame,
-- --     getElementProperty,
-- --     getElementCssValue,
-- --     getElementTagName,
-- --     getElementRect,
-- --     isElementEnabled,
-- --     getElementComputedRole,
-- --     getElementComputedLabel,
-- --     elementClear,
-- --     elementSendKeys,
-- --     printPage,
-- --     getPageSource,
-- --     takeScreenshot,
-- --     takeElementScreenshot,
-- --     executeScript,
-- --     executeScriptAsync,
-- --     getAllCookies,
-- --     getNamedCookie,
-- --     addCookie,
-- --     deleteCookie,
-- --     deleteAllCookies,
-- --     dismissAlert,
-- --     acceptAlert,
-- --     getAlertText,
-- --     sendAlertText
-- --   )
-- -- where

-- -- import Data.Aeson (Value)

-- -- import Data.Text  as T (Text)
-- -- import Prelude hiding (log)
-- -- import Http.IOUtils (sleepMs, encodeFileToBase64)
-- -- import Http.HttpRunner (run)

-- -- -- ############# API #############

-- -- status :: IO DriverStatus
-- -- status = run B.status

-- -- newSession :: B.FullCapabilities -> IO SessionId
-- -- newSession = run . B.newSession

-- -- getTimeouts :: SessionId -> IO B.Timeouts
-- -- getTimeouts = run . B.getTimeouts

-- -- setTimeouts :: SessionId -> B.Timeouts -> IO ()
-- -- setTimeouts s = run . B.setTimeouts s

-- -- getCurrentUrl :: SessionId -> IO Text
-- -- getCurrentUrl = run . B.getCurrentUrl

-- -- getTitle :: SessionId -> IO Text
-- -- getTitle = run . B.getTitle

-- -- maximizeWindow :: SessionId -> IO B.WindowRect
-- -- maximizeWindow = run . B.maximizeWindow

-- -- minimizeWindow :: SessionId -> IO B.WindowRect
-- -- minimizeWindow = run . B.minimizeWindow

-- -- fullScreenWindow :: SessionId -> IO B.WindowRect
-- -- fullScreenWindow = run . B.fullscreenWindow

-- -- getWindowHandle :: SessionId -> IO B.WindowHandle
-- -- getWindowHandle = run . B.getWindowHandle

-- -- getWindowRect :: SessionId -> IO B.WindowRect
-- -- getWindowRect = run . B.getWindowRect

-- -- getWindowHandles :: SessionId -> IO [B.WindowHandle]
-- -- getWindowHandles = run . B.getWindowHandles

-- -- newWindow :: SessionId -> IO B.WindowHandleSpec
-- -- newWindow = run . B.newWindow

-- -- switchToWindow :: SessionId -> B.WindowHandle -> IO ()
-- -- switchToWindow s = run . B.switchToWindow s

-- -- switchToFrame :: SessionId -> B.FrameReference -> IO ()
-- -- switchToFrame s = run . B.switchToFrame s

-- -- switchToParentFrame :: SessionId -> IO ()
-- -- switchToParentFrame = run . B.switchToParentFrame

-- -- closeWindow :: SessionId -> IO ()
-- -- closeWindow = run . B.closeWindow

-- -- back :: SessionId -> IO ()
-- -- back = run . B.back

-- -- forward :: SessionId -> IO ()
-- -- forward = run . B.forward

-- -- refresh :: SessionId -> IO ()
-- -- refresh = run . B.refresh

-- -- setWindowRect :: SessionId -> B.WindowRect -> IO B.WindowRect
-- -- setWindowRect s = run . B.setWindowRect s

-- -- minFirefoxSession :: IO SessionId
-- -- minFirefoxSession = newSession B.minFirefoxCapabilities

-- -- deleteSession :: SessionId -> IO ()
-- -- deleteSession = run . B.deleteSession

-- -- navigateTo :: SessionId -> Text -> IO ()
-- -- navigateTo s = run . B.navigateTo s

-- -- findElement :: SessionId -> Selector -> IO ElementId
-- -- findElement s = run . B.findElement s

-- -- findElementFromElement :: SessionId -> ElementId -> Selector -> IO ElementId
-- -- findElementFromElement s eid = run . B.findElementFromElement s eid

-- -- findElementsFromElement :: SessionId -> ElementId -> Selector -> IO [ElementId]
-- -- findElementsFromElement s eid = run . B.findElementsFromElement s eid

-- -- getActiveElement :: SessionId -> IO ElementId
-- -- getActiveElement = run . B.getActiveElement

-- -- isElementSelected :: SessionId -> ElementId -> IO Bool
-- -- isElementSelected s = run . B.isElementSelected s

-- -- getElementShadowRoot :: SessionId -> ElementId -> IO ElementId
-- -- getElementShadowRoot s = run . B.getElementShadowRoot s

-- -- findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> IO ElementId
-- -- findElementFromShadowRoot s e = run . B.findElementFromShadowRoot s e

-- -- getElementTagName :: SessionId -> ElementId -> IO Text
-- -- getElementTagName s = run . B.getElementTagName s

-- -- getElementRect :: SessionId -> ElementId -> IO B.WindowRect
-- -- getElementRect s = run . B.getElementRect s

-- -- isElementEnabled :: SessionId -> ElementId -> IO Bool
-- -- isElementEnabled s = run . B.isElementEnabled s

-- -- getElementComputedRole :: SessionId -> ElementId -> IO Text
-- -- getElementComputedRole s = run . B.getElementComputedRole s

-- -- getElementComputedLabel :: SessionId -> ElementId -> IO Text
-- -- getElementComputedLabel s = run . B.getElementComputedLabel s

-- -- findElements :: SessionId -> Selector -> IO [ElementId]
-- -- findElements s = run . B.findElements s

-- -- findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> IO [ElementId]
-- -- findElementsFromShadowRoot s e = run . B.findElementsFromShadowRoot s e

-- -- elementClick :: SessionId -> ElementId -> IO ()
-- -- elementClick s = run . B.elementClick s

-- -- getElementText :: SessionId -> ElementId -> IO Text
-- -- getElementText s = run . B.getElementText s

-- -- getElementProperty :: SessionId -> ElementId -> Text -> IO Value
-- -- getElementProperty s eid = run . B.getElementProperty s eid

-- -- getElementAttribute :: SessionId -> ElementId -> Text -> IO Text
-- -- getElementAttribute s eid = run . B.getElementAttribute s eid

-- -- getElementCssValue :: SessionId -> ElementId -> Text -> IO Text
-- -- getElementCssValue s eid = run . B.getElementCssValue s eid

-- -- elementClear :: SessionId -> ElementId -> IO ()
-- -- elementClear s = run . B.elementClear s


-- -- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- -- elementSendKeys s eid = run . B.elementSendKeys s eid

-- -- getPageSource :: SessionId -> IO Text
-- -- getPageSource = run . B.getPageSource

-- -- takeScreenshot :: SessionId -> IO Text
-- -- takeScreenshot = run . B.takeScreenshot

-- -- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- -- takeElementScreenshot s = run . B.takeElementScreenshot s

-- -- printPage :: SessionId -> IO Text
-- -- printPage = run . B.printPage



-- -- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- -- executeScript ses script = run . B.executeScript ses script

-- -- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- -- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- -- getAllCookies :: SessionId -> IO [B.Cookie]
-- -- getAllCookies = run . B.getAllCookies

-- -- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- -- getNamedCookie s = run . B.getNamedCookie s

-- -- addCookie :: SessionId -> B.Cookie -> IO ()
-- -- addCookie s = run . B.addCookie s

-- -- deleteCookie :: SessionId -> Text -> IO ()
-- -- deleteCookie s = run . B.deleteCookie s

-- -- deleteAllCookies :: SessionId -> IO ()

-- -- dismissAlert = run . B.dismissAlert

-- -- acceptAlert :: SessionId -> IO ()
-- -- acceptAlert = run . B.acceptAlert

-- -- getAlertText :: SessionId -> IO Text
-- -- getAlertText = run . B.getAlertText

-- -- sendAlertText :: SessionId -> Text -> IO ()
-- -- sendAlertText s = run . B.sendAlertText s

-- -- performActions :: SessionId -> B.Actions -> IO ()
-- -- performActions s = run . B.performActions s

-- -- releaseActions :: SessionId -> IO ()
-- -- releaseActions = run . B.releaseActions
-- -- getAllCookies = run . B.getAllCookies

-- -- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- -- getNamedCookie s = run . B.getNamedCookie s

-- -- addCookie :: SessionId -> B.Cookie -> IO ()
-- -- addCookie s = run . B.addCookie s

-- -- deleteCookie :: SessionId -> Text -> IO ()
-- -- deleteCookie s = run . B.deleteCookie s

-- -- deleteAllCookies :: SessionId -> IO ()

-- -- dismissAlert = run . B.dismissAlert

-- -- acceptAlert :: SessionId -> IO ()
-- -- acceptAlert = run . B.acceptAlert

-- -- getAlertText :: SessionId -> IO Text
-- -- getAlertText = run . B.getAlertText

-- -- sendAlertText :: SessionId -> Text -> IO ()
-- -- sendAlertText s = run . B.sendAlertText s

-- -- performActions :: SessionId -> B.Actions -> IO ()
-- -- performActions s = run . B.performActions s

-- -- releaseActions :: SessionId -> IO ()
-- -- releaseActions = run . B.releaseActions

-- -- elementClear :: SessionId -> ElementId -> IO ()
-- -- elementClear s = run . B.elementClear s


-- -- elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
-- -- elementSendKeys s eid = run . B.elementSendKeys s eid

-- -- getPageSource :: SessionId -> IO Text
-- -- getPageSource = run . B.getPageSource

-- -- takeScreenshot :: SessionId -> IO Text
-- -- takeScreenshot = run . B.takeScreenshot

-- -- takeElementScreenshot :: SessionId -> ElementId -> IO Text
-- -- takeElementScreenshot s = run . B.takeElementScreenshot s

-- -- printPage :: SessionId -> IO Text
-- -- printPage = run . B.printPage



-- -- executeScript :: SessionId -> Text -> [Value] -> IO Value
-- -- executeScript ses script = run . B.executeScript ses script

-- -- executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
-- -- executeScriptAsync ses script = run . B.executeScriptAsync ses script

-- -- getAllCookies :: SessionId -> IO [B.Cookie]
-- -- getAllCookies = run . B.getAllCookies

-- -- getNamedCookie :: SessionId -> Text -> IO B.Cookie
-- -- getNamedCookie s = run . B.getNamedCookie s

-- -- addCookie :: SessionId -> B.Cookie -> IO ()
-- -- addCookie s = run . B.addCookie s

-- -- deleteCookie :: SessionId -> Text -> IO ()
-- -- deleteCookie s = run . B.deleteCookie s

-- -- deleteAllCookies :: SessionId -> IO ()

-- -- dismissAlert = run . B.dismissAlert

-- -- acceptAlert :: SessionId -> IO ()
-- -- acceptAlert = run . B.acceptAlert

-- -- getAlertText :: SessionId -> IO Text
-- -- getAlertText = run . B.getAlertText

-- -- sendAlertText :: SessionId -> Text -> IO ()
-- -- sendAlertText s = run . B.sendAlertText s

-- -- performActions :: SessionId -> B.Actions -> IO ()
-- -- performActions s = run . B.performActions s

-- -- releaseActions :: SessionId -> IO ()
-- -- releaseActions = run . B.releaseActions