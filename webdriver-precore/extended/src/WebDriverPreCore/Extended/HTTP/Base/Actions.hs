{-|
Module: WebDriverPreCore.Extended.HTTP.Base.Actions
Description: HTTP WebDriver action functions using the Runner pattern

This module provides HTTP WebDriver action functions that accept a Runner
parameter to execute commands. These functions wrap the Command-based API
into a more flexible interface suitable for the Extended module system.
-}
module WebDriverPreCore.Extended.HTTP.Base.Actions
  ( -- * Runner Type
    Runner,
    -- * Root Methods
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
    -- * Fallback Methods
    runCommand,
  )
where

import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import WebDriverPreCore.Extended.HTTP.Base.API qualified as API
import WebDriverPreCore.Extended.HTTP.Base.Protocol
  ( Actions,
    Cookie,
    ElementId,
    FrameReference,
    FullCapabilities,
    Handle,
    Script,
    Selector,
    Session,
    SessionResponse,
    ShadowRootElementId,
    Status,
    Timeouts,
    URL,
    WindowHandleSpec,
    WindowRect,
    Command,
  )

-- ######################################################################
-- ########################### Type Aliases #############################
-- ######################################################################

-- | A 'Runner' is a function that executes a 'Command' in a monadic context.
-- This allows the Extended module to work with different execution strategies.
type Runner m a = Command a -> m a



-- ######################################################################
-- ########################### Root Methods #############################
-- ######################################################################

-- | Get the status of the WebDriver server.
--
-- Specification Entry: [HTMLSpecURL#status](https://www.w3.org/TR/webdriver/#status)
--
-- @GET \/status Status@
status :: forall m. Runner m Status -> m Status
status r = r API.status

-- | Create a new WebDriver session with the given capabilities.
--
-- Specification Entry: [HTMLSpecURL#new-session](https://www.w3.org/TR/webdriver/#new-session)
--
-- @POST \/session New Session@
newSession :: forall m. Runner m SessionResponse -> FullCapabilities -> m SessionResponse
newSession r = r . API.newSession

-- ######################################################################
-- ########################### Session Methods ##########################
-- ######################################################################

-- | Delete an existing session.
--
-- Specification Entry: [HTMLSpecURL#delete-session](https://www.w3.org/TR/webdriver/#delete-session)
--
-- @DELETE \/session\/{session id} Delete Session@
deleteSession :: forall m. Runner m () -> Session -> m ()
deleteSession r = r . API.deleteSession

-- | Get the session timeouts.
--
-- Specification Entry: [HTMLSpecURL#get-timeouts](https://www.w3.org/TR/webdriver/#get-timeouts)
--
-- @GET \/session\/{session id}\/timeouts Get Timeouts@
getTimeouts :: forall m. Runner m Timeouts -> Session -> m Timeouts
getTimeouts r = r . API.getTimeouts

-- | Set the session timeouts.
--
-- Specification Entry: [HTMLSpecURL#set-timeouts](https://www.w3.org/TR/webdriver/#set-timeouts)
--
-- @POST \/session\/{session id}\/timeouts Set Timeouts@
setTimeouts :: forall m. Runner m () -> Session -> Timeouts -> m ()
setTimeouts r sess = r . API.setTimeouts sess 

-- | Navigate to a URL.
--
-- Specification Entry: [HTMLSpecURL#navigate-to](https://www.w3.org/TR/webdriver/#navigate-to)
--
-- @POST \/session\/{session id}\/url Navigate To@
navigateTo :: forall m. Runner m () -> Session -> URL -> m ()
navigateTo r sess = r . API.navigateTo sess

-- | Get the current URL.
--
-- Specification Entry: [HTMLSpecURL#get-current-url](https://www.w3.org/TR/webdriver/#get-current-url)
--
-- @GET \/session\/{session id}\/url Get Current URL@
getCurrentUrl :: forall m. Runner m URL -> Session -> m URL
getCurrentUrl r = r . API.getCurrentUrl

-- | Navigate back in the browser history.
--
-- Specification Entry: [HTMLSpecURL#back](https://www.w3.org/TR/webdriver/#back)
--
-- @POST \/session\/{session id}\/back Back@
back :: forall m. Runner m () -> Session -> m ()
back r = r . API.back

-- | Navigate forward in the browser history.
--
-- Specification Entry: [HTMLSpecURL#forward](https://www.w3.org/TR/webdriver/#forward)
--
-- @POST \/session\/{session id}\/forward Forward@
forward :: forall m. Runner m () -> Session -> m ()
forward r = r . API.forward

-- | Refresh the current page.
--
-- Specification Entry: [HTMLSpecURL#refresh](https://www.w3.org/TR/webdriver/#refresh)
--
-- @POST \/session\/{session id}\/refresh Refresh@
refresh :: forall m. Runner m () -> Session -> m ()
refresh r = r . API.refresh

-- | Get the title of the current page.
--
-- Specification Entry: [HTMLSpecURL#get-title](https://www.w3.org/TR/webdriver/#get-title)
--
-- @GET \/session\/{session id}\/title Get Title@
getTitle :: forall m. Runner m Text -> Session -> m Text
getTitle r = r . API.getTitle

-- | Get the current window handle.
--
-- Specification Entry: [HTMLSpecURL#get-window-handle](https://www.w3.org/TR/webdriver/#get-window-handle)
--
-- @GET \/session\/{session id}\/window Get Window Handle@
getWindowHandle :: forall m. Runner m Handle -> Session -> m Handle
getWindowHandle r = r . API.getWindowHandle

-- | Create a new window or tab.
--
-- Specification Entry: [HTMLSpecURL#new-window](https://www.w3.org/TR/webdriver/#new-window)
--
-- @POST \/session\/{session id}\/window\/new New Window@
newWindow :: forall m. Runner m WindowHandleSpec -> Session -> m WindowHandleSpec
newWindow r = r . API.newWindow

-- | Close the current window.
--
-- Specification Entry: [HTMLSpecURL#close-window](https://www.w3.org/TR/webdriver/#close-window)
--
-- @DELETE \/session\/{session id}\/window Close Window@
closeWindow :: forall m. Runner m [Handle] -> Session -> m [Handle]
closeWindow r = r . API.closeWindow

-- | Switch to a different window.
--
-- Specification Entry: [HTMLSpecURL#switch-to-window](https://www.w3.org/TR/webdriver/#switch-to-window)
--
-- @POST \/session\/{session id}\/window Switch To Window@
switchToWindow :: forall m. Runner m () -> Session -> Handle -> m ()
switchToWindow r sess = r . API.switchToWindow sess

-- | Switch to a different frame.
--
-- Specification Entry: [HTMLSpecURL#switch-to-frame](https://www.w3.org/TR/webdriver/#switch-to-frame)
--
-- @POST \/session\/{session id}\/frame Switch To Frame@
switchToFrame :: forall m. Runner m () -> Session -> FrameReference -> m ()
switchToFrame r sess = r . API.switchToFrame sess

-- | Get the source of the current page.
--
-- Specification Entry: [HTMLSpecURL#get-page-source](https://www.w3.org/TR/webdriver/#get-page-source)
--
-- @GET \/session\/{session id}\/source Get Page Source@
getPageSource :: forall m. Runner m Text -> Session -> m Text
getPageSource r = r . API.getPageSource

-- | Execute a script synchronously.
--
-- Specification Entry: [HTMLSpecURL#execute-script](https://www.w3.org/TR/webdriver/#execute-script)
--
-- @POST \/session\/{session id}\/execute\/sync Execute Script@
executeScript :: forall m. Runner m Value -> Session -> Script -> m Value
executeScript r sess = r . API.executeScript sess

-- | Execute a script asynchronously.
--
-- Specification Entry: [HTMLSpecURL#execute-async-script](https://www.w3.org/TR/webdriver/#execute-async-script)
--
-- @POST \/session\/{session id}\/execute\/async Execute Async Script@
executeScriptAsync :: forall m. Runner m Value -> Session -> Script -> m Value
executeScriptAsync r sess = r . API.executeScriptAsync sess

-- | Add a cookie.
--
-- Specification Entry: [HTMLSpecURL#add-cookie](https://www.w3.org/TR/webdriver/#add-cookie)
--
-- @POST \/session\/{session id}\/cookie Add Cookie@
addCookie :: forall m. Runner m () -> Session -> Cookie -> m ()
addCookie r sess = r . API.addCookie sess

-- | Get all cookies.
--
-- Specification Entry: [HTMLSpecURL#get-all-cookies](https://www.w3.org/TR/webdriver/#get-all-cookies)
--
-- @GET \/session\/{session id}\/cookie Get All Cookies@
getAllCookies :: forall m. Runner m [Cookie] -> Session -> m [Cookie]
getAllCookies r = r . API.getAllCookies

-- | Get a named cookie.
--
-- Specification Entry: [HTMLSpecURL#get-named-cookie](https://www.w3.org/TR/webdriver/#get-named-cookie)
--
-- @GET \/session\/{session id}\/cookie\/{name} Get Named Cookie@
getNamedCookie :: forall m. Runner m Cookie -> Session -> Text -> m Cookie
getNamedCookie r sess = r . API.getNamedCookie sess

-- | Delete a cookie.
--
-- Specification Entry: [HTMLSpecURL#delete-cookie](https://www.w3.org/TR/webdriver/#delete-cookie)
--
-- @DELETE \/session\/{session id}\/cookie\/{name} Delete Cookie@
deleteCookie :: forall m. Runner m () -> Session -> Text -> m ()
deleteCookie r sess = r . API.deleteCookie sess

-- | Delete all cookies.
--
-- Specification Entry: [HTMLSpecURL#delete-all-cookies](https://www.w3.org/TR/webdriver/#delete-all-cookies)
--
-- @DELETE \/session\/{session id}\/cookie Delete All Cookies@
deleteAllCookies :: forall m. Runner m () -> Session -> m ()
deleteAllCookies r = r . API.deleteAllCookies

-- | Perform a sequence of actions.
--
-- Specification Entry: [HTMLSpecURL#perform-actions](https://www.w3.org/TR/webdriver/#perform-actions)
--
-- @POST \/session\/{session id}\/actions Perform Actions@
performActions :: forall m. Runner m () -> Session -> Actions -> m ()
performActions r sess = r . API.performActions sess

-- | Release all action state.
--
-- Specification Entry: [HTMLSpecURL#release-actions](https://www.w3.org/TR/webdriver/#release-actions)
--
-- @DELETE \/session\/{session id}\/actions Release Actions@
releaseActions :: forall m. Runner m () -> Session -> m ()
releaseActions r = r . API.releaseActions

-- | Dismiss an alert.
--
-- Specification Entry: [HTMLSpecURL#dismiss-alert](https://www.w3.org/TR/webdriver/#dismiss-alert)
--
-- @POST \/session\/{session id}\/alert\/dismiss Dismiss Alert@
dismissAlert :: forall m. Runner m () -> Session -> m ()
dismissAlert r = r . API.dismissAlert

-- | Accept an alert.
--
-- Specification Entry: [HTMLSpecURL#accept-alert](https://www.w3.org/TR/webdriver/#accept-alert)
--
-- @POST \/session\/{session id}\/alert\/accept Accept Alert@
acceptAlert :: forall m. Runner m () -> Session -> m ()
acceptAlert r = r . API.acceptAlert

-- | Get the text of an alert.
--
-- Specification Entry: [HTMLSpecURL#get-alert-text](https://www.w3.org/TR/webdriver/#get-alert-text)
--
-- @GET \/session\/{session id}\/alert\/text Get Alert Text@
getAlertText :: forall m. Runner m Text -> Session -> m Text
getAlertText r = r . API.getAlertText

-- | Send text to an alert.
--
-- Specification Entry: [HTMLSpecURL#send-alert-text](https://www.w3.org/TR/webdriver/#send-alert-text)
--
-- @POST \/session\/{session id}\/alert\/text Send Alert Text@
sendAlertText :: forall m. Runner m () -> Session -> Text -> m ()
sendAlertText r sess = r . API.sendAlertText sess

-- | Take a screenshot of the current page.
--
-- Specification Entry: [HTMLSpecURL#take-screenshot](https://www.w3.org/TR/webdriver/#take-screenshot)
--
-- @GET \/session\/{session id}\/screenshot Take Screenshot@
takeScreenshot :: forall m. Runner m Text -> Session -> m Text
takeScreenshot r = r . API.takeScreenshot

-- | Print the current page to PDF.
--
-- Specification Entry: [HTMLSpecURL#print-page](https://www.w3.org/TR/webdriver/#print-page)
--
-- @POST \/session\/{session id}\/print Print Page@
printPage :: forall m. Runner m Text -> Session -> m Text
printPage r = r . API.printPage

-- ######################################################################
-- ########################### Window Methods ###########################
-- ######################################################################

-- | Get all window handles.
--
-- Specification Entry: [HTMLSpecURL#get-window-handles](https://www.w3.org/TR/webdriver/#get-window-handles)
--
-- @GET \/session\/{session id}\/window\/handles Get Window Handles@
getWindowHandles :: forall m. Runner m [Handle] -> Session -> m [Handle]
getWindowHandles r = r . API.getWindowHandles

-- | Get the window rectangle.
--
-- Specification Entry: [HTMLSpecURL#get-window-rect](https://www.w3.org/TR/webdriver/#get-window-rect)
--
-- @GET \/session\/{session id}\/window\/rect Get Window Rect@
getWindowRect :: forall m. Runner m WindowRect -> Session -> m WindowRect
getWindowRect r = r . API.getWindowRect

-- | Set the window rectangle.
--
-- Specification Entry: [HTMLSpecURL#set-window-rect](https://www.w3.org/TR/webdriver/#set-window-rect)
--
-- @POST \/session\/{session id}\/window\/rect Set Window Rect@
setWindowRect :: forall m. Runner m WindowRect -> Session -> WindowRect -> m WindowRect
setWindowRect r sess = r . API.setWindowRect sess

-- | Maximize the window.
--
-- Specification Entry: [HTMLSpecURL#maximize-window](https://www.w3.org/TR/webdriver/#maximize-window)
--
-- @POST \/session\/{session id}\/window\/maximize Maximize Window@
maximizeWindow :: forall m. Runner m WindowRect -> Session -> m WindowRect
maximizeWindow r = r . API.maximizeWindow

-- | Minimize the window.
--
-- Specification Entry: [HTMLSpecURL#minimize-window](https://www.w3.org/TR/webdriver/#minimize-window)
--
-- @POST \/session\/{session id}\/window\/minimize Minimize Window@
minimizeWindow :: forall m. Runner m WindowRect -> Session -> m WindowRect
minimizeWindow r = r . API.minimizeWindow

-- | Make the window fullscreen.
--
-- Specification Entry: [HTMLSpecURL#fullscreen-window](https://www.w3.org/TR/webdriver/#fullscreen-window)
--
-- @POST \/session\/{session id}\/window\/fullscreen Fullscreen Window@
fullScreenWindow :: forall m. Runner m WindowRect -> Session -> m WindowRect
fullScreenWindow r = r . API.fullScreenWindow

-- ######################################################################
-- ########################### Frame Methods ############################
-- ######################################################################

-- | Switch to the parent frame.
--
-- Specification Entry: [HTMLSpecURL#switch-to-parent-frame](https://www.w3.org/TR/webdriver/#switch-to-parent-frame)
--
-- @POST \/session\/{session id}\/frame\/parent Switch To Parent Frame@
switchToParentFrame :: forall m. Runner m () -> Session -> m ()
switchToParentFrame r = r . API.switchToParentFrame

-- ######################################################################
-- ########################## Element(s) Methods ########################
-- ######################################################################

-- | Get the active element.
--
-- Specification Entry: [HTMLSpecURL#get-active-element](https://www.w3.org/TR/webdriver/#get-active-element)
--
-- @GET \/session\/{session id}\/element\/active Get Active Element@
getActiveElement :: forall m. Runner m ElementId -> Session -> m ElementId
getActiveElement r = r . API.getActiveElement

-- | Find an element using a selector.
--
-- Specification Entry: [HTMLSpecURL#find-element](https://www.w3.org/TR/webdriver/#find-element)
--
-- @POST \/session\/{session id}\/element Find Element@
findElement :: forall m. Runner m ElementId -> Session -> Selector -> m ElementId
findElement r sess = r . API.findElement sess

-- | Find elements using a selector.
--
-- Specification Entry: [HTMLSpecURL#find-elements](https://www.w3.org/TR/webdriver/#find-elements)
--
-- @POST \/session\/{session id}\/elements Find Elements@
findElements :: forall m. Runner m [ElementId] -> Session -> Selector -> m [ElementId]
findElements r sess = r . API.findElements sess

-- ######################################################################
-- ##################### Element Instance Methods #######################
-- ######################################################################

-- | Find an element from another element.
--
-- Specification Entry: [HTMLSpecURL#find-element-from-element](https://www.w3.org/TR/webdriver/#find-element-from-element)
--
-- @POST \/session\/{session id}\/element\/{element id}\/element Find Element From Element@
findElementFromElement :: forall m. Runner m ElementId -> Session -> ElementId -> Selector -> m ElementId
findElementFromElement r sess elemId = r . API.findElementFromElement sess elemId

-- | Find elements from another element.
--
-- Specification Entry: [HTMLSpecURL#find-elements-from-element](https://www.w3.org/TR/webdriver/#find-elements-from-element)
--
-- @POST \/session\/{session id}\/element\/{element id}\/elements Find Elements From Element@
findElementsFromElement :: forall m. Runner m [ElementId] -> Session -> ElementId -> Selector -> m [ElementId]
findElementsFromElement r sess elemId = r . API.findElementsFromElement sess elemId

-- | Check if an element is selected.
--
-- Specification Entry: [HTMLSpecURL#is-element-selected](https://www.w3.org/TR/webdriver/#is-element-selected)
--
-- @GET \/session\/{session id}\/element\/{element id}\/selected Is Element Selected@
isElementSelected :: forall m. Runner m Bool -> Session -> ElementId -> m Bool
isElementSelected r sess = r . API.isElementSelected sess

-- | Get an element's attribute.
--
-- Specification Entry: [HTMLSpecURL#get-element-attribute](https://www.w3.org/TR/webdriver/#get-element-attribute)
--
-- @GET \/session\/{session id}\/element\/{element id}\/attribute\/{name} Get Element Attribute@
getElementAttribute :: forall m. Runner m Text -> Session -> ElementId -> Text -> m Text
getElementAttribute r sess elemId = r . API.getElementAttribute sess elemId

-- | Get an element's property.
--
-- Specification Entry: [HTMLSpecURL#get-element-property](https://www.w3.org/TR/webdriver/#get-element-property)
--
-- @GET \/session\/{session id}\/element\/{element id}\/property\/{name} Get Element Property@
getElementProperty :: forall m. Runner m Value -> Session -> ElementId -> Text -> m Value
getElementProperty r sess elemId = r . API.getElementProperty sess elemId

-- | Get an element's CSS value.
--
-- Specification Entry: [HTMLSpecURL#get-element-css-value](https://www.w3.org/TR/webdriver/#get-element-css-value)
--
-- @GET \/session\/{session id}\/element\/{element id}\/css\/{property name} Get Element CSS Value@
getElementCssValue :: forall m. Runner m Text -> Session -> ElementId -> Text -> m Text
getElementCssValue r sess elemId = r . API.getElementCssValue sess elemId

-- | Get an element's shadow root.
--
-- Specification Entry: [HTMLSpecURL#get-element-shadow-root](https://www.w3.org/TR/webdriver/#get-element-shadow-root)
--
-- @GET \/session\/{session id}\/element\/{element id}\/shadow Get Element Shadow Root@
getElementShadowRoot :: forall m. Runner m ShadowRootElementId -> Session -> ElementId -> m ShadowRootElementId
getElementShadowRoot r sess = r . API.getElementShadowRoot sess

-- | Get an element's text.
--
-- Specification Entry: [HTMLSpecURL#get-element-text](https://www.w3.org/TR/webdriver/#get-element-text)
--
-- @GET \/session\/{session id}\/element\/{element id}\/text Get Element Text@
getElementText :: forall m. Runner m Text -> Session -> ElementId -> m Text
getElementText r sess = r . API.getElementText sess

-- | Get an element's tag name.
--
-- Specification Entry: [HTMLSpecURL#get-element-tag-name](https://www.w3.org/TR/webdriver/#get-element-tag-name)
--
-- @GET \/session\/{session id}\/element\/{element id}\/name Get Element Tag Name@
getElementTagName :: forall m. Runner m Text -> Session -> ElementId -> m Text
getElementTagName r sess = r . API.getElementTagName sess

-- | Get an element's rectangle.
--
-- Specification Entry: [HTMLSpecURL#get-element-rect](https://www.w3.org/TR/webdriver/#get-element-rect)
--
-- @GET \/session\/{session id}\/element\/{element id}\/rect Get Element Rect@
getElementRect :: forall m. Runner m WindowRect -> Session -> ElementId -> m WindowRect
getElementRect r sess = r . API.getElementRect sess

-- | Check if an element is enabled.
--
-- Specification Entry: [HTMLSpecURL#is-element-enabled](https://www.w3.org/TR/webdriver/#is-element-enabled)
--
-- @GET \/session\/{session id}\/element\/{element id}\/enabled Is Element Enabled@
isElementEnabled :: forall m. Runner m Bool -> Session -> ElementId -> m Bool
isElementEnabled r sess = r . API.isElementEnabled sess

-- | Get an element's computed role.
--
-- Specification Entry: [HTMLSpecURL#get-computed-role](https://www.w3.org/TR/webdriver/#get-computed-role)
--
-- @GET \/session\/{session id}\/element\/{element id}\/computedrole Get Element Computed Role@
getElementComputedRole :: forall m. Runner m Text -> Session -> ElementId -> m Text
getElementComputedRole r sess = r . API.getElementComputedRole sess

-- | Get an element's computed label.
--
-- Specification Entry: [HTMLSpecURL#get-computed-label](https://www.w3.org/TR/webdriver/#get-computed-label)
--
-- @GET \/session\/{session id}\/element\/{element id}\/computedlabel Get Element Computed Label@
getElementComputedLabel :: forall m. Runner m Text -> Session -> ElementId -> m Text
getElementComputedLabel r sess = r . API.getElementComputedLabel sess

-- | Click an element.
--
-- Specification Entry: [HTMLSpecURL#element-click](https://www.w3.org/TR/webdriver/#element-click)
--
-- @POST \/session\/{session id}\/element\/{element id}\/click Element Click@
elementClick :: forall m. Runner m () -> Session -> ElementId -> m ()
elementClick r sess = r . API.elementClick sess

-- | Clear an element.
--
-- Specification Entry: [HTMLSpecURL#element-clear](https://www.w3.org/TR/webdriver/#element-clear)
--
-- @POST \/session\/{session id}\/element\/{element id}\/clear Element Clear@
elementClear :: forall m. Runner m () -> Session -> ElementId -> m ()
elementClear r sess = r . API.elementClear sess

-- | Send keys to an element.
--
-- Specification Entry: [HTMLSpecURL#element-send-keys](https://www.w3.org/TR/webdriver/#element-send-keys)
--
-- @POST \/session\/{session id}\/element\/{element id}\/value Element Send Keys@
elementSendKeys :: forall m. Runner m () -> Session -> ElementId -> Text -> m ()
elementSendKeys r sess elemId = r . API.elementSendKeys sess elemId

-- | Take a screenshot of an element.
--
-- Specification Entry: [HTMLSpecURL#take-element-screenshot](https://www.w3.org/TR/webdriver/#take-element-screenshot)
--
-- @GET \/session\/{session id}\/element\/{element id}\/screenshot Take Element Screenshot@
takeElementScreenshot :: forall m. Runner m Text -> Session -> ElementId -> m Text
takeElementScreenshot r sess = r . API.takeElementScreenshot sess

-- ######################################################################
-- ######################### Shadow DOM Methods #########################
-- ######################################################################

-- | Find an element from a shadow root.
--
-- Specification Entry: [HTMLSpecURL#find-element-from-shadow-root](https://www.w3.org/TR/webdriver/#find-element-from-shadow-root)
--
-- @POST \/session\/{session id}\/shadow\/{shadow id}\/element Find Element From Shadow Root@
findElementFromShadowRoot :: forall m. Runner m ElementId -> Session -> ShadowRootElementId -> Selector -> m ElementId
findElementFromShadowRoot r sess shadowId = r . API.findElementFromShadowRoot sess shadowId

-- | Find elements from a shadow root.
--
-- Specification Entry: [HTMLSpecURL#find-elements-from-shadow-root](https://www.w3.org/TR/webdriver/#find-elements-from-shadow-root)
--
-- @POST \/session\/{session id}\/shadow\/{shadow id}\/elements Find Elements From Shadow Root@
findElementsFromShadowRoot :: forall m. Runner m [ElementId] -> Session -> ShadowRootElementId -> Selector -> m [ElementId]
findElementsFromShadowRoot r sess shadowId = r . API.findElementsFromShadowRoot sess shadowId

-- ######################################################################
-- ########################## Fallback Methods ##########################
-- ######################################################################

-- | Run a custom command directly.
--
-- This is a fallback method for when the standard API doesn't provide
-- the functionality you need. Use with caution as it bypasses the
-- type-safe command builders.
runCommand :: forall m a. Runner m a -> Command a -> m a
runCommand r cmd = r cmd
