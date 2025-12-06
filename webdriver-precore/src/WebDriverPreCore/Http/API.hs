{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Http.API
  ( -- * Root Methods
    newSession,
    status,

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
  )
where

import Data.Aeson as A
  ( KeyValue ((.=)),
    Value (..),
  )
import Data.Text (Text)
import WebDriverPreCore.Http.Protocol
  ( Actions (..),
    Cookie (..),
    Status (..),
    ElementId (..),
    FrameReference (..),
    FullCapabilities,
    Script,
    Selector (..),
    SessionId (..),
    SessionResponse (..),
    Timeouts,
    URL,
    Handle (..),
    WindowHandleSpec (..),
    WindowRect (..), 
    ShadowRootElementId(..),
    Command(..),
    mkPost,
    mkPost'
  )
import WebDriverPreCore.Internal.Utils (UrlPath (..))
import Prelude hiding (id, lookup)
import Data.Aeson.KeyMap (fromList)

-- ######################################################################
-- ########################### WebDriver API ############################
-- ######################################################################

-- ** Root Methods

-- |
--  Return a spec to create a new session given 'FullCapabilities' object.
--
-- 'newSession'' can be used if 'FullCapabilities' doesn't meet your requirements.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#new-session)
--
--  @POST 	\/session 	New Session@
newSession :: FullCapabilities -> Command SessionResponse
newSession = mkPost "New Session" newSessionUrl

-- |
--
-- Return a spec to get the status of the driver.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#status)
--
-- @GET 	\/status 	Status@
status :: Command Status
status = Get "Status" $ MkUrlPath ["status"]

-- ############################ Session Methods ##########################################

-- |
--
-- Return a spec to delete a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#delete-session)
--
-- @DELETE 	\/session\/{session id} 	Delete Session@
deleteSession :: SessionId -> Command ()
deleteSession sessionRef = Delete "Delete Session" $ sessionUri sessionRef.id

-- |
--
-- Return a spec to get the timeouts of a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-timeouts)
--
-- @GET 	\/session\/{session id}\/timeouts 	Get Timeouts@
getTimeouts :: SessionId -> Command Timeouts
getTimeouts sessionRef = Get "Get Timeouts" $ sessionUri1 sessionRef "timeouts"

-- |
--
-- Return a spec to set the timeouts of a session given a 'SessionId' and 'Timeouts'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#set-timeouts)
--
-- @POST 	\/session\/{session id}\/timeouts 	Set Timeouts@
setTimeouts :: SessionId -> Timeouts -> Command ()
setTimeouts sessionRef =
  mkPost "Set Timeouts" (sessionUri1 sessionRef "timeouts")

-- |
--
-- Return a spec to navigate to a URL given a 'SessionId' and a 'Text' URL.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#navigate-to)
--
-- @POST 	\/session\/{session id}\/url 	Navigate To@
navigateTo :: SessionId -> URL -> Command ()
navigateTo sessionRef = mkPost' "Navigate To" (sessionUri1 sessionRef "url") (\url -> fromList ["url" .= url])

-- |
--
-- Return a spec to get the current URL of a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-current-url)
--
-- @GET 	\/session\/{session id}\/url 	Get Current URL@
getCurrentUrl :: SessionId -> Command URL
getCurrentUrl sessionRef = Get "Get Current URL" (sessionUri1 sessionRef "url")

-- |
--
-- Return a spec to navigate back in the browser history given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#back)
--
-- @POST 	\/session\/{session id}\/back 	Back@
back :: SessionId -> Command ()
back sessionRef = PostEmpty "Back" (sessionUri1 sessionRef "back")

-- |
--
-- Return a spec to navigate forward in the browser history given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#forward)
--
-- @POST 	\/session\/{session id}\/forward 	Forward@
forward :: SessionId -> Command ()
forward sessionRef = PostEmpty "Forward" (sessionUri1 sessionRef "forward")

-- |
--
-- Return a spec to refresh the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#refresh)
--
-- @POST 	\/session\/{session id}\/refresh 	Refresh@
refresh :: SessionId -> Command ()
refresh sessionRef = PostEmpty "Refresh" (sessionUri1 sessionRef "refresh")

-- |
--
-- Return a spec to get the title of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-title)
--
-- @GET 	\/session\/{session id}\/title 	Get Title@
getTitle :: SessionId -> Command Text
getTitle sessionRef = Get "Get Title" (sessionUri1 sessionRef "title")

-- |
--
-- Return a spec to get the current window handle given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-window-handle)
--
-- @GET 	\/session\/{session id}\/window 	Get Window Handle@
getWindowHandle :: SessionId -> Command Handle
getWindowHandle sessionRef = Get "Get Window Handle" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to create a new window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#new-window)
--
-- @POST 	\/session\/{session id}\/window\/new 	New Window@
newWindow :: SessionId -> Command WindowHandleSpec
newWindow sessionRef = PostEmpty "New Window" (sessionUri2 sessionRef "window" "new")

-- |
--
-- Return a spec to close the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#close-window)
--
-- @DELETE 	\/session\/{session id}\/window 	Close Window@
closeWindow :: SessionId -> Command [Handle]
closeWindow sessionRef = Delete "Close Window" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to switch to a different window given a 'SessionId' and 'WindowHandle'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#switch-to-window)
--
-- @POST 	\/session\/{session id}\/window 	Switch To Window@
switchToWindow :: SessionId -> Handle -> Command ()
switchToWindow sessionRef = mkPost "Switch To Window" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to switch to a different frame given a 'SessionId' and 'FrameReference'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#switch-to-frame)
--
-- @POST 	\/session\/{session id}\/frame 	Switch To Frame@
switchToFrame :: SessionId -> FrameReference -> Command ()
switchToFrame sessionRef = mkPost "Switch To Frame" (sessionUri1 sessionRef "frame")

-- |
--
-- Return a spec to get the source of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-page-source)
--
-- @GET 	\/session\/{session id}\/source 	Get Page Source@
getPageSource :: SessionId -> Command Text
getPageSource sessionId = Get "Get Page Source" (sessionUri1 sessionId "source")

-- |
--
-- Return a spec to execute a script in the context of the current page given a 'SessionId', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#execute-script)
--
-- @POST 	\/session\/{session id}\/execute\/sync 	Execute Script@
executeScript :: SessionId -> Script -> Command Value
executeScript sessionId = mkPost "Execute Script" (sessionUri2 sessionId "execute" "sync")

-- |
--
-- Return a spec to execute an asynchronous script in the context of the current page given a 'SessionId', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#execute-async-script)
--
-- @POST 	\/session\/{session id}\/execute\/async 	Execute Async Script@
executeScriptAsync :: SessionId -> Script -> Command Value
executeScriptAsync sessionId = mkPost "Execute Async Script" (sessionUri2 sessionId "execute" "async")

-- |
--
-- Return a spec to get all cookies of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-all-cookies)
--
-- @GET 	\/session\/{session id}\/cookie 	Get All Cookies@
getAllCookies :: SessionId -> Command [Cookie]
getAllCookies sessionId = Get "Get All Cookies" (sessionUri1 sessionId "cookie")

-- |
--
-- Return a spec to get a named cookie of the current page given a 'SessionId' and cookie name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-named-cookie)
--
-- @GET 	\/session\/{session id}\/cookie\/{name} 	Get Named Cookie@
getNamedCookie :: SessionId -> Text -> Command Cookie
getNamedCookie sessionId cookieName = Get "Get Named Cookie" (sessionUri2 sessionId "cookie" cookieName)

-- |
--
-- Return a spec to add a cookie to the current page given a 'SessionId' and 'Cookie'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#add-cookie)
--
-- @POST 	\/session\/{session id}\/cookie 	Add Cookie@
addCookie :: SessionId -> Cookie -> Command ()
addCookie sessionId cookie = Post "Add Cookie" (sessionUri1 sessionId "cookie") (fromList ["cookie" .= cookie] )

-- |
--
-- Return a spec to delete a named cookie from the current page given a 'SessionId' and cookie name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#delete-cookie)
--
-- @DELETE 	\/session\/{session id}\/cookie\/{name} 	Delete Cookie@
deleteCookie :: SessionId -> Text -> Command ()
deleteCookie sessionId cookieName = Delete "Delete Cookie" (sessionUri2 sessionId "cookie" cookieName)

-- |
--
-- Return a spec to delete all cookies from the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#delete-all-cookies)
--
-- @DELETE 	\/session\/{session id}\/cookie 	Delete All Cookies@
deleteAllCookies :: SessionId -> Command ()
deleteAllCookies sessionId = Delete "Delete All Cookies" (sessionUri1 sessionId "cookie")

-- |
--
-- Return a spec to perform actions on the current page given a 'SessionId' and 'Actions'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#perform-actions)
--
-- @POST 	\/session\/{session id}\/actions 	Perform Actions@
performActions :: SessionId -> Actions -> Command ()
performActions sessionId = mkPost "Perform Actions" (sessionUri1 sessionId "actions")

-- |
--
-- Return a spec to release actions on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#release-actions)
--
-- @DELETE 	\/session\/{session id}\/actions 	Release Actions@
releaseActions :: SessionId -> Command ()
releaseActions sessionId = Delete "Release Actions" (sessionUri1 sessionId "actions")

-- |
--
-- Return a spec to dismiss an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dismiss-alert)
--
-- @POST 	\/session\/{session id}\/alert\/dismiss 	Dismiss Alert@
dismissAlert :: SessionId -> Command ()
dismissAlert sessionId = PostEmpty "Dismiss Alert" (sessionUri2 sessionId "alert" "dismiss")

-- |
--
-- Return a spec to accept an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#accept-alert)
--
-- @POST 	\/session\/{session id}\/alert\/accept 	Accept Alert@
acceptAlert :: SessionId -> Command ()
acceptAlert sessionId = PostEmpty "Accept Alert" (sessionUri2 sessionId "alert" "accept")

-- |
--
-- Return a spec to get the text of an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-alert-text)
--
-- @GET 	\/session\/{session id}\/alert\/text 	Get Alert Text@
getAlertText :: SessionId -> Command Text
getAlertText sessionId = Get "Get Alert Text" (sessionUri2 sessionId "alert" "text")

-- |
--
-- Return a spec to send text to an alert on the current page given a 'SessionId' and 'Text'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#send-alert-text)
--
-- @POST 	\/session\/{session id}\/alert\/text 	Send Alert Text@
sendAlertText :: SessionId -> Text -> Command ()
sendAlertText sessionId text = Post "Send Alert Text" (sessionUri2 sessionId "alert" "text") (fromList ["text" .= text])

-- |
--
-- Return a spec to take a screenshot of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#take-screenshot)
--
-- @GET 	\/session\/{session id}\/screenshot 	Take Screenshot@
takeScreenshot :: SessionId -> Command Text
takeScreenshot sessionId = Get "Take Screenshot" (sessionUri1 sessionId "screenshot")

-- |
--
-- Return a spec to print the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#print-page)
--
-- @POST 	\/session\/{session id}\/print 	Print Page@
printPage :: SessionId -> Command Text
printPage sessionId = PostEmpty "Print Page" (sessionUri1 sessionId "print")

-- ############################ Window Methods ##########################################

-- |
--
-- Return a spec to get all window handles of the current session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-window-handles)
--
-- @GET 	\/session\/{session id}\/window\/handles 	Get Window Handles@
getWindowHandles :: SessionId -> Command [Handle]
getWindowHandles sessionRef = Get "Get Window Handles" (sessionUri2 sessionRef "window" "handles")

-- |
--
-- Return a spec to get the window rect of the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-window-rect)
--
-- @GET 	\/session\/{session id}\/window\/rect 	Get Window Rect@
getWindowRect :: SessionId -> Command WindowRect
getWindowRect sessionRef = Get "Get Window Rect" (sessionUri2 sessionRef "window" "rect")

-- |
--
-- Return a spec to set the window rect of the current window given a 'SessionId' and 'WindowRect'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#set-window-rect)
--
-- @POST 	\/session\/{session id}\/window\/rect 	Set Window Rect@
setWindowRect :: SessionId -> WindowRect -> Command WindowRect
setWindowRect sessionRef = mkPost "Set Window Rect" (sessionUri2 sessionRef "window" "rect")

-- |
--
-- Return a spec to maximize the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#maximize-window)
--
-- @POST 	\/session\/{session id}\/window\/maximize 	Maximize Window@
maximizeWindow :: SessionId -> Command WindowRect
maximizeWindow sessionRef = PostEmpty "Maximize Window" (windowUri1 sessionRef "maximize")

-- |
--
-- Return a spec to minimize the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#minimize-window)
--
-- @POST 	\/session\/{session id}\/window\/minimize 	Minimize Window@
minimizeWindow :: SessionId -> Command WindowRect
minimizeWindow sessionRef = PostEmpty "Minimize Window" (windowUri1 sessionRef "minimize")

-- |
--
-- Return a spec to fullscreen the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#fullscreen-window)
--
-- @POST 	\/session\/{session id}\/window\/fullscreen 	Fullscreen Window@
fullScreenWindow :: SessionId -> Command WindowRect
fullScreenWindow sessionRef = PostEmpty "Fullscreen Window" (windowUri1 sessionRef "fullscreen")

-- ############################ Frame Methods ##########################################

-- |
--
-- Return a spec to switch to the parent frame given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#switch-to-parent-frame)
--
-- @POST 	\/session\/{session id}\/frame\/parent 	Switch To Parent Frame@
switchToParentFrame :: SessionId -> Command ()
switchToParentFrame sessionRef = PostEmpty "Switch To Parent Frame" (sessionUri2 sessionRef "frame" "parent")

-- ############################ Element(s) Methods ##########################################

-- |
--
-- Return a spec to get the active element of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-active-element)
--
-- @GET 	\/session\/{session id}\/element\/active 	Get Active Element@
getActiveElement :: SessionId -> Command ElementId
getActiveElement sessionId = Get "Get Active Element" (sessionUri2 sessionId "element" "active")

-- |
--
-- Return a spec to find an element on the current page given a 'SessionId' and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-element)
--
-- @POST 	\/session\/{session id}\/element 	Find Element@
findElement :: SessionId -> Selector -> Command ElementId
findElement sessionRef = mkPost "Find Element" (sessionUri1 sessionRef "element")

-- |
--
-- Return a spec to find elements on the current page given a 'SessionId' and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-elements)
--
-- @POST 	\/session\/{session id}\/elements 	Find Elements@
findElements :: SessionId -> Selector -> Command [ElementId]
findElements sessionRef = mkPost "Find Elements" (sessionUri1 sessionRef "elements")

-- ############################ Element Instance Methods ##########################################

-- |
--
-- Return a spec to get the shadow root of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-shadow-root)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/shadow 	Get Element Shadow Root@
getElementShadowRoot :: SessionId -> ElementId -> Command ShadowRootElementId
getElementShadowRoot sessionId elementId = Get "Get Element Shadow Root" (elementUri1 sessionId elementId "shadow")

-- |
--
-- Return a spec to find an element from another element given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-element-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/element 	Find Element From Element@
findElementFromElement :: SessionId -> ElementId -> Selector -> Command ElementId
findElementFromElement sessionId elementId = mkPost "Find Element From Element" (elementUri1 sessionId elementId "element") 

-- |
--
-- Return a spec to find elements from another element given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-elements-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/elements 	Find Elements From Element@
findElementsFromElement :: SessionId -> ElementId -> Selector -> Command [ElementId]
findElementsFromElement sessionId elementId = mkPost "Find Elements From Element" (elementUri1 sessionId elementId "elements")

-- |
--
-- Return a spec to check if an element is selected given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#is-element-selected)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/selected 	Is Element Selected@
isElementSelected :: SessionId -> ElementId -> Command Bool
isElementSelected sessionId elementId = Get "Is Element Selected" (elementUri1 sessionId elementId "selected")

-- |
--
-- Return a spec to get an attribute of an element given a 'SessionId', 'ElementId', and attribute name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-attribute)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/attribute\/{name} 	Get Element Attribute@
getElementAttribute :: SessionId -> ElementId -> Text -> Command Text
getElementAttribute sessionId elementId attributeName = Get "Get Element Attribute" (elementUri2 sessionId elementId "attribute" attributeName)

-- |
--
-- Return a spec to get a property of an element given a 'SessionId', 'ElementId', and property name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-property)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/property\/{name} 	Get Element Property@
getElementProperty :: SessionId -> ElementId -> Text -> Command Value
getElementProperty sessionId elementId propertyName = Get "Get Element Property" (elementUri2 sessionId elementId "property" propertyName)

-- |
--
-- Return a spec to get the CSS value of an element given a 'SessionId', 'ElementId', and CSS property name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-css-value)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/css\/{property name} 	Get Element CSS Value@
getElementCssValue :: SessionId -> ElementId -> Text -> Command Text
getElementCssValue sessionId elementId propertyName = Get "Get Element CSS Value" (elementUri2 sessionId elementId "css" propertyName)

-- |
--
-- Return a spec to get the text of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-text)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/text 	Get Element Text@
getElementText :: SessionId -> ElementId -> Command Text
getElementText sessionId elementId = Get "Get Element Text" (elementUri1 sessionId elementId "text")

-- |
--
-- Return a spec to get the tag name of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-tag-name)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/name 	Get Element Tag Name@
getElementTagName :: SessionId -> ElementId -> Command Text
getElementTagName sessionId elementId = Get "Get Element Tag Name" (elementUri1 sessionId elementId "name")

-- |
--
-- Return a spec to get the rect of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-rect)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/rect 	Get Element Rect@
getElementRect :: SessionId -> ElementId -> Command WindowRect
getElementRect sessionId elementId = Get "Get Element Rect" (elementUri1 sessionId elementId "rect")

-- |
--
-- Return a spec to check if an element is enabled given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#is-element-enabled)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/enabled 	Is Element Enabled@
isElementEnabled :: SessionId -> ElementId -> Command Bool
isElementEnabled sessionId elementId = Get "Is Element Enabled" (elementUri1 sessionId elementId "enabled")

-- |
--
-- Return a spec to get the computed role of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-computed-role)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedrole 	Get Computed Role@
getElementComputedRole :: SessionId -> ElementId -> Command Text
getElementComputedRole sessionId elementId = Get "Get Computed Role" (elementUri1 sessionId elementId "computedrole")

-- |
--
-- Return a spec to get the computed label of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-computed-label)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedlabel 	Get Computed Label@
getElementComputedLabel :: SessionId -> ElementId -> Command Text
getElementComputedLabel sessionId elementId = Get "Get Computed Label" (elementUri1 sessionId elementId "computedlabel")

-- |
--
-- Return a spec to click an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#element-click)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/click 	Element Click@
elementClick :: SessionId -> ElementId -> Command ()
elementClick sessionId elementId = PostEmpty "Element Click" (elementUri1 sessionId elementId "click")

-- |
--
-- Return a spec to clear an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#element-clear)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/clear 	Element Clear@
elementClear :: SessionId -> ElementId -> Command ()
elementClear sessionId elementId = PostEmpty "Element Clear" (elementUri1 sessionId elementId "clear")

-- |
--
-- Return a spec to send keys to an element given a 'SessionId', 'ElementId', and keys to send.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#element-send-keys)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/value 	Element Send Keys@
elementSendKeys :: SessionId -> ElementId -> Text -> Command ()
elementSendKeys sessionId elementId keysToSend = Post "Element Send Keys" (elementUri1 sessionId elementId "value") (fromList ["text" .= keysToSend])

-- |
--
-- Return a spec to take a screenshot of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#take-element-screenshot)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/screenshot 	Take Element Screenshot@
takeElementScreenshot :: SessionId -> ElementId -> Command Text
takeElementScreenshot sessionId elementId = Get "Take Element Screenshot" (elementUri1 sessionId elementId "screenshot")

-- ############################ Shadow DOM Methods ##########################################

-- |
--
-- Return a spec to find an element from the shadow root given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-element-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/element 	Find Element From Shadow Root@
findElementFromShadowRoot :: SessionId -> ShadowRootElementId -> Selector -> Command ElementId
findElementFromShadowRoot sessionId shadowId  = mkPost "Find Element From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "element") 

-- |
--
-- Return a spec to find elements from the shadow root given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-elements-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/elements 	Find Elements From Shadow Root@
findElementsFromShadowRoot :: SessionId -> ShadowRootElementId -> Selector -> Command [ElementId]
findElementsFromShadowRoot sessionId shadowId = mkPost "Find Elements From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "elements") 

-- ############################ Helper Functions ##########################################

newSessionUrl :: UrlPath
newSessionUrl = MkUrlPath [session]

session :: Text
session = "session"

sessionUri :: Text -> UrlPath
sessionUri sp = MkUrlPath [session, sp]

sessionUri1 :: SessionId -> Text -> UrlPath
sessionUri1 s sp = MkUrlPath [session, s.id, sp]

sessionUri2 :: SessionId -> Text -> Text -> UrlPath
sessionUri2 s sp sp2 = MkUrlPath [session, s.id, sp, sp2]

sessionUri3 :: SessionId -> Text -> Text -> Text -> UrlPath
sessionUri3 s sp sp2 sp3 = MkUrlPath [session, s.id, sp, sp2, sp3]

sessionUri4 :: SessionId -> Text -> Text -> Text -> Text -> UrlPath
sessionUri4 s sp sp2 sp3 sp4 = MkUrlPath [session, s.id, sp, sp2, sp3, sp4]

elementUri1 :: SessionId -> ElementId -> Text -> UrlPath
elementUri1 s er ep = sessionUri3 s "element" er.id ep

elementUri2 :: SessionId -> ElementId -> Text -> Text -> UrlPath
elementUri2 s er ep ep2 = sessionUri4 s "element" er.id ep ep2

window :: Text
window = "window"

windowUri1 :: SessionId -> Text -> UrlPath
windowUri1 sr sp = sessionUri2 sr window sp
