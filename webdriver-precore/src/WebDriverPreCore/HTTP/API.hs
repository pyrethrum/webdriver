{-# LANGUAGE CPP #-}

{-|

-- TODO: FIX
Once upon a time, the main browser automation tool was Selenium. Users of
this package had to start a Selenium session themselves, making sure to
configure it with a browser-specific driver program like @chromedriver@ or
@geckodriver@, and provide a hostname\/port. Then, this package would connect to
Selenium and use its wire protocol to control browsers.

Nowadays, there is an official W3C spec (<https://www.w3.org/TR/webdriver1>)
specifying the protocol, and a number of implementations. Selenium still exists,
but Chromedriver and Geckodriver can both serve as standalone WebDriver servers.
This library now helps you start up a driver in one of these supported
configurations:

1. Selenium.jar with one or more supported sub-drivers (@chromedriver@,
@geckodriver@). This is similar to the traditional picture.
2. Chromedriver standalone.
3. Geckodriver standalone.

You can pick the configuration you want by passing a 'DriverConfig' to the
'startSession' function. The WebDriver implementations have a few differences
between them, which this library tries to smooth over. For example, a single
Geckodriver instance can't start multiple Firefox sessions (see
<https://github.com/mozilla/geckodriver/issues/1946>). So, this library will spin
up a separate @geckodriver@ process for every session.

For an example of using this package by itself, see "Test.WebDriver.WD". For a
full test framework integrated with this package, see
[sandwich-webdriver](https://hackage.haskell.org/package/sandwich-webdriver).

For more information about error handling, see "WebDriverPreCore.SharedDocs#errorHandling#".

For session lifecycle details, see "WebDriverPreCore.SharedDocs#sessionManagement#".

-}

module WebDriverPreCore.HTTP.API
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
import WebDriverPreCore.HTTP.Protocol
  ( Actions (..),
    Cookie (..),
    Status (..),
    ElementId (..),
    FrameReference (..),
    FullCapabilities,
    Script,
    Selector (..),
    Session (..),
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
import Utils (UrlPath (..))
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
-- [spec](HTMLSpecURL#new-session)
--
--  @POST 	\/session 	New Session@
newSession :: FullCapabilities -> Command SessionResponse
newSession = mkPost "New Session" newSessionUrl

-- |
--
-- Return a spec to get the status of the driver.
--
-- [spec](HTMLSpecURL#status)
--
-- @GET 	\/status 	Status@
status :: Command Status
status = Get "Status" $ MkUrlPath ["status"]

-- ############################ Session Methods ##########################################

-- |
--
-- Return a spec to delete a session given a 'Session'.
--
-- [spec](HTMLSpecURL#delete-session)
--
-- @DELETE 	\/session\/{session id} 	Delete Session@
deleteSession :: Session -> Command ()
deleteSession sessionRef = Delete "Delete Session" $ sessionUri sessionRef.id

-- |
--
-- Return a spec to get the timeouts of a session given a 'Session'.
--
-- [spec](HTMLSpecURL#get-timeouts)
--
-- @GET 	\/session\/{session id}\/timeouts 	Get Timeouts@
getTimeouts :: Session -> Command Timeouts
getTimeouts sessionRef = Get "Get Timeouts" $ sessionUri1 sessionRef "timeouts"

-- |
--
-- Return a spec to set the timeouts of a session given a 'Session' and 'Timeouts'.
--
-- [spec](HTMLSpecURL#set-timeouts)
--
-- @POST 	\/session\/{session id}\/timeouts 	Set Timeouts@
setTimeouts :: Session -> Timeouts -> Command ()
setTimeouts sessionRef =
  mkPost "Set Timeouts" (sessionUri1 sessionRef "timeouts")

-- |
--
-- Return a spec to navigate to a URL given a 'Session' and a 'Text' URL.
--
-- [spec](HTMLSpecURL#navigate-to)
--
-- @POST 	\/session\/{session id}\/url 	Navigate To@
navigateTo :: Session -> URL -> Command ()
navigateTo sessionRef = mkPost' "Navigate To" (sessionUri1 sessionRef "url") (\url -> fromList ["url" .= url])

-- |
--
-- Return a spec to get the current URL of a session given a 'Session'.
--
-- [spec](HTMLSpecURL#get-current-url)
--
-- @GET 	\/session\/{session id}\/url 	Get Current URL@
getCurrentUrl :: Session -> Command URL
getCurrentUrl sessionRef = Get "Get Current URL" (sessionUri1 sessionRef "url")

-- |
--
-- Return a spec to navigate back in the browser history given a 'Session'.
--
-- [spec](HTMLSpecURL#back)
--
-- @POST 	\/session\/{session id}\/back 	Back@
back :: Session -> Command ()
back sessionRef = PostEmpty "Back" (sessionUri1 sessionRef "back")

-- |
--
-- Return a spec to navigate forward in the browser history given a 'Session'.
--
-- [spec](HTMLSpecURL#forward)
--
-- @POST 	\/session\/{session id}\/forward 	Forward@
forward :: Session -> Command ()
forward sessionRef = PostEmpty "Forward" (sessionUri1 sessionRef "forward")

-- |
--
-- Return a spec to refresh the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#refresh)
--
-- @POST 	\/session\/{session id}\/refresh 	Refresh@
refresh :: Session -> Command ()
refresh sessionRef = PostEmpty "Refresh" (sessionUri1 sessionRef "refresh")

-- |
--
-- Return a spec to get the title of the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#get-title)
--
-- @GET 	\/session\/{session id}\/title 	Get Title@
getTitle :: Session -> Command Text
getTitle sessionRef = Get "Get Title" (sessionUri1 sessionRef "title")

-- |
--
-- Return a spec to get the current window handle given a 'Session'.
--
-- [spec](HTMLSpecURL#get-window-handle)
--
-- @GET 	\/session\/{session id}\/window 	Get Window Handle@
getWindowHandle :: Session -> Command Handle
getWindowHandle sessionRef = Get "Get Window Handle" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to create a new window given a 'Session'.
--
-- [spec](HTMLSpecURL#new-window)
--
-- @POST 	\/session\/{session id}\/window\/new 	New Window@
newWindow :: Session -> Command WindowHandleSpec
newWindow sessionRef = PostEmpty "New Window" (sessionUri2 sessionRef "window" "new")

-- |
--
-- Return a spec to close the current window given a 'Session'.
--
-- [spec](HTMLSpecURL#close-window)
--
-- @DELETE 	\/session\/{session id}\/window 	Close Window@
closeWindow :: Session -> Command [Handle]
closeWindow sessionRef = Delete "Close Window" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to switch to a different window given a 'Session' and 'WindowHandle'.
--
-- [spec](HTMLSpecURL#switch-to-window)
--
-- @POST 	\/session\/{session id}\/window 	Switch To Window@
switchToWindow :: Session -> Handle -> Command ()
switchToWindow sessionRef = mkPost "Switch To Window" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to switch to a different frame given a 'Session' and 'FrameReference'.
--
-- [spec](HTMLSpecURL#switch-to-frame)
--
-- @POST 	\/session\/{session id}\/frame 	Switch To Frame@
switchToFrame :: Session -> FrameReference -> Command ()
switchToFrame sessionRef = mkPost "Switch To Frame" (sessionUri1 sessionRef "frame")

-- |
--
-- Return a spec to get the source of the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#get-page-source)
--
-- @GET 	\/session\/{session id}\/source 	Get Page Source@
getPageSource :: Session -> Command Text
getPageSource sessionId = Get "Get Page Source" (sessionUri1 sessionId "source")

-- |
--
-- Return a spec to execute a script in the context of the current page given a 'Session', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](HTMLSpecURL#execute-script)
--
-- @POST 	\/session\/{session id}\/execute\/sync 	Execute Script@
executeScript :: Session -> Script -> Command Value
executeScript sessionId = mkPost "Execute Script" (sessionUri2 sessionId "execute" "sync")

-- |
--
-- Return a spec to execute an asynchronous script in the context of the current page given a 'Session', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](HTMLSpecURL#execute-async-script)
--
-- @POST 	\/session\/{session id}\/execute\/async 	Execute Async Script@
executeScriptAsync :: Session -> Script -> Command Value
executeScriptAsync sessionId = mkPost "Execute Async Script" (sessionUri2 sessionId "execute" "async")

-- |
--
-- Return a spec to get all cookies of the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#get-all-cookies)
--
-- @GET 	\/session\/{session id}\/cookie 	Get All Cookies@
getAllCookies :: Session -> Command [Cookie]
getAllCookies sessionId = Get "Get All Cookies" (sessionUri1 sessionId "cookie")

-- |
--
-- Return a spec to get a named cookie of the current page given a 'Session' and cookie name.
--
-- [spec](HTMLSpecURL#get-named-cookie)
--
-- @GET 	\/session\/{session id}\/cookie\/{name} 	Get Named Cookie@
getNamedCookie :: Session -> Text -> Command Cookie
getNamedCookie sessionId cookieName = Get "Get Named Cookie" (sessionUri2 sessionId "cookie" cookieName)

-- |
--
-- Return a spec to add a cookie to the current page given a 'Session' and 'Cookie'.
--
-- [spec](HTMLSpecURL#add-cookie)
--
-- @POST 	\/session\/{session id}\/cookie 	Add Cookie@
addCookie :: Session -> Cookie -> Command ()
addCookie sessionId cookie = Post "Add Cookie" (sessionUri1 sessionId "cookie") (fromList ["cookie" .= cookie] )

-- |
--
-- Return a spec to delete a named cookie from the current page given a 'Session' and cookie name.
--
-- [spec](HTMLSpecURL#delete-cookie)
--
-- @DELETE 	\/session\/{session id}\/cookie\/{name} 	Delete Cookie@
deleteCookie :: Session -> Text -> Command ()
deleteCookie sessionId cookieName = Delete "Delete Cookie" (sessionUri2 sessionId "cookie" cookieName)

-- |
--
-- Return a spec to delete all cookies from the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#delete-all-cookies)
--
-- @DELETE 	\/session\/{session id}\/cookie 	Delete All Cookies@
deleteAllCookies :: Session -> Command ()
deleteAllCookies sessionId = Delete "Delete All Cookies" (sessionUri1 sessionId "cookie")

-- |
--
-- Return a spec to perform actions on the current page given a 'Session' and 'Actions'.
--
-- [spec](HTMLSpecURL#perform-actions)
--
-- @POST 	\/session\/{session id}\/actions 	Perform Actions@
performActions :: Session -> Actions -> Command ()
performActions sessionId = mkPost "Perform Actions" (sessionUri1 sessionId "actions")

-- |
--
-- Return a spec to release actions on the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#release-actions)
--
-- @DELETE 	\/session\/{session id}\/actions 	Release Actions@
releaseActions :: Session -> Command ()
releaseActions sessionId = Delete "Release Actions" (sessionUri1 sessionId "actions")

-- |
--
-- Return a spec to dismiss an alert on the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#dismiss-alert)
--
-- @POST 	\/session\/{session id}\/alert\/dismiss 	Dismiss Alert@
dismissAlert :: Session -> Command ()
dismissAlert sessionId = PostEmpty "Dismiss Alert" (sessionUri2 sessionId "alert" "dismiss")

-- |
--
-- Return a spec to accept an alert on the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#accept-alert)
--
-- @POST 	\/session\/{session id}\/alert\/accept 	Accept Alert@
acceptAlert :: Session -> Command ()
acceptAlert sessionId = PostEmpty "Accept Alert" (sessionUri2 sessionId "alert" "accept")

-- |
--
-- Return a spec to get the text of an alert on the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#get-alert-text)
--
-- @GET 	\/session\/{session id}\/alert\/text 	Get Alert Text@
getAlertText :: Session -> Command Text
getAlertText sessionId = Get "Get Alert Text" (sessionUri2 sessionId "alert" "text")

-- |
--
-- Return a spec to send text to an alert on the current page given a 'Session' and 'Text'.
--
-- [spec](HTMLSpecURL#send-alert-text)
--
-- @POST 	\/session\/{session id}\/alert\/text 	Send Alert Text@
sendAlertText :: Session -> Text -> Command ()
sendAlertText sessionId text = Post "Send Alert Text" (sessionUri2 sessionId "alert" "text") (fromList ["text" .= text])

-- |
--
-- Return a spec to take a screenshot of the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#take-screenshot)
--
-- @GET 	\/session\/{session id}\/screenshot 	Take Screenshot@
takeScreenshot :: Session -> Command Text
takeScreenshot sessionId = Get "Take Screenshot" (sessionUri1 sessionId "screenshot")

-- |
--
-- Return a spec to print the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#print-page)
--
-- @POST 	\/session\/{session id}\/print 	Print Page@
printPage :: Session -> Command Text
printPage sessionId = PostEmpty "Print Page" (sessionUri1 sessionId "print")

-- ############################ Window Methods ##########################################

-- |
--
-- Return a spec to get all window handles of the current session given a 'Session'.
--
-- [spec](HTMLSpecURL#get-window-handles)
--
-- @GET 	\/session\/{session id}\/window\/handles 	Get Window Handles@
getWindowHandles :: Session -> Command [Handle]
getWindowHandles sessionRef = Get "Get Window Handles" (sessionUri2 sessionRef "window" "handles")

-- |
--
-- Return a spec to get the window rect of the current window given a 'Session'.
--
-- [spec](HTMLSpecURL#get-window-rect)
--
-- @GET 	\/session\/{session id}\/window\/rect 	Get Window Rect@
getWindowRect :: Session -> Command WindowRect
getWindowRect sessionRef = Get "Get Window Rect" (sessionUri2 sessionRef "window" "rect")

-- |
--
-- Return a spec to set the window rect of the current window given a 'Session' and 'WindowRect'.
--
-- [spec](HTMLSpecURL#set-window-rect)
--
-- @POST 	\/session\/{session id}\/window\/rect 	Set Window Rect@
setWindowRect :: Session -> WindowRect -> Command WindowRect
setWindowRect sessionRef = mkPost "Set Window Rect" (sessionUri2 sessionRef "window" "rect")

-- |
--
-- Return a spec to maximize the current window given a 'Session'.
--
-- [spec](HTMLSpecURL#maximize-window)
--
-- @POST 	\/session\/{session id}\/window\/maximize 	Maximize Window@
maximizeWindow :: Session -> Command WindowRect
maximizeWindow sessionRef = PostEmpty "Maximize Window" (windowUri1 sessionRef "maximize")

-- |
--
-- Return a spec to minimize the current window given a 'Session'.
--
-- [spec](HTMLSpecURL#minimize-window)
--
-- @POST 	\/session\/{session id}\/window\/minimize 	Minimize Window@
minimizeWindow :: Session -> Command WindowRect
minimizeWindow sessionRef = PostEmpty "Minimize Window" (windowUri1 sessionRef "minimize")

-- |
--
-- Return a spec to fullscreen the current window given a 'Session'.
--
-- [spec](HTMLSpecURL#fullscreen-window)
--
-- @POST 	\/session\/{session id}\/window\/fullscreen 	Fullscreen Window@
fullScreenWindow :: Session -> Command WindowRect
fullScreenWindow sessionRef = PostEmpty "Fullscreen Window" (windowUri1 sessionRef "fullscreen")

-- ############################ Frame Methods ##########################################

-- |
--
-- Return a spec to switch to the parent frame given a 'Session'.
--
-- [spec](HTMLSpecURL#switch-to-parent-frame)
--
-- @POST 	\/session\/{session id}\/frame\/parent 	Switch To Parent Frame@
switchToParentFrame :: Session -> Command ()
switchToParentFrame sessionRef = PostEmpty "Switch To Parent Frame" (sessionUri2 sessionRef "frame" "parent")

-- ############################ Element(s) Methods ##########################################

-- |
--
-- Return a spec to get the active element of the current page given a 'Session'.
--
-- [spec](HTMLSpecURL#get-active-element)
--
-- @GET 	\/session\/{session id}\/element\/active 	Get Active Element@
getActiveElement :: Session -> Command ElementId
getActiveElement sessionId = Get "Get Active Element" (sessionUri2 sessionId "element" "active")

-- |
--
-- Return a spec to find an element on the current page given a 'Session' and 'Selector'.
--
-- [spec](HTMLSpecURL#find-element)
--
-- @POST 	\/session\/{session id}\/element 	Find Element@
findElement :: Session -> Selector -> Command ElementId
findElement sessionRef = mkPost "Find Element" (sessionUri1 sessionRef "element")

-- |
--
-- Return a spec to find elements on the current page given a 'Session' and 'Selector'.
--
-- [spec](HTMLSpecURL#find-elements)
--
-- @POST 	\/session\/{session id}\/elements 	Find Elements@
findElements :: Session -> Selector -> Command [ElementId]
findElements sessionRef = mkPost "Find Elements" (sessionUri1 sessionRef "elements")

-- ############################ Element Instance Methods ##########################################

-- |
--
-- Return a spec to get the shadow root of an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#get-element-shadow-root)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/shadow 	Get Element Shadow Root@
getElementShadowRoot :: Session -> ElementId -> Command ShadowRootElementId
getElementShadowRoot sessionId elementId = Get "Get Element Shadow Root" (elementUri1 sessionId elementId "shadow")

-- |
--
-- Return a spec to find an element from another element given a 'Session', 'ElementId', and 'Selector'.
--
-- [spec](HTMLSpecURL#find-element-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/element 	Find Element From Element@
findElementFromElement :: Session -> ElementId -> Selector -> Command ElementId
findElementFromElement sessionId elementId = mkPost "Find Element From Element" (elementUri1 sessionId elementId "element") 

-- |
--
-- Return a spec to find elements from another element given a 'Session', 'ElementId', and 'Selector'.
--
-- [spec](HTMLSpecURL#find-elements-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/elements 	Find Elements From Element@
findElementsFromElement :: Session -> ElementId -> Selector -> Command [ElementId]
findElementsFromElement sessionId elementId = mkPost "Find Elements From Element" (elementUri1 sessionId elementId "elements")

-- |
--
-- Return a spec to check if an element is selected given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#is-element-selected)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/selected 	Is Element Selected@
isElementSelected :: Session -> ElementId -> Command Bool
isElementSelected sessionId elementId = Get "Is Element Selected" (elementUri1 sessionId elementId "selected")

-- |
--
-- Return a spec to get an attribute of an element given a 'Session', 'ElementId', and attribute name.
--
-- [spec](HTMLSpecURL#get-element-attribute)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/attribute\/{name} 	Get Element Attribute@
getElementAttribute :: Session -> ElementId -> Text -> Command Text
getElementAttribute sessionId elementId attributeName = Get "Get Element Attribute" (elementUri2 sessionId elementId "attribute" attributeName)

-- |
--
-- Return a spec to get a property of an element given a 'Session', 'ElementId', and property name.
--
-- [spec](HTMLSpecURL#get-element-property)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/property\/{name} 	Get Element Property@
getElementProperty :: Session -> ElementId -> Text -> Command Value
getElementProperty sessionId elementId propertyName = Get "Get Element Property" (elementUri2 sessionId elementId "property" propertyName)

-- |
--
-- Return a spec to get the CSS value of an element given a 'Session', 'ElementId', and CSS property name.
--
-- [spec](HTMLSpecURL#get-element-css-value)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/css\/{property name} 	Get Element CSS Value@
getElementCssValue :: Session -> ElementId -> Text -> Command Text
getElementCssValue sessionId elementId propertyName = Get "Get Element CSS Value" (elementUri2 sessionId elementId "css" propertyName)

-- |
--
-- Return a spec to get the text of an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#get-element-text)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/text 	Get Element Text@
getElementText :: Session -> ElementId -> Command Text
getElementText sessionId elementId = Get "Get Element Text" (elementUri1 sessionId elementId "text")

-- |
--
-- Return a spec to get the tag name of an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#get-element-tag-name)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/name 	Get Element Tag Name@
getElementTagName :: Session -> ElementId -> Command Text
getElementTagName sessionId elementId = Get "Get Element Tag Name" (elementUri1 sessionId elementId "name")

-- |
--
-- Return a spec to get the rect of an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#get-element-rect)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/rect 	Get Element Rect@
getElementRect :: Session -> ElementId -> Command WindowRect
getElementRect sessionId elementId = Get "Get Element Rect" (elementUri1 sessionId elementId "rect")

-- |
--
-- Return a spec to check if an element is enabled given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#is-element-enabled)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/enabled 	Is Element Enabled@
isElementEnabled :: Session -> ElementId -> Command Bool
isElementEnabled sessionId elementId = Get "Is Element Enabled" (elementUri1 sessionId elementId "enabled")

-- |
--
-- Return a spec to get the computed role of an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#get-computed-role)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedrole 	Get Computed Role@
getElementComputedRole :: Session -> ElementId -> Command Text
getElementComputedRole sessionId elementId = Get "Get Computed Role" (elementUri1 sessionId elementId "computedrole")

-- |
--
-- Return a spec to get the computed label of an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#get-computed-label)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedlabel 	Get Computed Label@
getElementComputedLabel :: Session -> ElementId -> Command Text
getElementComputedLabel sessionId elementId = Get "Get Computed Label" (elementUri1 sessionId elementId "computedlabel")

-- |
--
-- Return a spec to click an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#element-click)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/click 	Element Click@
elementClick :: Session -> ElementId -> Command ()
elementClick sessionId elementId = PostEmpty "Element Click" (elementUri1 sessionId elementId "click")

-- |
--
-- Return a spec to clear an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#element-clear)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/clear 	Element Clear@
elementClear :: Session -> ElementId -> Command ()
elementClear sessionId elementId = PostEmpty "Element Clear" (elementUri1 sessionId elementId "clear")

-- |
--
-- Return a spec to send keys to an element given a 'Session', 'ElementId', and keys to send.
--
-- [spec](HTMLSpecURL#element-send-keys)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/value 	Element Send Keys@
elementSendKeys :: Session -> ElementId -> Text -> Command ()
elementSendKeys sessionId elementId keysToSend = Post "Element Send Keys" (elementUri1 sessionId elementId "value") (fromList ["text" .= keysToSend])

-- |
--
-- Return a spec to take a screenshot of an element given a 'Session' and 'ElementId'.
--
-- [spec](HTMLSpecURL#take-element-screenshot)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/screenshot 	Take Element Screenshot@
takeElementScreenshot :: Session -> ElementId -> Command Text
takeElementScreenshot sessionId elementId = Get "Take Element Screenshot" (elementUri1 sessionId elementId "screenshot")

-- ############################ Shadow DOM Methods ##########################################

-- |
--
-- Return a spec to find an element from the shadow root given a 'Session', 'ElementId', and 'Selector'.
--
-- [spec](HTMLSpecURL#find-element-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/element 	Find Element From Shadow Root@
findElementFromShadowRoot :: Session -> ShadowRootElementId -> Selector -> Command ElementId
findElementFromShadowRoot sessionId shadowId  = mkPost "Find Element From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "element") 

-- |
--
-- Return a spec to find elements from the shadow root given a 'Session', 'ElementId', and 'Selector'.
--
-- [spec](HTMLSpecURL#find-elements-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/elements 	Find Elements From Shadow Root@
findElementsFromShadowRoot :: Session -> ShadowRootElementId -> Selector -> Command [ElementId]
findElementsFromShadowRoot sessionId shadowId = mkPost "Find Elements From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "elements") 

-- ############################ Helper Functions ##########################################

newSessionUrl :: UrlPath
newSessionUrl = MkUrlPath [session]

session :: Text
session = "session"

sessionUri :: Text -> UrlPath
sessionUri sp = MkUrlPath [session, sp]

sessionUri1 :: Session -> Text -> UrlPath
sessionUri1 s sp = MkUrlPath [session, s.id, sp]

sessionUri2 :: Session -> Text -> Text -> UrlPath
sessionUri2 s sp sp2 = MkUrlPath [session, s.id, sp, sp2]

sessionUri3 :: Session -> Text -> Text -> Text -> UrlPath
sessionUri3 s sp sp2 sp3 = MkUrlPath [session, s.id, sp, sp2, sp3]

sessionUri4 :: Session -> Text -> Text -> Text -> Text -> UrlPath
sessionUri4 s sp sp2 sp3 sp4 = MkUrlPath [session, s.id, sp, sp2, sp3, sp4]

elementUri1 :: Session -> ElementId -> Text -> UrlPath
elementUri1 s er ep = sessionUri3 s "element" er.id ep

elementUri2 :: Session -> ElementId -> Text -> Text -> UrlPath
elementUri2 s er ep ep2 = sessionUri4 s "element" er.id ep ep2

window :: Text
window = "window"

windowUri1 :: Session -> Text -> UrlPath
windowUri1 sr sp = sessionUri2 sr window sp
