{-# OPTIONS_HADDOCK hide #-}

-- |
-- Description : All Webdriver W3C endpoints
--
--
-- Here is a longer description of this module, containing some
-- commentary with @some markup@.
module WebDriverPreCore.Http.SpecDefinition {-# DEPRECATED "HttpResponse - will be removed in a future release ~ 2027-02-01. See ChangeLog.md for upgrade instructions" #-}
  ( -- * The HttpSpec Type
    HttpSpec (..),

    -- * The API

    -- ** Root Methods
    newSession,
    newSession',
    status,

    -- ** Session Methods
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

    -- ** Window Methods
    getWindowHandles,
    getWindowRect,
    setWindowRect,
    maximizeWindow,
    minimizeWindow,
    fullscreenWindow,

    -- ** Frame Methods
    switchToParentFrame,

    -- ** Element(s) Methods
    getActiveElement,
    findElement,
    findElements,

    -- ** Element Instance Methods
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

    -- ** Shadow DOM Methods
    findElementFromShadowRoot,
    findElementsFromShadowRoot,
  )
where

import Data.Aeson as A
  ( FromJSON (..),
    KeyValue ((.=)),
    Result (..),
    ToJSON (toJSON),
    Value (..),
    object, (.:),
  )
import Data.Aeson.Types (parse, Parser)
import Data.Text (Text, unpack)
import WebDriverPreCore.Http.Capabilities as C
import WebDriverPreCore.Http.HttpResponse (HttpResponse (..))
import WebDriverPreCore.Http.Protocol
  ( Actions (..),
    Cookie (..),
    ElementId (..),
    FrameReference (..),
    Handle (..),
    Script (..),
    Selector (..),
    SessionId (..),
    SessionResponse (..),
    ShadowRootElementId (..),
    Status (..),
    URL (..),
    WindowHandleSpec (..),
    WindowRect (..)
  )
import WebDriverPreCore.Internal.AesonUtils (jsonToText)
import WebDriverPreCore.Internal.Utils (UrlPath (..))
import Prelude hiding (id, lookup)
import Data.Aeson (withObject)
import Control.Monad (when)
import Data.Function ((&))

-- |
--  The 'HttpSpec' type is a specification for a WebDriver Http command.
--  Every endpoint function in this module returns a 'HttpSpec' object.
data HttpSpec a
  = Get
      { description :: Text,
        path :: UrlPath,
        parser :: HttpResponse -> Result a
      }
  | Post
      { description :: Text,
        path :: UrlPath,
        body :: Value,
        parser :: HttpResponse -> Result a
      }
  | PostEmpty
      { description :: Text,
        path :: UrlPath,
        parser :: HttpResponse -> Result a
      }
  | Delete
      { description :: Text,
        path :: UrlPath,
        parser :: HttpResponse -> Result a
      }

get :: forall a. (FromJSON a) => Text -> UrlPath -> HttpSpec a
get description path =
  Get description path (getParser False)

post :: forall c r. (ToJSON c, FromJSON r) => Text -> UrlPath -> c -> HttpSpec r
post description path body =
  Post description path (toJSON body) (getParser False)

post_ :: forall c. (ToJSON c) => Text -> UrlPath -> c -> HttpSpec ()
post_ description path body =
  Post description path (toJSON body) (getParser True)

postEmpty :: forall a. (FromJSON a) => Text -> UrlPath -> HttpSpec a
postEmpty description path =
  PostEmpty description path (getParser False)

postEmpty_ :: Text -> UrlPath -> HttpSpec ()
postEmpty_ description path =
  PostEmpty description path (getParser True)

delete :: forall a. (FromJSON a) => Text -> UrlPath -> HttpSpec a
delete description path =
  Delete description path (getParser False)

delete_ :: Text -> UrlPath -> HttpSpec ()
delete_ description path =
  Delete description path (getParser True)

-- this is to shim the deprecated API with the new
getParser :: forall a. (FromJSON a) => Bool -> HttpResponse -> Result a
getParser expectNull r = parse (fromBodyValue expectNull) r.body

fromBodyValue :: forall a. (FromJSON a) => Bool -> Value -> Parser a
fromBodyValue expectNull body =
  body & withObject "body value" \b -> do
    val <- b .: "value"
    when (expectNull && val /= Null) $
      fail $
        unpack $
          "Null value expected but got:\n" <> jsonToText val
    parseJSON $ val

instance (Show a) => Show (HttpSpec a) where
  show :: HttpSpec a -> String
  show = Prelude.show . mkShowable

data HttpSpecShowable = Request
  { description :: Text,
    method :: Text,
    path :: UrlPath,
    body :: Maybe Text
  }
  deriving (Show)

mkShowable :: HttpSpec a -> HttpSpecShowable
mkShowable = \case
  Get d p _ -> Request d "GET" p Nothing
  Post d p b _ -> Request d "POST" p (Just $ jsonToText b)
  PostEmpty d p _ -> Request d "POST" p Nothing
  Delete d p _ -> Request d "DELETE" p Nothing

-- ######################################################################
-- ########################### WebDriver API ############################
-- ######################################################################

-- https://www.w3.org/TR/2025/WD-webdriver2-20251028/
-- 61 endpoints
-- Method 	URI Template 	Command

-- ** Root Methods

-- |
--  Return a spec to create a new session given 'FullCapabilities' object.
--
-- 'newSession'' can be used if 'FullCapabilities' doesn't meet your requirements.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#new-session)
--
--  @POST 	\/session 	New Session@
newSession :: FullCapabilities -> HttpSpec SessionResponse
newSession = newSession'

-- |
--
--  Return a spec to create a new session given an object of any type that implements `ToJSON`.
--
-- The 'FullCapabilities' type and associated types should work for the vast majority use cases, but if the required capabilities are not covered by the types provided, 'newSession''.
-- can be used with a custom type instead. 'newSession'' works with any type that implements 'ToJSON', (including an Aeson 'Value').
--
-- Obviously, any type used must produce a JSON object compatible with [capabilities as defined W3C spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#capabilities).
--
--  [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#new-session)
--
--  @POST 	\/session 	New Session@
newSession' :: (ToJSON a) => a -> HttpSpec SessionResponse
newSession' = post "New Session" newSessionUrl

-- |
--
-- Return a spec to get the status of the driver.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#status)
--
-- @GET 	\/status 	Status@
status :: HttpSpec Status
status = get "Status" (MkUrlPath ["status"])

-- ############################ Session Methods ##########################################

-- |
--
-- Return a spec to delete a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#delete-session)
--
-- @DELETE 	\/session\/{session id} 	Delete Session@
deleteSession :: SessionId -> HttpSpec ()
deleteSession sessionRef = delete_ "Delete Session" (sessionUri sessionRef.id)

-- |
--
-- Return a spec to get the timeouts of a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-timeouts)
--
-- @GET 	\/session\/{session id}\/timeouts 	Get Timeouts@
getTimeouts :: SessionId -> HttpSpec Timeouts
getTimeouts sessionRef = get "Get Timeouts" (sessionUri1 sessionRef "timeouts")

-- |
--
-- Return a spec to set the timeouts of a session given a 'SessionId' and 'Timeouts'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#set-timeouts)
--
-- @POST 	\/session\/{session id}\/timeouts 	Set Timeouts@
setTimeouts :: SessionId -> Timeouts -> HttpSpec ()
setTimeouts sessionRef timeouts =
  post_ "Set Timeouts" (sessionUri1 sessionRef "timeouts") (toJSON timeouts)

-- |
--
-- Return a spec to navigate to a URL given a 'SessionId' and a 'Text' URL.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#navigate-to)
--
-- @POST 	\/session\/{session id}\/url 	Navigate To@
navigateTo :: SessionId -> URL -> HttpSpec ()
navigateTo sessionRef url = post_ "Navigate To" (sessionUri1 sessionRef "url")  (object ["url" .= url])

-- |
--
-- Return a spec to get the current URL of a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-current-url)
--
-- @GET 	\/session\/{session id}\/url 	Get Current URL@
getCurrentUrl :: SessionId -> HttpSpec URL
getCurrentUrl sessionRef = get "Get Current URL" (sessionUri1 sessionRef "url")

-- |
--
-- Return a spec to navigate back in the browser history given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#back)
--
-- @POST 	\/session\/{session id}\/back 	Back@
back :: SessionId -> HttpSpec ()
back sessionRef = postEmpty_ "Back" (sessionUri1 sessionRef "back")

-- |
--
-- Return a spec to navigate forward in the browser history given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#forward)
--
-- @POST 	\/session\/{session id}\/forward 	Forward@
forward :: SessionId -> HttpSpec ()
forward sessionRef = postEmpty_ "Forward" (sessionUri1 sessionRef "forward")

-- |
--
-- Return a spec to refresh the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#refresh)
--
-- @POST 	\/session\/{session id}\/refresh 	Refresh@
refresh :: SessionId -> HttpSpec ()
refresh sessionRef = postEmpty_ "Refresh" (sessionUri1 sessionRef "refresh")

-- |
--
-- Return a spec to get the title of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-title)
--
-- @GET 	\/session\/{session id}\/title 	Get Title@
getTitle :: SessionId -> HttpSpec Text
getTitle sessionRef = get "Get Title" (sessionUri1 sessionRef "title")

-- |
--
-- Return a spec to get the current window handle given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-window-handle)
--
-- @GET 	\/session\/{session id}\/window 	Get Window Handle@
getWindowHandle :: SessionId -> HttpSpec Handle
getWindowHandle sessionRef = get "Get Window Handle" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to create a new window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#new-window)
--
-- @POST 	\/session\/{session id}\/window\/new 	New Window@
newWindow :: SessionId -> HttpSpec WindowHandleSpec
newWindow sessionRef = postEmpty "New Window" (sessionUri2 sessionRef "window" "new")

-- |
--
-- Return a spec to close the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#close-window)
--
-- @DELETE 	\/session\/{session id}\/window 	Close Window@
closeWindow :: SessionId -> HttpSpec [Handle]
closeWindow sessionRef = delete "Close Window" (sessionUri1 sessionRef "window")

-- |
--
-- Return a spec to switch to a different window given a 'SessionId' and 'WindowHandle'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#switch-to-window)
--
-- @POST 	\/session\/{session id}\/window 	Switch To Window@
switchToWindow :: SessionId -> Handle -> HttpSpec ()
switchToWindow sessionRef MkHandle {handle} = post_ "Switch To Window" (sessionUri1 sessionRef "window") (object ["handle" .= handle])

-- |
--
-- Return a spec to switch to a different frame given a 'SessionId' and 'FrameReference'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#switch-to-frame)
--
-- @POST 	\/session\/{session id}\/frame 	Switch To Frame@
switchToFrame :: SessionId -> FrameReference -> HttpSpec ()
switchToFrame sessionRef frameRef = post_ "Switch To Frame" (sessionUri1 sessionRef "frame") (toJSON frameRef)

-- |
--
-- Return a spec to get the source of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-page-source)
--
-- @GET 	\/session\/{session id}\/source 	Get Page Source@
getPageSource :: SessionId -> HttpSpec Text
getPageSource sessionId = get "Get Page Source" (sessionUri1 sessionId "source")

-- |
--
-- Return a spec to execute a script in the context of the current page given a 'SessionId', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#execute-script)
--
-- @POST 	\/session\/{session id}\/execute\/sync 	Execute Script@
executeScript :: SessionId -> Script -> HttpSpec Value
executeScript sessionId script = post "Execute Script" (sessionUri2 sessionId "execute" "sync") (toJSON script)

-- |
--
-- Return a spec to execute an asynchronous script in the context of the current page given a 'SessionId', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#execute-async-script)
--
-- @POST 	\/session\/{session id}\/execute\/async 	Execute Async Script@
executeScriptAsync :: SessionId -> Script -> HttpSpec Value
executeScriptAsync sessionId script = post "Execute Async Script" (sessionUri2 sessionId "execute" "async") script

-- |
--
-- Return a spec to get all cookies of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-all-cookies)
--
-- @GET 	\/session\/{session id}\/cookie 	Get All Cookies@
getAllCookies :: SessionId -> HttpSpec [Cookie]
getAllCookies sessionId = get "Get All Cookies" (sessionUri1 sessionId "cookie")

-- |
--
-- Return a spec to get a named cookie of the current page given a 'SessionId' and cookie name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-named-cookie)
--
-- @GET 	\/session\/{session id}\/cookie\/{name} 	Get Named Cookie@
getNamedCookie :: SessionId -> Text -> HttpSpec Cookie
getNamedCookie sessionId cookieName = get "Get Named Cookie" (sessionUri2 sessionId "cookie" cookieName)

-- |
--
-- Return a spec to add a cookie to the current page given a 'SessionId' and 'Cookie'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#add-cookie)
--
-- @POST 	\/session\/{session id}\/cookie 	Add Cookie@
addCookie :: SessionId -> Cookie -> HttpSpec ()
addCookie sessionId cookie = post_ "Add Cookie" (sessionUri1 sessionId "cookie") (object ["cookie" .= cookie])

-- |
--
-- Return a spec to delete a named cookie from the current page given a 'SessionId' and cookie name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#delete-cookie)
--
-- @DELETE 	\/session\/{session id}\/cookie\/{name} 	Delete Cookie@
deleteCookie :: SessionId -> Text -> HttpSpec ()
deleteCookie sessionId cookieName = delete_ "Delete Cookie" (sessionUri2 sessionId "cookie" cookieName)

-- |
--
-- Return a spec to delete all cookies from the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#delete-all-cookies)
--
-- @DELETE 	\/session\/{session id}\/cookie 	Delete All Cookies@
deleteAllCookies :: SessionId -> HttpSpec ()
deleteAllCookies sessionId = delete_ "Delete All Cookies" (sessionUri1 sessionId "cookie")

-- |
--
-- Return a spec to perform actions on the current page given a 'SessionId' and 'Actions'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#perform-actions)
--
-- @POST 	\/session\/{session id}\/actions 	Perform Actions@
performActions :: SessionId -> Actions -> HttpSpec ()
performActions sessionId actions = post_ "Perform Actions" (sessionUri1 sessionId "actions") (toJSON actions)

-- |
--
-- Return a spec to release actions on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#release-actions)
--
-- @DELETE 	\/session\/{session id}\/actions 	Release Actions@
releaseActions :: SessionId -> HttpSpec ()
releaseActions sessionId = delete_ "Release Actions" (sessionUri1 sessionId "actions")

-- |
--
-- Return a spec to dismiss an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dismiss-alert)
--
-- @POST 	\/session\/{session id}\/alert\/dismiss 	Dismiss Alert@
dismissAlert :: SessionId -> HttpSpec ()
dismissAlert sessionId = postEmpty_ "Dismiss Alert" (sessionUri2 sessionId "alert" "dismiss")

-- |
--
-- Return a spec to accept an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#accept-alert)
--
-- @POST 	\/session\/{session id}\/alert\/accept 	Accept Alert@
acceptAlert :: SessionId -> HttpSpec ()
acceptAlert sessionId = postEmpty_ "Accept Alert" (sessionUri2 sessionId "alert" "accept")

-- |
--
-- Return a spec to get the text of an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-alert-text)
--
-- @GET 	\/session\/{session id}\/alert\/text 	Get Alert Text@
getAlertText :: SessionId -> HttpSpec Text
getAlertText sessionId = get "Get Alert Text" (sessionUri2 sessionId "alert" "text")

-- |
--
-- Return a spec to send text to an alert on the current page given a 'SessionId' and 'Text'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#send-alert-text)
--
-- @POST 	\/session\/{session id}\/alert\/text 	Send Alert Text@
sendAlertText :: SessionId -> Text -> HttpSpec ()
sendAlertText sessionId text = post_ "Send Alert Text" (sessionUri2 sessionId "alert" "text") (object ["text" .= text])

-- |
--
-- Return a spec to take a screenshot of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#take-screenshot)
--
-- @GET 	\/session\/{session id}\/screenshot 	Take Screenshot@
takeScreenshot :: SessionId -> HttpSpec Text
takeScreenshot sessionId = get "Take Screenshot" (sessionUri1 sessionId "screenshot")

-- |
--
-- Return a spec to print the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#print-page)
--
-- @POST 	\/session\/{session id}\/print 	Print Page@
printPage :: SessionId -> HttpSpec Text
printPage sessionId = postEmpty "Print Page" (sessionUri1 sessionId "print")

-- ############################ Window Methods ##########################################

-- |
--
-- Return a spec to get all window handles of the current session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-window-handles)
--
-- @GET 	\/session\/{session id}\/window\/handles 	Get Window Handles@
getWindowHandles :: SessionId -> HttpSpec [Handle]
getWindowHandles sessionRef = get "Get Window Handles" (sessionUri2 sessionRef "window" "handles")

-- |
--
-- Return a spec to get the window rect of the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-window-rect)
--
-- @GET 	\/session\/{session id}\/window\/rect 	Get Window Rect@
getWindowRect :: SessionId -> HttpSpec WindowRect
getWindowRect sessionRef = get "Get Window Rect" (sessionUri2 sessionRef "window" "rect")

-- |
--
-- Return a spec to set the window rect of the current window given a 'SessionId' and 'WindowRect'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#set-window-rect)
--
-- @POST 	\/session\/{session id}\/window\/rect 	Set Window Rect@
setWindowRect :: SessionId -> WindowRect -> HttpSpec WindowRect
setWindowRect sessionRef = post "Set Window Rect" (sessionUri2 sessionRef "window" "rect")

-- |
--
-- Return a spec to maximize the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#maximize-window)
--
-- @POST 	\/session\/{session id}\/window\/maximize 	Maximize Window@
maximizeWindow :: SessionId -> HttpSpec WindowRect
maximizeWindow sessionRef = postEmpty "Maximize Window" (windowUri1 sessionRef "maximize")

-- |
--
-- Return a spec to minimize the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#minimize-window)
--
-- @POST 	\/session\/{session id}\/window\/minimize 	Minimize Window@
minimizeWindow :: SessionId -> HttpSpec WindowRect
minimizeWindow sessionRef = postEmpty "Minimize Window" (windowUri1 sessionRef "minimize")

-- |
--
-- Return a spec to fullscreen the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#fullscreen-window)
--
-- @POST 	\/session\/{session id}\/window\/fullscreen 	Fullscreen Window@
fullscreenWindow :: SessionId -> HttpSpec WindowRect
fullscreenWindow sessionRef = postEmpty "Fullscreen Window" (windowUri1 sessionRef "fullscreen")

-- ############################ Frame Methods ##########################################

-- |
--
-- Return a spec to switch to the parent frame given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#switch-to-parent-frame)
--
-- @POST 	\/session\/{session id}\/frame\/parent 	Switch To Parent Frame@
switchToParentFrame :: SessionId -> HttpSpec ()
switchToParentFrame sessionRef = postEmpty_ "Switch To Parent Frame" (sessionUri2 sessionRef "frame" "parent")

-- ############################ Element(s) Methods ##########################################

-- |
--
-- Return a spec to get the active element of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-active-element)
--
-- @GET 	\/session\/{session id}\/element\/active 	Get Active Element@
getActiveElement :: SessionId -> HttpSpec ElementId
getActiveElement sessionId = get "Get Active Element" (sessionUri2 sessionId "element" "active")

-- |
--
-- Return a spec to find an element on the current page given a 'SessionId' and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-element)
--
-- @POST 	\/session\/{session id}\/element 	Find Element@
findElement :: SessionId -> Selector -> HttpSpec ElementId
findElement sessionRef = post "Find Element" (sessionUri1 sessionRef "element")

-- |
--
-- Return a spec to find elements on the current page given a 'SessionId' and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-elements)
--
-- @POST 	\/session\/{session id}\/elements 	Find Elements@
findElements :: SessionId -> Selector -> HttpSpec [ElementId]
findElements sessionRef = post "Find Elements" (sessionUri1 sessionRef "elements")

-- ############################ Element Instance Methods ##########################################

-- |
--
-- Return a spec to get the shadow root of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-shadow-root)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/shadow 	Get Element Shadow Root@
getElementShadowRoot :: SessionId -> ElementId -> HttpSpec ShadowRootElementId
getElementShadowRoot sessionId elementId = get "Get Element Shadow Root" (elementUri1 sessionId elementId "shadow")

-- |
--
-- Return a spec to find an element from another element given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-element-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/element 	Find Element From Element@
findElementFromElement :: SessionId -> ElementId -> Selector -> HttpSpec ElementId
findElementFromElement sessionId elementId = post "Find Element From Element" (elementUri1 sessionId elementId "element")

-- |
--
-- Return a spec to find elements from another element given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-elements-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/elements 	Find Elements From Element@
findElementsFromElement :: SessionId -> ElementId -> Selector -> HttpSpec [ElementId]
findElementsFromElement sessionId elementId = post "Find Elements From Element" (elementUri1 sessionId elementId "elements")

-- |
--
-- Return a spec to check if an element is selected given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#is-element-selected)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/selected 	Is Element Selected@
isElementSelected :: SessionId -> ElementId -> HttpSpec Bool
isElementSelected sessionId elementId = get "Is Element Selected" (elementUri1 sessionId elementId "selected")

-- |
--
-- Return a spec to get an attribute of an element given a 'SessionId', 'ElementId', and attribute name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-attribute)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/attribute\/{name} 	Get Element Attribute@
getElementAttribute :: SessionId -> ElementId -> Text -> HttpSpec Text
getElementAttribute sessionId elementId attributeName = get "Get Element Attribute" (elementUri2 sessionId elementId "attribute" attributeName)

-- |
--
-- Return a spec to get a property of an element given a 'SessionId', 'ElementId', and property name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-property)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/property\/{name} 	Get Element Property@
getElementProperty :: SessionId -> ElementId -> Text -> HttpSpec Value
getElementProperty sessionId elementId propertyName = get "Get Element Property" (elementUri2 sessionId elementId "property" propertyName)

-- |
--
-- Return a spec to get the CSS value of an element given a 'SessionId', 'ElementId', and CSS property name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-css-value)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/css\/{property name} 	Get Element CSS Value@
getElementCssValue :: SessionId -> ElementId -> Text -> HttpSpec Text
getElementCssValue sessionId elementId propertyName = get "Get Element CSS Value" (elementUri2 sessionId elementId "css" propertyName)

-- |
--
-- Return a spec to get the text of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-text)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/text 	Get Element Text@
getElementText :: SessionId -> ElementId -> HttpSpec Text
getElementText sessionId elementId = get "Get Element Text" (elementUri1 sessionId elementId "text")

-- |
--
-- Return a spec to get the tag name of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-tag-name)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/name 	Get Element Tag Name@
getElementTagName :: SessionId -> ElementId -> HttpSpec Text
getElementTagName sessionId elementId = get "Get Element Tag Name" (elementUri1 sessionId elementId "name")

-- |
--
-- Return a spec to get the rect of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-element-rect)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/rect 	Get Element Rect@
getElementRect :: SessionId -> ElementId -> HttpSpec WindowRect
getElementRect sessionId elementId = get "Get Element Rect" (elementUri1 sessionId elementId "rect")

-- |
--
-- Return a spec to check if an element is enabled given a 'SessionId' and 'ElementId'.
--SAP will foc
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#is-element-enabled)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/enabled 	Is Element Enabled@
isElementEnabled :: SessionId -> ElementId -> HttpSpec Bool
isElementEnabled sessionId elementId = get "Is Element Enabled" (elementUri1 sessionId elementId "enabled")

-- |
--
-- Return a spec to get the computed role of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-computed-role)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedrole 	Get Computed Role@
getElementComputedRole :: SessionId -> ElementId -> HttpSpec Text
getElementComputedRole sessionId elementId = get "Get Computed Role" (elementUri1 sessionId elementId "computedrole")

-- |
--
-- Return a spec to get the computed label of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#get-computed-label)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedlabel 	Get Computed Label@
getElementComputedLabel :: SessionId -> ElementId -> HttpSpec Text
getElementComputedLabel sessionId elementId = get "Get Computed Label" (elementUri1 sessionId elementId "computedlabel")

-- |
--
-- Return a spec to click an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#element-click)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/click 	Element Click@
elementClick :: SessionId -> ElementId -> HttpSpec ()
elementClick sessionId elementId = postEmpty_ "Element Click" (elementUri1 sessionId elementId "click")

-- |
--
-- Return a spec to clear an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#element-clear)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/clear 	Element Clear@
elementClear :: SessionId -> ElementId -> HttpSpec ()
elementClear sessionId elementId = postEmpty_ "Element Clear" (elementUri1 sessionId elementId "clear")

-- |
--
-- Return a spec to send keys to an element given a 'SessionId', 'ElementId', and keys to send.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#element-send-keys)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/value 	Element Send Keys@
elementSendKeys :: SessionId -> ElementId -> Text -> HttpSpec ()
elementSendKeys sessionId elementId keysToSend = post_ "Element Send Keys" (elementUri1 sessionId elementId "value") (object ["text" .= keysToSend])

-- |
--
-- Return a spec to take a screenshot of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#take-element-screenshot)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/screenshot 	Take Element Screenshot@
takeElementScreenshot :: SessionId -> ElementId -> HttpSpec Text
takeElementScreenshot sessionId elementId = get "Take Element Screenshot" (elementUri1 sessionId elementId "screenshot")

-- ############################ Shadow DOM Methods ##########################################

-- |
--
-- Return a spec to find an element from the shadow root given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-element-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/element 	Find Element From Shadow Root@
findElementFromShadowRoot :: SessionId -> ShadowRootElementId -> Selector -> HttpSpec ElementId
findElementFromShadowRoot sessionId shadowId = post "Find Element From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "element")

-- |
--
-- Return a spec to find elements from the shadow root given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#find-elements-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/elements 	Find Elements From Shadow Root@
findElementsFromShadowRoot :: SessionId -> ShadowRootElementId -> Selector -> HttpSpec [ElementId]
findElementsFromShadowRoot sessionId shadowId = post "Find Elements From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "elements")

-- ############################ Utils ##########################################

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

window :: Text
window = "window"

windowUri1 :: SessionId -> Text -> UrlPath
windowUri1 sr sp = sessionUri2 sr window sp

elementUri1 :: SessionId -> ElementId -> Text -> UrlPath
elementUri1 s er ep = sessionUri3 s "element" er.id ep

elementUri2 :: SessionId -> ElementId -> Text -> Text -> UrlPath
elementUri2 s er ep ep2 = sessionUri4 s "element" er.id ep ep2

newSessionUrl :: UrlPath
newSessionUrl = MkUrlPath [session]

session :: Text
session = "session"
