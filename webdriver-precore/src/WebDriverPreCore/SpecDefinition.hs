{-# OPTIONS_HADDOCK hide #-}

-- |
-- Description : All Webdriver W3C endpoints
--
--
-- Here is a longer description of this module, containing some
-- commentary with @some markup@.
module WebDriverPreCore.SpecDefinition
  ( -- * The W3Spec Type
    W3Spec (..),

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

    -- * auxiliary Types
    Cookie (..),
    DriverStatus (..),
    ElementId (..),
    FrameReference (..),
    HandleType (..),
    HttpResponse (..),
    SameSite (..),
    Selector (..),
    SessionId (..),
    Timeouts (..),
    WindowHandle (..),
    WindowHandleSpec (..),
    WindowRect (..),
    UrlPath (..),

    -- ** Action Types
    Action (..),
    Actions (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    WheelAction (..),
  )
where

import Data.Aeson as A
  ( FromJSON (..),
    Key,
    KeyValue ((.=)),
    Result (..),
    ToJSON (toJSON),
    Value (..),
    fromJSON,
    object,
    withObject,
    withText,
    (.:),
  )
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Generics (Generic)
import WebDriverPreCore.Internal.Utils (jsonToText, opt, txt)
import WebDriverPreCore.Capabilities as Capabilities
import WebDriverPreCore.HttpResponse (HttpResponse (..))
import Prelude hiding (id, lookup)

-- | Url as returned by 'W3Spec'
-- The 'UrlPath' type is a newtype wrapper around a list of 'Text' segments representing a path.
--
-- e.g. the path: @\/session\/session-no-1-2-3\/window@ would be represented as: @MkUrlPath ["session", "session-no-1-2-3", "window"]@
newtype UrlPath = MkUrlPath {segments :: [Text]}
  deriving newtype (Show, Eq, Ord, Semigroup)

{-|
  The 'W3Spec' type is a specification for a WebDriver command.
  Every endpoint function in this module returns a 'W3Spec' object.
-}
data W3Spec a
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

instance (Show a) => Show (W3Spec a) where
  show :: W3Spec a -> String
  show = Prelude.show . mkShowable

data W3SpecShowable = Request
  { description :: Text,
    method :: Text,
    path :: UrlPath,
    body :: Maybe Text
  }
  deriving (Show)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#dfn-get-window-handle)
newtype WindowHandle = Handle {handle :: Text}
  deriving (Show, Eq)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#new-window)
data WindowHandleSpec = HandleSpec
  { handle :: WindowHandle,
    handletype :: HandleType
  }
  deriving (Show, Eq)

instance ToJSON WindowHandleSpec where
  toJSON :: WindowHandleSpec -> Value
  toJSON HandleSpec {handle, handletype} =
    object
      [ "handle" .= handle.handle,
        "type" .= handletype
      ]

instance FromJSON WindowHandleSpec where
  parseJSON :: Value -> Parser WindowHandleSpec
  parseJSON = withObject "WindowHandleSpec" $ \v -> do
    handle <- Handle <$> v .: "handle"
    handletype <- v .: "type"
    pure $ HandleSpec {..}

data HandleType
  = Window
  | Tab
  deriving (Show, Eq)

instance ToJSON HandleType where
  toJSON :: HandleType -> Value
  toJSON = String . T.toLower . pack . show

instance FromJSON HandleType where
  parseJSON :: Value -> Parser HandleType
  parseJSON = withText "HandleType" $ \case
    "window" -> pure Window
    "tab" -> pure Tab
    v -> fail $ unpack $ "Unknown HandleType " <> v

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#dfn-find-element)
newtype ElementId = Element {id :: Text}
  deriving (Show, Eq, Generic)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#dfn-new-sessions)
newtype SessionId = Session {id :: Text}
  deriving (Show)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#dfn-status)
data DriverStatus
  = Ready
  | Running
  | ServiceError {statusCode :: Int, statusMessage :: Text}
  | Unknown {statusCode :: Int, statusMessage :: Text}
  deriving (Show, Eq)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#cookies)
data SameSite
  = Lax
  | Strict
  | None
  deriving (Show, Eq, Ord)

instance ToJSON SameSite where
  toJSON :: SameSite -> Value
  toJSON = String . txt

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#dfn-switch-to-frame)
data FrameReference
  = TopLevelFrame
  | FrameNumber Word16
  | FrameElementId ElementId
  deriving (Show, Eq)

frameJson :: FrameReference -> Value
frameJson fr =
  object
    ["id" .= toJSON (frameVariant fr)]
  where
    frameVariant =
      \case
        TopLevelFrame -> Null
        FrameNumber n -> Number $ fromIntegral n
        FrameElementId elm -> object [elementFieldName .= elm.id]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#cookies)
data Cookie = MkCookie
  { name :: Text,
    value :: Text,
    -- optional
    path :: Maybe Text,
    domain :: Maybe Text,
    secure :: Maybe Bool,
    httpOnly :: Maybe Bool,
    sameSite :: Maybe SameSite,
    -- When the cookie expires, specified in seconds since Unix Epoch.
    expiry :: Maybe Int
  }
  deriving (Show, Eq)

instance ToJSON Cookie where
  toJSON :: Cookie -> Value
  toJSON MkCookie {name, value, path, domain, secure, httpOnly, sameSite, expiry} =
    object $
      [ "name" .= name,
        "value" .= value
      ]
        <> catMaybes
          [ opt "path" path,
            opt "domain" domain,
            opt "secure" secure,
            opt "httpOnly" httpOnly,
            opt "sameSite" sameSite,
            opt "expiry" expiry
          ]

cookieJSON :: Cookie -> Value
cookieJSON c = object ["cookie" .= c]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#locator-strategies)
data Selector
  = CSS Text
  | XPath Text
  | LinkText Text
  | PartialLinkText Text
  | TagName Text
  deriving (Show, Eq)

-- ######################################################################
-- ########################### WebDriver API ############################
-- ######################################################################

-- https://www.w3.org/TR/2025/WD-webdriver2-20250210/
-- 61 endpoints
-- Method 	URI Template 	Command

-- ** Root Methods

-- |
--  Return a spec to create a new session given 'FullCapabilities' object.
--
-- 'newSession'' can be used if 'FullCapabilities' doesn't meet your requirements.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#new-session)
--
--  @POST 	\/session 	New Session@
newSession :: FullCapabilities -> W3Spec SessionId
newSession = newSession'

-- |
--
--  Return a spec to create a new session given an object of any type that implements `ToJSON`.
--
-- The 'FullCapabilities' type and associated types should work for the vast majority use cases, but if the required capabilities are not covered by the types provided, 'newSession''.
-- can be used instead. newSession' works with any type that implements 'ToJSON', (including an Aeson 'Value').
-- 
-- Obviously, any type used must produece a JSON object compatible with [capabilities as defined W3C spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities).
--
--  [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#new-session)
--
--  @POST 	\/session 	New Session@
newSession' :: (ToJSON a) => a -> W3Spec SessionId
newSession' capabilities = Post "New Session" (MkUrlPath [session]) (toJSON capabilities) parseSessionRef

-- |
--
-- Return a spec to get the status of the driver.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#status)
--
-- @GET 	\/status 	Status@
status :: W3Spec DriverStatus
status = Get "Status" (MkUrlPath ["status"]) parseDriverStatus

-- ############################ Session Methods ##########################################

-- |
--
-- Return a spec to delete a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#delete-session)
--
-- @DELETE 	\/session\/{session id} 	Delete Session@
deleteSession :: SessionId -> W3Spec ()
deleteSession sessionRef = Delete "Delete Session" (sessionUri sessionRef.id) voidParser

-- |
--
-- Return a spec to get the timeouts of a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-timeouts)
--
-- @GET 	\/session\/{session id}\/timeouts 	Get Timeouts@
getTimeouts :: SessionId -> W3Spec Timeouts
getTimeouts sessionRef = Get "Get Timeouts" (sessionUri1 sessionRef "timeouts") parseTimeouts

-- |
--
-- Return a spec to set the timeouts of a session given a 'SessionId' and 'Timeouts'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#set-timeouts)
--
-- @POST 	\/session\/{session id}\/timeouts 	Set Timeouts@
setTimeouts :: SessionId -> Timeouts -> W3Spec ()
setTimeouts sessionRef timeouts =
  Post "Set Timeouts" (sessionUri1 sessionRef "timeouts") (toJSON timeouts) voidParser

-- |
--
-- Return a spec to navigate to a URL given a 'SessionId' and a 'Text' URL.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#navigate-to)
--
-- @POST 	\/session\/{session id}\/url 	Navigate To@
navigateTo :: SessionId -> Text -> W3Spec ()
navigateTo sessionRef url = Post "Navigate To" (sessionUri1 sessionRef "url") (object ["url" .= url]) voidParser

-- |
--
-- Return a spec to get the current URL of a session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-current-url)
--
-- @GET 	\/session\/{session id}\/url 	Get Current URL@
getCurrentUrl :: SessionId -> W3Spec Text
getCurrentUrl sessionRef = Get "Get Current URL" (sessionUri1 sessionRef "url") parseBodyTxt

-- |
--
-- Return a spec to navigate back in the browser history given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#back)
--
-- @POST 	\/session\/{session id}\/back 	Back@
back :: SessionId -> W3Spec ()
back sessionRef = PostEmpty "Back" (sessionUri1 sessionRef "back") voidParser

-- |
--
-- Return a spec to navigate forward in the browser history given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#forward)
--
-- @POST 	\/session\/{session id}\/forward 	Forward@
forward :: SessionId -> W3Spec ()
forward sessionRef = PostEmpty "Forward" (sessionUri1 sessionRef "forward") voidParser

-- |
--
-- Return a spec to refresh the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#refresh)
--
-- @POST 	\/session\/{session id}\/refresh 	Refresh@
refresh :: SessionId -> W3Spec ()
refresh sessionRef = PostEmpty "Refresh" (sessionUri1 sessionRef "refresh") voidParser

-- |
--
-- Return a spec to get the title of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-title)
--
-- @GET 	\/session\/{session id}\/title 	Get Title@
getTitle :: SessionId -> W3Spec Text
getTitle sessionRef = Get "Get Title" (sessionUri1 sessionRef "title") parseBodyTxt

-- |
--
-- Return a spec to get the current window handle given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-window-handle)
--
-- @GET 	\/session\/{session id}\/window 	Get Window Handle@
getWindowHandle :: SessionId -> W3Spec WindowHandle
getWindowHandle sessionRef = Get "Get Window Handle" (sessionUri1 sessionRef "window") (fmap Handle . parseBodyTxt)

-- |
--
-- Return a spec to create a new window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#new-window)
--
-- @POST 	\/session\/{session id}\/window\/new 	New Window@
newWindow :: SessionId -> W3Spec WindowHandleSpec
newWindow sessionRef = PostEmpty "New Window" (sessionUri2 sessionRef "window" "new") windowHandleParser

-- |
--
-- Return a spec to close the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#close-window)
--
-- @DELETE 	\/session\/{session id}\/window 	Close Window@
closeWindow :: SessionId -> W3Spec ()
closeWindow sessionRef = Delete "Close Window" (sessionUri1 sessionRef "window") voidParser

-- |
--
-- Return a spec to switch to a different window given a 'SessionId' and 'WindowHandle'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#switch-to-window)
--
-- @POST 	\/session\/{session id}\/window 	Switch To Window@
switchToWindow :: SessionId -> WindowHandle -> W3Spec ()
switchToWindow sessionRef Handle {handle} = Post "Switch To Window" (sessionUri1 sessionRef "window") (object ["handle" .= handle]) voidParser

-- |
--
-- Return a spec to switch to a different frame given a 'SessionId' and 'FrameReference'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#switch-to-frame)
--
-- @POST 	\/session\/{session id}\/frame 	Switch To Frame@
switchToFrame :: SessionId -> FrameReference -> W3Spec ()
switchToFrame sessionRef frameRef = Post "Switch To Frame" (sessionUri1 sessionRef "frame") (frameJson frameRef) voidParser

-- |
--
-- Return a spec to get the source of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-page-source)
--
-- @GET 	\/session\/{session id}\/source 	Get Page Source@
getPageSource :: SessionId -> W3Spec Text
getPageSource sessionId = Get "Get Page Source" (sessionUri1 sessionId "source") parseBodyTxt

-- |
--
-- Return a spec to execute a script in the context of the current page given a 'SessionId', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#execute-script)
--
-- @POST 	\/session\/{session id}\/execute\/sync 	Execute Script@
executeScript :: SessionId -> Text -> [Value] -> W3Spec Value
executeScript sessionId script args = Post "Execute Script" (sessionUri2 sessionId "execute" "sync") (mkScript script args) bodyValue

-- |
--
-- Return a spec to execute an asynchronous script in the context of the current page given a 'SessionId', 'Text' script, and a list of 'Value' arguments.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#execute-async-script)
--
-- @POST 	\/session\/{session id}\/execute\/async 	Execute Async Script@
executeScriptAsync :: SessionId -> Text -> [Value] -> W3Spec Value
executeScriptAsync sessionId script args = Post "Execute Async Script" (sessionUri2 sessionId "execute" "async") (mkScript script args) bodyValue

-- |
--
-- Return a spec to get all cookies of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-all-cookies)
--
-- @GET 	\/session\/{session id}\/cookie 	Get All Cookies@
getAllCookies :: SessionId -> W3Spec [Cookie]
getAllCookies sessionId = Get "Get All Cookies" (sessionUri1 sessionId "cookie") parseCookies

-- |
--
-- Return a spec to get a named cookie of the current page given a 'SessionId' and cookie name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-named-cookie)
--
-- @GET 	\/session\/{session id}\/cookie\/{name} 	Get Named Cookie@
getNamedCookie :: SessionId -> Text -> W3Spec Cookie
getNamedCookie sessionId cookieName = Get "Get Named Cookie" (sessionUri2 sessionId "cookie" cookieName) parseCookie

-- |
--
-- Return a spec to add a cookie to the current page given a 'SessionId' and 'Cookie'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#add-cookie)
--
-- @POST 	\/session\/{session id}\/cookie 	Add Cookie@
addCookie :: SessionId -> Cookie -> W3Spec ()
addCookie sessionId cookie = Post "Add Cookie" (sessionUri1 sessionId "cookie") (cookieJSON cookie) voidParser

-- |
--
-- Return a spec to delete a named cookie from the current page given a 'SessionId' and cookie name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#delete-cookie)
--
-- @DELETE 	\/session\/{session id}\/cookie\/{name} 	Delete Cookie@
deleteCookie :: SessionId -> Text -> W3Spec ()
deleteCookie sessionId cookieName = Delete "Delete Cookie" (sessionUri2 sessionId "cookie" cookieName) voidParser

-- |
--
-- Return a spec to delete all cookies from the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#delete-all-cookies)
--
-- @DELETE 	\/session\/{session id}\/cookie 	Delete All Cookies@
deleteAllCookies :: SessionId -> W3Spec ()
deleteAllCookies sessionId = Delete "Delete All Cookies" (sessionUri1 sessionId "cookie") voidParser

-- |
--
-- Return a spec to perform actions on the current page given a 'SessionId' and 'Actions'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#perform-actions)
--
-- @POST 	\/session\/{session id}\/actions 	Perform Actions@
performActions :: SessionId -> Actions -> W3Spec ()
performActions sessionId actions = Post "Perform Actions" (sessionUri1 sessionId "actions") (actionsToJson actions) voidParser

-- |
--
-- Return a spec to release actions on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#release-actions)
--
-- @DELETE 	\/session\/{session id}\/actions 	Release Actions@
releaseActions :: SessionId -> W3Spec ()
releaseActions sessionId = Delete "Release Actions" (sessionUri1 sessionId "actions") voidParser

-- |
--
-- Return a spec to dismiss an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#dismiss-alert)
--
-- @POST 	\/session\/{session id}\/alert\/dismiss 	Dismiss Alert@
dismissAlert :: SessionId -> W3Spec ()
dismissAlert sessionId = PostEmpty "Dismiss Alert" (sessionUri2 sessionId "alert" "dismiss") voidParser

-- |
--
-- Return a spec to accept an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#accept-alert)
--
-- @POST 	\/session\/{session id}\/alert\/accept 	Accept Alert@
acceptAlert :: SessionId -> W3Spec ()
acceptAlert sessionId = PostEmpty "Accept Alert" (sessionUri2 sessionId "alert" "accept") voidParser

-- |
--
-- Return a spec to get the text of an alert on the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-alert-text)
--
-- @GET 	\/session\/{session id}\/alert\/text 	Get Alert Text@
getAlertText :: SessionId -> W3Spec Text
getAlertText sessionId = Get "Get Alert Text" (sessionUri2 sessionId "alert" "text") parseBodyTxt

-- |
--
-- Return a spec to send text to an alert on the current page given a 'SessionId' and 'Text'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#send-alert-text)
--
-- @POST 	\/session\/{session id}\/alert\/text 	Send Alert Text@
sendAlertText :: SessionId -> Text -> W3Spec ()
sendAlertText sessionId text = Post "Send Alert Text" (sessionUri2 sessionId "alert" "text") (object ["text" .= text]) voidParser

-- |
--
-- Return a spec to take a screenshot of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#take-screenshot)
--
-- @GET 	\/session\/{session id}\/screenshot 	Take Screenshot@
takeScreenshot :: SessionId -> W3Spec Text
takeScreenshot sessionId = Get "Take Screenshot" (sessionUri1 sessionId "screenshot") parseBodyTxt

-- |
--
-- Return a spec to print the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#print-page)
--
-- @POST 	\/session\/{session id}\/print 	Print Page@
printPage :: SessionId -> W3Spec Text
printPage sessionId = PostEmpty "Print Page" (sessionUri1 sessionId "print") parseBodyTxt

-- ############################ Window Methods ##########################################

-- |
--
-- Return a spec to get all window handles of the current session given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-window-handles)
--
-- @GET 	\/session\/{session id}\/window\/handles 	Get Window Handles@
getWindowHandles :: SessionId -> W3Spec [WindowHandle]
getWindowHandles sessionRef = Get "Get Window Handles" (sessionUri2 sessionRef "window" "handles") windowHandlesParser

-- |
--
-- Return a spec to get the window rect of the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-window-rect)
--
-- @GET 	\/session\/{session id}\/window\/rect 	Get Window Rect@
getWindowRect :: SessionId -> W3Spec WindowRect
getWindowRect sessionRef = Get "Get Window Rect" (sessionUri2 sessionRef "window" "rect") parseWindowRect

-- |
--
-- Return a spec to set the window rect of the current window given a 'SessionId' and 'WindowRect'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#set-window-rect)
--
-- @POST 	\/session\/{session id}\/window\/rect 	Set Window Rect@
setWindowRect :: SessionId -> WindowRect -> W3Spec WindowRect
setWindowRect sessionRef rect = Post "Set Window Rect" (sessionUri2 sessionRef "window" "rect") (toJSON rect) parseWindowRect

-- |
--
-- Return a spec to maximize the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#maximize-window)
--
-- @POST 	\/session\/{session id}\/window\/maximize 	Maximize Window@
maximizeWindow :: SessionId -> W3Spec WindowRect
maximizeWindow sessionRef = PostEmpty "Maximize Window" (windowUri1 sessionRef "maximize") parseWindowRect

-- |
--
-- Return a spec to minimize the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#minimize-window)
--
-- @POST 	\/session\/{session id}\/window\/minimize 	Minimize Window@
minimizeWindow :: SessionId -> W3Spec WindowRect
minimizeWindow sessionRef = PostEmpty "Minimize Window" (windowUri1 sessionRef "minimize") parseWindowRect

-- |
--
-- Return a spec to fullscreen the current window given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#fullscreen-window)
--
-- @POST 	\/session\/{session id}\/window\/fullscreen 	Fullscreen Window@
fullscreenWindow :: SessionId -> W3Spec WindowRect
fullscreenWindow sessionRef = PostEmpty "Fullscreen Window" (windowUri1 sessionRef "fullscreen") parseWindowRect

-- ############################ Frame Methods ##########################################

-- |
--
-- Return a spec to switch to the parent frame given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#switch-to-parent-frame)
--
-- @POST 	\/session\/{session id}\/frame\/parent 	Switch To Parent Frame@
switchToParentFrame :: SessionId -> W3Spec ()
switchToParentFrame sessionRef = PostEmpty "Switch To Parent Frame" (sessionUri2 sessionRef "frame" "parent") voidParser

-- ############################ Element(s) Methods ##########################################

-- |
--
-- Return a spec to get the active element of the current page given a 'SessionId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-active-element)
--
-- @GET 	\/session\/{session id}\/element\/active 	Get Active Element@
getActiveElement :: SessionId -> W3Spec ElementId
getActiveElement sessionId = Get "Get Active Element" (sessionUri2 sessionId "element" "active") parseElementRef

-- |
--
-- Return a spec to find an element on the current page given a 'SessionId' and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#find-element)
--
-- @POST 	\/session\/{session id}\/element 	Find Element@
findElement :: SessionId -> Selector -> W3Spec ElementId
findElement sessionRef = findElement' sessionRef . selectorJson

-- |
--
-- Return a spec to find elements on the current page given a 'SessionId' and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#find-elements)
--
-- @POST 	\/session\/{session id}\/elements 	Find Elements@
findElements :: SessionId -> Selector -> W3Spec [ElementId]
findElements sessionRef selector = Post "Find Elements" (sessionUri1 sessionRef "elements") (selectorJson selector) parseElementsRef

-- ############################ Element Instance Methods ##########################################

-- |
--
-- Return a spec to get the shadow root of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-element-shadow-root)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/shadow 	Get Element Shadow Root@
getElementShadowRoot :: SessionId -> ElementId -> W3Spec ElementId
getElementShadowRoot sessionId elementId = Get "Get Element Shadow Root" (elementUri1 sessionId elementId "shadow") parseShadowElementRef

-- |
--
-- Return a spec to find an element from another element given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#find-element-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/element 	Find Element From Element@
findElementFromElement :: SessionId -> ElementId -> Selector -> W3Spec ElementId
findElementFromElement sessionId elementId selector = Post "Find Element From Element" (elementUri1 sessionId elementId "element") (selectorJson selector) parseElementRef

-- |
--
-- Return a spec to find elements from another element given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#find-elements-from-element)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/elements 	Find Elements From Element@
findElementsFromElement :: SessionId -> ElementId -> Selector -> W3Spec [ElementId]
findElementsFromElement sessionId elementId selector = Post "Find Elements From Element" (elementUri1 sessionId elementId "elements") (selectorJson selector) parseElementsRef

-- |
--
-- Return a spec to check if an element is selected given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#is-element-selected)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/selected 	Is Element Selected@
isElementSelected :: SessionId -> ElementId -> W3Spec Bool
isElementSelected sessionId elementId = Get "Is Element Selected" (elementUri1 sessionId elementId "selected") parseBodyBool

-- |
--
-- Return a spec to get an attribute of an element given a 'SessionId', 'ElementId', and attribute name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-element-attribute)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/attribute\/{name} 	Get Element Attribute@
getElementAttribute :: SessionId -> ElementId -> Text -> W3Spec Text
getElementAttribute sessionId elementId attributeName = Get "Get Element Attribute" (elementUri2 sessionId elementId "attribute" attributeName) parseBodyTxt

-- |
--
-- Return a spec to get a property of an element given a 'SessionId', 'ElementId', and property name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-element-property)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/property\/{name} 	Get Element Property@
getElementProperty :: SessionId -> ElementId -> Text -> W3Spec Value
getElementProperty sessionId elementId propertyName = Get "Get Element Property" (elementUri2 sessionId elementId "property" propertyName) bodyValue

-- |
--
-- Return a spec to get the CSS value of an element given a 'SessionId', 'ElementId', and CSS property name.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-element-css-value)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/css\/{property name} 	Get Element CSS Value@
getElementCssValue :: SessionId -> ElementId -> Text -> W3Spec Text
getElementCssValue sessionId elementId propertyName = Get "Get Element CSS Value" (elementUri2 sessionId elementId "css" propertyName) parseBodyTxt

-- |
--
-- Return a spec to get the text of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-element-text)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/text 	Get Element Text@
getElementText :: SessionId -> ElementId -> W3Spec Text
getElementText sessionId elementId = Get "Get Element Text" (elementUri1 sessionId elementId "text") parseBodyTxt

-- |
--
-- Return a spec to get the tag name of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-element-tag-name)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/name 	Get Element Tag Name@
getElementTagName :: SessionId -> ElementId -> W3Spec Text
getElementTagName sessionId elementId = Get "Get Element Tag Name" (elementUri1 sessionId elementId "name") parseBodyTxt

-- |
--
-- Return a spec to get the rect of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-element-rect)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/rect 	Get Element Rect@
getElementRect :: SessionId -> ElementId -> W3Spec WindowRect
getElementRect sessionId elementId = Get "Get Element Rect" (elementUri1 sessionId elementId "rect") parseWindowRect

-- |
--
-- Return a spec to check if an element is enabled given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#is-element-enabled)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/enabled 	Is Element Enabled@
isElementEnabled :: SessionId -> ElementId -> W3Spec Bool
isElementEnabled sessionId elementId = Get "Is Element Enabled" (elementUri1 sessionId elementId "enabled") parseBodyBool

-- |
--
-- Return a spec to get the computed role of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-computed-role)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedrole 	Get Computed Role@
getElementComputedRole :: SessionId -> ElementId -> W3Spec Text
getElementComputedRole sessionId elementId = Get "Get Computed Role" (elementUri1 sessionId elementId "computedrole") parseBodyTxt

-- |
--
-- Return a spec to get the computed label of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#get-computed-label)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/computedlabel 	Get Computed Label@
getElementComputedLabel :: SessionId -> ElementId -> W3Spec Text
getElementComputedLabel sessionId elementId = Get "Get Computed Label" (elementUri1 sessionId elementId "computedlabel") parseBodyTxt

-- |
--
-- Return a spec to click an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#element-click)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/click 	Element Click@
elementClick :: SessionId -> ElementId -> W3Spec ()
elementClick sessionId elementId = PostEmpty "Element Click" (elementUri1 sessionId elementId "click") voidParser

-- |
--
-- Return a spec to clear an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#element-clear)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/clear 	Element Clear@
elementClear :: SessionId -> ElementId -> W3Spec ()
elementClear sessionId elementId = PostEmpty "Element Clear" (elementUri1 sessionId elementId "clear") voidParser

-- |
--
-- Return a spec to send keys to an element given a 'SessionId', 'ElementId', and keys to send.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#element-send-keys)
--
-- @POST 	\/session\/{session id}\/element\/{element id}\/value 	Element Send Keys@
elementSendKeys :: SessionId -> ElementId -> Text -> W3Spec ()
elementSendKeys sessionId elementId keysToSend = Post "Element Send Keys" (elementUri1 sessionId elementId "value") (keysJson keysToSend) voidParser

-- |
--
-- Return a spec to take a screenshot of an element given a 'SessionId' and 'ElementId'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#take-element-screenshot)
--
-- @GET 	\/session\/{session id}\/element\/{element id}\/screenshot 	Take Element Screenshot@
takeElementScreenshot :: SessionId -> ElementId -> W3Spec Text
takeElementScreenshot sessionId elementId = Get "Take Element Screenshot" (elementUri1 sessionId elementId "screenshot") parseBodyTxt

-- ############################ Shadow DOM Methods ##########################################

-- |
--
-- Return a spec to find an element from the shadow root given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#find-element-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/element 	Find Element From Shadow Root@
findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> W3Spec ElementId
findElementFromShadowRoot sessionId shadowId selector = Post "Find Element From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "element") (selectorJson selector) parseElementRef

-- |
--
-- Return a spec to find elements from the shadow root given a 'SessionId', 'ElementId', and 'Selector'.
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#find-elements-from-shadow-root)
--
-- @POST 	\/session\/{session id}\/shadow\/{shadow id}\/elements 	Find Elements From Shadow Root@
findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> W3Spec [ElementId]
findElementsFromShadowRoot sessionId shadowId selector = Post "Find Elements From Shadow Root" (sessionUri3 sessionId "shadow" shadowId.id "elements") (selectorJson selector) parseElementsRef

-- ############################ Utils ##########################################

findElement' :: SessionId -> Value -> W3Spec ElementId
findElement' sessionRef selector = Post "Find Element" (sessionUri1 sessionRef "element") selector parseElementRef

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#dfn-get-element-rect)
data WindowRect = Rect
  { x :: Int,
    y :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show, Eq)

instance ToJSON WindowRect where
  toJSON :: WindowRect -> Value
  toJSON Rect {x, y, width, height} =
    object
      [ "x" .= x,
        "y" .= y,
        "width" .= width,
        "height" .= height
      ]

parseTimeouts :: HttpResponse -> Result Timeouts
parseTimeouts r = do
  r' <- bodyValue r
  fromJSON r'

parseWindowRect :: HttpResponse -> Result WindowRect
parseWindowRect r =
  do
    x <- bdyInt "x"
    y <- bdyInt "y"
    width <- bdyInt "width"
    height <- bdyInt "height"
    pure $ Rect {..}
  where
    bdyInt = bodyInt r

mkScript :: Text -> [Value] -> Value
mkScript script args = object ["script" .= script, "args" .= args]

windowHandleParser :: HttpResponse -> Result WindowHandleSpec
windowHandleParser r =
  bodyValue r
    >>= fromJSON

windowHandlesParser :: HttpResponse -> Result [WindowHandle]
windowHandlesParser r = do
  bodyValue r
    >>= \case
      Array a -> (Handle <$>) <$> (sequence . toList $ asText <$> a)
      v -> aesonTypeError "Array" v

-- windowHandleFromValue :: Value -> Maybe WindowHandleSpec
-- windowHandleFromValue v =
--   liftA2 HandleSpec (Handle <$> lookupTxt "handle" v) ( <$> lookupTxt "type" v)

parseCookies :: HttpResponse -> Result [Cookie]
parseCookies r =
  bodyValue r
    >>= \case
      Array a -> mapM cookieFromBody (toList a)
      v -> aesonTypeError "Array" v

parseCookie :: HttpResponse -> Result Cookie
parseCookie r =
  bodyValue r
    >>= cookieFromBody

cookieFromBody :: Value -> Result Cookie
cookieFromBody b = case b of
  Object kv -> do
    name <- lookupTxt "name" b
    value <- lookupTxt "value" b
    path <- opt' "path"
    domain <- opt' "domain"
    secure <- optBool "secure"
    httpOnly <- optBool "httpOnly"
    sameSite <- optBase toSameSite "sameSite"
    expiry <- optInt "expiry"
    pure $ MkCookie {..}
    where
      optBase :: (Value -> Result a) -> Key -> Result (Maybe a)
      optBase typeCaster k = AKM.lookup k kv & maybe (Success Nothing) (fmap Just . typeCaster)
      opt' = optBase asText
      optInt = optBase asInt
      optBool = optBase asBool
  v -> aesonTypeError "Object" v

selectorJson :: Selector -> Value
selectorJson = \case
  CSS css -> sJSON "css selector" css
  XPath xpath -> sJSON "xpath" xpath
  LinkText lt -> sJSON "link text" lt
  PartialLinkText plt -> sJSON "partial link text" plt
  TagName tn -> sJSON "tag name" tn
  where
    sJSON using value = object ["using" .= using, "value" .= value]

voidParser :: HttpResponse -> Result ()
voidParser _ = pure ()

bodyText' :: Result Value -> Key -> Result Text
bodyText' v k = v >>= lookupTxt k

bodyText :: HttpResponse -> Key -> Result Text
bodyText r = bodyText' (bodyValue r)

bodyInt' :: Result Value -> Key -> Result Int
bodyInt' v k = v >>= lookupInt k

bodyInt :: HttpResponse -> Key -> Result Int
bodyInt r = bodyInt' (bodyValue r)

parseBodyTxt :: HttpResponse -> Result Text
parseBodyTxt r = bodyValue r >>= asText

parseBodyBool :: HttpResponse -> Result Bool
parseBodyBool r =
  bodyValue r >>= asBool

asBool :: Value -> Result Bool
asBool = \case
  Bool b -> Success b
  v -> aesonTypeError "Bool" v

parseElementsRef :: HttpResponse -> Result [ElementId]
parseElementsRef r =
  bodyValue r
    >>= \case
      Array a -> mapM elemtRefFromBody $ toList a
      v -> aesonTypeError "Array" v

-- TODO Aeson helpers separate module
lookup :: Key -> Value -> Result Value
lookup k v =
  v & \case
    Object o -> AKM.lookup k o & maybe (A.Error ("the key: " <> show k <> "does not exist in the object:\n" <> jsonPrettyString v)) pure
    _ -> aesonTypeError "Object" v

lookupTxt :: Key -> Value -> Result Text
lookupTxt k v = lookup k v >>= asText

toSameSite :: Value -> Result SameSite
toSameSite = \case
  String "Lax" -> Success Lax
  String "Strict" -> Success Strict
  String "None" -> Success None
  v -> aesonTypeError' "SameSite" "Expected one of: Lax, Strict, None" v

lookupInt :: Key -> Value -> Result Int
lookupInt k v = lookup k v >>= asInt

aesonTypeErrorMessage :: Text -> Value -> Text
aesonTypeErrorMessage t v = "Expected Json Value to be of type: " <> t <> "\nbut got:\n" <> jsonToText v

aesonTypeError :: Text -> Value -> Result a
aesonTypeError t v = A.Error . unpack $ aesonTypeErrorMessage t v

aesonTypeError' :: Text -> Text -> Value -> Result a
aesonTypeError' typ info v = A.Error . unpack $ aesonTypeErrorMessage typ v <> "\n" <> info

asText :: Value -> Result Text
asText = \case
  String t -> Success t
  v -> aesonTypeError "Text" v

asInt :: Value -> Result Int
asInt = \case
  Number t -> Success $ floor t
  v -> aesonTypeError "Int" v

parseSessionRef :: HttpResponse -> Result SessionId
parseSessionRef r =
  Session
    <$> bodyText r "sessionId"

bodyValue :: HttpResponse -> Result Value
bodyValue r = lookup "value" r.body

-- https://www.w3.org/TR/webdriver2/#elements
elementFieldName :: Key
elementFieldName = "element-6066-11e4-a52e-4f735466cecf"

-- https://www.w3.org/TR/webdriver2/#shadow-root
shadowRootFieldName :: Key
shadowRootFieldName = "shadow-6066-11e4-a52e-4f735466cecf"

parseElementRef :: HttpResponse -> Result ElementId
parseElementRef r =
  Element <$> bodyText r elementFieldName

parseShadowElementRef :: HttpResponse -> Result ElementId
parseShadowElementRef r =
  Element <$> bodyText r shadowRootFieldName

elemtRefFromBody :: Value -> Result ElementId
elemtRefFromBody b = Element <$> lookupTxt elementFieldName b

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

window :: Text
window = "window"

windowUri1 :: SessionId -> Text -> UrlPath
windowUri1 sr sp = sessionUri2 sr window sp

elementUri1 :: SessionId -> ElementId -> Text -> UrlPath
elementUri1 s er ep = sessionUri3 s "element" er.id ep

elementUri2 :: SessionId -> ElementId -> Text -> Text -> UrlPath
elementUri2 s er ep ep2 = sessionUri4 s "element" er.id ep ep2

jsonPrettyString :: Value -> String
jsonPrettyString = unpack . jsonToText

mkShowable :: W3Spec a -> W3SpecShowable
mkShowable = \case
  Get d p _ -> Request d "GET" p Nothing
  Post d p b _ -> Request d "POST" p (Just $ jsonToText b)
  PostEmpty d p _ -> Request d "POST" p Nothing
  Delete d p _ -> Request d "DELETE" p Nothing

parseDriverStatus :: HttpResponse -> Result DriverStatus
parseDriverStatus MkHttpResponse {statusCode, statusMessage} =
  Success $
    statusCode & \case
      200 -> Ready
      500 -> ServiceError {statusCode, statusMessage}
      501 -> Running
      _ -> Unknown {statusCode, statusMessage}

keysJson :: Text -> Value
keysJson keysToSend = object ["text" .= keysToSend]

-- actions
-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#actions)
newtype Actions = MkActions {actions :: [Action]}

actionsToJson :: Actions -> Value
actionsToJson MkActions {actions} =
  object
    [ "actions" .= actions
    ]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#actions)
data KeyAction
  = PauseKey {duration :: Maybe Int} -- ms
  | KeyDown
      { value :: Text
      }
  | KeyUp
      { value :: Text
      }
  deriving (Show, Eq)

instance ToJSON KeyAction where
  toJSON :: KeyAction -> Value
  toJSON PauseKey {duration} =
    object $
      [ "type" .= ("pause" :: Text)
      ]
        <> catMaybes [opt "duration" duration]
  toJSON KeyDown {value} =
    object
      [ "type" .= ("keyDown" :: Text),
        "value" .= String value
      ]
  toJSON KeyUp {value} =
    object
      [ "type" .= ("keyUp" :: Text),
        "value" .= String value
      ]

-- Pointer subtypes
-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#actions)
data Pointer
  = Mouse
  | Pen
  | Touch
  deriving (Show, Eq)

mkLwrTxt :: (Show a) => a -> Value
mkLwrTxt = String . T.toLower . txt

instance ToJSON Pointer where
  toJSON :: Pointer -> Value
  toJSON = mkLwrTxt

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#actions)
data PointerOrigin
  = Viewport
  | OriginPointer
  | OriginElement ElementId
  deriving (Show, Eq)

instance ToJSON PointerOrigin where
  toJSON :: PointerOrigin -> Value
  toJSON = \case
    Viewport -> "viewport"
    OriginPointer -> "pointer"
    OriginElement (Element id') -> object ["element" .= id']


-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#actions)
data Action
  = NoneAction
      { id :: Text,
        -- the numeric id of the pointing device. This is a positive integer, with the values 0 and 1 reserved for mouse-type pointers.
        noneActions :: [Maybe Int] -- delay
      }
  | Key
      { id :: Text,
        keyActions :: [KeyAction]
        -- https://github.com/jlipps/simple-wd-spec?tab=readme-ov-file#perform-actions
        -- keys codepoint https://www.w3.org/TR/webdriver2/#keyboard-actions
      }
  | Pointer
      { id :: Text,
        subType :: Pointer,
        -- the numeric id of the pointing device. This is a positive integer, with the values 0 and 1 reserved for mouse-type pointers.
        pointerId :: Int,
        pressed :: Set Int, -- pressed buttons
        x :: Int, -- start x location in viewport coordinates.
        y :: Int, -- start y location in viewport coordinates
        actions :: [PointerAction]
      }
  | Wheel
      { id :: Text,
        wheelActions :: [WheelAction]
      }
  deriving (Show, Eq)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#actions)
data WheelAction
  = PauseWheel {duration :: Maybe Int} -- ms
  | Scroll
      { origin :: PointerOrigin,
        x :: Int,
        y :: Int,
        deltaX :: Int,
        deltaY :: Int,
        duration :: Maybe Int -- ms
      }
  deriving (Show, Eq)

instance ToJSON WheelAction where
  toJSON :: WheelAction -> Value
  toJSON wa =
    object $ base <> catMaybes [opt "duration" wa.duration]
    where
      base = case wa of
        PauseWheel _ -> ["type" .= ("pause" :: Text)]
        Scroll
          { origin,
            x,
            y,
            deltaX,
            deltaY
          } ->
            [ "type" .= ("scroll" :: Text),
              "origin" .= origin,
              "x" .= x,
              "y" .= y,
              "deltaX" .= deltaX,
              "deltaY" .= deltaY
            ]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#actions)
data PointerAction
  = PausePointer {duration :: Maybe Int} -- ms
  | Up
      { button :: Int,
        width :: Maybe Int,
        height :: Maybe Int,
        pressure :: Maybe Float, -- 0 -> 1
        tangentialPressure :: Maybe Float, -- -1 -> 1
        tiltX :: Maybe Int, -- -90 -> 90
        tiltY :: Maybe Int, -- -90 -> 90
        twist :: Maybe Int, -- 0 -> 359
        altitudeAngle :: Maybe Double, -- 0 -> pi/2
        azimuthAngle :: Maybe Double -- 0 -> 2pi-- button} -- button
      }
  | Down
      { button :: Int,
        width :: Maybe Int,
        height :: Maybe Int,
        pressure :: Maybe Float, -- 0 -> 1
        tangentialPressure :: Maybe Float, -- -1 -> 1
        tiltX :: Maybe Int, -- -90 -> 90
        tiltY :: Maybe Int, -- -90 -> 90
        twist :: Maybe Int, -- 0 -> 359
        altitudeAngle :: Maybe Double, -- 0 -> pi/2
        azimuthAngle :: Maybe Double -- 0 -> 2pi-- button
      }
  | Move
      { origin :: PointerOrigin,
        duration :: Maybe Int, -- ms
        -- where to move to
        -- though the spec seems to indicate width and height are double
        -- gecko driver was blowing up with anything other than int
        width :: Maybe Int,
        height :: Maybe Int,
        pressure :: Maybe Float, -- 0 -> 1
        tangentialPressure :: Maybe Float, -- -1 -> 1
        tiltX :: Maybe Int, -- -90 -> 90
        tiltY :: Maybe Int, -- -90 -> 90
        twist :: Maybe Int, -- 0 -> 359
        altitudeAngle :: Maybe Double, -- 0 -> pi/2
        azimuthAngle :: Maybe Double, -- 0 -> 2pi
        x :: Int,
        y :: Int
      }
  | -- looks like not supported yet by gecko driver 02-02-2025
    -- https://searchfox.org/mozilla-central/source/remote/shared/webdriver/Actions.sys.mjs#2340
    Cancel
  deriving (Show, Eq)

instance ToJSON PointerAction where
  toJSON :: PointerAction -> Value
  toJSON = \case
    PausePointer d ->
      object $
        ["type" .= ("pause" :: Text)]
          <> catMaybes [opt "duration" d]
    Up
      { -- https://www.w3.org/TR/pointerevents/#dom-pointerevent-pointerid
        button,
        width, -- magnitude on the X axis), in CSS pixels (see [CSS21]) -- default = 1
        height, -- (magnitude on the Y axis), in CSS pixels (see [CSS21]) -- default = 1
        pressure, -- 0 - 1
        tangentialPressure, -- -1 -> 1
        tiltX, -- -90 -> 90
        tiltY, -- -90 -> 90
        twist, -- 0 -> 359
        altitudeAngle, -- 0 -> pi/2
        azimuthAngle -- 0 -> 2pi
      } ->
        object $
          [ "type" .= ("pointerUp" :: Text),
            "button" .= button
          ]
            <> catMaybes
              [ opt "height" height,
                opt "width" width,
                opt "pressure" pressure,
                opt "tangentialPressure" tangentialPressure,
                opt "tiltX" tiltX,
                opt "tiltY" tiltY,
                opt "twist" twist,
                opt "altitudeAngle" altitudeAngle,
                opt "azimuthAngle" azimuthAngle
              ]
    Down
      { button,
        width,
        height,
        pressure,
        tangentialPressure, -- -1 -> 1
        tiltX, -- -90 -> 90
        tiltY, -- -90 -> 90
        twist, -- 0 -> 359
        altitudeAngle, -- 0 -> pi/2
        azimuthAngle -- 0 -> 2pi
      } ->
        object $
          [ "type" .= ("pointerDown" :: Text),
            "button" .= button
          ]
            <> catMaybes
              [ opt "height" height,
                opt "width" width,
                opt "pressure" pressure,
                opt "tangentialPressure" tangentialPressure,
                opt "tiltX" tiltX,
                opt "tiltY" tiltY,
                opt "twist" twist,
                opt "altitudeAngle" altitudeAngle,
                opt "azimuthAngle" azimuthAngle
              ]
    Move
      { origin,
        duration,
        width,
        height,
        pressure,
        tangentialPressure, -- -1 -> 1
        tiltX, -- -90 -> 90
        tiltY, -- -90 -> 90
        twist, -- 0 -> 359
        altitudeAngle, -- 0 -> pi/2
        azimuthAngle, -- 0 -> 2pi
        x,
        y
      } ->
        object $
          [ "type" .= ("pointerMove" :: Text),
            "origin" .= origin,
            "x" .= x,
            "y" .= y
          ]
            <> catMaybes
              [ opt "duration" duration,
                opt "height" height,
                opt "width" width,
                opt "pressure" pressure,
                opt "tangentialPressure" tangentialPressure,
                opt "tiltX" tiltX,
                opt "tiltY" tiltY,
                opt "twist" twist,
                opt "altitudeAngle" altitudeAngle,
                opt "azimuthAngle" azimuthAngle
              ]
    -- looks like Cancel not supported yet by gecko driver 02-02-2025
    -- https://searchfox.org/mozilla-central/source/remote/shared/webdriver/Actions.sys.mjs#2340
    Cancel -> object ["type" .= ("pointerCancel" :: Text)]

mkPause :: Maybe Int -> Value
mkPause d = object $ ["type" .= ("pause" :: Text)] <> catMaybes [opt "duration" d]

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = \case
    NoneAction
      { id,
        noneActions
      } ->
        object
          [ "type" .= ("none" :: Text),
            "id" .= id,
            "actions" .= (mkPause <$> noneActions)
          ]
    Key {id, keyActions} ->
      object
        [ "id" .= id,
          "type" .= ("key" :: Text),
          "actions" .= keyActions
        ]
    Pointer
      { subType,
        actions,
        pointerId,
        pressed,
        id,
        x,
        y
      } ->
        object
          [ "id" .= id,
            "type" .= ("pointer" :: Text),
            "subType" .= subType,
            "pointerId" .= pointerId,
            "pressed" .= pressed,
            "x" .= x,
            "y" .= y,
            "actions" .= actions
          ]
    Wheel {id, wheelActions} ->
      object
        [ "id" .= id,
          "type" .= ("wheel" :: Text),
          "actions" .= wheelActions
        ]
