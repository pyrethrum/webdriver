module ApiCoverageTest where

import Data.Set as S (Set, difference, fromList, null)
import Data.Text as T (Text, lines, null, pack, replace, strip, unwords, words, split, intercalate)
import GHC.Utils.Misc (filterOut)
import Test.Tasty.HUnit as HUnit ( assertBool, Assertion, (@=?) )
import Text.RawString.QQ (r)
import WebDriverPreCore.Spec
    ( SessionId(Session),
      ElementId(Element),
      Selector(CSS),
      WindowHandle(Handle),
      WindowRect(Rect),
      FrameReference(TopLevelFrame),
      Cookie(MkCookie),
      Actions(MkActions),
      Timeouts(..),
      W3Spec(description, Get, Post, PostEmpty, Delete, path),
      errorTypeToErrorCode,
      minFirefoxCapabilities,
      errorDescription,
      acceptAlert,
      addCookie,
      back,
      closeWindow,
      deleteAllCookies,
      deleteCookie,
      deleteSession,
      dismissAlert,
      elementClear,
      elementClick,
      elementSendKeys,
      errorCodeToErrorType,
      executeScript,
      executeScriptAsync,
      findElement,
      findElementFromElement,
      findElementFromShadowRoot,
      findElements,
      findElementsFromElement,
      findElementsFromShadowRoot,
      forward,
      fullscreenWindow,
      getActiveElement,
      getAlertText,
      getAllCookies,
      getCurrentUrl,
      getElementAttribute,
      getElementComputedLabel,
      getElementComputedRole,
      getElementCssValue,
      getElementProperty,
      getElementRect,
      getElementShadowRoot,
      getElementTagName,
      getElementText,
      getNamedCookie,
      getPageSource,
      getTimeouts,
      getTitle,
      getWindowHandle,
      getWindowHandles,
      getWindowRect,
      isElementEnabled,
      isElementSelected,
      maximizeWindow,
      minimizeWindow,
      navigateTo,
      newSession,
      newWindow,
      performActions,
      printPage,
      refresh,
      releaseActions,
      sendAlertText,
      setTimeouts,
      setWindowRect,
      status,
      switchToFrame,
      switchToParentFrame,
      switchToWindow,
      takeElementScreenshot,
      takeScreenshot )
import GHC.Show (Show (..))
import Data.Eq (Eq ((==)))
import Data.Ord (Ord)
import Data.Function (($), (.), (&))
import Data.Semigroup ((<>))
import Data.List ((!!), drop)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import GHC.Base (error)
import WebDriverPreCore.Internal.Utils (enumerate)
import Data.Foldable (traverse_)
import Data.Either (either)
import WebDriverPreCore.Spec (UrlPath(..))


{-- TODO use Haddock variable
 Covers Spec Version https://www.w3.org/TR/2025/WD-webdriver2-20250210 
 --}


{-
!! Replace this the endepoints from the spec with every release
https://www.w3.org/TR/2025/WD-webdriver2-20250210 - W3C Editor's Draft 10 February 2025
61 endpoints
Method 	URI Template 	Command
POST 	/session 	New Session
-}
endPointsCopiedFromSpc :: Text
endPointsCopiedFromSpc = pack 
  [r|POST 	/session 	New Session
DELETE 	/session/{session id} 	Delete Session
GET 	/status 	Status
GET 	/session/{session id}/timeouts 	Get Timeouts
POST 	/session/{session id}/timeouts 	Set Timeouts
POST 	/session/{session id}/url 	Navigate To
GET 	/session/{session id}/url 	Get Current URL
POST 	/session/{session id}/back 	Back
POST 	/session/{session id}/forward 	Forward
POST 	/session/{session id}/refresh 	Refresh
GET 	/session/{session id}/title 	Get Title
GET 	/session/{session id}/window 	Get Window Handle
DELETE 	/session/{session id}/window 	Close Window
POST 	/session/{session id}/window 	Switch To Window
GET 	/session/{session id}/window/handles 	Get Window Handles
POST 	/session/{session id}/window/new 	New Window
POST 	/session/{session id}/frame 	Switch To Frame
POST 	/session/{session id}/frame/parent 	Switch To Parent Frame
GET 	/session/{session id}/window/rect 	Get Window Rect
POST 	/session/{session id}/window/rect 	Set Window Rect
POST 	/session/{session id}/window/maximize 	Maximize Window
POST 	/session/{session id}/window/minimize 	Minimize Window
POST 	/session/{session id}/window/fullscreen 	Fullscreen Window
GET 	/session/{session id}/element/active 	Get Active Element
GET 	/session/{session id}/element/{element id}/shadow 	Get Element Shadow Root
POST 	/session/{session id}/element 	Find Element
POST 	/session/{session id}/elements 	Find Elements
POST 	/session/{session id}/element/{element id}/element 	Find Element From Element
POST 	/session/{session id}/element/{element id}/elements 	Find Elements From Element
POST 	/session/{session id}/shadow/{shadow id}/element 	Find Element From Shadow Root
POST 	/session/{session id}/shadow/{shadow id}/elements 	Find Elements From Shadow Root
GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute
GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
GET 	/session/{session id}/element/{element id}/text 	Get Element Text
GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
POST 	/session/{session id}/element/{element id}/click 	Element Click
POST 	/session/{session id}/element/{element id}/clear 	Element Clear
POST 	/session/{session id}/element/{element id}/value 	Element Send Keys
GET 	/session/{session id}/source 	Get Page Source
POST 	/session/{session id}/execute/sync 	Execute Script
POST 	/session/{session id}/execute/async 	Execute Async Script
GET 	/session/{session id}/cookie 	Get All Cookies
GET 	/session/{session id}/cookie/{name} 	Get Named Cookie
POST 	/session/{session id}/cookie 	Add Cookie
DELETE 	/session/{session id}/cookie/{name} 	Delete Cookie
DELETE 	/session/{session id}/cookie 	Delete All Cookies
POST 	/session/{session id}/actions 	Perform Actions
DELETE 	/session/{session id}/actions 	Release Actions
POST 	/session/{session id}/alert/dismiss 	Dismiss Alert
POST 	/session/{session id}/alert/accept 	Accept Alert
GET 	/session/{session id}/alert/text 	Get Alert Text
POST 	/session/{session id}/alert/text 	Send Alert Text
GET 	/session/{session id}/screenshot 	Take Screenshot
GET 	/session/{session id}/element/{element id}/screenshot 	Take Element Screenshot
POST 	/session/{session id}/print   Print Page
|]

parseLine :: Text -> SpecLine
parseLine line = MkSpecLine method uriTemplate command
  where
    segments = filterOut T.null $ T.words $ replaceTemplateTxt line
    method = case segments of
      [] -> "PARSER ERROR NO METHOD IN LINE" <> line
      x : _ -> x
    uriTemplate = segments !! 1
    command = T.unwords $ drop 2 segments

specLinesFromSpec :: Set SpecLine
specLinesFromSpec = fromList $ parseLine <$> filterOut T.null (strip <$> T.lines endPointsCopiedFromSpc)

sessionId :: Text
sessionId = "session_id"

session :: SessionId
session = Session sessionId

elementId :: Text
elementId = "element_id"

element :: ElementId
element = Element elementId

windowHandle :: WindowHandle
windowHandle = Handle "window-handle"

selector :: Selector
selector = CSS "Blahh"

name :: Text
name = "name"

data SpecLine = MkSpecLine
  { method :: Text,
    uriTemplate :: Text,
    command :: Text
  }
  deriving (Show, Eq, Ord)

replaceTemplateTxt :: Text -> Text
replaceTemplateTxt =
  replace "{property name}" name
    . replace "{name}" name
    . replace "{session id}" sessionId
    . replace "{element id}" elementId
    . replace "{shadow id}" elementId

toSpecLine :: W3Spec a -> SpecLine
toSpecLine w3 = case w3 of
  Get {} -> MkSpecLine "GET" path command
  Post {} -> MkSpecLine "POST" path command
  PostEmpty {} -> MkSpecLine "POST" path command
  Delete {} -> MkSpecLine "DELETE" path command
  where
    command = w3.description
    path = replaceTemplateTxt $ "/" <> intercalate "/" w3.path.segments

allSpecsSample :: Set SpecLine
allSpecsSample =
  fromList
    [ toSpecLine $ newSession minFirefoxCapabilities,
      toSpecLine status,
      toSpecLine $ maximizeWindow session,
      toSpecLine $ minimizeWindow session,
      toSpecLine $ fullscreenWindow session,
      toSpecLine $ getTimeouts session,
      toSpecLine $ setTimeouts session $ MkTimeouts Nothing Nothing Nothing,
      toSpecLine $ switchToFrame session TopLevelFrame,
      toSpecLine $ getCurrentUrl session,
      toSpecLine $ findElementFromElement session element selector,
      toSpecLine $ findElementsFromElement session element selector,
      toSpecLine $ findElements session selector,
      toSpecLine $ getTitle session,
      toSpecLine $ getWindowHandle session,
      toSpecLine $ isElementSelected session element,
      toSpecLine $ closeWindow session,
      toSpecLine $ back session,
      toSpecLine $ forward session,
      toSpecLine $ refresh session,
      toSpecLine $ newSession minFirefoxCapabilities,
      toSpecLine $ deleteSession session,
      toSpecLine $ getActiveElement session,
      toSpecLine $ getWindowHandles session,
      toSpecLine $ newWindow session,
      toSpecLine $ switchToWindow session windowHandle,
      toSpecLine $ navigateTo session "url",
      toSpecLine $ findElement session selector,
      toSpecLine $ getWindowRect session,
      toSpecLine $ elementClick session element,
      toSpecLine $ getElementText session element,
      toSpecLine $ switchToParentFrame session,
      toSpecLine $ getElementProperty session element name,
      toSpecLine $ getElementAttribute session element name,
      toSpecLine $ getElementCssValue session element name,
      toSpecLine $ setWindowRect session (Rect 0 0 1280 720),
      toSpecLine $ findElementsFromShadowRoot session element selector,
      toSpecLine $ getElementShadowRoot session element,
      toSpecLine $ findElementFromShadowRoot session element selector,
      toSpecLine $ getElementTagName session element,
      toSpecLine $ getElementRect session element,
      toSpecLine $ isElementEnabled session element,
      toSpecLine $ getElementComputedRole session element,
      toSpecLine $ getElementComputedLabel session element,
      toSpecLine $ elementClear session element,
      toSpecLine $ elementSendKeys session element "Some keys",
      toSpecLine $ getPageSource session,
      toSpecLine $ takeScreenshot session,
      toSpecLine $ takeElementScreenshot session element,
      toSpecLine $ printPage session,
      toSpecLine $ executeScript session "console.log('test');" [],
      toSpecLine $ executeScriptAsync session "console.log('test');" [],
      toSpecLine $ getAllCookies session,
      toSpecLine $ getNamedCookie session name,
      toSpecLine $ addCookie session $ MkCookie "testCookie" "testValue" Nothing Nothing Nothing Nothing Nothing Nothing,
      toSpecLine $ deleteCookie session name,
      toSpecLine $ deleteAllCookies session,
      toSpecLine $ dismissAlert session,
      toSpecLine $ acceptAlert session,
      toSpecLine $ getAlertText session,
      toSpecLine $ sendAlertText session "Test alert",
      toSpecLine $ performActions session $ MkActions [],
      toSpecLine $ releaseActions session
    ]

-- >>> unit_test_all_endpoints_covered
unit_test_all_endpoints_covered :: Assertion
unit_test_all_endpoints_covered = do
  -- print allSpecsSample
  -- putStrLn ""
  assertBool ("Missing specs:\n " <> show missing) (S.null missing)
  assertBool ("Extra specs:\n " <> show extra) (S.null extra)
  where
    missing = specLinesFromSpec `difference` allSpecsSample
    extra = allSpecsSample `difference` specLinesFromSpec

{-
!! Replace this the endepoints from the spec with every release
!! remove full stops and replace tabs with " | "
https://www.w3.org/TR/2025/WD-webdriver2-20250210 - W3C Editor's Draft 10 February 2025
61 endpoints
Error Code 	HTTP Status 	JSON Error Code 	Description 
-}
errorsFromSpec :: Text
errorsFromSpec = pack
  [r|element click intercepted  | 400  | element click intercepted  | The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked
element not interactable  | 400  | element not interactable  | A command could not be completed because the element is not pointer- or keyboard interactable
insecure certificate  | 400  | insecure certificate  | Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate
invalid argument  | 400  | invalid argument  | The arguments passed to a command are either invalid or malformed
invalid cookie domain  | 400  | invalid cookie domain  | An illegal attempt was made to set a cookie under a different domain than the current page
invalid element state  | 400  | invalid element state  | A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable
invalid selector  | 400  | invalid selector  | Argument was an invalid selector
invalid session id  | 404  | invalid session id  | Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active
javascript error  | 500  | javascript error  | An error occurred while executing JavaScript supplied by the user
move target out of bounds  | 500  | move target out of bounds  | The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport
no such alert  | 404  | no such alert  | An attempt was made to operate on a modal dialog when one was not open
no such cookie  | 404  | no such cookie  | No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document
no such element  | 404  | no such element  | An element could not be located on the page using the given search parameters
no such frame  | 404  | no such frame  | A command to switch to a frame could not be satisfied because the frame could not be found
no such window  | 404  | no such window  | A command to switch to a window could not be satisfied because the window could not be found
no such shadow root  | 404  | no such shadow root  | The element does not have a shadow root
script timeout error  | 500  | script timeout  | A script did not complete before its timeout expired
session not created  | 500  | session not created  | A new session could not be created
stale element reference  | 404  | stale element reference  | A command failed because the referenced element is no longer attached to the DOM
detached shadow root  | 404  | detached shadow root  | A command failed because the referenced shadow root is no longer attached to the DOM
timeout  | 500  | timeout  | An operation did not complete before its timeout expired
unable to set cookie  | 500  | unable to set cookie  | A command to set a cookie's value could not be satisfied
unable to capture screen  | 500  | unable to capture screen  | A screen capture was made impossible
unexpected alert open  | 500  | unexpected alert open  | A modal dialog was open, blocking this operation
unknown command  | 404  | unknown command  | A command could not be executed because the remote end is not aware of it
unknown error  | 500  | unknown error  | An unknown error occurred in the remote end while processing the command
unknown method  | 405  | unknown method  | The requested command matched a known URL but did not match any method for that URL
unsupported operation  | 500  | unsupported operation  | Indicates that a command that should have executed properly cannot be supported for some reason 
|]

data ErrorLine = MkErrorLine
  { 
    jsonErrorCode :: Text,
    description :: Text
  }
  deriving (Show, Eq, Ord)

-- >>> allErrorsFromSpec
-- fromList [MkErrorLine {jsonErrorCode = "detached shadow root", description = "A command failed because the referenced shadow root is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "element click intercepted", description = "The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked"},MkErrorLine {jsonErrorCode = "element not interactable", description = "A command could not be completed because the element is not pointer- or keyboard interactable"},MkErrorLine {jsonErrorCode = "insecure certificate", description = "Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate"},MkErrorLine {jsonErrorCode = "invalid argument", description = "The arguments passed to a command are either invalid or malformed"},MkErrorLine {jsonErrorCode = "invalid cookie domain", description = "An illegal attempt was made to set a cookie under a different domain than the current page"},MkErrorLine {jsonErrorCode = "invalid element state", description = "A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable"},MkErrorLine {jsonErrorCode = "invalid selector", description = "Argument was an invalid selector"},MkErrorLine {jsonErrorCode = "invalid session id", description = "Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active"},MkErrorLine {jsonErrorCode = "javascript error", description = "An error occurred while executing JavaScript supplied by the user"},MkErrorLine {jsonErrorCode = "move target out of bounds", description = "The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport"},MkErrorLine {jsonErrorCode = "no such alert", description = "An attempt was made to operate on a modal dialog when one was not open"},MkErrorLine {jsonErrorCode = "no such cookie", description = "No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document"},MkErrorLine {jsonErrorCode = "no such element", description = "An element could not be located on the page using the given search parameters"},MkErrorLine {jsonErrorCode = "no such frame", description = "A command to switch to a frame could not be satisfied because the frame could not be found"},MkErrorLine {jsonErrorCode = "no such shadow root", description = "The element does not have a shadow root"},MkErrorLine {jsonErrorCode = "no such window", description = "A command to switch to a window could not be satisfied because the window could not be found"},MkErrorLine {jsonErrorCode = "script timeout", description = "A script did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "session not created", description = "A new session could not be created"},MkErrorLine {jsonErrorCode = "stale element reference", description = "A command failed because the referenced element is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "timeout", description = "An operation did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "unable to capture screen", description = "A screen capture was made impossible"},MkErrorLine {jsonErrorCode = "unable to set cookie", description = "A command to set a cookie's value could not be satisfied"},MkErrorLine {jsonErrorCode = "unexpected alert open", description = "A modal dialog was open, blocking this operation"},MkErrorLine {jsonErrorCode = "unknown command", description = "A command could not be executed because the remote end is not aware of it"},MkErrorLine {jsonErrorCode = "unknown error", description = "An unknown error occurred in the remote end while processing the command"},MkErrorLine {jsonErrorCode = "unknown method", description = "The requested command matched a known URL but did not match any method for that URL"},MkErrorLine {jsonErrorCode = "unsupported operation", description = "Indicates that a command that should have executed properly cannot be supported for some reason"}]
allErrorsFromSpec :: Set ErrorLine
allErrorsFromSpec = fromList $ parseErrorLine <$> filterOut T.null (T.lines errorsFromSpec)
 where
  parseErrorLine :: Text -> ErrorLine
  parseErrorLine line = 
    T.split (== '|') line & \case
      [_errrorCode, _httpStatus , jsonErrorCode, description] -> MkErrorLine (strip jsonErrorCode) $ strip description
      _ -> error $ "Error parsing line: " <> show line

-- >>> allErrors
-- fromList [MkErrorLine {jsonErrorCode = "detached shadow root", description = "A command failed because the referenced shadow root is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "element click intercepted", description = "The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked"},MkErrorLine {jsonErrorCode = "element not interactable", description = "A command could not be completed because the element is not pointer- or keyboard interactable"},MkErrorLine {jsonErrorCode = "insecure certificate", description = "Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate"},MkErrorLine {jsonErrorCode = "invalid argument", description = "The arguments passed to a command are either invalid or malformed"},MkErrorLine {jsonErrorCode = "invalid cookie domain", description = "An illegal attempt was made to set a cookie under a different domain than the current page"},MkErrorLine {jsonErrorCode = "invalid element state", description = "A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable"},MkErrorLine {jsonErrorCode = "invalid selector", description = "Argument was an invalid selector"},MkErrorLine {jsonErrorCode = "invalid session id", description = "Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active"},MkErrorLine {jsonErrorCode = "javascript error", description = "An error occurred while executing JavaScript supplied by the user"},MkErrorLine {jsonErrorCode = "move target out of bounds", description = "The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport"},MkErrorLine {jsonErrorCode = "no such alert", description = "An attempt was made to operate on a modal dialog when one was not open"},MkErrorLine {jsonErrorCode = "no such cookie", description = "No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document"},MkErrorLine {jsonErrorCode = "no such element", description = "An element could not be located on the page using the given search parameters"},MkErrorLine {jsonErrorCode = "no such frame", description = "A command to switch to a frame could not be satisfied because the frame could not be found"},MkErrorLine {jsonErrorCode = "no such shadow root", description = "The element does not have a shadow root"},MkErrorLine {jsonErrorCode = "no such window", description = "A command to switch to a window could not be satisfied because the window could not be found"},MkErrorLine {jsonErrorCode = "script timeout", description = "A script did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "session not created", description = "A new session could not be created"},MkErrorLine {jsonErrorCode = "stale element reference", description = "A command failed because the referenced element is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "timeout", description = "An operation did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "unable to capture screen", description = "A screen capture was made impossible"},MkErrorLine {jsonErrorCode = "unable to set cookie", description = "A command to set a cookie's value could not be satisfied"},MkErrorLine {jsonErrorCode = "unexpected alert open", description = "A modal dialog was open, blocking this operation"},MkErrorLine {jsonErrorCode = "unknown command", description = "A command could not be executed because the remote end is not aware of it"},MkErrorLine {jsonErrorCode = "unknown error", description = "An unknown error occurred in the remote end while processing the command"},MkErrorLine {jsonErrorCode = "unknown method", description = "The requested command matched a known URL but did not match any method for that URL"},MkErrorLine {jsonErrorCode = "unsupported operation", description = "Indicates that a command that should have executed properly cannot be supported for some reason"}]
allErrors :: Set ErrorLine
allErrors = fromList $
 (\et -> MkErrorLine { 
    jsonErrorCode  = errorTypeToErrorCode et,
    description = errorDescription et
  })  <$> enumerate

-- >>> unit_test_all_errors_covered
unit_test_all_errors_covered :: Assertion
unit_test_all_errors_covered = do
  -- print allErrors
  -- putStrLn ""
  assertBool ("Missing errors (in spec not captured by WebDriverErrorType):\n " <> show missing) (S.null missing)
  assertBool ("Extra errors (in WebDriverErrorType but not in spec):\n " <> show extra) (S.null extra)
  where
    missing = allErrors `difference` allErrorsFromSpec
    extra = allErrorsFromSpec `difference` allErrors


-- >>> unit_round_trip_error_codes
unit_round_trip_error_codes :: Assertion
unit_round_trip_error_codes = do
  traverse_ checkRoundTripErrorCodes enumerate
  where
    checkRoundTripErrorCodes errorType = do
      let errorCode = errorTypeToErrorCode errorType
          errorType' = errorCodeToErrorType errorCode
      errorType' & either (error . show) (errorType @=?)