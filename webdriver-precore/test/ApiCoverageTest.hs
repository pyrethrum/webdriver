module ApiCoverageTest where

import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (drop, (!!))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import Data.Semigroup ((<>))
import Data.Set as S (Set, difference, fromList, null)
import Data.Text as T (Text, intercalate, lines, null, pack, replace, strip, unwords, words)
import GHC.Show (Show (..))
import GHC.Utils.Misc (filterOut)
import Test.Tasty.HUnit as HUnit (Assertion, assertBool)
import Text.RawString.QQ (r)
import  WebDriverPreCore.Internal.Utils (UrlPath(..))
import WebDriverPreCore.Http
import WebDriverPreCore.Http.Protocol (ElementId (..), FrameReference (..), Handle (..), Script (..), Selector (..), SessionId (..), WindowRect (..), URL(..))
import WebDriverPreCore.Http.Protocol (Cookie(..))
import WebDriverPreCore.Http.Protocol (Actions(..))
import WebDriverPreCore.Http.Protocol (ShadowRootElementId)
import WebDriverPreCore.Http.Protocol (ShadowRootElementId(..))

{-- TODO use Haddock variable
 Covers Spec Version https://www.w3.org/TR/2025/WD-webdriver2-20251028
 --}

{-
!! Replace this the endepoints from the spec with every release
https://www.w3.org/TR/2025/WD-webdriver2-20251028 - W3C Editor's Draft 10 February 2025
61 endpoints
Method 	URI Template 	Command
POST 	/session 	New Session
-}
endPointsCopiedFromSpc :: Text
endPointsCopiedFromSpc =
  pack
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
element = MkElement elementId

shadowDOMElement :: ShadowRootElementId
shadowDOMElement = MkShadowRootElementId elementId

windowHandle :: Handle
windowHandle = MkHandle "window-handle"

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

toSpecLine :: HttpSpec a -> SpecLine
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
  fromList $
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
      toSpecLine $ navigateTo session $ MkUrl "url",
      toSpecLine $ findElement session selector,
      toSpecLine $ getWindowRect session,
      toSpecLine $ elementClick session element,
      toSpecLine $ getElementText session element,
      toSpecLine $ switchToParentFrame session,
      toSpecLine $ getElementProperty session element name,
      toSpecLine $ getElementAttribute session element name,
      toSpecLine $ getElementCssValue session element name,
      toSpecLine $ setWindowRect session (Rect 0 0 1280 720),
      toSpecLine $ findElementsFromShadowRoot session shadowDOMElement selector,
      toSpecLine $ getElementShadowRoot session element,
      toSpecLine $ findElementFromShadowRoot session shadowDOMElement selector,
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
      toSpecLine $ executeScript session $ MkScript "console.log('test');" [],
      toSpecLine $ executeScriptAsync session $ MkScript "console.log('test');" [],
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