module E2EConst (
  theInternet,
  subDomain,
  alertsUrl,
  infinitScrollUrl,
  framesUrl,
  inputsUrl,
  loginUrl,
  checkBoxesUrl,
  shadowDomUrl,
  checkBoxesLinkCss,
  checkBoxesCss,
  topFrameCSS,
  midFrameCss,
  bottomFrameCss,
  jsAlertXPath,
  jsPromptXPath,
  divCss,
  midFrameTitle,
  userNameCss,
  contentCss,
  inputTagCss,
  h3TagCss,
  anyElmCss,
  second,
  seconds,
  minute,
  minutes,
  hour,
  hours,
  RequestArgs(..),
  defaultRequest
) where

import WebDriverPreCore.Spec (Selector (CSS, XPath))
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))
import Data.Int (Int)
import GHC.Num((*))

import Network.HTTP.Req as R
  ( GET (GET),
    HttpBody,
    HttpBodyAllowed,
    HttpMethod (AllowsBody),
    NoReqBody (NoReqBody),
    ProvidesBody,
  )


-- ################### urls ##################

theInternet :: Text
theInternet = "https://the-internet.herokuapp.com/"

subDomain :: Text -> Text
subDomain sd = theInternet <> sd

alertsUrl :: Text
alertsUrl = subDomain "javascript_alerts"

infinitScrollUrl :: Text
infinitScrollUrl = subDomain "infinite_scroll"

framesUrl :: Text
framesUrl = subDomain "nested_frames"

inputsUrl :: Text
inputsUrl = subDomain "inputs"

loginUrl :: Text
loginUrl = subDomain "login"

checkBoxesUrl :: Text
checkBoxesUrl = subDomain "checkboxes"

shadowDomUrl :: Text
shadowDomUrl = subDomain "shadowdom"

-- ################### selectors  ##################

checkBoxesLinkCss :: Selector
checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

checkBoxesCss :: Selector
checkBoxesCss = CSS "input[type='checkbox']"

topFrameCSS :: Selector
topFrameCSS = CSS "frame[name='frame-top']"

midFrameCss :: Selector
midFrameCss = CSS "frame[name='frame-middle']"

bottomFrameCss :: Selector
bottomFrameCss = CSS "frame[name='frame-bottom']"

jsAlertXPath  :: Selector
jsAlertXPath  = XPath "//button[text()='Click for JS Alert']"

jsPromptXPath :: Selector
jsPromptXPath = XPath "//button[text()='Click for JS Prompt']"

divCss :: Selector
divCss = CSS "div"

midFrameTitle :: Selector
midFrameTitle = CSS "#content"

userNameCss :: Selector
userNameCss = CSS "#username"

contentCss :: Selector
contentCss = CSS "#content"

inputTagCss :: Selector
inputTagCss = CSS "input"

h3TagCss :: Selector
h3TagCss = CSS "h3"

anyElmCss :: Selector
anyElmCss = CSS "*"


-- ################### time ##################


second :: Int
second = 1_000

seconds :: Int
seconds = second

minute :: Int
minute = 60 * seconds

minutes :: Int
minutes = minute

hour :: Int
hour = 60 * minutes

hours :: Int
hours = hour

-- ################### request ##################

data RequestArgs where
  RequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { subDirs :: [Text],
      method :: method,
      body :: body,
      port :: Int
    } ->
    RequestArgs
  

defaultRequest :: RequestArgs
defaultRequest = RequestParams [] GET NoReqBody 4444




