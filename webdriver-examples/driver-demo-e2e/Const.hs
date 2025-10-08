module Const
  ( ReqRequestParams (..),
    Timeout (..),
    theInternet,
    subDomain,
    alertsUrl,
    infiniteScrollUrl,
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
    defaultRequest,
    millisecond,
    milliseconds
  )
where

import Data.Text (Text)
import Network.HTTP.Req as R
  ( GET (GET),
    HttpBody,
    HttpBodyAllowed,
    HttpMethod (AllowsBody),
    NoReqBody (NoReqBody),
    ProvidesBody,
    Scheme (..),
    Url,
    http,
  )
import WebDriverPreCore.Http as WPC
  ( Selector (..),
  )
import Prelude (Eq, Int, Num (..), Semigroup (..), Show, (.))

-- ################### urls ##################

theInternet :: Text
theInternet = "https://the-internet.herokuapp.com/"

subDomain :: Text -> Text
subDomain sd = theInternet <> sd

alertsUrl :: Text
alertsUrl = subDomain "javascript_alerts"

infiniteScrollUrl :: Text
infiniteScrollUrl = subDomain "infinite_scroll"

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

jsAlertXPath :: Selector
jsAlertXPath = XPath "//button[text()='Click for JS Alert']"

jsPromptXPath :: Selector
jsPromptXPath = XPath "//button[text()='Click for JS Prompt']"

divCss :: Selector
divCss = CSS "div"

-- ################### time ##################

newtype Timeout = MkTimeout {timeoutMs :: Int} 
  deriving (Show, Eq)
  deriving newtype (Num)

millisecond :: Int -> Timeout
millisecond = MkTimeout

milliseconds :: Int -> Timeout
milliseconds = millisecond

second :: Int -> Timeout
second = MkTimeout . (*) 1_000

seconds :: Int -> Timeout
seconds = second

minute :: Int -> Timeout
minute = MkTimeout . (*) 60_000

minutes :: Int -> Timeout
minutes = minute

hour :: Int -> Timeout
hour = MkTimeout . (*) 3_600_000

hours :: Int -> Timeout
hours = hour

-- ################### request ##################

data ReqRequestParams where
  MkRequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { url :: Url 'Http,
      method :: method,
      body :: body,
      port :: Int
    } ->
    ReqRequestParams

defaultRequest :: ReqRequestParams
defaultRequest =
  MkRequestParams
    { url = http "127.0.0.1",
      method = GET,
      body = NoReqBody,
      port = 4444
    }

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
