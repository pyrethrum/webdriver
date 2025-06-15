module E2EConst
  ( ReqRequestParams (..),
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
    httpCapabilities,
    httpFullCapabilities,
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
  )
where

import Config
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
import WebDriverPreCore.Http (BrowserName (..), Capabilities (..), FullCapabilities (..), Selector (CSS, XPath), VendorSpecific (..))
import Prelude (Int, Maybe (..), Num (..), ($), (<>))

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

-- ################### capabilities ##################

httpCapabilities :: Config -> Capabilities
httpCapabilities MkConfig {useFirefox, firefoxHeadless, customFirefoxProfilePath} =
  MkCapabilities
    { browserName = Just $ if useFirefox then Firefox else Chrome,
      browserVersion = Nothing,
      platformName = Nothing,
      acceptInsecureCerts = Nothing,
      pageLoadStrategy = Nothing,
      proxy = Nothing,
      setWindowRect = Nothing,
      timeouts = Nothing,
      strictFileInteractability = Nothing,
      unhandledPromptBehavior = Nothing,
      webSocketUrl = Nothing,
      vendorSpecific =
        if useFirefox
          then
            Just $
              FirefoxOptions
                { firefoxArgs = if firefoxHeadless then Just ["--headless"] else Nothing,
                  firefoxBinary = Nothing,
                  firefoxProfile = customFirefoxProfilePath,
                  firefoxLog = Nothing
                }
          else Nothing
    }

httpFullCapabilities :: Config ->  FullCapabilities
httpFullCapabilities cfg =
  MkFullCapabilities
    { alwaysMatch =
        Just $ httpCapabilities cfg,
      firstMatch = []
    }
