module IORunner
  ( 
    W.Cookie (..),
    W.Capabilities(..),
    W.FullCapabilities(..), 
    W.VendorSpecific(..),
    W.DriverStatus(..),
    W.Timeouts (..),
    W.WindowHandleSpec (..),
    W.WindowHandle(..),
    W.SameSite (..),
    W.Selector (..),
    W.SessionId (..),
    W.FrameReference (..),
    W.WindowRect (..),
    W.PointerOrigin (..),
    W.Action (..),
    W.Actions (..),
    W.KeyAction (..),
    W.Pointer (..),
    W.PointerAction (..),
    W.WheelAction (..),
    W.errorCodeToErrorType,
    W.minFirefoxCapabilities,
    W.minStandardCapabilities,
    W.BrowserName(..),
    encodeFileToBase64,
    status,
    findElementFromElement,
    findElementsFromElement,
    findElements,
    getTimeouts,
    setTimeouts,
    back,
    forward,
    getActiveElement,
    refresh,
    getCurrentUrl,
    getElementAttribute,
    getElementShadowRoot,
    findElementFromShadowRoot,
    findElementsFromShadowRoot,
    getTitle,
    getWindowHandles,
    isElementSelected,
    maximizeWindow,
    minimizeWindow,
    fullScreenWindow,
    getWindowHandle,
    getWindowRect,
    closeWindow,
    newWindow,
    newSession,
    minFirefoxSession,
    performActions,
    releaseActions,
    deleteSession,
    navigateTo,
    findElement,
    elementClick,
    getElementText,
    setWindowRect,
    sleepMs,
    switchToWindow,
    switchToFrame,
    switchToParentFrame,
    getElementProperty,
    getElementCssValue,
    getElementTagName,
    getElementRect,
    isElementEnabled,
    getElementComputedRole,
    getElementComputedLabel,
    elementClear,
    elementSendKeys,
    printPage,
    getPageSource,
    takeScreenshot,
    takeElementScreenshot,
    executeScript,
    executeScriptAsync,
    getAllCookies,
    getNamedCookie,
    addCookie,
    deleteCookie,
    deleteAllCookies,
    dismissAlert,
    acceptAlert,
    getAlertText,
    sendAlertText
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (..), Value, object)

import Data.Function ((&))
import Data.Text  as T (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.IO qualified as T
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (Http),
    Url,
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    responseBody,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    HttpConfig (httpConfigCheckResponse), (/:),
  )
import WebDriverPreCore.Internal.Utils (txt, prettyPrintJson)
import E2EConst (RequestArgs (..))
import WebDriverPreCore.Spec (DriverStatus, ElementId, HttpResponse (..), Selector, SessionId, W3Spec (..), parseWebDriverError, ErrorClassification (..))
import WebDriverPreCore.Spec qualified as W
import Prelude hiding (log)
import Network.HTTP.Req (JsonResponse)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy qualified as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Base64.Types as B64T

wantConsoleLogging :: Bool
wantConsoleLogging = True

-- ############# IO Implementation #############

status :: IO DriverStatus
status = run W.status

newSession :: W.FullCapabilities -> IO SessionId
newSession = run . W.newSession

getTimeouts :: SessionId -> IO W.Timeouts
getTimeouts = run . W.getTimeouts

setTimeouts :: SessionId -> W.Timeouts -> IO ()
setTimeouts s = run . W.setTimeouts s

getCurrentUrl :: SessionId -> IO Text
getCurrentUrl = run . W.getCurrentUrl

getTitle :: SessionId -> IO Text
getTitle = run . W.getTitle

maximizeWindow :: SessionId -> IO W.WindowRect
maximizeWindow = run . W.maximizeWindow

minimizeWindow :: SessionId -> IO W.WindowRect
minimizeWindow = run . W.minimizeWindow

fullScreenWindow :: SessionId -> IO W.WindowRect
fullScreenWindow = run . W.fullscreenWindow

getWindowHandle :: SessionId -> IO W.WindowHandle
getWindowHandle = run . W.getWindowHandle

getWindowRect :: SessionId -> IO W.WindowRect
getWindowRect = run . W.getWindowRect

getWindowHandles :: SessionId -> IO [W.WindowHandle]
getWindowHandles = run . W.getWindowHandles

newWindow :: SessionId -> IO W.WindowHandleSpec
newWindow = run . W.newWindow

switchToWindow :: SessionId -> W.WindowHandle -> IO ()
switchToWindow s = run . W.switchToWindow s

switchToFrame :: SessionId -> W.FrameReference -> IO ()
switchToFrame s = run . W.switchToFrame s

switchToParentFrame :: SessionId -> IO ()
switchToParentFrame = run . W.switchToParentFrame

closeWindow :: SessionId -> IO ()
closeWindow = run . W.closeWindow

back :: SessionId -> IO ()
back = run . W.back

forward :: SessionId -> IO ()
forward = run . W.forward

refresh :: SessionId -> IO ()
refresh = run . W.refresh

setWindowRect :: SessionId -> W.WindowRect -> IO W.WindowRect
setWindowRect s = run . W.setWindowRect s

minFirefoxSession :: IO SessionId
minFirefoxSession = newSession W.minFirefoxCapabilities

deleteSession :: SessionId -> IO ()
deleteSession = run . W.deleteSession

navigateTo :: SessionId -> Text -> IO ()
navigateTo s = run . W.navigateTo s

findElement :: SessionId -> Selector -> IO ElementId
findElement s = run . W.findElement s

findElementFromElement :: SessionId -> ElementId -> Selector -> IO ElementId
findElementFromElement s eid = run . W.findElementFromElement s eid

findElementsFromElement :: SessionId -> ElementId -> Selector -> IO [ElementId]
findElementsFromElement s eid = run . W.findElementsFromElement s eid

getActiveElement :: SessionId -> IO ElementId
getActiveElement = run . W.getActiveElement

isElementSelected :: SessionId -> ElementId -> IO Bool
isElementSelected s = run . W.isElementSelected s

getElementShadowRoot :: SessionId -> ElementId -> IO ElementId
getElementShadowRoot s = run . W.getElementShadowRoot s

findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> IO ElementId
findElementFromShadowRoot s e = run . W.findElementFromShadowRoot s e

getElementTagName :: SessionId -> ElementId -> IO Text
getElementTagName s = run . W.getElementTagName s

getElementRect :: SessionId -> ElementId -> IO W.WindowRect
getElementRect s = run . W.getElementRect s

isElementEnabled :: SessionId -> ElementId -> IO Bool
isElementEnabled s = run . W.isElementEnabled s

getElementComputedRole :: SessionId -> ElementId -> IO Text
getElementComputedRole s = run . W.getElementComputedRole s

getElementComputedLabel :: SessionId -> ElementId -> IO Text
getElementComputedLabel s = run . W.getElementComputedLabel s

findElements :: SessionId -> Selector -> IO [ElementId]
findElements s = run . W.findElements s

findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> IO [ElementId]
findElementsFromShadowRoot s e = run . W.findElementsFromShadowRoot s e

elementClick :: SessionId -> ElementId -> IO ()
elementClick s = run . W.elementClick s

getElementText :: SessionId -> ElementId -> IO Text
getElementText s = run . W.getElementText s

getElementProperty :: SessionId -> ElementId -> Text -> IO Value
getElementProperty s eid = run . W.getElementProperty s eid

getElementAttribute :: SessionId -> ElementId -> Text -> IO Text
getElementAttribute s eid = run . W.getElementAttribute s eid

getElementCssValue :: SessionId -> ElementId -> Text -> IO Text
getElementCssValue s eid = run . W.getElementCssValue s eid

elementClear :: SessionId -> ElementId -> IO ()
elementClear s = run . W.elementClear s

elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
elementSendKeys s eid = run . W.elementSendKeys s eid

getPageSource :: SessionId -> IO Text
getPageSource = run . W.getPageSource

takeScreenshot :: SessionId -> IO Text
takeScreenshot = run . W.takeScreenshot

takeElementScreenshot :: SessionId -> ElementId -> IO Text
takeElementScreenshot s = run . W.takeElementScreenshot s

printPage :: SessionId -> IO Text
printPage = run . W.printPage

executeScript :: SessionId -> Text -> [Value] -> IO Value
executeScript ses script = run . W.executeScript ses script

executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
executeScriptAsync ses script = run . W.executeScriptAsync ses script

getAllCookies :: SessionId -> IO [W.Cookie]
getAllCookies = run . W.getAllCookies

getNamedCookie :: SessionId -> Text -> IO W.Cookie
getNamedCookie s = run . W.getNamedCookie s

addCookie :: SessionId -> W.Cookie -> IO ()
addCookie s = run . W.addCookie s

deleteCookie :: SessionId -> Text -> IO ()
deleteCookie s = run . W.deleteCookie s

deleteAllCookies :: SessionId -> IO ()
deleteAllCookies = run . W.deleteAllCookies

dismissAlert :: SessionId -> IO ()
dismissAlert = run . W.dismissAlert

acceptAlert :: SessionId -> IO ()
acceptAlert = run . W.acceptAlert

getAlertText :: SessionId -> IO Text
getAlertText = run . W.getAlertText

sendAlertText :: SessionId -> Text -> IO ()
sendAlertText s = run . W.sendAlertText s

performActions :: SessionId -> W.Actions -> IO ()
performActions s = run . W.performActions s

releaseActions :: SessionId -> IO ()
releaseActions = run . W.releaseActions

-- ############# Utils #############

sleepMs :: Int -> IO ()
sleepMs = threadDelay . (* 1_000)

-- Returns the Base64-encoded bytestring of the file content.
encodeFileToBase64 :: FilePath -> IO Text
encodeFileToBase64 filePath = do
    contents <- BS.readFile filePath
    pure . B64T.extractBase64 $ B64.encodeBase64 contents

run :: (Show a) => W3Spec a -> IO a
run spec = do
  when wantConsoleLogging $ do
    devLog "Request"
    devLog . txt $ spec
    case spec of
      Get {} -> pure ()
      Post {body} -> do 
        devLog "body PP"
        prettyPrintJson body
        devLog "Body Raw"
        T.putStrLn ( LT.toStrict $ encodeToLazyText body)
      PostEmpty {} -> pure ()
      Delete {} -> pure ()
  callWebDriver wantConsoleLogging (mkRequest spec) >>= parseIO spec

mkRequest :: forall a. W3Spec a -> RequestArgs
mkRequest = \case
  Get {path} -> RequestParams path GET NoReqBody 4444
  Post {path, body} -> RequestParams path POST (ReqBodyJson body) 4444
  PostEmpty {path} -> RequestParams path POST (ReqBodyJson $ object []) 4444
  Delete {path} -> RequestParams path DELETE NoReqBody 4444

parseIO :: W3Spec a -> W.HttpResponse -> IO a
parseIO spec r =
  spec.parser r
    & \case
      Error msg -> fail $ parseWebDriverError r & \case
          e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
          e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
          e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a

devLog :: Text -> IO ()
devLog = T.putStrLn

responseStatusText :: Network.HTTP.Req.JsonResponse Value -> Text
responseStatusText = decodeUtf8Lenient . responseStatusMessage

callWebDriver :: Bool -> RequestArgs -> IO HttpResponse
callWebDriver wantLog RequestParams {path, method, body, port = prt} =
  runReq defaultHttpConfig  {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    log $ "URL: " <> txt url
    r <- req method url body jsonResponse $ port prt 
    log $ "JSON Response:\n" <> txt r
    let fr =
          Response
            { statusCode = responseStatusCode r,
              statusMessage = responseStatusText r,
              body = responseBody r :: Value
            }
    log $ "Framework Response:\n" <> txt fr
    pure fr
  where
    log m = liftIO $ when wantLog $ devLog m
    url :: Url 'Http
    url =  foldl' (/:) (http "127.0.0.1") path.segments
