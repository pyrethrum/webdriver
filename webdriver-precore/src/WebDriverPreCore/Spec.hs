module WebDriverPreCore.Spec
  ( 
    -- * Introduction

    -- ** What is This Library?
    -- $whatThisLibrary

    -- ** Why This Library?
    -- $whyThisLibrary

    -- ** Implementing WebDriver Interaction
    -- $highLevelImplementation

    -- *** 1. Implementing a /runner/
    -- $runnerImp

    -- *** Example
    -- | The full source for this example can be found in [the project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IORunnerMinimal.hs).
    -- $modHeader

    -- **** @run@ function
    -- $runFunction

    -- **** 1. 'W3Spec' to @ReqRequestParams@
    -- $mkRequest

    -- **** 2. Call the WebDriver
    -- $callReq

    -- **** 3. Parse 'HttpResponse'
    -- $parseResponse

    -- *** 2. Applying the /runner/ to the 'W3Spec' functions
    -- $applicationOfRunner

    -- ** Using the API
    -- $usingTheAPI

    -- * The API

    -- ** The W3Spec Type
    module WC3Spec,

    -- ** Root Methods
    module RootMethods,

    -- ** Session Methods
    -- | /See also 'newSession' and 'newSession''/
    module SessionMethods,

    -- ** Window Methods
    module WindowMethods,

    -- ** Frame Methods
    module FrameMethods,

    -- ** Element(s) Methods
    module ElementMethods,

    -- ** Element Instance Methods
    module ElementInstanceMethods,

    -- ** Shadow DOM Methods
    module ShadowDOMMethods,

    -- * HTTP Response
    module WebDriverPreCore.Spec.HttpResponse,

    -- * Capabilities
    module CoreCapabilities,
    module WebDriverPreCore.Spec.Capabilities,

    -- * Errors
    module WebDriverPreCore.Spec.Error,

    -- * Action Types
    module ActionTypes,

    -- * Auxiliary Spec Types
    module AuxTypes,
  )
where

import WebDriverPreCore.Spec.Capabilities as CoreCapabilities (FullCapabilities(..), Capabilities(..) )
import WebDriverPreCore.Spec.Capabilities
import WebDriverPreCore.Spec.Error
import WebDriverPreCore.Spec.HttpResponse
import WebDriverPreCore.Spec.SpecDefinition as ActionTypes
  ( Action (..),
    Actions (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    WheelAction (..),
  )
import WebDriverPreCore.Spec.SpecDefinition as AuxTypes
  ( Cookie (..),
    DriverStatus (..),
    ElementId (..),
    FrameReference (..),
    HttpResponse (..),
    SameSite (..),
    Selector (..),
    SessionId (..),
    Timeouts (..),
    UrlPath (..),
    WindowHandle (..),
    WindowHandleSpec (..),
    WindowRect (..),
  )
import WebDriverPreCore.Spec.SpecDefinition as ElementInstanceMethods
  ( elementClear,
    elementClick,
    elementSendKeys,
    findElementFromElement,
    findElementsFromElement,
    getElementAttribute,
    getElementComputedLabel,
    getElementComputedRole,
    getElementCssValue,
    getElementProperty,
    getElementRect,
    getElementShadowRoot,
    getElementTagName,
    getElementText,
    isElementEnabled,
    isElementSelected,
    takeElementScreenshot,
  )
import WebDriverPreCore.Spec.SpecDefinition as ElementMethods
  ( findElement,
    findElements,
    getActiveElement,
  )
import WebDriverPreCore.Spec.SpecDefinition as FrameMethods (switchToParentFrame)
import WebDriverPreCore.Spec.SpecDefinition as RootMethods (newSession, newSession', status)
import WebDriverPreCore.Spec.SpecDefinition as SessionMethods
  ( acceptAlert,
    addCookie,
    back,
    closeWindow,
    deleteAllCookies,
    deleteCookie,
    deleteSession,
    dismissAlert,
    executeScript,
    executeScriptAsync,
    forward,
    fullscreenWindow,
    getAlertText,
    getAllCookies,
    getCurrentUrl,
    getNamedCookie,
    getPageSource,
    getTimeouts,
    getTitle,
    getWindowHandle,
    getWindowHandles,
    getWindowRect,
    maximizeWindow,
    minimizeWindow,
    navigateTo,
    newWindow,
    performActions,
    printPage,
    refresh,
    releaseActions,
    sendAlertText,
    setTimeouts,
    setWindowRect,
    switchToFrame,
    switchToWindow,
    takeScreenshot,
  )
import WebDriverPreCore.Spec.SpecDefinition as ShadowDOMMethods (findElementFromShadowRoot, findElementsFromShadowRoot)
import WebDriverPreCore.Spec.SpecDefinition as WC3Spec (W3Spec (..))
import WebDriverPreCore.Spec.SpecDefinition as WindowMethods
  ( closeWindow,
    fullscreenWindow,
    getWindowHandles,
    getWindowRect,
    maximizeWindow,
    minimizeWindow,
    newWindow,
    setWindowRect,
    switchToWindow,
  )

-- $whatThisLibrary
-- This library provides a minimal abstraction over the [WebDriver W3C Protocol](https://www.w3.org/TR/2025/WD-webdriver2-20250210/) endpoints
-- __without providing any implementation__. It provides a description of the W3C API as a list of functions returning a 'W3Spec'.
-- The intention is that other libraries will provide the actual implementation.
--
-- You can not use this library directly to drive a browser. If you are looking for a library to drive a browser, you may be interested in
-- an alternative library such [haskell-webdriver](https://github.com/haskell-webdriver/haskell-webdriver#readme).

-- $whyThisLibrary
-- There are a number of libraries that provide WebDriver bindings for Haskell. However, at the time work on this library commenced
-- the available libraries were either in need of maintenance, required Selenium, or part of larger opinionated testing frameworks.
--
-- The goal of the authors of this library is to enable browser interaction in our own high level testing framework. We would prefer to
-- communicate with drivers directly, using the W3C standards (developed largely by the Selenium core contributors) rather than depend on Selenium
-- itself. We would also like to avoid pulling in too many dependencies or potentially incompatible concepts from other high level libraries.
-- To achieve this, we plan to first develop a number of unopinionated, lower level libraries that can be used by others 
-- without buying into our entire stack.
--
-- This library is the first of those libraries, and is intended to provide a low-dependency base on which fully featured
-- (W3C) WebDriver libraries can be built.

{- $highLevelImplementation
Using [webdriver-precore](TODO) to build a webdriver library requires two steps to attain the basic functionality provided by the WebDriver API:

1. Implement a /runner/ (@run@ function) that, given a 'W3Spec', can make HTTP requests to the WebDriver server and parse the response.
2. Apply the runner to the 'W3Spec' functions provided by this library to transform the 'W3Spec' into actual @IO@ browser interactions (this is just boilerplate).
-}

{- $runnerImp
This is an example of a minimal \runner\ that implements the interaction with WebDriver endpoint definitions as provided by this library.

The first step in writing a WebDriver implementation is to choose an HTTP library. In this example we have chosen [req](https://hackage.haskell.org/package/req).

Then to implement a \run\ function requires the following:

    1. Define a function to transform a 'W3Spec' to @RequestParams@ compatible with the chosen HTTP library.
    2. Make an HTTP call to WebDriver as per the @RequestParams@ and return a simplified 'HttpResponse'.
    3. Use the parser provided by the 'W3Spec' to transform the 'HttpResponse' to the desired result type and handle any errors.
-}

{- $modHeader
  ==== __Module Header__
  @
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE DuplicateRecordFields #-}
  {-# LANGUAGE ExistentialQuantification #-}
  {-# LANGUAGE GADTs #-}
  {-# LANGUAGE LambdaCase #-}
  {-# LANGUAGE NamedFieldPuns #-}
  {-# LANGUAGE NoImplicitPrelude #-}
  {-# LANGUAGE OverloadedRecordDot #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE NoFieldSelectors #-}
  module IORunnerMinimal ( run ) where
  import Data.Aeson (Result (..), Value, object)
  import Data.Function ((&), ($), (.))
  import Data.Text  as T (Text, unpack)
  import Data.Text.Encoding (decodeUtf8Lenient)
  import WebDriverPreCore.Spec (
    HttpResponse (..),
    W3Spec (..),
    parseWebDriverError,
    ErrorClassification (..),
    UrlPath (..) )
  import GHC.IO (IO)
  import Data.Int (Int)
  import Control.Monad (Monad(..))
  import Data.Foldable (foldl')
  import GHC.Maybe (Maybe(..))
  import Control.Applicative (Applicative(..))
  import Control.Monad.Fail (MonadFail(..))
  import Data.Monoid ((<>))
  import GHC.Show (Show(..))
  import Network.HTTP.Req as R
    ( DELETE (DELETE),
      GET (GET),
      NoReqBody (NoReqBody),
      POST (POST),
      ReqBodyJson (ReqBodyJson),
      JsonResponse,
      defaultHttpConfig,
      http,
      jsonResponse,
      port,
      req,
      responseBody,
      responseStatusCode,
      responseStatusMessage,
      runReq,
      HttpConfig (httpConfigCheckResponse), (/:), HttpBodyAllowed, HttpMethod (..), ProvidesBody, HttpBody, Url, Scheme (..),
    )
@
-}

{- $runFunction
@
run :: W3Spec a -> IO a
run spec = do
  let request = mkRequest spec -- 1. Convert W3Spec to params for req
  response <- callReq request  -- 2. Call WebDriver server (via req) and return a simplified HttpResponse
  parseResponse spec response  -- 3. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors
@
-}

{- $mkRequest
/Create request params consumable by the HTTP library (@ReqRequestParams@ in this example)/

@
mkRequest :: forall a. W3Spec a -> ReqRequestParams
mkRequest spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody 4444
  Post {body} -> MkRequestParams url POST (ReqBodyJson body) 4444
  PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) 4444
  Delete {} -> MkRequestParams url DELETE NoReqBody 4444
  where
    url =  foldl' (/:) (http "127.0.0.1") spec.path.segments

data ReqRequestParams where
  MkRequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { url :: Url 'Http,
      method :: method,
      body :: body,
      port :: Int
    } ->
    ReqRequestParams
@
-}

{- $callReq
/Call WebDriver endpoints based on @ReqRequestParams@/

@
callReq :: ReqRequestParams -> IO HttpResponse
callReq MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    r <- req method url body jsonResponse $ port prt
    pure $
      MkHttpResponse
        { statusCode = responseStatusCode r,
          statusMessage = responseStatusText r,
          body = responseBody r :: Value
        }
  where
    responseStatusText = decodeUtf8Lenient . responseStatusMessage
@

-}

{- $parseResponse
/Parse the WebDriver response (in this implimentation we are just throwing exceptions on failure)/

@
parseResponse :: W3Spec a -> HttpResponse -> IO a
parseResponse spec r =
  spec.parser r
    & \case
      Error msg -> fail $ parseWebDriverError r & \case
          e\@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
          e\@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
          e\@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a
@
-}

{- $applicationOfRunner

/Create an IO API by applying @run@ to each endpoint definition exposed by this library/

Full source can be found in the [project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IOAPI.hs)

@
module IOAPI where 

import WebDriverPreCore.Spec qualified as W

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

-- ... and 50+ more API functions
@
-}

{- $usingTheAPI
/Once all the required endpoints are implemented you will be able to interact with browsers via WebDriver/

__Prerequisites__:

 1. An appropriate browser and WebDriver installed.
 2. WebDriver started.

 e.g. For Firefox and geckodriver on Linux or WSL you could start geckodriver as follows:

>>> pkill -f geckodriver || true  && geckodriver --log trace &

With the driver running you can now interact with the browser:

__Example__:
A full example source file can be found in the [project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/WebDriverE2EDemoTest.hs).

@
demoForwardBackRefresh :: IO ()
demoForwardBackRefresh = do
  ses <- newSession $ minFullCapabilities Firefox
  navigateTo ses "https://the-internet.herokuapp.com/"
  link <- findElement ses $ CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"
  elementClick ses link
  back ses
  forward ses
  refresh ses
  deleteSession ses
@

Note this is a minimal API. There is plenty of scope to build on this to provide a more user-friendly functions.
-}
