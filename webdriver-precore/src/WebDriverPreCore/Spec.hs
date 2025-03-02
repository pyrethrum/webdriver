module WebDriverPreCore.Spec (
    -- * Introduction
    -- $whyThisLibrary
    -- + How to Use This Library
    -- $usage
    -- $example

   -- * The W3Spec Type
    S.W3Spec (..),
    -- * The API
    -- ** Root Methods
    S.newSession,
    S.newSession',
    S.status,

    -- ** Session Methods
    S.deleteSession,
    S.getTimeouts,
    S.setTimeouts,
    S.navigateTo,
    S.getCurrentUrl,
    S.back,
    S.forward,
    S.refresh,
    S.getTitle,
    S.getWindowHandle,
    S.newWindow,
    S.closeWindow,
    S.switchToWindow,
    S.switchToFrame,
    S.getPageSource,
    S.executeScript,
    S.executeScriptAsync,
    S.getAllCookies,
    S.getNamedCookie,
    S.addCookie,
    S.deleteCookie,
    S.deleteAllCookies,
    S.performActions,
    S.releaseActions,
    S.dismissAlert,
    S.acceptAlert,
    S.getAlertText,
    S.sendAlertText,
    S.takeScreenshot,
    S.printPage,
    
    -- ** Window Methods
    S.getWindowHandles,
    S.getWindowRect,
    S.setWindowRect,
    S.maximizeWindow,
    S.minimizeWindow,
    S.fullscreenWindow,

    -- ** Frame Methods
    S.switchToParentFrame,

    -- ** Element(s) Methods
    S.getActiveElement,
    S.findElement,
    S.findElements,

    -- ** Element Instance Methods
    S.getElementShadowRoot,
    S.findElementFromElement,
    S.findElementsFromElement,
    S.isElementSelected,
    S.getElementAttribute,
    S.getElementProperty,
    S.getElementCssValue,
    S.getElementText,
    S.getElementTagName,
    S.getElementRect,
    S.isElementEnabled,
    S.getElementComputedRole,
    S.getElementComputedLabel,
    S.elementClick,
    S.elementClear,
    S.elementSendKeys,
    S.takeElementScreenshot,
    
    -- ** Shadow DOM Methods
    S.findElementFromShadowRoot,
    S.findElementsFromShadowRoot,
    
    -- * Auxiliary Spec Types
    S.Cookie (..),
    S.DriverStatus (..),
    S.ElementId (..),
    S.FrameReference (..),
    S.HttpResponse (..),
    S.SameSite (..),
    S.Selector (..),
    S.SessionId (..),
    S.Timeouts (..),
    S.WindowHandle (..),
    S.WindowHandleSpec (..),
    S.WindowRect (..),
    S.UrlPath (..),

    -- ** Action Types
    S.Action (..),
    S.Actions (..),
    S.KeyAction (..),
    S.Pointer (..),
    S.PointerAction (..),
    S.PointerOrigin (..),
    S.WheelAction (..),

    -- * HTTP Response
    module WebDriverPreCore.Spec.HttpResponse,

    -- * Capabilities
    module WebDriverPreCore.Spec.Capabilities,

     -- * Errors
    module WebDriverPreCore.Spec.Error,

) where

import WebDriverPreCore.Spec.SpecDefinition qualified as S

import WebDriverPreCore.Spec.HttpResponse
import WebDriverPreCore.Spec.Capabilities
import WebDriverPreCore.Spec.Error

-- $whyThisLibrary
--
-- == What is This Library?
-- This library provides a minimal abstraction over the [WebDriver W3C Protocol](https://www.w3.org/TR/2025/WD-webdriver2-20250210/) endpoints
-- __without providing any implementation. It provides a description of the W3C API as typed (primarily `W3Spec`) functions.
-- The intention is that other libraries will provide the actual implementation.
-- 
-- You can not use this library directly to drive a browser.
--
-- == Why This Library?
-- There are a number of libraries that provide WebDriver bindings for Haskell. However, at the time work on this library commenced 
-- the available libraries were either unmaintained, required Selnium, or part of larger opinionated testing frameworks.
--
-- The end goal of the authors of this library is to enble UI interaction in our own high level testing framework. We plan to to implement 
-- this via a number of unopinioated lower level libraries that can be used by others without buying into our entire stack.
-- 
-- This library is intended to provide a simple, low-dependency base on which fully featured (W3C) WebDriver libraries can be built.
--

-- $usage
-- 
-- == Minimal WebDriver IO Runner
-- 
-- 'webdriver-w3c-typed-endpoints' does not provide any implementation to drive a browser. 
-- 
-- This is an example of a minimal \runner\ that implements the interaction with WebDriver endpoint definitions as provided by this library.
-- The full example can be found at [IORunnerMinimal.hs](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IORunnerMinimal.hs)
--
-- To \run\ a `W3Spec`, requires the following:
-- 
-- 1. Choose a library to make the HTTP request to the WebDriver server. In this case, we use `req`.
-- 2. Define a function to convert a `W3Spec` to a `RequestParams` (such as url, port and body) that can be used by the chosen library.
-- 3. Call the WebDriver server with the `ReqRequestParams` and construct the result in the form of a simplified `HttpResponse`.
-- 4. Use the parser provided by the `W3Spec` to transform the `HttpResponse` to the desired result type and handle any errors.



-- $example
--
-- @
--  {-# LANGUAGE DataKinds #-}
--  {-# LANGUAGE DuplicateRecordFields #-}
--  {-# LANGUAGE ExistentialQuantification #-}
--  {-# LANGUAGE GADTs #-}
--  {-# LANGUAGE LambdaCase #-}
--  {-# LANGUAGE NamedFieldPuns #-}
--  {-# LANGUAGE NoImplicitPrelude #-}
--  {-# LANGUAGE OverloadedRecordDot #-}
--  {-# LANGUAGE OverloadedStrings #-}
--  {-# LANGUAGE NoFieldSelectors #-}
--
--  module IORunnerMinimal ( run ) where
--
--  import Data.Aeson (Result (..), Value, object)
--
--  import Data.Function ((&), ($), (.))
--  import Data.Text  as T (Text, unpack)
--  import Data.Text.Encoding (decodeUtf8Lenient)
--  import Network.HTTP.Req as R
--    ( DELETE (DELETE),
--      GET (GET),
--      NoReqBody (NoReqBody),
--      POST (POST),
--      ReqBodyJson (ReqBodyJson),
--      JsonResponse,
--      defaultHttpConfig,
--      http,
--      jsonResponse,
--      port,
--      req,
--      responseBody,
--      responseStatusCode,
--      responseStatusMessage,
--      runReq,
--      HttpConfig (httpConfigCheckResponse), (/:), HttpBodyAllowed, HttpMethod (..), ProvidesBody, HttpBody, Url, Scheme (..),
--    )
--  import WebDriverPreCore.Spec (
--    HttpResponse (..),
--     W3Spec (..), 
--     parseWebDriverError, 
--     ErrorClassification (..),
--     UrlPath (..) )
--  import GHC.IO (IO)
--  import Data.Int (Int)
--  import Control.Monad (Monad(..))
--  import Data.Foldable (foldl')
--  import GHC.Maybe (Maybe(..))
--  import Control.Applicative (Applicative(..))
--  import Control.Monad.Fail (MonadFail(..))
--  import Data.Monoid ((<>))
--  import GHC.Show (Show(..))
--
--  run :: W3Spec a -> IO a
--  run spec = 
--    -- req chosen               -- 1. HTTP library chosen is req
--    mkRequest spec              -- 2. Convert W3Spec to params for req
--      & callReq                 -- 3. Call WebDriver server (via req) and return a simplified HttpResponse
--      >>= parseResponse spec    -- 4. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors
--
--
--  data ReqRequestParams where
--    MkRequestParams ::
--      (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
--      { url :: Url 'Http,
--        method :: method,
--        body :: body,
--        port :: Int
--      } ->
--      ReqRequestParams
--  
--  -- 2. Define a function to convert a `W3Spec` to a `RequestParams`
--  mkRequest :: forall a. W3Spec a -> ReqRequestParams
--  mkRequest spec = case spec of
--    Get {} -> MkRequestParams url GET NoReqBody 4444
--    Post {body} -> MkRequestParams url POST (ReqBodyJson body) 4444
--    PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) 4444
--    Delete {} -> MkRequestParams url DELETE NoReqBody 4444
--    where 
--      url =  foldl' (/:) (http "127.0.0.1") spec.path.segments

--  -- 3. Call the WebDriver server with the `ReqRequestParams` and return the result in the form of a simplified `HttpResponse` 
--  callReq :: ReqRequestParams -> IO HttpResponse
--  callReq MkRequestParams {url, method, body, port = prt} =
--    runReq defaultHttpConfig  {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
--      r <- req method url body jsonResponse $ port prt 
--      pure $ MkHttpResponse
--              { statusCode = responseStatusCode r,
--                statusMessage = responseStatusText r,
--                body = responseBody r :: Value
--              }
--
--
--  -- 4. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors (in this case just throwing an exception)
--  parseResponse :: W3Spec a -> HttpResponse -> IO a
--  parseResponse spec r =
--    spec.parser r
--      & \case
--        Error msg -> fail $ parseWebDriverError r & \case
--            e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
--            e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
--            e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
--        Success a -> pure a
--
--
--  responseStatusText :: R.JsonResponse Value -> Text
--  responseStatusText = decodeUtf8Lenient . responseStatusMessage
-- @



{-
The following assumes that an appropriate browser and WebDriver have been installed, and the WebDriver has been started.

e.g. For Firefox and geckodriver on Linux or WSL:
```bash
pkill -f geckodriver || true  && geckodriver --log trace &

-}
