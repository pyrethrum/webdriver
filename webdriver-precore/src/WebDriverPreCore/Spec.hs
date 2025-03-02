module WebDriverPreCore.Spec
  ( -- * Introduction
    -- $whyThisLibrary
    -- $usage
    -- $example

    -- * The W3Spec Type
    module WC3Spec,

    -- * The API

    -- ** Root Methods
    module RootMethods,

    -- ** Session Methods
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
    module WebDriverPreCore.Spec.Capabilities,

    -- * Errors
    module WebDriverPreCore.Spec.Error,

    -- * Action Types
    module ActionTypes,

    -- * Auxiliary Spec Types
    module AuxTypes,
  )
where

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

-- $whyThisLibrary
-- == What is This Library?
-- This library provides a minimal abstraction over the [WebDriver W3C Protocol](https://www.w3.org/TR/2025/WD-webdriver2-20250210/) endpoints
-- __without providing any implementation__. It provides a description of the W3C API as typed (primarily 'W3Spec' ) functions.
-- The intention is that other libraries will provide the actual implementation.
--
-- You can not use this library directly to drive a browser.
--
-- == Why This Library?
-- There are a number of libraries that provide WebDriver bindings for Haskell. However, at the time work on this library commenced
-- the available libraries were either in need of maintenance, required Selenium, or part of larger opinionated testing frameworks.
--
-- The end goal of the authors of this library is to enable UI interaction in our own high level testing framework. We would prefer to
-- communicate with drivers directly, using the W3C standards (developed largely by the Selenium core contributors) rather than depend on Selenium
-- itself. We would also like to avoid pulling in too many dependencies or potentially incompatible concepts from other high level libraries.
-- To achieve this, we plan to develop a number of unopinionated lower level libraries that can be used by others without buying into our entire stack.
--
-- This library is the first of those libraries and is intended to provide a low-dependency base on which fully featured
-- (W3C) WebDriver libraries can be built.

-- $usage
--
-- == Minimal WebDriver IO Runner
--
-- [webdriver-precore](TODO) does not provide any implementation to drive a browser.
--
-- This is an example of a minimal \runner\ that implements the interaction with WebDriver endpoint definitions as provided by this library.
-- The full example can be found at [IORunnerMinimal.hs](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IORunnerMinimal.hs)
-- To \run\ a 'W3Spec', requires the following:
--
-- 1. Choose a library to make the HTTP request to the WebDriver server. In this case, we have chosen [req](https://hackage.haskell.org/package/req).
-- 2. Define a function to convert a 'W3Spec' to a @RequestParams@ (such as url, port and body) that can be used by the chosen library.
-- 3. Call the WebDriver server with the @ReqRequestParams@ and construct the result in the form of a simplified 'HttpResponse'.
-- 4. Use the parser provided by the 'W3Spec' to transform the 'HttpResponse' to the desired result type and handle any errors.

-- $example
-- === __Module Header__
-- @
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoFieldSelectors #-}
--
-- module IORunnerMinimal ( run ) where
--
-- import Data.Aeson (Result (..), Value, object)
--
-- import Data.Function ((&), ($), (.))
-- import Data.Text  as T (Text, unpack)
-- import Data.Text.Encoding (decodeUtf8Lenient)
-- import WebDriverPreCore.Spec (
--   HttpResponse (..),
--    W3Spec (..),
--    parseWebDriverError,
--    ErrorClassification (..),
--    UrlPath (..) )
-- import GHC.IO (IO)
-- import Data.Int (Int)
-- import Control.Monad (Monad(..))
-- import Data.Foldable (foldl')
-- import GHC.Maybe (Maybe(..))
-- import Control.Applicative (Applicative(..))
-- import Control.Monad.Fail (MonadFail(..))
-- import Data.Monoid ((<>))
-- import GHC.Show (Show(..))
-- import Network.HTTP.Req as R
--   ( DELETE (DELETE),
--     GET (GET),
--     NoReqBody (NoReqBody),
--     POST (POST),
--     ReqBodyJson (ReqBodyJson),
--     JsonResponse,
--     defaultHttpConfig,
--     http,
--     jsonResponse,
--     port,
--     req,
--     responseBody,
--     responseStatusCode,
--     responseStatusMessage,
--     runReq,
--     HttpConfig (httpConfigCheckResponse), (/:), HttpBodyAllowed, HttpMethod (..), ProvidesBody, HttpBody, Url, Scheme (..),
--   )
-- @
--
-- === The Run Function
-- @
-- run :: W3Spec a -> IO a
-- run spec =
--   -- req chosen               -- 1. HTTP library chosen is req
--   mkRequest spec              -- 2. Convert W3Spec to params for req
--     & callReq                 -- 3. Call WebDriver server (via req) and return a simplified HttpResponse
--     >>= parseResponse spec    -- 4. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors
-- @
--
-- === 1. Import Chosen HTTP Library
-- see [req](https://hackage.haskell.org/package/req) documentation for more details
--
-- === 2. Define a function to convert a 'W3Spec' to a @ReqRequestParams@
-- @
-- mkRequest :: forall a. W3Spec a -> ReqRequestParams
-- mkRequest spec = case spec of
--   Get {} -> MkRequestParams url GET NoReqBody 4444
--   Post {body} -> MkRequestParams url POST (ReqBodyJson body) 4444
--   PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) 4444
--   Delete {} -> MkRequestParams url DELETE NoReqBody 4444
--   where
--     url =  foldl' (/:) (http "127.0.0.1") spec.path.segments
--
-- data ReqRequestParams where
--   MkRequestParams ::
--     (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
--     { url :: Url 'Http,
--       method :: method,
--       body :: body,
--       port :: Int
--     } ->
--     ReqRequestParams
-- @
--
-- === 3. Call the WebDriver server with the @ReqRequestParams@ and return the result in the form of a simplified 'HttpResponse'
-- @
-- callReq :: ReqRequestParams -> IO HttpResponse
-- callReq MkRequestParams {url, method, body, port = prt} =
--   runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
--     r <- req method url body jsonResponse $ port prt
--     pure $
--       MkHttpResponse
--         { statusCode = responseStatusCode r,
--           statusMessage = responseStatusText r,
--           body = responseBody r :: Value
--         }
--   where
--     responseStatusText = decodeUtf8Lenient . responseStatusMessage
-- @
--
-- === 4. Use the 'S.W3Spec' parser to convert the 'HttpResponse' to the desired result type and handle any errors (in this case just throwing an exception)
-- @
-- parseResponse :: W3Spec a -> HttpResponse -> IO a
-- parseResponse spec r =
--   spec.parser r
--     & \case
--       Error msg -> fail $ parseWebDriverError r & \case
--           e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
--           e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
--           e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
--       Success a -> pure a
-- @

-- Up TO HERE
{-
The following assumes that an appropriate browser and WebDriver have been installed, and the WebDriver has been started.

e.g. For Firefox and geckodriver on Linux or WSL:
```bash
pkill -f geckodriver || true  && geckodriver --log trace &

-}
