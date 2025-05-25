{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Http.HttpRunnerMinimalForDocs (run) where

import Control.Applicative (Applicative (..))
import Control.Monad.Fail (MonadFail (..))
import Data.Aeson (Result (..), Value, object)
import Data.Foldable (foldl')
import Data.Function (($), (&), (.))
import Data.Int (Int)
import Data.Monoid ((<>))
import Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.IO (IO)
import GHC.Maybe (Maybe (..))
import GHC.Show (Show (..))
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    HttpBody,
    HttpBodyAllowed,
    HttpConfig (httpConfigCheckResponse),
    HttpMethod (..),
    NoReqBody (NoReqBody),
    POST (POST),
    ProvidesBody,
    ReqBodyJson (ReqBodyJson),
    Scheme (..),
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
    (/:),
  )
import WebDriverPreCore.Http
  ( ErrorClassification (..),
    HttpResponse (..),
    UrlPath (..),
    W3Spec (..),
    parseWebDriverError,
  )

{-
# Minimal WebDriver IO Runner

`webdriver-w3c-typed-endpoints` does not provide any implementation to drive a browser.

This is a minimal example of a \runner\ that implements the interaction with WebDriver endpoint definitions as provided by this library.

To \run\ a 'W3Spec', requires the following:

1. Chose a library to make the HTTP request to the WebDriver server. In this case, we use `req`.
2. Define a function to convert a `C.W3Spec` to a `RequestParams` (such as url, port and body) that can be used by the chosen library.
3. Call the WebDriver server with the `ReqRequestParams` and construct the result in the form of a simplified `HttpResponse`.
4. Use the parser provided by the `W3Spec` to transform the `HttpResponse` to the desired result type and handle any errors.

-}
run :: W3Spec a -> IO a
run spec = do
  let request = mkRequest spec -- 2. Convert W3Spec to params for req
  response <- callReq request  -- 3. Call WebDriver server (via req) and return a simplified HttpResponse
  parseResponse spec response  -- 4. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors

data ReqRequestParams where
  MkRequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { url :: Url 'Http,
      method :: method,
      body :: body,
      port :: Int
    } ->
    ReqRequestParams

-- 2. Define a function to convert a `W3Spec` to a `RequestParams`
mkRequest :: forall a. W3Spec a -> ReqRequestParams
mkRequest spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody 4444
  Post {body} -> MkRequestParams url POST (ReqBodyJson body) 4444
  PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) 4444
  Delete {} -> MkRequestParams url DELETE NoReqBody 4444
  where
    url = foldl' (/:) (http "127.0.0.1") spec.path.segments


-- 3. Call the WebDriver server with the `ReqRequestParams` and return the result in the form of a simplified `HttpResponse`
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

-- 4. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors (in this case just throwing an exception)
parseResponse :: W3Spec a -> HttpResponse -> IO a
parseResponse spec r =
  spec.parser r
    & \case
      Error msg ->
        fail $
          parseWebDriverError r & \case
            e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
            e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
            e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a