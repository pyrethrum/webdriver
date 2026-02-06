-- |
-- Module: WebDriverPreCore.HttpRunnerBase
-- Description: JSON-based HTTP runner for WebDriver
--
-- This module provides an HTTP runner that works with JSON Values rather than
-- typed WebDriver commands, allowing it to be used independently of the
-- webdriver-precore type definitions.
module WebDriverPreCore.HttpRunnerBase
  ( -- * HTTP Runner
    HttpRunnerBase (..),
    mkHttpRunnerBase,

    -- * Request Types
    HttpMethod (..),
    HttpRequest (..),

    -- * HTTP Response
    HttpResponse (..),

    -- * URL Utilities
    UrlPath (..),
    buildUrl,

    -- * Low-level HTTP
    callWebDriverJson,
    callWebDriverResponse,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object)
import Data.Foldable qualified as F
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word16)
import Network.HTTP.Req
  ( DELETE (DELETE),
    GET (GET),
    HttpConfig (httpConfigCheckResponse),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (..),
    Url,
    defaultHttpConfig,
    http,
    jsonResponse,
    req,
    responseBody,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    (/:),
  )
import Network.HTTP.Req qualified as R
import WebDriverPreCore.HttpRunnerBase.HttpResponse (HttpResponse (..))
import Prelude hiding (log)

-- | URL path segments
newtype UrlPath = MkUrlPath {segments :: [Text]}
  deriving (Show, Eq)

-- | HTTP methods supported by WebDriver
data HttpMethod = GET_METHOD | POST_METHOD | DELETE_METHOD
  deriving (Show, Eq)

-- | An HTTP request to send to WebDriver
data HttpRequest = MkHttpRequest
  { method :: HttpMethod,
    path :: UrlPath,
    body :: Maybe Value
  }
  deriving (Show, Eq)

-- | Base HTTP runner that works with JSON values
data HttpRunnerBase = MkHttpRunnerBase
  { -- | Execute a request and return just the response body
    runJson :: HttpRequest -> IO Value,
    -- | Execute a request and return the full HTTP response
    runResponse :: HttpRequest -> IO HttpResponse
  }

-- | Create an HTTP runner base
mkHttpRunnerBase ::
  -- | Host (e.g. "127.0.0.1")
  Text ->
  -- | Port (e.g. 4444)
  Word16 ->
  -- | Optional logger
  Maybe (Text -> IO ()) ->
  HttpRunnerBase
mkHttpRunnerBase host port mLogger =
  MkHttpRunnerBase
    { runJson = callWebDriverJson baseUrl port mLogger,
      runResponse = callWebDriverResponse baseUrl port mLogger
    }
  where
    baseUrl = http host

-- | Build a full URL from base URL and path segments
buildUrl :: Url 'Http -> UrlPath -> Url 'Http
buildUrl basePath urlPath = F.foldl' (/:) basePath urlPath.segments

-- | Execute a WebDriver HTTP request, returning just the JSON body
callWebDriverJson ::
  Url 'Http ->
  Word16 ->
  Maybe (Text -> IO ()) ->
  HttpRequest ->
  IO Value
callWebDriverJson baseUrl port mLogger = 
  fmap (.body) . callWebDriverResponse baseUrl port mLogger 

-- | Execute a WebDriver HTTP request, returning the full HTTP response
callWebDriverResponse ::
  Url 'Http ->
  Word16 ->
  Maybe (Text -> IO ()) ->
  HttpRequest ->
  IO HttpResponse
callWebDriverResponse baseUrl port mLogger request = do

  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    log $ "HTTP " <> methodText request.method <> " " <> pack (show url)

    r <- case request.method of
      GET_METHOD ->
        req GET url NoReqBody jsonResponse (R.port iPort)
      POST_METHOD ->
        req POST url (ReqBodyJson $ maybe (object []) id request.body) jsonResponse (R.port iPort)
      DELETE_METHOD ->
        req DELETE url NoReqBody jsonResponse (R.port iPort)

    let body' = responseBody r :: Value
        response =
          MkHttpResponse
            { statusCode = responseStatusCode r,
              statusMessage = decodeUtf8Lenient $ responseStatusMessage r,
              body = body'
            }
    log $ "Response: " <> pack (show response.statusCode)
    pure response
  where
    iPort = fromIntegral port
    url = buildUrl baseUrl request.path
    log = liftIO . fromMaybe (const $ pure ()) mLogger
    methodText = \case
      GET_METHOD -> "GET"
      POST_METHOD -> "POST"
      DELETE_METHOD -> "DELETE"
