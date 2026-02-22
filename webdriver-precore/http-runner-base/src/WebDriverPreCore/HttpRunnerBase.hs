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
    HttpEndpoint (..),

    -- * Request Types
    HttpMethod (..),
    HttpRequest (..),

    -- * HTTP Response
    HttpResponse (..),

    -- * URL Utilities
    SubPath (..),
    buildUrl,

    -- * Low-level HTTP
    callWebDriverBody,
    callWebDriverResponse,
  )
where

import Control.Monad.IO.Class (MonadIO)
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

-- | URL path parts
newtype SubPath = MkSubPath {parts :: [Text]}
  deriving (Show, Eq)

-- | HTTP methods supported by WebDriver
data HttpMethod = GET_METHOD | POST_METHOD | DELETE_METHOD
  deriving (Show, Eq)

-- | An HTTP request to send to WebDriver
data HttpRequest = MkHttpRequest
  { method :: HttpMethod,
    path :: SubPath,
    body :: Maybe Value
  }
  deriving (Show, Eq)

data HttpEndpoint = MkHttpEndpoint
  { -- | Host (e.g. "127.0.0.1")
    host :: Text,
    -- | Port (e.g. 4444)
    port :: Word16
  }
  deriving (Show, Eq)

-- | Base HTTP runner that works with JSON values
data HttpRunnerBase m = MkHttpRunnerBase
  { -- | Execute a request and return just the response body
    runBody :: HttpRequest -> m Value,
    -- | Execute a request and return the full HTTP response
    runFull :: HttpRequest -> m HttpResponse
  }

-- TODO SIMPLIFY runFull Only and rename to run - cascade to typed
-- | Create an HTTP runner base
mkHttpRunnerBase ::
  (MonadIO m) =>
  -- | HTTP endpoint (host and port)
  HttpEndpoint ->
  -- | Optional logger
  Maybe (Text -> m ()) ->
  HttpRunnerBase m
mkHttpRunnerBase MkHttpEndpoint {host, port} mLogger =
  MkHttpRunnerBase
    { runBody = callWebDriverBody baseUrl port mLogger,
      runFull = callWebDriverResponse baseUrl port mLogger
    }
  where
    baseUrl = http host

-- | Build a full URL from base URL and path parts
buildUrl :: Url 'Http -> SubPath -> Url 'Http
buildUrl basePath urlPath = F.foldl' (/:) basePath urlPath.parts

-- | Execute a WebDriver HTTP request, returning just the JSON body
callWebDriverBody ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m Value
callWebDriverBody baseUrl port mLogger =
  fmap (.body) . callWebDriverResponse baseUrl port mLogger

-- | Execute a WebDriver HTTP request, returning the full HTTP response
callWebDriverResponse ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m HttpResponse
callWebDriverResponse baseUrl port mLogger request = do
  log $ "HTTP " <> methodText request.method <> " " <> pack (show url)
  response <- runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    r <- case request.method of
      GET_METHOD ->
        req GET url NoReqBody jsonResponse (R.port iPort)
      POST_METHOD ->
        req POST url (ReqBodyJson $ fromMaybe (object []) request.body) jsonResponse (R.port iPort)
      DELETE_METHOD ->
        req DELETE url NoReqBody jsonResponse (R.port iPort)
    pure
      MkHttpResponse
        { statusCode = responseStatusCode r,
          statusMessage = decodeUtf8Lenient $ responseStatusMessage r,
          body = responseBody r
        }
  log $ "Response: " <> pack (show response.statusCode)
  pure response
  where
    iPort = fromIntegral port
    url = buildUrl baseUrl request.path
    log = fromMaybe (const $ pure ()) mLogger
    methodText = \case
      GET_METHOD -> "GET"
      POST_METHOD -> "POST"
      DELETE_METHOD -> "DELETE"
