-- |
-- Module: WebDriverPreCore.HttpRunner.Utils
-- Description: Low-level HTTP runner utilities for WebDriver commands
--
-- This module provides the low-level plumbing: HTTP execution, typed-to-raw
-- request conversion, and response parsing.
module WebDriverPreCore.HttpRunner.Utils
  ( -- * Request-level runners
    callWebDriver',
    callWebDriverBody',
    callWebDriverResponse',

    -- * Command conversion
    commandToRequest,

    -- * Types
    HttpEndpoint (..),
    HttpResponse (..),
    HttpMethod (..),
    HttpRequest (..),
    ParseFailure (..),

    -- * URL Utilities
    SubPath (..),
    buildUrl,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), Value (..), object, (.:))
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Bifunctor (first)
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word16)
import WebDriverPreCore.ParseFailure (ParseFailure (..))
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
import Utils (SubPath (..))
import WebDriverPreCore.HTTP.Command (Command (..))
import Prelude hiding (log)

-- | HTTP response from a WebDriver endpoint
data HttpResponse = MkHttpResponse
  { statusCode :: Int,
    statusMessage :: Text,
    body :: Value
  }
  deriving (Show, Eq)

-- | Host and port of a WebDriver HTTP endpoint
data HttpEndpoint = MkHttpEndpoint
  { host :: Text,
    port :: Word16
  }
  deriving (Show, Eq)

-- | HTTP methods supported by WebDriver
data HttpMethod = GET_METHOD | POST_METHOD | DELETE_METHOD
  deriving (Show, Eq)

-- | A raw HTTP request to send to a WebDriver endpoint
data HttpRequest = MkHttpRequest
  { method :: HttpMethod,
    path :: SubPath,
    body :: Maybe Value
  }
  deriving (Show, Eq)

-- | Build a full URL from base URL and path parts
buildUrl :: Url 'Http -> SubPath -> Url 'Http
buildUrl basePath urlPath = F.foldl' (/:) basePath urlPath.parts

-- | Execute a WebDriver 'Command', returning just the parsed JSON body.
-- Low-level variant that takes a raw 'HttpRequest'.
callWebDriver' ::
  (MonadIO m, FromJSON r) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m (Either ParseFailure r)
callWebDriver' MkHttpEndpoint {host, port} mLogger request =
  parseResult <$> callWebDriverBody' (http host) port mLogger request

-- | Execute a WebDriver HTTP request, returning just the JSON body.
-- Low-level variant that takes a raw 'HttpRequest'.
callWebDriverBody' ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m Value
callWebDriverBody' baseUrl port mLogger =
  fmap (.body) . runHttpRequest baseUrl port mLogger

-- | Execute a WebDriver HTTP request, returning the full HTTP response.
-- Low-level variant that takes a raw 'HttpRequest'.
callWebDriverResponse' ::
  (MonadIO m) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m HttpResponse
callWebDriverResponse' MkHttpEndpoint {host, port} mLogger request =
  runHttpRequest (http host) port mLogger request

-- | Convert a typed 'Command' to a raw 'HttpRequest'.
commandToRequest :: Command r -> HttpRequest
commandToRequest cmd = case cmd of
  Get {} ->
    MkHttpRequest GET_METHOD cmd.path Nothing
  Post {body} ->
    MkHttpRequest POST_METHOD cmd.path (Just $ Object body)
  PostEmpty {} ->
    MkHttpRequest POST_METHOD cmd.path Nothing
  Delete {} ->
    MkHttpRequest DELETE_METHOD cmd.path Nothing

-- | Execute a WebDriver HTTP request via @req@, returning the full HTTP response.
runHttpRequest ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m HttpResponse
runHttpRequest baseUrl port mLogger request = do
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

-- todo expand err type eg no session

-- | Parse a WebDriver response, extracting the @value@ property
parseResult :: forall r. (FromJSON r) => Value -> Either ParseFailure r
parseResult body =
  valueParser body
    & maybe
      (Left $ MkParseFailure "No 'value' property found in WebDriver response" body)
      ( \val ->
          first (flip MkParseFailure val . pack) (parseEither @_ @r parseJSON val)
      )

-- | Extract the @value@ property from a WebDriver JSON response
valueParser :: Value -> Maybe Value
valueParser = \case
  Object obj -> parseMaybe (\o -> o .: "value") obj
  _ -> Nothing