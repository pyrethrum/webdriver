-- |
-- Module: WebDriverPreCore.HttpRunner
-- Description: Typed HTTP runner for WebDriver commands
--
-- This module provides a typed HTTP runner that works with webdriver-precore
-- Command types, built on top of the JSON-based runner in HttpRunnerBase.
module WebDriverPreCore.HttpRunner
  ( HttpEndpoint (..),
    HttpRequest (..),
    HttpResponse (..),
    SubPath (..),
    callWebDriver,
    callWebDriverBody,
    callWebDriverResponse,

    --  low level
    callWebDriverBody',
    commandToRequest,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Word (Word16)
import GHC.Exception (throw)
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
import Utils qualified
import WebDriverPreCore.HTTP.Protocol
  ( Command (..),
    WebDriverException (..),
    parseWebDriverException,
  )
import WebDriverPreCore.HttpRunnerBase 
  ( HttpEndpoint (..),
    HttpMethod (..),
    HttpRequest (..),
    HttpResponse (..),
    SubPath (..),
    callWebDriverResponse,
  )
import WebDriverPreCore.HttpRunnerBase qualified as HttpRunnerBase

import Prelude hiding (log)

callWebDriver ::
  (MonadIO m, FromJSON r) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m r
callWebDriver MkHttpEndpoint {host, port} mLogger request =
  parseResult <$> HttpRunnerBase.callWebDriverBody baseUrl port mLogger request
  where
    baseUrl = http host

callWebDriverBody ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  Command a ->
  m Value
callWebDriverBody url port lgr =
  callWebDriverBody' url port lgr . commandToRequest

callWebDriverBody' ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m Value
callWebDriverBody' =
  HttpRunnerBase.callWebDriverBody

-- | Convert a typed Command to an HttpRequest
commandToRequest :: Command r -> HttpRequest
commandToRequest cmd = case cmd of
  Get {} ->
    MkHttpRequest GET_METHOD path Nothing
  Post {body} ->
    MkHttpRequest POST_METHOD path (Just $ Object body)
  PostEmpty {} ->
    MkHttpRequest POST_METHOD path Nothing
  Delete {} ->
    MkHttpRequest DELETE_METHOD path Nothing
  where
    -- Unpack Utils.SubPath and repack as HttpRunnerBase.SubPath
    path :: SubPath
    path = let Utils.MkSubPath ps = cmd.path in MkSubPath ps

-- | Parse a WebDriver response, extracting the 'value' property
parseResult :: forall r. (FromJSON r) => Value -> r
parseResult body =
  valueParser body
    & maybe
      (throw $ ResponseParseException "No value property found in WebDriver response" body)
      ( \val ->
          parseEither @_ @r parseJSON val
            & either
              (\e -> throw $ parseWebDriverException (pack e) val)
              id
      )

-- Parser for the "value" property in WebDriver responses
valueParser :: Value -> Maybe Value
valueParser = \case
  Object obj -> parseMaybe (\o -> o .: "value") obj
  _ -> Nothing
