-- |
-- Module: WebDriverPreCore.HttpRunner.Utils
-- Description: Low-level HTTP runner utilities for WebDriver commands
--
-- This module provides the low-level plumbing: raw HttpRequest-based variants
-- of the runners and the conversion from typed Commands to HttpRequests.
module WebDriverPreCore.HttpRunner.Utils
  ( -- * Request-level runners
    callWebDriver',
    callWebDriverBody',
    callWebDriverResponse',

    -- * Command conversion
    commandToRequest,

    -- * Re-exports from HttpRunnerBase
    HttpMethod (..),
    HttpRequest (..),
    SubPath (..),
  )
where

import Control.Exception (throw)
import Data.Function ((&))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Text (Text, pack)
import Data.Word (Word16)
import Network.HTTP.Req
  ( Scheme (..),
    Url,
    http,
  )
import Utils qualified
import WebDriverPreCore.HTTP.Protocol (Command (..), WebDriverException (..), parseWebDriverException)
import WebDriverPreCore.HttpRunnerBase
  ( HttpEndpoint (..),
    HttpMethod (..),
    HttpRequest (..),
    HttpResponse (..),
    SubPath (..),
  )
import WebDriverPreCore.HttpRunnerBase qualified as HttpRunnerBase

-- | Execute a WebDriver 'Command', returning just the parsed JSON body.
-- Low-level variant that takes a raw 'HttpRequest'.
callWebDriver' ::
  (MonadIO m, FromJSON r) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m r
callWebDriver' MkHttpEndpoint {host, port} mLogger request =
  parseResult <$> HttpRunnerBase.callWebDriverBody (http host) port mLogger request

-- | Execute a WebDriver HTTP request, returning just the JSON body.
-- Low-level variant that takes a raw 'HttpRequest'.
callWebDriverBody' ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m Value
callWebDriverBody' =
  HttpRunnerBase.callWebDriverBody

-- | Execute a WebDriver HTTP request, returning the full HTTP response.
-- Low-level variant that takes a raw 'HttpRequest'.
callWebDriverResponse' ::
  (MonadIO m) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m HttpResponse
callWebDriverResponse' MkHttpEndpoint {host, port} mLogger request =
  HttpRunnerBase.callWebDriverResponse (http host) port mLogger request

-- | Convert a typed 'Command' to a raw 'HttpRequest'.
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
    path :: SubPath
    path = let Utils.MkSubPath ps = cmd.path in MkSubPath ps

-- imported by callWebDriver' above; re-declared here to avoid circular import

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