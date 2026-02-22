-- |
-- Module: WebDriverPreCore.HttpRunner
-- Description: Typed HTTP runner for WebDriver commands
--
-- This module provides a typed HTTP runner that works with webdriver-precore
-- Command types, built on top of the JSON-based runner in HttpRunnerBase.
module WebDriverPreCore.HttpRunner
  ( HttpEndpoint (..),
    callWebDriver,
    callWebDriverResponse,
    callWebDriverJson,
    commandToRequest,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Function ((&))
import Data.Text (Text, pack)
import GHC.Exception (throw)
import Network.HTTP.Req (http)
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
    SubPath (..),
    callWebDriverJson,
    callWebDriverResponse,
  )
import Prelude hiding (log)

callWebDriver ::
  (MonadIO m, FromJSON r) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  HttpRequest ->
  m r
callWebDriver MkHttpEndpoint {host, port} mLogger request =
  parseResult <$> callWebDriverJson baseUrl port mLogger request
  where
    baseUrl = http host

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
