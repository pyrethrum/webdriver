-- |
-- Module: WebDriverPreCore.HttpRunner
-- Description: Typed HTTP runner for WebDriver commands
--
-- This module provides a typed HTTP runner that works with webdriver-precore
-- Command types, built on top of the JSON-based runner in HttpRunnerBase.
module WebDriverPreCore.HttpRunner
  ( -- * HTTP Runner
    HttpRunner (..),
    mkHttpRunner,
    HttpEndpoint (..),
    callWebDriver,
    callWebDriverResponse,
    callWebDriverJson,
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
    HttpResponse (..),
    HttpRunnerBase (..),
    SubPath (..),
    callWebDriverJson,
    callWebDriverResponse,
    mkHttpRunnerBase,
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

-- | Typed HTTP runner for WebDriver commands
data HttpRunner m = MkHttpRunner
  { -- | Execute a command and return the typed result
    run :: forall r. (FromJSON r) => Command r -> m r,
    -- | Execute a command and return the JSON body
    runBody :: forall r. (FromJSON r) => Command r -> m Value,
    -- | Execute a command and return the full HTTP response
    runFull :: forall r. (FromJSON r) => Command r -> m HttpResponse
  }

-- | Create a typed HTTP runner
mkHttpRunner ::
  (MonadIO m) =>
  -- | Host (e.g. "127.0.0.1")
  HttpEndpoint ->
  -- | Optional logger
  Maybe (Text -> m ()) ->
  HttpRunner m
mkHttpRunner httpEndpoint mLogger =
  MkHttpRunner
    { run = runCommand base,
      runBody = runCommandBody base,
      runFull = runCommandFullResponse base
    }
  where
    base = mkHttpRunnerBase httpEndpoint mLogger

-- | Execute a typed command and return the parsed result
runCommand :: forall r m. (FromJSON r, Functor m) => HttpRunnerBase m -> Command r -> m r
runCommand base cmd =
  parseResult <$> runCommandBody base cmd

-- | Execute a typed command and return the full JSON response
runCommandBody :: forall r m. HttpRunnerBase m -> Command r -> m Value
runCommandBody base = base.runBody . commandToRequest

-- | Execute a typed command and return the full HTTP response
runCommandFullResponse :: forall r m. HttpRunnerBase m -> Command r -> m HttpResponse
runCommandFullResponse base = base.runFull . commandToRequest

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
    -- Coerce the SubPath from Utils to our local SubPath
    path :: SubPath
    path = MkSubPath cmd.path.parts

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
