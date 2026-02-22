-- |
-- Module: WebDriverPreCore.HttpRunner
-- Description: Typed HTTP runner for WebDriver commands
--
-- This module provides a typed HTTP runner that works with webdriver-precore
-- Command types, built on top of the JSON-based runner in HttpRunnerBase.
--
-- For low-level utilities (HttpRequest-based runners, commandToRequest, etc.) see
-- "WebDriverPreCore.HttpRunner.Utils".
module WebDriverPreCore.HttpRunner
  ( -- * Typed runners
    callWebDriver,
    callWebDriverBody,
    callWebDriverResponse,

    -- * Types
    HttpEndpoint (..),
    HttpResponse (..),

    -- * Re-exports
    Command (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), Value)
import Data.Text (Text)
import Data.Word (Word16)
import Network.HTTP.Req
  ( Scheme (..),
    Url,
    http,
  )
import WebDriverPreCore.HTTP.Protocol (Command (..))
import WebDriverPreCore.HttpRunnerBase
  ( HttpEndpoint (..),
    HttpResponse (..),
  )
import WebDriverPreCore.HttpRunner.Utils
  ( callWebDriver',
    callWebDriverBody',
    commandToRequest,
  )
import WebDriverPreCore.HttpRunnerBase qualified as HttpRunnerBase

import Prelude hiding (log)

-- | Execute a typed 'Command', returning the parsed result.
callWebDriver ::
  (MonadIO m, FromJSON r) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  Command a ->
  m r
callWebDriver endpoint mLogger = callWebDriver' endpoint mLogger . commandToRequest

-- | Execute a typed 'Command', returning just the raw JSON body.
callWebDriverBody ::
  (MonadIO m) =>
  Url 'Http ->
  Word16 ->
  Maybe (Text -> m ()) ->
  Command a ->
  m Value
callWebDriverBody url port lgr =
  callWebDriverBody' url port lgr . commandToRequest

-- | Execute a typed 'Command', returning the full HTTP response.
callWebDriverResponse ::
  (MonadIO m) =>
  HttpEndpoint ->
  Maybe (Text -> m ()) ->
  Command a ->
  m HttpResponse
callWebDriverResponse endpoint mLogger cmd =
  HttpRunnerBase.callWebDriverResponse (http host) port mLogger (commandToRequest cmd)
  where
    MkHttpEndpoint {host, port} = endpoint

