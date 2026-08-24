-- |
-- Module: WebDriverPreCore.HttpRunner
-- Description: Typed HTTP runner for WebDriver commands
--
-- This module provides a typed HTTP runner that works with webdriver-precore
-- Command types.
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
    ParseFailure(..),

    -- * Re-exports
    Command (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), Value)
import Data.Text (Text)

import WebDriverPreCore.HTTP.Command (Command (..))
import WebDriverPreCore.HttpRunner.Utils
  ( HttpEndpoint (..),
    HttpResponse (..),
    callWebDriver',
    callWebDriverBody',
    callWebDriverResponse',
    commandToRequest,
  )
import WebDriverPreCore.ParseFailure (ParseFailure (..))

import Prelude hiding (log)

-- | Execute a typed 'Command', returning the parsed result.
callWebDriver ::
  (MonadIO m, FromJSON r) =>
  HttpEndpoint ->
  (Text -> m ()) ->
  Command r ->
  m (Either ParseFailure r)
callWebDriver endpoint logger = callWebDriver' endpoint logger . commandToRequest

-- | Execute a typed 'Command', returning just the raw JSON body.
callWebDriverBody ::
  (MonadIO m) =>
  HttpEndpoint ->
  (Text -> m ()) ->
  Command r ->
  m Value
callWebDriverBody endpoint logger =
  callWebDriverBody' endpoint logger . commandToRequest

-- | Execute a typed 'Command', returning the full HTTP response.
callWebDriverResponse ::
  (MonadIO m) =>
  HttpEndpoint ->
  (Text -> m ()) ->
  Command r ->
  m HttpResponse
callWebDriverResponse endpoint logger =
  callWebDriverResponse' endpoint logger . commandToRequest

