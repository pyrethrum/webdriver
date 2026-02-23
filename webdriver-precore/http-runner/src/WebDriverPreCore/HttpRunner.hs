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
  )
import WebDriverPreCore.HTTP.Command (Command (..))
import WebDriverPreCore.HttpRunner.Utils
  ( HttpEndpoint (..),
    HttpResponse (..),
    callWebDriver',
    callWebDriverBody',
    callWebDriverResponse',
    commandToRequest,
  )

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
  callWebDriverResponse' endpoint mLogger (commandToRequest cmd)

