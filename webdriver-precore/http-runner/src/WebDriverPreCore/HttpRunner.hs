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

    -- * Re-exports from base
    HttpResponse (..),
  )
where

import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Word (Word16)
import GHC.Exception (throw)
import Control.Monad.IO.Class (MonadIO)
import Utils qualified
import WebDriverPreCore.HTTP.Protocol
  ( Command (..),
    WebDriverException (..),
    parseWebDriverException,
  )
import WebDriverPreCore.HttpRunnerBase
  ( HttpMethod (..),
    HttpRequest (..),
    HttpResponse (..),
    HttpRunnerBase (..),
    UrlPath (..),
    mkHttpRunnerBase,
  )
import Prelude hiding (log)

-- | Typed HTTP runner for WebDriver commands
data HttpRunner m = MkHttpRunner
  { -- | Execute a command and return the typed result
    run :: forall r. (FromJSON r) => Command r -> m r,
    -- | Execute a command and return the full JSON response
    fullResponse :: forall r. (FromJSON r) => Command r -> m Value
  }

-- | Create a typed HTTP runner
mkHttpRunner ::
  (MonadIO m) =>
  -- | Host (e.g. "127.0.0.1")
  Text ->
  -- | Port (e.g. 4444)
  Word16 ->
  -- | Optional logger
  Maybe (Text -> m ()) ->
  HttpRunner m
mkHttpRunner host port mLogger =
  MkHttpRunner
    { run = runCommand base,
      fullResponse = runCommandFullResponse base
    }
  where
    base = mkHttpRunnerBase host port mLogger

-- | Execute a typed command and return the parsed result
runCommand :: forall r m. (FromJSON r, Functor m) => HttpRunnerBase m -> Command r -> m r
runCommand base cmd =
  parseResult <$> runCommandFullResponse base cmd

-- | Execute a typed command and return the full JSON response
runCommandFullResponse :: forall r m. HttpRunnerBase m -> Command r -> m Value
runCommandFullResponse base cmd =
  base.runJson $ commandToRequest cmd

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
    -- Coerce the UrlPath from Utils to our local UrlPath
    path :: UrlPath
    path = MkUrlPath cmd.path.segments

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
