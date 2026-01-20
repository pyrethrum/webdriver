{-|
Module: WebDriverPreCore.HttpRunner
Description: Typed HTTP runner for WebDriver commands

This module provides a typed HTTP runner that works with webdriver-precore
Command types, built on top of the JSON-based runner in HttpRunnerBase.
-}
module WebDriverPreCore.HttpRunner
  ( -- * HTTP Runner
    HttpRunner (..),
    mkHttpRunner,
    
    -- * Re-exports from base
    HttpResponse (..),
    
    -- * Command Execution
    runCommand,
    runCommandFullResponse,
    
    -- * Response Parsing
    parseResultIO,
  )
where

import Data.Aeson (FromJSON (..), Value (..), Object, (.:))
import Data.Aeson.Types (parseMaybe, parseEither)
import Data.Function ((&))
import Data.Text (Text, pack)
import GHC.Exception (throw)
import Utils qualified
import WebDriverPreCore.Exception 
  ( WebDriverException (..),
    parseWebDriverException,
  )
import WebDriverPreCore.HttpRunnerBase 
  ( HttpRunnerBase (..),
    HttpRequest (..),
    HttpMethod (..),
    UrlPath (..),
    HttpResponse (..),
    mkHttpRunnerBase,
  )
import WebDriverPreCore.HTTP.Protocol (Command (..))
import Prelude hiding (log)

-- | Typed HTTP runner for WebDriver commands
data HttpRunner = MkHttpRunner
  { -- | Execute a command and return the typed result
    run :: forall r. (FromJSON r) => Command r -> IO r,
    -- | Execute a command and return the full JSON response
    fullResponse :: forall r. (FromJSON r) => Command r -> IO Value
  }

-- | Create a typed HTTP runner
mkHttpRunner 
  :: Text          -- ^ Host (e.g. "127.0.0.1")
  -> Int           -- ^ Port (e.g. 4444)
  -> Maybe (Text -> IO ())  -- ^ Optional logger
  -> HttpRunner
mkHttpRunner host port mLogger =
  MkHttpRunner
    { run = runCommand base,
      fullResponse = runCommandFullResponse base
    }
  where
    base = mkHttpRunnerBase host port mLogger

-- | Execute a typed command and return the parsed result
runCommand :: forall r. (FromJSON r) => HttpRunnerBase -> Command r -> IO r
runCommand base cmd = do
  rsp <- runCommandFullResponse base cmd
  parseResultIO rsp

-- | Execute a typed command and return the full JSON response
runCommandFullResponse :: forall r. HttpRunnerBase -> Command r -> IO Value
runCommandFullResponse base cmd = 
  base.runJson $ commandToRequest cmd

-- | Convert a typed Command to an HttpRequest
commandToRequest :: Command r -> HttpRequest
commandToRequest = \case
  Get {path} -> 
    MkHttpRequest GET_METHOD (coerceUrlPath path) Nothing
  Post {path, body} -> 
    MkHttpRequest POST_METHOD (coerceUrlPath path) (Just $ Object body)
  PostEmpty {path} -> 
    MkHttpRequest POST_METHOD (coerceUrlPath path) Nothing
  Delete {path} -> 
    MkHttpRequest DELETE_METHOD (coerceUrlPath path) Nothing
  where
    -- Coerce the UrlPath from Utils to our local UrlPath
    coerceUrlPath :: Utils.UrlPath -> UrlPath
    coerceUrlPath (Utils.MkUrlPath segs) = MkUrlPath segs

-- | Parse a WebDriver response, extracting the 'value' property
parseResultIO :: forall r. (FromJSON r) => Value -> IO r
parseResultIO body =
  valueParser body
    & maybe
      (throw $ ResponseParseException "No value property found in WebDriver response" body)
      ( \val ->
          parseEither @_ @r parseJSON val
            & either
              (\e -> throw $ parseWebDriverException (pack e) val)
              pure
      )

-- Parser for the "value" property in WebDriver responses
valueParser :: Value -> Maybe Value
valueParser = \case
  Object obj -> parseMaybe (\o -> o .: "value") obj
  _ -> Nothing
