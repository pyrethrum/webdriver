{-# LANGUAGE DataKinds #-}

-- |
-- Test suite for webdriver-rio-poc library
module Main where

import RIO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import WebDriver.RIO hiding (runHttp, withHttpSession)
import WebDriver.RIO qualified as R
import WebDriver.RIO.HTTP.Base.Actions
import WebDriverPreCore.Extended.Capabilities
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as HTTP
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "RIO Tests"
    []

-- testCase "Basic Demo" basic_demo

-- challenge
-- basic bidi
-- basic http
-- bidi events
-- bidi and http mixed on the same instance
-- hooks to start runner and close connection
--
-- concurrency - 2 browsers

runHttp' :: (Config -> RIO HttpEnv a) -> IO a
runHttp' httpAction = do
  config@MkConfig {httpPort, httpUrl, logging} <- loadConfig
  let apiLogging =
        if logging
          then WebDriverLogging
          else NoWebDriverLogging
      logConfig = ConsoleAndFile "eval.log"
      endPoint = MkHttpEndpoint {host = httpUrl, port = httpPort}
  R.runHttp MkHttpEnv logConfig endPoint apiLogging (httpAction config)

runHttp :: RIO HttpEnv a -> IO a
runHttp httpAction = runHttp' (const httpAction)

withSession :: RIO HttpSessionEnv a -> IO a
withSession sessionAction = runHttp' $ \config -> do
  let caps = mkHttpCaps config
  R.withHttpSession caps sessionAction

mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch = []
    }

loadCapabilities :: IO HttpCapabilities
loadCapabilities = do
  config <- loadConfig
  pure $
    MkFullCapabilities
      { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
        firstMatch = []
      }

-- >>> basic_demo
basic_demo :: IO ()
basic_demo =
  runHttp $ logInfo "Loaded eval config"

-- | Example showing how to use withHttpSession to set and get timeouts

--- >>> session_demo
session_demo :: IO ()
session_demo = withSession $ do
  logInfo "Session created, setting timeouts"

  -- Set new timeout values
  let newTimeouts =
        MkTimeouts
          { implicit = Just 5000, -- 5 seconds
            pageLoad = Just 60000, -- 60 seconds
            script = Just 30000 -- 30 seconds
          }
  setTimeouts newTimeouts

  -- Get and log the current timeouts
  currentTimeouts <- getTimeouts
  logInfo $ "Current timeouts: " <> displayShow currentTimeouts
