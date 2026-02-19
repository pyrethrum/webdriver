{-|
Test suite for webdriver-rio-poc library
-}
{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import RIO
import WebDriver.RIO as R
import WebDriverPreCore.Test.ConfigLoader (loadConfig)
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as HTTP
import WebDriverPreCore.Extended.Capabilities 
import WebDriver.RIO.App
import WebDriver.RIO.HTTP.Base.Actions

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "RIO Tests"
    [ 
      -- testCase "Basic Demo" basic_demo
    ]


-- challenge 
  -- basic bidi
  -- basic http
  -- bidi events
  -- bidi and http mixed on the same instance
  -- hooks to start runner and close connection
  -- 
  -- concurrency - 2 browsers

  {-
  import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = withResource acquire release $ \getResource ->
  testGroup "My Tests"
    [ testCase "Test 1" $ do
        resource <- getResource
        -- use resource
        
    , testCase "Test 2" $ do
        resource <- getResource
        -- use same resource
        
    , testCase "Test 3" $ do
        resource <- getResource
        -- use same resource
    ]

-- Before hook: create resource
acquire :: IO MyResource
acquire = doMkFullCapabilitiesRequest
  putStrLn "Setting up resource..."
  -- create and return your resource
  
-- After hook: dispose resource
release :: MyResource -> IO ()
release resource = do
  putStrLn "Cleaning up resource..."
  -- dispose of resource
  
  
  -}

-- >>> basic_demo
basic_demo :: IO ()
basic_demo = do
    fullCapabilities <- loadCapabilities
    runHttp (ConsoleAndFile "eval.log") fullCapabilities $ do
      logInfo "Successfully started WebDriverRIO with provided capabilities"



-- | Example showing how to use withHttpSession to set and get timeouts
--- >>> session_demo
session_demo :: IO ()
session_demo = runHttp MkHttpEnv Console defaultEndpoint WebDriverLogging $ do
  -- Define minimal capabilities
  caps <- liftIO loadCapabilities

  -- Create a session, run actions, and automatically clean up
  withHttpSession caps $ do
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


loadCapabilities :: IO HttpCapabilities
loadCapabilities = do
  config <- loadConfig
  pure $ MkFullCapabilities
    { alwaysMatch = Just . _ $ httpCapabilities config,
      firstMatch = []
    }
   
