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
import WebDriverPreCore.Extended.HTTP.Base.Protocol as HTTP
import WebDriverPreCore.Extended.Capabilities (FullCapabilitiesRequest (..), fromHttpCapabilities)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "RIO Tests"
    [ testCase "Basic Demo" basic_demo
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
acquire = do
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
    runWebDriver (ConsoleAndFile "eval.log") fullCapabilities $ do
      logInfo "Successfully started WebDriverRIO with provided capabilities"


loadCapabilities :: IO FullCapabilitiesRequest
loadCapabilities = do
  config <- loadConfig
  pure $ MkFullCapabilitiesRequest
    { alwaysMatch = Just . fromHttpCapabilities $ httpCapabilities config,
      firstMatch = []
    }
   
