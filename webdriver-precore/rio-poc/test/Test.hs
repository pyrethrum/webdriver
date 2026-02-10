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
   
