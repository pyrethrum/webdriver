{-|
Test suite for webdriver-rio-poc library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import RIO
import WebDriver.RIO as R
import WebDriverPreCore.Test.ConfigLoader (loadConfig)
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Extended.HTTP.Base.Protocol as HTTP

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


loadCapabilities :: IO (R.FullCapabilities HTTP.Capabilities)
loadCapabilities = do
  config <- loadConfig
  pure $ R.MkFullCapabilities
    { alwaysMatch = Just $ httpCapabilities config,
      firstMatch = []
    }
   
