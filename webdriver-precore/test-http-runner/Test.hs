{-|
Test suite for webdriver-precore-http-runner library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import WebDriverPreCore.HttpRunner
import WebDriverPreCore.HTTP.Protocol (Command(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "HTTP Runner Tests"
    [ testGroup
        "Module Exports"
        [ testCase "HttpRunner type available" test_httpRunnerType
        ]
    ]

-- Basic test to ensure module exports are accessible
test_httpRunnerType :: IO ()
test_httpRunnerType = do
  -- Just verify the types are importable
  let _ = undefined :: HttpRunner
  pure ()
