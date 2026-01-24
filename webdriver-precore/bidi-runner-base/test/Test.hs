{-|
Test suite for webdriver-precore-bidi-runner-base library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Aeson (object, (.=))
import Data.Text (Text)
import WebDriverPreCore.BiDiRunnerBase

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "BiDi Runner Base Tests"
    [ testGroup
        "BiDiUrl Parsing"
        [ testCase "Parse valid BiDi URL" test_parseBiDiUrl,
          testCase "Parse BiDi URL without port" test_parseBiDiUrlNoPort
        ]
    ]

test_parseBiDiUrl :: IO ()
test_parseBiDiUrl = do
  case parseBiDiUrl "ws://localhost:9222/session/abc" of
    Nothing -> fail "Failed to parse BiDi URL"
    Just (MkBiDiUrl host port _) -> do
      host @?= "localhost"
      port @?= 9222

test_parseBiDiUrlNoPort :: IO ()
test_parseBiDiUrlNoPort = do
  -- URLs without explicit port should fail to parse (port is required)
  case parseBiDiUrl "ws://localhost/session/abc" of
    Nothing -> pure ()  -- Expected: no port means parse failure
    Just _ -> fail "Expected parse failure for URL without port"
