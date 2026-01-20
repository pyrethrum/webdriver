{-|
Test suite for webdriver-precore-bidi-runner library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import WebDriverPreCore.BiDiRunner
import WebDriverPreCore.BiDiRunnerBase (BiDiUrl(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "BiDi Runner Tests"
    [ testGroup
        "Module Exports"
        [ testCase "BiDiRunner type available" test_bidiRunnerType,
          testCase "BiDiUrl parsing" test_bidiUrlParsing
        ]
    ]

-- Basic test to ensure module exports are accessible
test_bidiRunnerType :: IO ()
test_bidiRunnerType = do
  -- Just verify the types are importable
  let _ = undefined :: BiDiRunner
  pure ()

test_bidiUrlParsing :: IO ()
test_bidiUrlParsing = do
  case parseBiDiUrl "ws://localhost:9222/session" of
    Nothing -> fail "Failed to parse BiDi URL"
    Just url -> url.host @?= "localhost"
