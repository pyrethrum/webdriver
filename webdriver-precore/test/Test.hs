{-# LANGUAGE CPP #-}

module Main where

import ApiCoverageTest qualified as API
-- NOTE: BiDi demos have been migrated to bidi-runner/test
-- All BiDi.Demos imports have been removed
import ErrorCoverageTest qualified as Error
-- NOTE: HTTP demos have been migrated to http-runner/test
-- import HTTP.DemoUtils (HttpDemo (..), runDemoWithConfig)
-- import HTTP.ErrorDemo qualified as HttpError
-- import HTTP.HttpDemo qualified as Http
-- #ifndef LEGACY_TEST
-- import HTTP.FallbackDemo qualified as HttpFallback
-- #endif
import JSONParsingTest qualified as JSON
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)


main :: IO ()
main = do
  -- testCfg <- loadConfig
  -- defaultMain $ httpDemoSingleIsolated testCfg 
  -- defaultMain $ bidiSingleForDebug testCfg 
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
#ifdef LEGACY_TEST
    [ 
      unitTests
      -- NOTE: httpDemos have been migrated to http-runner/test
      -- httpDemos cfg
    ]
#else
    [ unitTests,
      propertyTests
      -- NOTE: httpDemos have been migrated to http-runner/test
      -- httpDemos cfg
      -- NOTE: bidiDemos have been migrated to bidi-runner/test
      -- bidiDemos cfg
    ]
#endif

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testGroup
        "API Coverage"
        [ testCase "All endpoints covered" API.unit_test_all_endpoints_covered
        ],
      testGroup
        "Error Coverage"
        [ testCase "All errors covered" Error.unit_test_all_errors_covered,
          testCase "Round trip error codes" Error.unit_round_trip_error_codes,
          testCase "All BiDi errors covered" Error.unit_test_all_errors_covered,
          testCase "Round trip BiDi error codes" Error.unit_round_trip_error_codes
        ],
      testGroup
        "JSON Parsing"
        [ testCase "WebSocket URL from JSON" JSON.unit_websocketUrlFromJSon
        ]
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ testGroup
        "JSON Parsing"
        [ JSON.test_round_trip
        ]
    ]

