module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified ApiCoverageTest as API
import qualified ErrorCoverageTest as Error
import qualified JSONParsingTest as JSON
import BiDi.DemoUtils (BiDiDemo(..), runDemo')
import Const (Timeout(MkTimeout, microseconds))
import Prelude
import Data.Text (unpack)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

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
          testCase "Round trip error codes" Error.unit_round_trip_error_codes
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

fromBidiDemo :: BiDiDemo -> TestTree
fromBidiDemo demo = 
  testCase (unpack demo.name) $ runDemo' MkTimeout {microseconds = 0} demo


bidiDemos :: TestTree
bidiDemos =