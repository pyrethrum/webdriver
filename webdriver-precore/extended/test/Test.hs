{-|
Test suite for webdriver-precore-extended library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Internal.LocatorsTest as InternalLocatorsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Extended Tests"
    [ testCase "Placeholder test" test_placeholder,
      InternalLocatorsTest.tests
    ]

test_placeholder :: IO ()
test_placeholder = True @?= True






