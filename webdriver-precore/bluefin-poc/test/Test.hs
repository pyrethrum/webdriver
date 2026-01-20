{-|
Test suite for webdriver-bluefin-poc library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Bluefin Tests"
    [ testCase "Placeholder test" test_placeholder
    ]

test_placeholder :: IO ()
test_placeholder = True @?= True
