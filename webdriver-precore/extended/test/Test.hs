{-|
Test suite for webdriver-precore-extended library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified LocatorsTest
import WebDriverPreCore.Extended.Locators

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Extended Tests"
    [ testCase "Placeholder test" test_placeholder,
      LocatorsTest.tests
    ]

test_placeholder :: IO ()
test_placeholder = True @?= True

trueLoc :: Locator
trueLoc = css "NA"

falseLoc :: Locator
falseLoc = button "NA"




