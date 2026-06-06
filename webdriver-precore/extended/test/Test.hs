{-|
Test suite for webdriver-precore-extended library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Internal.LocatorsTest as InternalLocatorsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Extended Tests"
    [
      InternalLocatorsTest.tests
    ]







