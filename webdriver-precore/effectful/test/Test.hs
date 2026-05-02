module Main where

import Bidi.SimpleDemo (bidi_login_demo)
import HTTP.SimpleDemo (http_login_navigation_demo)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Effectful Tests"
    [ testCase "HTTP login and navigation demo" http_login_navigation_demo,
      testCase "BiDi login demo" bidi_login_demo
    ]
