module Main where

import Bidi.Runner (runBiDiTest)
import Bidi.SimpleDemo (bidi_login_demo)
import HTTP.Runner (withHttp)
import HTTP.SimpleDemo (http_login_navigation_demo)
import HTTP.BaseLocateTest qualified as BaseLocateTest
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Effectful Tests"
    [ testCase "HTTP login and navigation demo" (withHttp http_login_navigation_demo),
      testCase "BiDi login demo" (runBiDiTest bidi_login_demo),
      BaseLocateTest.tests
    ]
