module Main where

import BiDi.Runner (withBidi)
import BiDi.SimpleDemo (bidi_login_demo)
import BiDi.BaseLocateTest qualified as BiDiBaseLocateTest
import BiDi.LocateCombinatorTest qualified as BiDiLocateCombinatorTest
import HTTP.Runner (withHttp)
import HTTP.SimpleDemo (http_login_navigation_demo)
import HTTP.BaseLocateTest qualified as BaseLocateTest
import HTTP.LocateCombinatorTest qualified as LocateCombinatorTest
import HTTP.MatchTypeTest qualified as MatchTypeTest
import Common.LocatorCombinatorSanityTests qualified as LocatorCombinatorSanityTests
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Effectful Tests"
    [ testCase "HTTP login and navigation demo" (withHttp http_login_navigation_demo),
      testCase "BiDi login demo" (withBidi bidi_login_demo),
      BaseLocateTest.tests,
      BiDiBaseLocateTest.tests,
      MatchTypeTest.tests,
      LocatorCombinatorSanityTests.tests,
      LocateCombinatorTest.tests,
      BiDiLocateCombinatorTest.tests
    ]
