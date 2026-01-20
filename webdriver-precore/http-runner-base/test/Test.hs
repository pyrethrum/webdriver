{-|
Test suite for webdriver-precore-http-runner-base library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import WebDriverPreCore.HttpRunnerBase

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "HTTP Runner Base Tests"
    [ testGroup
        "UrlPath"
        [ testCase "Create UrlPath" test_createUrlPath
        ],
      testGroup
        "HttpResponse"
        [ testCase "HttpResponse type available" test_httpResponseType
        ]
    ]

test_createUrlPath :: IO ()
test_createUrlPath = do
  let MkUrlPath segs = MkUrlPath ["session", "abc123", "url"]
  length segs @?= 3

test_httpResponseType :: IO ()
test_httpResponseType = do
  -- Just verify the type is accessible
  let _ = undefined :: HttpResponse
  pure ()
