{-|
Test suite for webdriver-precore-exception library
-}
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Data.Aeson (decode, encode, Value(..), object, (.=))
import Data.Text (Text)
import WebDriverPreCore.Exception
import Control.Exception (SomeException, fromException, toException)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Exception Tests"
    [ testGroup
        "Error Type Parsing"
        [ testCase "Parse known error type" test_parseKnownError,
          testCase "Parse unknown error type" test_parseUnknownError
        ],
      testGroup
        "Exception Construction"  
        [ testCase "Create ProtocolException" test_createProtocolException,
          testCase "Create ResponseParseException" test_createResponseParseException
        ]
    ]

test_parseKnownError :: IO ()
test_parseKnownError = do
  case toErrorType "invalid session id" of
    Right InvalidSessionId -> pure ()
    Right other -> fail $ "Expected InvalidSessionId, got: " <> show other
    Left err -> fail $ "Parse failed: " <> show err

test_parseUnknownError :: IO ()
test_parseUnknownError = do
  case toErrorType "some completely made up error" of
    Left _ -> pure ()  -- Unknown errors fail to parse
    Right et -> fail $ "Expected parse failure, got: " <> show et

test_createProtocolException :: IO ()
test_createProtocolException = do
  let ex = ProtocolException
        { error = InvalidSessionId
        , description = "invalid session"
        , message = "test message"
        , stacktrace = Nothing
        , errorData = Nothing
        , response = Null
        }
  case ex of
    ProtocolException {} -> pure ()
    _ -> fail "Expected ProtocolException"

test_createResponseParseException :: IO ()
test_createResponseParseException = do
  let ex = ResponseParseException
        { message = "parse error"
        , response = Null
        }
  case ex of
    ResponseParseException {} -> pure ()
    _ -> fail "Expected ResponseParseException"
