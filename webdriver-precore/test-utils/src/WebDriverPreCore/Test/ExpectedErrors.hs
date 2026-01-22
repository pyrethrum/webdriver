{-|
Expected error handling utilities for webdriver-precore tests

This module provides functions for testing that actions throw expected errors.
-}
module WebDriverPreCore.Test.ExpectedErrors
  ( FailTest (..),
    toLambda,
    toText,
    expectError,
    expectErrorText,
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text, isInfixOf, unpack)
import Utils (txt)

-- | Test specification for expected error
data FailTest
  = Predicate (Text -> Bool)
  | Fragment Text

toLambda :: FailTest -> (Text -> Bool)
toLambda = \case
  Predicate f -> f
  Fragment t -> \errText -> t `isInfixOf` errText

toText :: FailTest -> Text
toText (Fragment t) = t
toText (Predicate _) = "<custom lambda>"

-- | Test that an IO action throws an exception containing expected text fragment
expectErrorText :: Text -> Text -> IO () -> IO ()
expectErrorText testName expectedFragment =
  expectError testName (Fragment expectedFragment)

-- | General function to test that an IO action throws an exception matching the FailTest
expectError :: Text -> FailTest -> IO () -> IO ()
expectError testName failTest action = do
  result <- try action
  case result of
    Left (e :: SomeException) -> do
      let errText = txt $ show e
      if toLambda failTest errText
        then pure ()
        else
          fail . unpack $
            testName
              <> ": Error did not contain expected fragment."
              <> "\n"
              <> " Expected Fragment was: "
              <> "\n"
              <> toText failTest 
              <> "\n"
              <> "Actual Error was:"
              <> "\n"
              <> errText
    Right _ ->
      fail $ unpack $ testName <> ": Expected error, but action completed successfully."
