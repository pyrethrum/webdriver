-- |
-- Module: WebDriverPreCore.Extended.Error
-- Description: Error types and utilities for WebDriver, extended with ParseFailure support
--
-- Re-exports all error types from "WebDriverPreCore.Error" and provides a
-- variant of 'parseWebDriverException' that accepts a 'ParseFailure' value
-- directly instead of raw @Text@ and @Value@ arguments.
module WebDriverPreCore.Extended.Error
  ( -- * Re-exports from WebDriverPreCore.Error
    ErrorType (..),
    WebDriverException (..),
    JSONEncodeException (..),
    errorDescription,
    toErrorType,
    toErrorCode,
    parseErrorType,

    -- * Extended parse function
    parseWebDriverException,
  )
where

import WebDriverPreCore.Error
  ( ErrorType (..),
    JSONEncodeException (..),
    WebDriverException (..),
    errorDescription,
    parseErrorType,
    toErrorCode,
    toErrorType,
  )
import WebDriverPreCore.Error qualified as E (parseWebDriverException)
import WebDriverPreCore.ParseFailure (ParseFailure (..))

-- | Parse a 'WebDriverException' from a 'ParseFailure'.
-- This is a convenience wrapper around the base 'WebDriverPreCore.Error.parseWebDriverException'
-- that takes the @info@ and @response@ fields from the 'ParseFailure'.
parseWebDriverException :: ParseFailure -> WebDriverException
parseWebDriverException MkParseFailure {info, response} =
  E.parseWebDriverException info response
