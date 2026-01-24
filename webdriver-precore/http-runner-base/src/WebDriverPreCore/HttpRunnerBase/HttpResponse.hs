{-|
Module: WebDriverPreCore.HttpRunnerBase.HttpResponse
Description: HTTP Response type decoupled from WebDriver-specific types

This module provides the HttpResponse type for representing HTTP responses
without any dependency on webdriver-precore types.
-}
module WebDriverPreCore.HttpRunnerBase.HttpResponse
  ( HttpResponse (..)
  )
where

import Data.Aeson (Value)
import Data.Text (Text)

-- | HTTP response from a WebDriver endpoint
data HttpResponse = MkHttpResponse
  { -- | HTTP status code
    statusCode :: Int,
    -- | HTTP status message
    statusMessage :: Text,
    -- | Response body as JSON
    body :: Value
  }
  deriving (Show, Eq)
