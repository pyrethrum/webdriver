{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Http.HttpResponse {-# DEPRECATED "HttpResponse" #-} (
  HttpResponse(..)
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Prelude

-- TODO: mark as deprecated
-- | 'HttpResponse' represents a WebDriver HTTP response.
--
-- An instance of 'HttpResponse' needs to be constructed in order to run the 'WebDriverPreCore.parser' suppplied in the [W3CSpec](WebDriverPreCore.Get)
data HttpResponse = MkHttpResponse
  { -- | HTTP status code.
    statusCode :: Int,
    -- | HTTP status message.
    statusMessage :: Text,
    -- | Response body in JSON format.
    body :: Value
  }
  deriving (Show, Eq, Ord)


