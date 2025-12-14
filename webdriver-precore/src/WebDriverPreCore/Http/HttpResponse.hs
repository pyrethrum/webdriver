{-|
Description : Deprecated

-}
module WebDriverPreCore.Http.HttpResponse {-# DEPRECATED "HttpResponse - will be removed in a future release ~ 2027-02-01. See ChangeLog.md for upgrade instructions" #-} (
  HttpResponse(..)
) where

import Data.Aeson (Value)
import Data.Text (Text)

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


