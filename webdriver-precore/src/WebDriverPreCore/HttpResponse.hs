-- | This module defines a minimal 'HttpResponse' data type, used to WebDriver responses.
module WebDriverPreCore.HttpResponse (
  HttpResponse(..)
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Int (Int)
import GHC.Show (Show)
import Data.Eq (Eq)
import Data.Ord (Ord)

-- | 'HttpResponse' represents a WebDriver HTTP response.
data HttpResponse = Response
  { -- | HTTP status code.
    statusCode :: Int,
    -- | HTTP status message.
    statusMessage :: Text,
    -- | Response body in JSON format.
    body :: Value
  }
  deriving (Show, Eq, Ord)