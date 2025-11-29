{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Http.HttpResponse (
  HttpResponse(..),
  fromBodyValue
) where

import Data.Aeson (Value, withObject)
import Data.Text (Text)
import Data.Aeson.Types (FromJSON, Parser)
import Data.Function ((&))
import Data.Aeson ((.:))
import Data.Text (unpack)
import Prelude
import Data.Aeson (Value (..), parseJSON)
import Control.Monad (when)
import WebDriverPreCore.Internal.AesonUtils (jsonToText)

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

-- TODO: mark as deprecated
fromBodyValue :: forall a. (FromJSON a) => Bool -> Value -> Parser a
fromBodyValue expectNull rsp =
  rsp & withObject "body value" \b -> do
    val <- b .: "value"
    when (expectNull && val /= Null) $
      fail $ unpack $ "Null value expected but got:\n" <> jsonToText val
    parseJSON $ val

