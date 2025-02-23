module WebDriverPreCore.HttpResponse (
  HttpResponse(..)
) where

import Data.Aeson (Value)
import Data.Text (Text)
Import Data.Int (Int)


data HttpResponse = Response
  { statusCode :: Int,
    statusMessage :: Text,
    body :: Value
  }
  deriving (Show, Eq)