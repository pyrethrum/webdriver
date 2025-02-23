module WebDriverPreCore.HttpResponse (
  HttpResponse(..)
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Int (Int)
import GHC.Show (Show)
import Data.Eq (Eq)


data HttpResponse = Response
  { statusCode :: Int,
    statusMessage :: Text,
    body :: Value
  }
  deriving (Show, Eq)