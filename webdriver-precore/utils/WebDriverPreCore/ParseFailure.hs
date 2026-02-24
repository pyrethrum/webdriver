module WebDriverPreCore.ParseFailure
  ( ParseFailure (..),
  )
where

import Control.Exception (Exception)
import Data.Aeson (Value)
import Data.Text (Text)

-- | Represents a failure to parse a WebDriver HTTP response.
data ParseFailure = MkParseFailure
  { info :: Text,
    response :: Value
  }
  deriving (Show, Eq)

instance Exception ParseFailure
