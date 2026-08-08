module WebDriverPreCore.Extended.LocateCommon
  (
    PreLocateException(..),
    LocateException(..),
    LeafCardinality(..),
    addLocToException
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics (Generic)

import WebDriverPreCore.Extended.Locators.Internal (Locator)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.Error ( WebDriverException )
import Prelude as P hiding (log)

data PreLocateException
  = AmbiguousLocator'  Text
  | ElementNotFound' Text
  | InvalidLocator' LI.InvalidLocator
  | DriverException' WebDriverException
  deriving (Show, Eq)

data LocateException
  = AmbiguousLocator
      { description :: Text,
        locator :: Locator
      }
  | ElementNotFound
      { description :: Text,
        locator :: Locator
      }
  | InvalidLocator LI.InvalidLocator
  | DriverException {
      driverException :: WebDriverException,
      locator :: Locator
    }
  deriving (Show, Eq)

addLocToException :: Locator -> PreLocateException -> LocateException
addLocToException loc = \case
  AmbiguousLocator' desc -> AmbiguousLocator desc loc
  ElementNotFound' desc -> ElementNotFound desc loc
  InvalidLocator' e -> InvalidLocator e
  DriverException' e -> DriverException e loc

instance Exception LocateException
instance Exception PreLocateException

data LeafCardinality = FindFirst | FindAll deriving (Show, Eq, Generic)

instance ToJSON LeafCardinality

instance FromJSON LeafCardinality
