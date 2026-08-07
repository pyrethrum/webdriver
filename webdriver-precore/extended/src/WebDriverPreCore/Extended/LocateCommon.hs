module WebDriverPreCore.Extended.LocateCommon
  (
    PreLocateException(..),
    LocateException(..),
    LeafCardinality(..),
    LocateTracing(..),
    LocateResult(..),
    addLocToException
  )
where

import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.Text

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

data LeafCardinality = FindFirst | FindAll deriving (Show, Eq)


data LocateTracing = LocateTracing | NoLocateTracing deriving (Show, Eq)

data LocateResult t r = 
  Locate
  { result :: Either LocateException r
  } |
  LocateWithTrace 
  { result :: Either LocateException r
  , trace :: [t]
  } deriving (Show, Eq)
