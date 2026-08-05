module WebDriverPreCore.Extended.LocateCommon
  (
    PreLocateException(..),
    LocateException(..),
    LeafCardinality(..),
    LocateTracing(..),
    LocateResult(..),
    completeLocException
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

completeLocException :: forall a m. Functor m => Locator -> m (Either PreLocateException a) ->  m (Either LocateException a)
completeLocException  locator action = 
  first convert <$> action
  where 
    convert = \case 
      AmbiguousLocator' desc -> AmbiguousLocator desc locator
      ElementNotFound' desc -> ElementNotFound desc locator
      InvalidLocator' e -> InvalidLocator e
      DriverException' e -> DriverException e locator

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
