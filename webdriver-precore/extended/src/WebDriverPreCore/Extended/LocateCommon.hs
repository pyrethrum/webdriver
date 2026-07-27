module WebDriverPreCore.Extended.LocateCommon
  (
    PreLocateException(..),
    LocateException(..),
    LeafCardinality(..),
    WDTrace(..),
    LocateTracing(..),
    LocateResult(..),
    completeLocException
  )
where

import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.Text

import WebDriverPreCore.Extended.HTTP.Base.Protocol as HTTPB (ElementId)
import WebDriverPreCore.Extended.Locators.Internal (Locator, RoleLocator (..), CompoundLocator, HttpLoc (..))
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.Extended.Protocol (WebDriverException)
import WebDriverPreCore.HTTP.Protocol as HTTPP (Selector (..))
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

data WDTrace = Prepared {
  loc :: Locator,
  reducedLoc :: CompoundLocator HttpLoc
} |
 PrepareFailed {
  loc :: Locator,
  error :: LI.InvalidLocator
} | 
 JSDisplayedCheck {
  beforeCheck :: [ElementId],
  afterCheck :: [ElementId]
} |
 LeafLocate {
  selector :: Selector,
  cardinality :: LeafCardinality,
  found :: [ElementId]
  } | 
  RoleSecondPassLabeledBy {
    role :: RoleLocator,
    elms :: [ElementId]
  } |
  RoleSecondPassFor {
    role :: RoleLocator,
    elms :: [ElementId]
  }
 deriving (Show, Eq)

data LocateTracing = LocateTracing | NoLocateTracing deriving (Show, Eq)

data LocateResult = 
  Locate
  { result :: Either LocateException [ElementId]
  } |
  LocateWithTrace 
  { result :: Either LocateException [ElementId]
  , trace :: [WDTrace]
  } deriving (Show, Eq)
