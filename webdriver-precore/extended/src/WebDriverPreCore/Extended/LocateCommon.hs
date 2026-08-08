module WebDriverPreCore.Extended.LocateCommon
  (
    PreLocateException(..),
    LocateException(..),
    LeafCardinality(..),
    addLocToException,
    runLoc
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

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

-- | Run a locator action, catching:
--
--   1. the driver exception (e.g. 'WebDriverException' from the underlying HTTP
--      or BiDi call) — wrapped as 'DriverException''
--   2. any 'PreLocateException' thrown via 'throw'
--      (e.g. 'AmbiguousLocator'', 'ElementNotFound'')
--
-- Generalised over the locator type @l@ and the result type @r@ so it can be
-- shared between the HTTP and BiDi backends. For HTTP @r ~ [ElementId]@; for
-- BiDi @r ~ 'BiDiP.LocateNodesResult'@.
runLoc :: forall m l r. Applicative m =>
  (forall x e. (HasCallStack, Exception e) => m x -> (e -> m x) -> m x) -- ^ catch
  -> (l -> m r) -- ^ locator action
  -> l -- ^ locator
  -> m (Either PreLocateException r)
runLoc catch locAction loc =
  catch  -- catch PreLocateException thrown via 'throw' (e.g. AmbiguousLocator', ElementNotFound')
    (catch -- catch the driver exception (e.g. WebDriverException) from underlying HTTP / BiDi calls
      (Right <$> locAction loc)
      (pure . Left . DriverException'))
    (pure . Left)

data LeafCardinality = FindFirst | FindAll deriving (Show, Eq, Generic)

instance ToJSON LeafCardinality

instance FromJSON LeafCardinality
