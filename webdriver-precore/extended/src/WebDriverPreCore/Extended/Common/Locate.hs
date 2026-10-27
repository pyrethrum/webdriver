module WebDriverPreCore.Extended.Common.Locate
  (
    DisplayedCheck(..),
    PreLocateException(..),
    LocateException(..),
    LeafCardinality(..),
    addLocToException,
    runLoc,
    displayedFunctionDeclaration,
    displayedJS,
    shouldRunDisplayedCheck,
    filterDisplayed,
    filterDisplayedForSingleton
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import WebDriverPreCore.Extended.Common.Locators.Internal (Locator)
import WebDriverPreCore.Extended.Common.Locators.Internal qualified as LI
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

-- | Controls whether located elements are post-filtered by the WebDriver
--   "element displayed" algorithm.
--
--   * 'DisplayedCheckNever' — no visibility filtering.
--   * 'DisplayedCheckDisambiguateUnique' — only filter when a unique singleton
--     locate finds multiple candidates, to resolve the ambiguity.
--   * 'DisplayedCheckAlways' — always filter by displayedness.
data DisplayedCheck = DisplayedCheckNever | DisplayedCheckDisambiguateUnique | DisplayedCheckAlways deriving (Show, Eq)

-- | JavaScript function expression implementing the W3C WebDriver "element
--   displayed" algorithm. It takes an array of elements and returns an array of
--   booleans in the same order. This is the shape required by BiDi's
--   @script.callFunction@ @functionDeclaration@.
displayedFunctionDeclaration :: Text
displayedFunctionDeclaration =
  "function(els) {\n\
  \  function isDisplayed(el) {\n\
  \    if (!el || !el.isConnected) return false;\n\
  \\n\
  \    const style = getComputedStyle(el);\n\
  \\n\
  \    if (style.display === \"none\") return false;\n\
  \    if (style.visibility === \"hidden\" || style.visibility === \"collapse\") return false;\n\
  \    if (parseFloat(style.opacity) === 0) return false;\n\
  \\n\
  \    if (el.tagName === \"INPUT\" && el.type === \"hidden\")\n\
  \      return false;\n\
  \\n\
  \    if (el.offsetWidth === 0 || el.offsetHeight === 0)\n\
  \      return false;\n\
  \\n\
  \    return true;\n\
  \  }\n\
  \  return Array.from(els).map(isDisplayed);\n\
  \}"

-- | Classic WebDriver script body for HTTP @executeScript@. It invokes
--   'displayedFunctionDeclaration' with @arguments[0]@ (the array of element
--   references) and returns the resulting array of booleans.
displayedJS :: Text
displayedJS = "return (" <> displayedFunctionDeclaration <> ")(arguments[0]);"

-- | Pure decision: should a displayed post-filter be applied for a locate?
--
--   The boolean indicates whether the locate is a unique singleton, which is
--   the only case where 'DisplayedCheckDisambiguateUnique' triggers a re-check.
shouldRunDisplayedCheck :: DisplayedCheck -> Bool -> Bool
shouldRunDisplayedCheck check isUnique =
  check == DisplayedCheckAlways || (check == DisplayedCheckDisambiguateUnique && isUnique)

-- | Run a displayed post-filter using a backend-provided script runner.
--
--   @runScript@ receives the candidate element references and returns one
--   boolean per element (the result of the displayed check). The candidates are
--   then zipped with the results, keeping only the displayed elements.
filterDisplayed :: forall m el. (Monad m) => ([el] -> m [Bool]) -> [el] -> m [el]
filterDisplayed runScript elms = do
  bools <- runScript elms
  pure $ P.map P.fst . P.filter P.snd $ P.zip elms bools

-- | Displayed-filter a singleton candidate list, running the filter only when
--   there is more than one candidate (a lone candidate, or none, needs no
--   JavaScript round-trip).
filterDisplayedForSingleton :: (Monad m) => ([el] -> m [el]) -> [el] -> m [el]
filterDisplayedForSingleton filterAction = \case
  [] -> pure []
  [x] -> pure [x]
  xs -> filterAction xs
