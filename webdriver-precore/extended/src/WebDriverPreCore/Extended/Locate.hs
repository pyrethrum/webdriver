module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    Cardinality (..),
    LocateOps (..),
    locateHttp,
    displayedJS,
    isDisplayedHttp,
  )
where

import Control.Exception (Exception, SomeException)
import Control.Monad (filterM, (>=>))
import Data.Aeson as A (Result (..), Value, fromJSON, toJSON)
import Data.Function ((&))
import Data.List qualified as LST
import Data.Maybe (fromMaybe)
import Data.Text
import GHC.Stack (HasCallStack)
import WebDriverPreCore.Extended.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol as HTTPB (ElementId)
import WebDriverPreCore.Extended.HTTP.Internal (Runner)
import WebDriverPreCore.Extended.Locators.Internal (Locator, Protocol (..), RoleLocator (..))
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.Extended.Protocol (Session, WebDriverException)
import WebDriverPreCore.Extended.SimplifiedLocator.Internal as L (SimplifiedLocator (..), prepareSimplify)
import WebDriverPreCore.HTTP.Protocol as HTTPP (Command, Script (..), Selector (..))
import Prelude as P

data LocateException
  = AmbiguousLocateResult
      { description :: Text,
        locator :: Locator
      }
  | ElementNotFound
      { description :: Text,
        locator :: Locator
      }
  | InvalidLocator LI.InvalidLocator
  | DriverException WebDriverException
  deriving (Show)

instance Exception LocateException

data Cardinality = Unique | First | Many deriving (Show, Eq)

data DisplayedCheck = Never | DisambiguateUnique | Always deriving (Show, Eq)

data LocateDirectives = MkLocateDirectives
  { cardinality :: Cardinality,
    protocol :: Protocol
  }
  deriving (Show, Eq)

data LocateOps = MkLocateOps
  { displayedCheck :: DisplayedCheck
  }
  deriving (Show, Eq)

data LocatorSource
  = PlainSource
  | RoleSource RoleLocator
  | InnerTextSource Text
  deriving (Show, Eq, Ord)

data SingleResult = MkSingleResult
  { source :: LocatorSource,
    elms :: [ElementId]
  }
  deriving (Show, Eq)

data LocateResult
  = SingleResult SingleResult
  | OrResult
      { found :: [LocateResult]
      }
  | AndResult
      { found :: [LocateResult]
      }
  | NotResult
      { found :: [LocateResult]
      }
  deriving (Show, Eq)

-- TODO
-- 1. get unretried http working with tests
--   1.1 simple locators (css, xpath, role, inner text)
--   1.2 compound locators (parent, all, any, none)
--   1.3 displayed checks (disambiguate unique and always)
-- 2. failing tests
--   2.1 role edge cases (ess edgecases md)
--   2.2 inner text edge cases (ess edgecases md)
-- 3. fix edge cases - get tests passing
-- 4. retries / wait Http
-- 5. retry tests
-- 6. BiDi - repeat all of the above for BiDi, but with the much simpler locateMany as the basis, and no need for retries as BiDi supports waiting for conditions natively via the maxNodeCount parameter.

-- browsingContextLocateNodes :: forall m. Runner m LocateNodesResult -> LocateNodes -> m LocateNodesResult

-- findElement :: forall m. Runner m ElementId -> Session -> Selector -> m ElementId
-- findElements :: forall m. Runner m [ElementId] -> Session -> Selector -> m [ElementId]

locateHttp ::
  forall m.
  (Monad m) =>
  -- | throw exceptions
  (forall a e. (HasCallStack, Exception e) => e -> m a) ->
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | runner
  (forall b. Command b -> m b) ->
  -- | default locator
  (Text -> Locator) ->
  -- | cardinality
  Cardinality ->
  -- | locate ops
  LocateOps ->
  -- | session
  Session ->
  -- | locator
  Locator ->
  m ElementId
locateHttp throw catch runner defLoc cardinality MkLocateOps {displayedCheck} ses locator =
  preparedLoc
    & either
      (throw . InvalidLocator)
      \loc -> undefined
  where
    preparedLoc = prepareSimplify defLoc HTTP locator

    runCommand :: forall a. ((Command a -> m a) -> Session -> Selector -> m a) -> Selector -> m a
    runCommand f sel =
      catch
        (f runner ses sel)
        (throw . DriverException)

    findElm :: Selector -> m ElementId
    findElm = runCommand findElement

    findElms :: Selector -> m [ElementId]
    findElms = runCommand findElements

    filterDisplayedIf :: DisplayedCheck -> [ElementId] -> m [ElementId]
    filterDisplayedIf dc elms =
      if dc == displayedCheck
        then filterM (isDisplayedHttp throw catch runner ses) elms
        else pure elms

    locateAll :: Selector -> m [ElementId]
    locateAll s = findElms s >>= filterDisplayedIf Always

    locateSingleUnchecked :: Bool -> LocatorSource -> Selector -> m SingleResult
    locateSingleUnchecked checkUnique ls sel =
      MkSingleResult ls
        <$> ( if checkUnique
                -- checkUnique - get all results and check for uniqueness ourselves
                then locateAll
                -- if non-unique just lean lean on webdriver - brings first result back
                else fmap LST.singleton . findElm
            )
          sel

    checkSingleResult :: SingleResult -> m SingleResult
    checkSingleResult sr@MkSingleResult {source, elms} = do
      case elms of
        [] -> throw $ ElementNotFound {description = "Expected exactly one element, but found none.", locator}
        [x] -> pure sr
        xs -> do
          elmsRechecked <- filterDisplayedIf DisambiguateUnique xs
          case elmsRechecked of
            [] -> throw $ ElementNotFound {description = "Expected exactly one element, but found none (after filtering for displayed).", locator}
            [x] -> pure $ MkSingleResult source elmsRechecked
            xs' -> throw $ AmbiguousLocateResult {description = "Expected exactly one element, but found: " <> pack (show (P.length xs')) <> ".", locator}

    locateSingleChecked :: Bool -> LocatorSource -> Selector -> m SingleResult
    locateSingleChecked checkUnique ls =
      locateSingleUnchecked checkUnique ls >=> checkSingleResult

    -- locate :: Bool -> Selector -> m ElementId
    -- locate unique sel =
    --   if unique
    --     then do
    --       elms <- locateAll sel
    --       case elms of
    --         [] -> throw $ ElementNotFound {description = "Expected exactly one element, but found none.", locator = locator}
    --         [x] -> pure x
    --         xs -> do
    --           elmsRechecked <- filterDisplayedIf DisambiguateUnique xs
    --           case elmsRechecked of
    --             [] -> throw $ ElementNotFound {description = "Expected exactly one element, but found none (after filtering for displayed).", locator = locator}
    --             [x] -> pure x
    --             xs' -> throw $ AmbiguousLocateResult {description = "Expected exactly one element, but found: " <> pack (show (P.length xs')) <> ".", locator = locator}
    --     else
    --       findElm sel

    -- locateSingleton = locate (cardinality == Unique)

    toSelector :: SimplifiedLocator -> Selector
    toSelector = \case
      L.CSS {value} -> HTTPP.CSS value
      L.XPath {value} -> HTTPP.XPath value
      Role {role} -> HTTPP.XPath $ LI.roleToXPath role
      InnerText {value, matchType, caseSesnsitivity, maxDepth} -> HTTPP.XPath $ LI.innerTextToXPath value caseSesnsitivity matchType maxDepth
      _ -> error "toSelector: only CSS, XPath, Role and InnerText locators can be converted to Selector for HTTP WebDriver"

    httpLocate :: SimplifiedLocator -> m LocateResult
    httpLocate sl = case sl of
      L.CSS {} -> locateUnnested PlainSource
      L.XPath {} -> locateUnnested PlainSource
      -- TODO: extended inner text rules
      Role {role} -> locateUnnested $ RoleSource role
      InnerText {value} -> locateUnnested $ InnerTextRslt value
      -- will never happen - already filtered out by prepareSimplify
      BiDiContext {} -> error "BiDiContext locators are not supported in HTTP WebDriver"
      Parent {} -> undefined
      All {} -> undefined
      Any {} -> undefined
      None {} -> undefined
      PostFilter {} -> undefined
      where
        locateUnnested ls = SingleResult <$> locateSingleChecked (cardinality == Unique) ls (toSelector sl) 

    -- !!!!!!!! compound locates and retries  - need a pointer back to the orional locator so ca retry for
    -- special cases such as role inner test and displayed when ambiguous.
    httpLocateMany :: SimplifiedLocator -> m [ElementId]
    httpLocateMany = \case
      L.CSS {} -> undefined
      L.XPath {} -> undefined
      Role {} -> undefined
      InnerText {} -> undefined
      -- will never happen - already filtered out by prepareSimplify
      BiDiContext {} -> error "BiDiContext locators are not supported in HTTP WebDriver"
      Parent {} -> undefined
      All {} -> undefined
      Any {} -> undefined
      None {} -> undefined
      PostFilter {} -> undefined

displayedJS :: Text
displayedJS =
  "function isDisplayed(el) {\n\
  \  if (!el || !el.isConnected) return false;\n\
  \\n\
  \  const style = getComputedStyle(el);\n\
  \\n\
  \  if (style.display === \"none\") return false;\n\
  \  if (style.visibility === \"hidden\" || style.visibility === \"collapse\") return false;\n\
  \\n\
  \  if (el.tagName === \"INPUT\" && el.type === \"hidden\")\n\
  \    return false;\n\
  \\n\
  \  const rect = el.getBoundingClientRect();\n\
  \\n\
  \  if (rect.width === 0 || rect.height === 0)\n\
  \    return false;\n\
  \\n\
  \  const vpW = window.innerWidth;\n\
  \  const vpH = window.innerHeight;\n\
  \\n\
  \  if (\n\
  \    rect.bottom < 0 ||\n\
  \    rect.right < 0 ||\n\
  \    rect.top > vpH ||\n\
  \    rect.left > vpW\n\
  \  )\n\
  \    return false;\n\
  \\n\
  \  const points = [\n\
  \    [rect.left + rect.width / 2, rect.top + rect.height / 2],\n\
  \    [rect.left + 1, rect.top + 1],\n\
  \    [rect.right - 1, rect.bottom - 1]\n\
  \  ];\n\
  \\n\
  \  for (const [x, y] of points) {\n\
  \    if (x < 0 || y < 0 || x > vpW || y > vpH)\n\
  \      continue;\n\
  \\n\
  \    const hit = document.elementFromPoint(x, y);\n\
  \\n\
  \    if (hit === el || el.contains(hit))\n\
  \      return true;\n\
  \  }\n\
  \\n\
  \  return false;\n\
  \}\n\
  \return isDisplayed(arguments[0]);"

isDisplayedHttp ::
  forall m.
  (Monad m) =>
  -- | throw exceptions
  (forall a e. (HasCallStack, Exception e) => e -> m a) ->
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | runner
  (forall b. Command b -> m b) ->
  -- | session
  Session ->
  -- | element to check
  ElementId ->
  m Bool
isDisplayedHttp throw catch runner ses eid = do
  result <-
    catch
      (executeScript runner ses MkScript {script = displayedJS, args = [toJSON eid]})
      (throw . DriverException)
  case (fromJSON result :: A.Result Bool) of
    A.Success b -> pure b
    A.Error msg -> error $ "isDisplayedHttp: isDisplayed script returned unexpected value: " <> msg

locateHttpBiDi = undefined

--  use all findElements but limit to 2 results (not supported in standard HTTP WebDriver, but available in BiDi via maxNodeCount).