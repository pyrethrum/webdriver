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
import Control.Monad (filterM, foldM, (>=>))
import Data.Aeson as A (Result (..), Value (Bool), fromJSON, toJSON)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List qualified as LST
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe)
import Data.Text
import GHC.Stack (HasCallStack)
import WebDriverPreCore.Extended.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol as HTTPB (ElementId)
import WebDriverPreCore.Extended.HTTP.Internal (Runner)
import WebDriverPreCore.Extended.Locators.Internal (Locator, Protocol (..), RoleLocator (..), innerTextToXPath, roleToXPath)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.Extended.Protocol (Session, WebDriverException)
import WebDriverPreCore.Extended.ReducedLocator.Internal as RL
  ( BiDiNativeLoc (..),
    CombinatorLoc (..),
    LeafLoc (..),
    ReducedHttpLoc (..),
    ReducedLoc (..),
    prepareSimplify,
    toHttpLocator,
  )
import WebDriverPreCore.HTTP.Protocol as HTTPP (Command, Script (..), Selector (..))
import Prelude as P

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
  | DriverException WebDriverException
  deriving (Show, Eq)

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

data LeafResult
  = MkLeafResult
  { source :: LocatorSource,
    elms :: [ElementId]
  }
  deriving (Show, Eq)

data LocateResult
  = LeafResult LeafResult
  | ContainsResult
      { found :: [LocateResult]
      }
  | AndResult
      { found :: [LocateResult]
      }
  | OrResult
      { found :: [LocateResult]
      }
  | PostFilterResult
      { predicate :: LI.Predicate,
        found :: [LocateResult]
      }
  deriving (Show, Eq)

foldResult :: (b -> LocateResult -> b) -> b -> LocateResult -> b
foldResult f z lr = runIdentity $ traverseResult (\acc x -> Identity (f acc x)) z lr

traverseResult :: (Monad m) => (b -> LocateResult -> m b) -> b -> LocateResult -> m b
traverseResult f z lr = do
  z' <- f z lr
  foldM (traverseResult f) z' children
  where
    children = case lr of
      LeafResult _ -> []
      ContainsResult {found} -> found
      AndResult {found} -> found
      OrResult {found} -> found
      PostFilterResult {found} -> found

-- TODO - may need to reintroduce locateDirectives param
-- extractIds :: LocateDirectives -> LocateResult -> Either LocateResult [ElementId]
-- extractIds _ lr = recurse lr
extractIds :: LocateResult -> Either [ElementId]
extractIds lr = recurse lr
  where
    recurse :: LocateResult -> Either [ElementId]
    recurse = \case
      LeafResult (MkLeafResult {elms}) -> elms
      PostFilterResult {} -> postfilterNotImplemented
      ContainsResult {found} -> recurseConcatAll found
      OrResult {found} -> recurseConcatAll found
      AndResult {found} ->
        recurseAll found
          & fmap \case
            [] -> []
            (x : xs) -> P.foldl' LST.intersect x xs
    recurseAll = traverse recurse
    recurseConcatAll = fmap mconcat . recurseAll

-- locateNested :: ReducedLoc -> LocateResult
-- locateNested = \case
--   Contains {container, contained} -> ContainsResult {found = [locateNested contained]}
--   All {elms} -> AndResult {found = fmap locateNested elms}
--   Any {elms} -> OrResult {found = fmap locateNested elms}
--   PostFilter {predicate, locator} -> PostFilterResult {predicate, found = [locateNested locator]}
--   -- will never happen - already filtered out by prepareSimplify
--   BiDiContext {} -> error "BiDiContext locators are not supported in HTTP WebDriver"

-- sin

-- TODO
-- 0. locateHttp Compiles (NoImp postfilter)
-- 1. get unretried http working with tests
--   1.1 simple locators (css, xpath, role, inner text)
--   1.2 compound locators (parent, all, any)
--   1.3 compound locators are lazy
--   1.4 displayed checks (disambiguate unique and always)
--   1.5 visible text
--

-- 2. tests
-- all of the above with tests, including edge cases such as:
--   2.1 role edge cases (ess edgecases md)
--   2.2 visible  text edge cases (css edgecases md)
--   2.3 find all elements not displayed
-- 3. implement / redesign related to postfilter HTTP
-- 4. postfilter tests

-- 5. BiDi - repeat all of the above for BiDi, but with the much simpler locateMany as the basis,
--   and no need for retries as BiDi supports waiting for conditions natively via the maxNodeCount parameter.

-- 6. refactor / shared code

-- 7. locate all - http
-- 7. locate all - bidi

-- 9. retries / wait / logging / recovery Http - need directives eg. log pagesource
--   -- should this be an asyc race
-- 10. retry tests

-- 11. adapt retries to Bidi

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
    preparedLoc :: Either LI.InvalidLocator ReducedHttpLoc
    preparedLoc = prepareSimplify defLoc HTTP locator >>= toHttpLocator

    runCommand :: forall a. ((Command a -> m a) -> Session -> Selector -> m a) -> Selector -> m a
    runCommand f sel =
      catch
        (f runner ses sel)
        (throw . DriverException)

    findElm :: Maybe ElementId -> Selector -> m ElementId
    findElm mRoots =
      mRoots
        & maybe
          (runCommand findElement)
          (runCommand . findElementFromElement')
      where
        findElementFromElement' :: ElementId -> (Command ElementId -> m ElementId) -> Session -> Selector -> m ElementId
        findElementFromElement' rootId runner' ses' sel = findElementFromElement runner' ses' rootId sel

    findElms :: Maybe ElementId -> Selector -> m [ElementId]
    findElms mRoot = runCommand findElements

    recheckDisplayed :: ElementId -> m Bool
    recheckDisplayed = isDisplayedHttp catch runner ses

    filterDisplayedIf :: DisplayedCheck -> [ElementId] -> m [ElementId]
    filterDisplayedIf dc elms =
      if dc == displayedCheck
        then filterM recheckDisplayed elms
        else pure elms

    locateAll :: Maybe ElementId -> Selector -> m [ElementId]
    locateAll mRoot s = findElms mRoot s >>= filterDisplayedIf Always

    locateSingleUnchecked :: Maybe ElementId -> Bool -> LocatorSource -> Selector -> m LeafResult
    locateSingleUnchecked mRoot findFirst ls sel =
      SingleSuccess ls
        <$> ( if findFirst
                -- lean on webdriver - brings first result back (faster)
                then fmap LST.singleton . findElm mRoot
                -- get all results for downstream uniqueness check (slower)
                else (locateAll mRoot)
            )
          sel

    checkSingleResult ::
      Bool -> -- want recheck with deep dispalyed
      LeafResult ->
      m (Either LocateException LeafResult)
    checkSingleResult Ambiguous lr@MkLeafResult{elms} =
      case chkSingleton elms of
        SingletonSuccess -> pure $ Right lr
        Missing -> pure $ Left $ ElementNotFound {description = "Expected exactly one element, but found none.", locator}
        Ambiguous -> do
          if recheck then do
            elmsRechecked <- filterDisplayedIf DisambiguateUnique elms
            checkSingleResult False (MkLeafResult elmsRechecked)
          else
            pure $ Left $ AmbiguousLocator {description = "Expected exactly one element, but found: " <> pack (show (P.length elms)) <> ".", locator}
 
    locateSingleChecked :: Maybe ElementId -> Cardinality -> LocatorSource -> Selector -> m LeafResult
    locateSingleChecked mRoot cardinality' ls =
      locateSingleUnchecked mRoot (cardinality' == First) ls >=> checkSingleResult

    toSelector :: LeafLoc -> Selector
    toSelector = \case
      RL.CSS {value} -> HTTPP.CSS value
      RL.XPath {value} -> HTTPP.XPath value
      -- shim BiDiNative locators
      BiDiNative sl -> case sl of
        Role {role} -> HTTPP.XPath $ roleToXPath role
        InnerText {value, matchType, caseSesnsitivity, maxDepth} -> HTTPP.XPath $ innerTextToXPath value caseSesnsitivity matchType maxDepth

    httpLocateLeaf :: Maybe ElementId -> Cardinality -> LeafLoc -> m LocateResult
    httpLocateLeaf mRoot cardinality' cl =
      LeafResult <$> locateSingleChecked mRoot cardinality' ls (toSelector cl)
      where
        ls = case cl of
          RL.CSS {} -> PlainSource
          RL.XPath {} -> PlainSource
          BiDiNative sl -> case sl of
            Role {role} -> RoleSource role
            InnerText {value} -> InnerTextSource value

    -- recursive version of http locate
    -- httpLocate' :: ReducedHttpLoc -> m (Either LocateException LocateResult)

    httpLocate' :: Maybe ElementId -> ReducedHttpLoc -> m LocateResult
    httpLocate' mRoot = \case
      LeafHttp cl ->
        -- need to find all elms for combinator and later checks and retries
        httpLocateLeaf mRoot Many cl
      CombintorHttp cb -> case cb of
        Contains {container, contained} -> do
          containers <- locate container
          locateContained containers
          undefined
        All {elms} -> do
          results <- traverse locate elms
          pure AndResult {found = toList results}
        Any {elms} -> do
          results <- traverse locate elms
          pure OrResult {found = toList results}
      PostFilterHttpLoc {} -> postfilterNotImplemented
      where
        locate = httpLocate' mRoot
        locateContained :: LocateResult -> m LocateResult
        locateContained containers = do
          -- for each container, locate contained with root of container element, and combine results
          let ids = extractIds <$> containers.found
          containedResults <- traverse (locate . Just <=< getSingleElementId) containers
          pure $ ContainsResult {found = toList containedResults}

    httpLocate :: ReducedHttpLoc -> m LocateResult
    httpLocate = \case
      LeafHttp cl ->
        -- for simple single shot locator locate as per cardinality directive
        httpLocateLeaf Nothing cardinality cl
      PostFilterHttpLoc {} ->
        -- will neeed to postfilter &&& all
        postfilterNotImplemented
      loc@CombintorHttp {} -> httpLocate' Nothing loc

postfilterNotImplemented :: a
postfilterNotImplemented = error "PostFilter locators are not yet implemented in HTTP WebDriver"

-- !!!!!!!! compound locates and retries  - need a pointer back to the orional locator so ca retry for
-- special cases such as role inner test and displayed when ambiguous.
{-
httpLocateMany :: ReducedHttpLoc -> m [ElementId]
httpLocateMany = \case
  L.CSS {} -> undefined
  L.XPath {} -> undefined
  Role {} -> undefined
  InnerText {} -> undefined
  -- will never happen - already filtered out by prepareSimplify
  BiDiContext {} -> error "BiDiContext locators are not supported in HTTP WebDriver"
  Contains {} -> undefined
  All {} -> undefined
  Any {} -> undefined
  PostFilter {} -> undefined
  -}

data SingletonCheckResult = SingletonSuccess | Missing | Ambiguous deriving (Show, Eq, Ord)

chkSingleton :: [a] -> SingletonCheckResult
chkSingleton = \case
  [] -> Missing
  [_] -> SingletonSuccess
  _ -> Ambiguous

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
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | runner
  (forall b. Command b -> m b) ->
  -- | session
  Session ->
  -- | element to check
  ElementId ->
  m Bool
isDisplayedHttp catch runner ses eid = do
  result <-
    catch
      (executeScript runner ses MkScript {script = displayedJS, args = [toJSON eid]})
      -- if any error occurs when checking displayed, assume element is displayed
      -- eg. if element becomes stale between finding and checking displayed, or if the driver does not support executeScript
      (\(_e :: WebDriverException) -> pure $ Bool True)
  case (fromJSON result :: A.Result Bool) of
    A.Success b -> pure b
    A.Error msg ->
      -- this should not happen unless the script is broken
      error $ "isDisplayedHttp: isDisplayed script returned unexpected value: " <> msg

locateBiDi = undefined

--  use all findElements but limit to 2 results (not supported in standard HTTP WebDriver, but available in BiDi via maxNodeCount).