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
  { jsRecheckDisplayed :: DisplayedCheck
  }
  deriving (Show, Eq)

data LeafResult
  = MkLeafResult
  { source :: LeafLoc,
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
extractIds :: LocateResult -> [ElementId]
extractIds lr = recurse lr
  where
    recurse :: LocateResult -> [ElementId]
    recurse = \case
      LeafResult (MkLeafResult {elms}) -> elms
      PostFilterResult {} -> postfilterNotImplemented
      ContainsResult {found} -> recurseConcatAll found
      OrResult {found} -> recurseConcatAll found
      AndResult {found} ->
        traverse recurse found
          & \case
            [] -> []
            (x : xs) -> P.foldl' LST.intersect x xs
    recurseConcatAll :: [LocateResult] -> [ElementId]
    recurseConcatAll = (>>= recurse)

-- TODO
-- 0. locateHttp Compiles (NoImp postfilter)
-- 1. get unretried http working with tests
--   1.1 simple locators (css, xpath, role, inner text)
--   1.2 compound locators (parent, all, any)
--   1.3 compound locators are lazy
--   1.4 displayed checks (disambiguate unique and always)
--   1.5 visible text
--   1.6 BiDi special cases

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
  m (Either LocateException ElementId)
locateHttp throw catch runner defLoc cardinality MkLocateOps {jsRecheckDisplayed} ses locator =
  preparedLoc
    & either
      (pure . Left . InvalidLocator)
      \loc ->
        catch
          undefined
          (pure . Left)
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
    findElms mRoot s =
      mRoot
        & maybe
          (runCommand findElements s)
          (\rootId -> runCommand (findElementsFromElement' rootId) s)
      where
        findElementsFromElement' :: ElementId -> (Command [ElementId] -> m [ElementId]) -> Session -> Selector -> m [ElementId]
        findElementsFromElement' rootId runner' ses' sel = findElementsFromElement runner' ses' rootId sel

    recheckDisplayed :: ElementId -> m Bool
    recheckDisplayed eid = isDisplayedHttp catch runner ses eid >>= either throw pure

    filterDisplayedIf :: DisplayedCheck -> [ElementId] -> m [ElementId]
    filterDisplayedIf dc =
      if dc == jsRecheckDisplayed
        then jsFilterDisplayed
        else pure

    jsFilterDisplayed :: [ElementId] -> m [ElementId]
    jsFilterDisplayed elms = do
      results <- traverse doCheck elms
      pure $ fmap fst . P.filter snd $ results
      where
        doCheck elm = (elm,) <$> recheckDisplayed elm

    locateAll :: Maybe ElementId -> Selector -> m [ElementId]
    locateAll mRoot s = findElms mRoot s -- >>= filterDisplayedIf Always
    --
    locateLeaf :: Maybe ElementId -> Bool -> LeafLoc -> m LeafResult
    locateLeaf mRoot findFirst loc =
      MkLeafResult loc
        <$> ( if findFirst
                -- lean on webdriver - brings first result back (faster)
                then \s -> LST.singleton <$> findElm mRoot s
                -- get all results for downstream uniqueness check (slower)
                else locateAll mRoot
            )
          (toSelector loc)

    ensureSingleton ::
      Bool -> -- recheck ambiguous
      LocateResult ->
      [ElementId] ->
      m (Either LocateException LocateResult)
    ensureSingleton recheckAmbiguous lr elmIds =
      case chkSingleton elmIds of
        SingletonSuccess -> pure $ Right lr
        Missing -> pure $ Left $ ElementNotFound {description = "Expected exactly one element, but found none.", locator}
        Ambiguous ->
          if recheckAmbiguous
            then
              jsFilterDisplayed elmIds
                >>= ensureSingleton False lr
            else
              pure $ Left $ AmbiguousLocator {description = "Expected exactly one element, but found: " <> pack (show (P.length elmIds)) <> ".", locator}

    locateLeafChecked :: Maybe ElementId -> Cardinality -> LeafLoc -> m LocateResult
    locateLeafChecked mRoot cardinality' leafLoc = do
      lr <- locateLeaf mRoot (cardinality' == First) leafLoc
      chkedRslt <- ensureSingleton (jsRecheckDisplayed `P.elem` [DisambiguateUnique, Always]) (LeafResult lr) lr.elms
      either throw pure chkedRslt

    toSelector :: LeafLoc -> Selector
    toSelector = \case
      RL.CSS {value} -> HTTPP.CSS value
      RL.XPath {value} -> HTTPP.XPath value
      -- shim BiDiNative locators
      BiDiNative sl -> case sl of
        Role {role} -> HTTPP.XPath $ roleToXPath role
        InnerText {value, matchType, caseSesnsitivity, maxDepth} -> HTTPP.XPath $ innerTextToXPath value caseSesnsitivity matchType maxDepth

    httpLocate'' :: Maybe ElementId -> ReducedHttpLoc -> m LocateResult
    httpLocate'' mRoot =
      \case
        LeafHttp cl ->
          -- need to find all elms for combinator and later checks and retries
          LeafResult <$> locateLeaf mRoot False cl
        CombintorHttp cb -> case cb of
          Contains {container, contained} -> do
            containers <- locate container
            locateContained containers contained
          All {elms = locs} -> do
            results <- traverse locate locs
            pure AndResult {found = toList results}
          Any {elms = locs} -> do
            results <- traverse locate locs
            pure OrResult {found = toList results}
        PostFilterHttpLoc {} -> postfilterNotImplemented
      where
        locate = httpLocate'' mRoot

        locateContained :: LocateResult -> ReducedHttpLoc -> m LocateResult
        locateContained containers subLoc = do
          -- for each container, locate contained with root of container element, and combine results
          let ids = containers.found >>= extractIds
          containedResults <- traverse (\rootId -> httpLocate'' (Just rootId) subLoc) ids
          pure $ ContainsResult containedResults

    httpLocate' :: Maybe ElementId -> ReducedHttpLoc -> m LocateResult
    httpLocate' mRoot loc = do
      result1 <- httpLocate'' mRoot loc
      let ids = extractIds result1
      -- this assumes that the jscheck function is good enough to pick up when a parent element
      -- is not displayed, even if a child is => and return false
      checked <- ensureSingleton (jsRecheckDisplayed `P.elem` [DisambiguateUnique, Always]) result1 ids
      case checked of
        Left (ElementNotFound {}) -> undefined
        Left err -> throw err
        Right _ -> pure result1

    httpLocate :: ReducedHttpLoc -> m LocateResult
    httpLocate = \case
      LeafHttp cl ->
        -- for simple single shot locator locate as per cardinality directive
        locateLeafChecked Nothing cardinality cl
      PostFilterHttpLoc {} ->
        -- will neeed to postfilter &&& all
        postfilterNotImplemented
      loc@CombintorHttp {} -> httpLocate' Nothing loc

postfilterNotImplemented :: a
postfilterNotImplemented = error "PostFilter locators are not yet implemented in HTTP WebDriver"

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
  m (Either LocateException Bool)
isDisplayedHttp catch runner ses eid =
  catch
    (Right . toBool <$> executeScript runner ses MkScript {script = displayedJS, args = [toJSON eid]})
    -- if any error occurs when checking displayed, assume element is displayed
    -- eg. if element becomes stale between finding and checking displayed, or if the driver does not support executeScript
    (pure . Left . DriverException)
  where
    toBool :: Value -> Bool
    toBool = \case
      Bool b -> b
      val -> error $ "library defect - isDisplayedHttp: isDisplayed script returned unexpected value (expected Bool) - got:\n  " <> show val

locateBiDi = undefined


roleToXPathHttpSecondPass :: (Maybe ElementId -> Selector -> m [ElementId]) -> RoleLocator -> m LocateResult
roleToXPathHttpSecondPass locateAll = undefined

implement #sym:roleToXPathHttpSecondPass to find matching mapped attributes such as aria-labelledby
There is a number of mapped attribute edge cases in locator_edge_cases.md that can be covered in muti shot a second pass

--  use all findElements but limit to 2 results (not supported in standard HTTP WebDriver, but available in BiDi via maxNodeCount).