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
import Data.Aeson as A (Result (..), Value (Bool), fromJSON, toJSON)
import Data.Function ((&))
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
  ( CombinatorLocator (..),
    CommonLocator (..),
    ReducedHttpLocator (..),
    ReducedLocator (..),
    ShimmedLocator (..),
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

data SingleResult
  = SingleSuccess
      { source :: LocatorSource,
        elms :: [ElementId]
      }
  | SingleFailure
      { source :: LocatorSource,
        error :: LocateException,
        elms :: [ElementId]
      }
  deriving (Show, Eq)

data LocateResult
  = SingleResult SingleResult
  | ParentResult
      { found :: [LocateResult]
      }
  | AndResult
      { found :: [LocateResult]
      }
  | OrResult
      { found :: [LocateResult]
      }
  | NotResult
      { found :: [LocateResult]
      }
  | PostFilterResult
      { predicate :: LI.Predicate,
        found :: [LocateResult]
      }
  deriving (Show, Eq)

-- locateNested :: ReducedLocator -> LocateResult
-- locateNested = \case
--   Parent {parent, child} -> ParentResult {found = [locateNested child]}
--   All {elms} -> AndResult {found = fmap locateNested elms}
--   Any {elms} -> OrResult {found = fmap locateNested elms}
--   None {elms} -> NotResult {found = fmap locateNested elms}
--   PostFilter {predicate, locator} -> PostFilterResult {predicate, found = [locateNested locator]}
--   -- will never happen - already filtered out by prepareSimplify
--   BiDiContext {} -> error "BiDiContext locators are not supported in HTTP WebDriver"

-- sin

-- TODO
-- 0. locateHttp Compiles (NoImp postfilter)
-- 1. get unretried http working with tests
--   1.1 simple locators (css, xpath, role, inner text)
--   1.2 compound locators (parent, all, any, none)
--   1.3 displayed checks (disambiguate unique and always)
--   1.4 visible text
--

-- 2. tests
-- all of the above with tests, including edge cases such as:
--   2.1 role edge cases (ess edgecases md)
--   2.2 visible  text edge cases (ess edgecases md)

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
    preparedLoc :: Either LI.InvalidLocator ReducedHttpLocator
    preparedLoc = prepareSimplify defLoc HTTP locator >>= toHttpLocator

    runCommand :: forall a. ((Command a -> m a) -> Session -> Selector -> m a) -> Selector -> m a
    runCommand f sel =
      catch
        (f runner ses sel)
        (throw . DriverException)

    findElm :: Maybe [ElementId] -> Selector -> m ElementId
    findElm mRoots =
        mRoots
        & maybe
          (runCommand findElement)
          (\roots -> do 
            runCommand . findElementFromElement')
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

    locateAll :: Maybe LocateResult -> Selector -> m [ElementId]
    locateAll mRoot s = findElms mRoot s >>= filterDisplayedIf Always

    locateSingleUnchecked :: Maybe LocateResult -> Bool -> LocatorSource -> Selector -> m SingleResult
    locateSingleUnchecked mRoot findFirst ls sel =
      SingleSuccess ls
        <$> ( if findFirst
                -- lean on webdriver - brings first result back (faster)
                then fmap LST.singleton . findElm mRoot
                -- get all results for downstream uniqueness check (slower)
                else (locateAll mRoot)
            )
          sel

    hasSingletonErr :: [ElementId] -> Maybe LocateException
    hasSingletonErr =
      \case
        [] -> Just $ ElementNotFound {description = "Expected exactly one element, but found none.", locator}
        [x] -> Nothing
        xs -> Just $ AmbiguousLocator {description = "Expected exactly one element, but found: " <> pack (show (P.length xs)) <> ".", locator}

    checkSingleResult :: SingleResult -> m SingleResult
    checkSingleResult =
      \case
        sf@SingleFailure {} -> pure sf
        ss@SingleSuccess {source, elms} ->
          case hasSingletonErr elms of
            Nothing -> pure ss
            Just err -> case err of
              -- refilter and by JS displayed and recheck if ambiguous and directive is to disambiguate unique
              AmbiguousLocator {} -> do
                elmsRechecked <- filterDisplayedIf DisambiguateUnique elms
                pure $ case hasSingletonErr elmsRechecked of
                  Nothing -> SingleSuccess source elmsRechecked
                  Just e -> SingleFailure source e elmsRechecked
              e -> pure $ SingleFailure source e elms

    locateSingleChecked :: Maybe LocateResult -> Bool -> LocatorSource -> Selector -> m SingleResult
    locateSingleChecked mRoot findFirst ls =
      locateSingleUnchecked mRoot findFirst ls >=> checkSingleResult

    toSelector :: CommonLocator -> Selector
    toSelector = \case
      RL.CSS {value} -> HTTPP.CSS value
      RL.XPath {value} -> HTTPP.XPath value
      Shimmed sl -> case sl of
        Role {role} -> HTTPP.XPath $ roleToXPath role
        InnerText {value, matchType, caseSesnsitivity, maxDepth} -> HTTPP.XPath $ innerTextToXPath value caseSesnsitivity matchType maxDepth

    httpLocateCommon :: Maybe LocateResult -> Bool -> CommonLocator -> m LocateResult
    httpLocateCommon mRoot findFirst cl =
      SingleResult <$> locateSingleChecked mRoot findFirst ls (toSelector cl)
      where
        ls = case cl of
          RL.CSS {} -> PlainSource
          RL.XPath {} -> PlainSource
          Shimmed sl -> case sl of
            Role {role} -> RoleSource role
            InnerText {value} -> InnerTextSource value

    -- recursive version of http locate
    -- httpLocate' :: ReducedHttpLocator -> m (Either LocateException LocateResult)

    httpLocate' :: Maybe LocateResult -> ReducedHttpLocator -> m LocateResult
    httpLocate' mRoot = \case
      CommonHttp cl ->
        -- for simple single shot locator locate as per cardinality directive
        httpLocateCommon mRoot False cl
      CombintorHttp cb -> case cb of
        Parent {parent, child} -> do
          -- TODO: FIX THIS
          p <- locate parent
          c <- locate child
          pure ParentResult {found = [p, c]}
        All {elms} -> do
          results <- traverse locate elms
          pure AndResult {found = toList results}
        Any {elms} -> do
          results <- traverse locate elms
          pure OrResult {found = toList results}
        None {elms} -> do
          results <- traverse locate elms
          pure NotResult {found = toList results}
      PostFilterHttpLocator {} -> postfilterNotImplemented
      where
        locate = httpLocate' mRoot

    httpLocate :: ReducedHttpLocator -> m LocateResult
    httpLocate = \case
      CommonHttp cl ->
        -- for simple single shot locator locate as per cardinality directive
        httpLocateCommon Nothing (cardinality == First) cl
      PostFilterHttpLocator {} ->
        -- will neeed to postfilter &&& all
        postfilterNotImplemented
      loc@CombintorHttp {} -> httpLocate' Nothing loc

postfilterNotImplemented :: a
postfilterNotImplemented = error "PostFilter locators are not yet implemented in HTTP WebDriver"

-- !!!!!!!! compound locates and retries  - need a pointer back to the orional locator so ca retry for
-- special cases such as role inner test and displayed when ambiguous.
{-
httpLocateMany :: ReducedHttpLocator -> m [ElementId]
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
  -}

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