module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    SingletonCardinality (..),
    HttpLocateOpts (..),
    HttpLocateAllOpts (..),
    BaseLocateActions (..),
    locateHttp,
    locateFirstHttp,
    locateFromElementHttp,
    locateAllHttp,
    locateAllFromElementHttp
  )
where

import Control.Exception (Exception)
import Control.Monad (foldM, join)
import Data.Aeson as A (Value (Bool), toJSON)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes)
import Data.Text
import Data.Text qualified as T
import GHC.Stack (HasCallStack)

import WebDriverPreCore.Extended.HTTP.Base.Protocol as HTTPB (ElementId)
import WebDriverPreCore.Extended.Locators.Internal (Locator, Protocol (..), RoleLocator (..), innerTextToXPath, roleToXPath)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.Extended.Protocol (WebDriverException)
import WebDriverPreCore.Extended.ReducedLocator.Internal as RL
  ( BiDiNativeLoc (..),
    CombinatorLoc (..),
    LeafLoc (..),
    ReducedHttpLoc (..),
    prepareSimplify,
    toHttpLocator
  )
import WebDriverPreCore.HTTP.Protocol as HTTPP (Script (..), Selector (..))
import Prelude as P
import Utils (txt)
import Data.Bifunctor (first, Bifunctor (bimap))

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

type LocatorExceptionBuilder = Locator -> LocateException

-- | Whether to find the unique element (error if multiple match) or just the first.
data SingletonCardinality = Unique | First deriving (Show, Eq)

data LeafCardinality = LeafFirst | LeafMany deriving (Show, Eq)

data DisplayedCheck = DisplayedCheckNever | DisplayedCheckDisambiguateUnique | DisplayedCheckAlways deriving (Show, Eq)

data ExtendedRoleLocateSingleton = ExtLocateSingletonNever | ExtLocateSingletonMiss | ExtLocateSingletonAlways deriving (Show, Eq)

data ExtendedRoleLocateAll = ExtLocateAllNever | ExtLocateAllAlways deriving (Show, Eq)

data RoleLocateSecondPass = WantSecondPass | NoSecondPass deriving (Show, Eq)

-- | Options for singleton locate functions ('locateHttp', 'locateFromElementHttp').
data HttpLocateOpts = MkHttpLocateOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateSingleton,
    singletonCardinality :: SingletonCardinality,
    defaultLocator :: Text -> Locator
  }

-- | Options for multi-locate functions ('locateAllHttp', 'locateAllFromElementHttp').
data HttpLocateAllOpts = MkHttpLocateAllOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateAll,
    defaultLocator :: Text -> Locator
  }

-- TODO
-- 0. locateHttp Compiles (NoImp postfilter) [x]
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
--   2.4 shadow DOM
--     - may not work with xpath
--     - basic tests
--     - check role locators work in shadow DOM
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

-- | Shared actions that do not depend on find scope (element vs. root).
data BaseLocateActions m = BaseLocateActions
  { executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text
  }

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Locate a unique or first-matching element from the document root.
locateHttp ::
  forall m.
  (Monad m) =>
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | find a single element by selector
  (Selector -> m ElementId) ->
  -- | find multiple elements by selector
  (Selector -> m [ElementId]) ->
  -- | shared locate actions
  BaseLocateActions m ->
  -- | locate opts
  HttpLocateOpts ->
  -- | locator
  Locator ->
  m (Either LocateException [ElementId])
locateHttp catch findElm' findElms' actions opts locator =
  preparedLoc &
  either
    (pure . Left . InvalidLocator)
    (\rloc -> (first (locator &)) <$> httpLocateSingleton catch findElm'' findElms'' actions opts rloc)
  
  where

    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator
    withCatch' :: forall a. m a -> m (Either LocateException a)
    withCatch' action = catch (Right <$> action) (pure . Left . DriverException)
    -- For root-scoped functions mRoot is Nothing at the top level; for sub-searches
    -- within combinators it carries the container element. Since the caller only
    -- provides root-level finders, we ignore the sub-root (combinators handle
    -- scoping via the fromElement variants).
    findElm'' :: FindElm m
    findElm'' _ sel = withCatch' $ findElm' sel
    findElms'' :: FindElms m
    findElms'' _ sel = withCatch' $ findElms' sel

-- | Locate the first-matching element from the document root.
locateFirstHttp ::
  forall m.
  (Monad m) =>
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  (Selector -> m ElementId) ->
  (Selector -> m [ElementId]) ->
  BaseLocateActions m ->
  HttpLocateOpts ->
  Locator ->
  m (Either LocateException [ElementId])
locateFirstHttp catch findElm' findElms' actions opts locator =
  locateHttp catch findElm' findElms' actions opts{singletonCardinality = First} locator

-- | Locate a unique or first-matching element rooted at a given element.
locateFromElementHttp ::
  forall m.
  (Monad m) =>
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | find a single element by selector rooted at an element
  (ElementId -> Selector -> m ElementId) ->
  -- | find multiple elements by selector rooted at an element
  (ElementId -> Selector -> m [ElementId]) ->
  BaseLocateActions m ->
  HttpLocateOpts ->
  -- | root element
  ElementId ->
  Locator ->
  m (Either LocateException [ElementId])
locateFromElementHttp catch findElmFrom' findElmsFrom' actions opts rootId locator =
  either
    (pure . Left . InvalidLocator)
    (httpLocateSingleton catch findElm'' findElms'' actions opts locator)
    preparedLoc
  where
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator
    withCatch' :: forall a. m a -> m (Either LocateException a)
    withCatch' action = catch (Right <$> action) (pure . Left . DriverException)
    findElm'' :: FindElm m
    findElm'' mRoot sel = maybe
      (withCatch' $ findElmFrom' rootId sel)
      (\subRoot -> withCatch' $ findElmFrom' subRoot sel)
      mRoot
    findElms'' :: FindElms m
    findElms'' mRoot sel = maybe
      (withCatch' $ findElmsFrom' rootId sel)
      (\subRoot -> withCatch' $ findElmsFrom' subRoot sel)
      mRoot

-- | Locate all matching elements from the document root.
locateAllHttp ::
  forall m.
  (Monad m) =>
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | find multiple elements by selector
  (Selector -> m [ElementId]) ->
  BaseLocateActions m ->
  HttpLocateAllOpts ->
  Locator ->
  m (Either LocateException [ElementId])
locateAllHttp catch findElms' actions opts locator =
  either
    (pure . Left . InvalidLocator)
    (\rloc -> httpLocateAll catch findElms'' actions opts rloc)
    preparedLoc
  where
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator
    withCatch' action = catch (Right <$> action) (pure . Left . DriverException)
    findElms'' _ sel = withCatch' $ findElms' sel

-- | Locate all matching elements rooted at a given element.
locateAllFromElementHttp ::
  forall m.
  (Monad m) =>
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | find multiple elements by selector rooted at an element
  (ElementId -> Selector -> m [ElementId]) ->
  BaseLocateActions m ->
  HttpLocateAllOpts ->
  -- | root element
  ElementId ->
  Locator ->
  m (Either LocateException [ElementId])
locateAllFromElementHttp catch findElmsFrom' actions opts rootId locator =
    preparedLoc &
    either
      (pure . Left . InvalidLocator)
      (httpLocateAll catch findElms'' actions opts)
  where
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator
    withCatch' action = catch (Right <$> action) (pure . Left . DriverException)
    findElms'' mRoot sel = maybe
      (withCatch' $ findElmsFrom' rootId sel)
      (\subRoot -> withCatch' $ findElmsFrom' subRoot sel)
      mRoot

-- ---------------------------------------------------------------------------
-- Internal shared helpers
-- ---------------------------------------------------------------------------

type FindElm m = Maybe ElementId -> Selector -> m (Either LocateException ElementId)
type FindElms m = Maybe ElementId -> Selector -> m (Either LocateException [ElementId])

jsFilterDisplayedI ::
  forall m.
  (Monad m) =>
  (ElementId -> m (Either LocateException Bool)) ->
  [ElementId] ->
  m (Either LocateException [ElementId])
jsFilterDisplayedI recheckDisplayed' elms = do
  results <- traverse doCheck elms
  pure $ fmap (fmap fst . P.filter snd) (sequence results)
  where
    doCheck elm = fmap (elm,) <$> recheckDisplayed' elm

locateAllI :: FindElms m -> Maybe ElementId -> Selector -> m (Either LocateException [ElementId])
locateAllI findElms' mRoot s = findElms' mRoot s

-- finds leaf without display filtering
locateLeafI ::
  forall m.
  (Monad m) =>
  FindElm m ->
  FindElms m ->
  BaseLocateActions m ->
  (ElementId -> m (Either LocateException Bool)) ->
  Maybe ElementId ->
  LeafCardinality ->
  RoleLocateSecondPass ->
  LeafLoc ->
  m (Either LocateException [ElementId])
locateLeafI findElm' findElms' actions _recheckDisplayed mRoot leafCardinality rolesSecondPass loc = do
  let findFirst = leafCardinality == LeafFirst
  firstPass <-
    ( if findFirst
        then fmap (fmap pure) . findElm' mRoot
        else locateAllI findElms' mRoot
    )
      (toSelector loc)
  let baseResult = pure firstPass
  case loc of
    RL.CSS {} -> baseResult
    RL.XPath {} -> baseResult
    RL.BiDiNative sl -> case sl of
      Role {role} ->
        if rolesSecondPass == NoSecondPass
          then baseResult
          else fmap Right $ roleToXPathHttpSecondPass locateAllLenient actions.getElementAttribute actions.getElementText mRoot findFirst role
      InnerText {} -> baseResult
  where
    locateAllLenient r s = locateAllI findElms' r s >>= pure . either (const []) id

chkRefilterSingletonI ::
  forall m.
  (Monad m) =>
  (ElementId -> m (Either LocateException Bool)) ->
  [ElementId] ->
  m (Either LocateException [ElementId])
chkRefilterSingletonI recheckDisplayed' elmIds =
  chkkSingleton' True elmIds
  where
    chkkSingleton' recheckAmbiguous' =
      \case
        [] -> pure (Right [])
        [x] -> pure (Right [x])
        xs ->
          recheckAmbiguous'
            & bool
              (pure (Right xs))
              (jsFilterDisplayedI recheckDisplayed' xs >>= either (pure . Left) (chkkSingleton' False))

-- single shot base locate (all cardinality)
locateElmsUncheckedI ::
  forall m.
  (Monad m) =>
  FindElm m ->
  FindElms m ->
  BaseLocateActions m ->
  (ElementId -> m (Either LocateException Bool)) ->
  Maybe ElementId ->
  LeafCardinality ->
  RoleLocateSecondPass ->
  ReducedHttpLoc ->
  m (Either LocateException [ElementId])
locateElmsUncheckedI findElm' findElms' actions recheckDisplayed' mRoot leafCardinality rolesSecondPass =
  fmap (fmap LST.nub)
    . \case
      LeafHttp cl ->
        locateLeafI findElm' findElms' actions recheckDisplayed' mRoot leafCardinality rolesSecondPass cl
      CombintorHttp cb -> case cb of
        Contains {container, contained} -> do
          eContainers <- locate LeafMany rolesSecondPass container
          case eContainers of
            Left e -> pure (Left e)
            Right containers -> locateContained containers contained
        All {elms = locs} -> do
          let (l :| ls) = locs
              step eAcc loc = case eAcc of
                Left e -> pure (Left e)
                Right acc
                  | P.null acc -> pure (Right [])
                  | otherwise -> fmap (fmap (LST.intersect acc)) (locate LeafMany rolesSecondPass loc)
          initial <- locate LeafMany rolesSecondPass l
          foldM step initial ls
        Any {elms = locs} ->
          fmap (fmap join . sequence) $
            traverse (locate LeafMany rolesSecondPass) (toList locs)
      PostFilterHttpLoc {} -> postfilterNotImplemented
  where
    locate = locateElmsUncheckedI findElm' findElms' actions recheckDisplayed' mRoot

    locateContained :: [ElementId] -> ReducedHttpLoc -> m (Either LocateException [ElementId])
    locateContained containerIds subLoc = do
      containedResults <- traverse (\rootId -> locateElmsUncheckedI findElm' findElms' actions recheckDisplayed' (Just rootId) LeafMany rolesSecondPass subLoc) containerIds
      pure . fmap join . sequence $ containedResults

-- ---------------------------------------------------------------------------
-- Internal locate implementations
-- ---------------------------------------------------------------------------

httpLocateSingleton ::
  forall m.
  (Monad m) =>
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  FindElm m ->
  FindElms m ->
  BaseLocateActions m ->
  HttpLocateOpts ->
  ReducedHttpLoc ->
  m (Either LocatorExceptionBuilder [ElementId])
httpLocateSingleton catch findElm' findElms' actions opts loc = do
  case loc of
    LeafHttp ll -> do
      lr <- locateLeafI findElm' findElms' actions recheckDisplayed' Nothing LeafMany secondPassOnInitial ll
      filtered <- chkElmsSingleton (displayChkAlways || isUnique && displayChkDisambiguate) lr
      case filtered of
        Left e -> pure (Left (const e))
        Right [] ->
          if opts.extendedRoleLocation == ExtLocateSingletonMiss && isRole
            then do
              missRetryRslt <- locateLeafI findElm' findElms' actions recheckDisplayed' Nothing LeafMany WantSecondPass ll
              retryChked <- chkElmsSingleton (displayChkAlways || displayChkDisambiguate) missRetryRslt
              case retryChked of
                Left e -> pure (Left $ const e)
                Right [] -> notFoundErr
                Right [x] -> mkLocResult [x]
                Right (x : xs) ->
                  if isUnique
                    then throwAmbiguous xs
                    else mkLocResult [x]
            else notFoundErr
        Right [x] -> mkLocResult [x]
        Right elms@(x : _xs) ->
          if isUnique
            then throwAmbiguous elms
            else mkLocResult [x]
    PostFilterHttpLoc {} ->
      postfilterNotImplemented
    CombintorHttp {} ->
      locateElmsUncheckedI findElm' findElms' actions recheckDisplayed' Nothing LeafMany secondPassOnInitial loc
  where
      recheckDisplayed' = isDisplayedHttp catch actions.executeScript

      notFoundErr  :: m (Either (Locator -> LocateException) [ElementId])
      notFoundErr = pure . Left $ (ElementNotFound "No element found matching locator.")
      
      throwAmbiguous elms = pure . Left $ (AmbiguousLocator ("Multiple elements found matching locator: " <> txt elms))
      mkLocResult = pure . Right 
      displayChkAlways = opts.jsRecheckDisplayed == DisplayedCheckAlways
      displayChkDisambiguate = opts.jsRecheckDisplayed == DisplayedCheckDisambiguateUnique
      isUnique = opts.singletonCardinality == Unique
      isRole = case loc of
        LeafHttp (RL.BiDiNative (Role {})) -> True
        _ -> False
      secondPassOnInitial = case opts.extendedRoleLocation of
        ExtLocateSingletonNever -> NoSecondPass
        ExtLocateSingletonMiss -> NoSecondPass
        ExtLocateSingletonAlways -> WantSecondPass
      -- for singleton we always need all to check uniqueness
      chkElmsSingleton doChk eElms =
        case eElms of
          Left e -> pure (Left e)
          Right elms ->
            if doChk
              then chkRefilterSingletonI recheckDisplayed' elms
              else pure (Right elms)

httpLocateAll ::
  forall m.
  (Monad m) =>
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  FindElms m ->
  BaseLocateActions m ->
  HttpLocateAllOpts ->
  ReducedHttpLoc ->
  m (Either LocateException [ElementId])
httpLocateAll catch findElms' actions opts loc = do
  let recheckDisplayed' = isDisplayedHttp catch actions.executeScript
      mkLocResult = pure . Right
      findElm' _ _ = error "library defect: findElm called in locate-all context"
      secondPassOnInitial = case opts.extendedRoleLocation of
        ExtLocateAllNever -> NoSecondPass
        ExtLocateAllAlways -> WantSecondPass
  result <- locateElmsUncheckedI findElm' findElms' actions recheckDisplayed' Nothing LeafMany secondPassOnInitial loc
  case result of
    Left e -> pure (Left e)
    Right elms ->
      if opts.jsRecheckDisplayed == DisplayedCheckAlways
        then jsFilterDisplayedI recheckDisplayed' elms >>= either (pure . Left) mkLocResult
        else mkLocResult elms

postfilterNotImplemented :: a
postfilterNotImplemented = error "PostFilter locators are not yet implemented in HTTP WebDriver"

data SingletonCheckResult
  = SingletonSuccess {elms :: [ElementId]}
  | Missing {elms :: [ElementId]}
  | Ambiguous {elms :: [ElementId]}
  deriving (Show, Eq, Ord)

toSelector :: LeafLoc -> Selector
toSelector = \case
  RL.CSS {value} -> HTTPP.CSS value
  RL.XPath {value} -> HTTPP.XPath value
  -- shim BiDiNative locators
  BiDiNative sl -> case sl of
    Role {role} -> HTTPP.XPath $ roleToXPath role
    InnerText {value, matchType, caseSesnsitivity, maxDepth} -> HTTPP.XPath $ innerTextToXPath value caseSesnsitivity matchType maxDepth

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
  -- | execute script action
  (Script -> m Value) ->
  -- | element to check
  ElementId ->
  m (Either LocateException Bool)
isDisplayedHttp catch execScript eid =
  catch
    (Right . toBool <$> execScript MkScript {script = displayedJS, args = [toJSON eid]})
    -- if any error occurs when checking displayed, assume element is displayed
    -- eg. if element becomes stale between finding and checking displayed, or if the driver does not support executeScript
    (pure . Left . DriverException)
  where
    toBool :: Value -> Bool
    toBool = \case
      Bool b -> b
      val -> error $ "library defect - isDisplayedHttp: isDisplayed script returned unexpected value (expected Bool) - got:\n  " <> P.show val

_locateBiDi :: a
_locateBiDi = undefined

notNull :: [a] -> Bool
notNull = not . P.null

roleToXPathHttpSecondPass ::
  forall m.
  (Monad m) =>
  -- | locate all elements matching a selector
  (Maybe ElementId -> Selector -> m [ElementId]) ->
  -- | get an element attribute; 'Nothing' when the attribute is absent
  (ElementId -> Text -> m (Maybe Text)) ->
  -- | get the visible text of an element
  (ElementId -> m Text) ->
  Maybe ElementId -> -- root to search within
  Bool ->
  RoleLocator ->
  m [ElementId]
roleToXPathHttpSecondPass
  locAll
  getAttr
  getText
  rootElm
  findFirst
  roleLoc =
    case roleLoc of
      -- role type has no name / label so nothing to do
      RoleType {} -> pure []
      _ -> do
        labelledByElms <- roleToXPathHttpLabeledBy locAll getAttr getText rootElm findFirst roleLoc
        if findFirst && notNull labelledByElms
          then pure labelledByElms
          else do
            forElms <- roleToXPathFor locAll getAttr getText rootElm findFirst roleLoc
            pure . nubOrd $ mconcat [labelledByElms, forElms]

roleToXPathHttpLabeledBy ::
  forall m.
  (Monad m) =>
  -- | locate all elements matching a selector
  (Maybe ElementId -> Selector -> m [ElementId]) ->
  -- | get an element attribute; 'Nothing' when the attribute is absent
  (ElementId -> Text -> m (Maybe Text)) ->
  -- | get the visible text of an element
  (ElementId -> m Text) ->
  Maybe ElementId -> -- root to search within
  Bool ->
  RoleLocator ->
  m [ElementId]
roleToXPathHttpLabeledBy locAll getAttr getText rootElm findFirst roleLoc =
  case roleLoc of
    RoleType {} -> pure []
    _ -> do
      candidates <-
        -- matching role and an aria-labelledby attribute
        locAll rootElm (HTTPP.XPath $ "//*" <> roleXPath roleLoc <> "[@" <> ariaLabeledBy <> "]")
      filterElms findFirst labledByMatchesRoleText candidates
      where
        ariaLabeledBy = "aria-labelledby"

        -- Resolve aria-labelledby on @eid@: split on whitespace to get ID-refs,
        -- look up the text of each referenced element, concatenate with spaces,
        -- and compare (after stripping) to @targetName@.
        labledByMatchesRoleText :: ElementId -> m Bool
        labledByMatchesRoleText eid =
          getAttr eid ariaLabeledBy
            >>= \case
              Nothing -> pure False
              Just lblIds -> do
                mappedTxts <- traverse textForId $ T.words lblIds
                pure $ T.strip (T.unwords $ catMaybes mappedTxts) == T.strip roleLoc.name

        -- Find the element whose @id@ matches @idRef@ and return its text, or
        -- 'Nothing' if no such element exists.
        textForId :: Text -> m (Maybe Text)
        textForId idRef = do
          elms <- locAll rootElm . HTTPP.XPath $ "//*[@id='" <> idRef <> "']"
          case elms of
            [] -> pure Nothing
            (e : _) -> Just <$> getText e

--  use all findElements but limit to 2 results (not supported in standard HTTP WebDriver, but available in BiDi via maxNodeCount).

roleToXPathFor ::
  forall m.
  (Monad m) =>
  -- | locate all elements matching a selector
  (Maybe ElementId -> Selector -> m [ElementId]) ->
  -- | get an element attribute; 'Nothing' when the attribute is absent
  (ElementId -> Text -> m (Maybe Text)) ->
  -- | get the visible text of an element
  (ElementId -> m Text) ->
  Maybe ElementId -> -- root to search within
  Bool -> -- find first only
  RoleLocator ->
  m [ElementId]
roleToXPathFor locAll getAttr getText rootElm findFirst roleLoc =
  case roleLoc of
    RoleType {} -> pure []
    _ -> do
      candidates <-
        -- has an @id and matches the role name
        locAll rootElm $ HTTPP.XPath $ "//*" <> roleXPath roleLoc <> "[@id]"
      filterElms findFirst forTxtMatchesId candidates
      where
        forTxtMatchesId :: ElementId -> m Bool
        forTxtMatchesId eid = do
          mId <- getAttr eid "id"
          case mId of
            Nothing -> pure False
            Just idVal -> do
              labels <- locAll Nothing . HTTPP.XPath $ "//label[@for='" <> idVal <> "']"
              case labels of
                [] -> pure False
                (lbl : _) -> do
                  labelText <- getText lbl
                  pure $ T.strip labelText == T.strip roleLoc.name

roleXPath :: RoleLocator -> Text
roleXPath = \case
  RoleName {} -> "[not(@role='presentation' or @role='none')]"
  r -> LI.roleTypeXPathContent True r.role

filterElms :: forall m. (Monad m) => Bool -> (ElementId -> m Bool) -> [ElementId] -> m [ElementId]
filterElms findFirst matcher = recurse []
  where
    recurse :: [ElementId] -> [ElementId] -> m [ElementId]
    recurse acc rem' =
      if findFirst && notNull acc
        then
          pure acc
        else case rem' of
          [] -> pure $ P.reverse acc
          (e : es) -> do
            matches <- matcher e
            recurse (if matches then e : acc else acc) es
