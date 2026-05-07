module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    SingletonCardinality (..),
    HttpLocateOpts (..),
    HttpLocateAllOpts (..),
    LocateActions (..),
    LocateAllActions (..),
    LocateFromElementActions (..),
    LocateAllFromElementActions (..),
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

mapLeftException :: forall a m. Functor m => Locator -> m (Either PreLocateException a) ->  m (Either LocateException a)
mapLeftException  locator action = 
  first convert <$> action
  where 
    convert = \case 
      AmbiguousLocator' desc -> AmbiguousLocator desc locator
      ElementNotFound' desc -> ElementNotFound desc locator
      InvalidLocator' e -> InvalidLocator e
      DriverException' e -> DriverException e locator

instance Exception LocateException

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

-- | Actions for singleton locate functions ('locateHttp', 'locateFirstHttp', 'locateFromElementHttp').
data LocateActions m = MkLocateActions
  { catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    findElement :: Selector -> m ElementId,
    findElements :: Selector -> m [ElementId],
    executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text
  }

-- | Actions for multi-locate functions ('locateAllHttp', 'locateAllFromElementHttp').
data LocateAllActions m = MkLocateAllActions
  { catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    findElements :: Selector -> m [ElementId],
    executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text
  }

-- | Actions for singleton element-scoped locate functions ('locateFromElementHttp').
data LocateFromElementActions m = MkLocateFromElementActions
  { catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    findElementFromElement :: ElementId -> Selector -> m ElementId,
    findElementsFromElement :: ElementId -> Selector -> m [ElementId],
    executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text
  }

-- | Actions for multi element-scoped locate functions ('locateAllFromElementHttp').
data LocateAllFromElementActions m = MkLocateAllFromElementActions
  { catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    findElementsFromElement :: ElementId -> Selector -> m [ElementId],
    executeScript :: Script -> m Value,
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
  -- | locate actions
  LocateActions m ->
  -- | locate opts
  HttpLocateOpts ->
  -- | locator
  Locator ->
  m (Either LocateException [ElementId])
locateHttp act@MkLocateActions{catch} opts locator =
  preparedLoc &
  either
    (pure . Left . InvalidLocator)
    (mapLeftException locator
      . httpLocateSingleton (fromRoot act.findElement) (fromRoot act.findElements) act opts)
  where
    preparedLoc :: Either LI.InvalidLocator ReducedHttpLoc
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator

    fromRoot :: (Selector -> m a) -> Maybe ElementId -> Selector -> m (Either PreLocateException a)
    fromRoot actn = const (mkTry catch . actn)

-- | Locate the first-matching element from the document root.
locateFirstHttp ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  HttpLocateOpts ->
  Locator ->
  m (Either LocateException [ElementId])
locateFirstHttp actions opts locator =
  locateHttp actions opts{singletonCardinality = First} locator

-- | Locate a unique or first-matching element rooted at a given element.
locateFromElementHttp ::
  forall m.
  (Monad m) =>
  LocateFromElementActions m ->
  HttpLocateOpts ->
  -- | root element
  ElementId ->
  Locator ->
  m (Either LocateException [ElementId])
locateFromElementHttp act@MkLocateFromElementActions{catch} opts rootId locator =
  either
    (pure . Left . InvalidLocator)
    (mapLeftException locator . httpLocateSingleton findElm'' findElms'' (toLocateActions act) opts)
    preparedLoc
  where
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator
    try' :: forall a. m a -> m (Either PreLocateException a)
    try' = mkTry catch
    findElm'' :: FindElm m
    findElm'' mRoot sel =
      try' $ maybe
        (act.findElementFromElement rootId sel)
        (\subRoot -> act.findElementFromElement subRoot sel)
        mRoot
    findElms'' :: FindElms m
    findElms'' mRoot sel =
      try' $ maybe
        (act.findElementsFromElement rootId sel)
        (\subRoot -> act.findElementsFromElement subRoot sel)
        mRoot

-- | Locate all matching elements from the document root.
locateAllHttp ::
  forall m.
  (Monad m) =>
  LocateAllActions m ->
  HttpLocateAllOpts ->
  Locator ->
  m (Either LocateException [ElementId])
locateAllHttp actions@MkLocateAllActions{catch} opts locator =
  either
    (pure . Left . InvalidLocator)
    (mapLeftException locator . httpLocateAll findElms'' actions opts)
    preparedLoc
  where
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator
    findElms'' _ sel = mkTry catch $ actions.findElements sel

-- | Locate all matching elements rooted at a given element.
locateAllFromElementHttp ::
  forall m.
  (Monad m) =>
  LocateAllFromElementActions m ->
  HttpLocateAllOpts ->
  -- | root element
  ElementId ->
  Locator ->
  m (Either LocateException [ElementId])
locateAllFromElementHttp act@MkLocateAllFromElementActions{catch} opts rootId locator =
    preparedLoc &
    either
      (pure . Left . InvalidLocator)
      (mapLeftException locator . httpLocateAll findElms'' (toLocateAllActions act) opts)
  where
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator
    findElms'' mRoot sel = maybe
      (mkTry catch $ act.findElementsFromElement rootId sel)
      (\subRoot -> mkTry catch $ act.findElementsFromElement subRoot sel)
      mRoot

-- ---------------------------------------------------------------------------
-- Internal action conversions
-- ---------------------------------------------------------------------------

-- | Promote a 'LocateFromElementActions' to a 'LocateActions' for use with
-- internal helpers that only need the shared (non-finder) fields.
toLocateActions :: LocateFromElementActions m -> LocateActions m
toLocateActions MkLocateFromElementActions{catch, executeScript, getElementAttribute, getElementText} = MkLocateActions
  { catch,
    findElement = noRootFind,
    findElements = noRootFind,
    executeScript,
    getElementAttribute,
    getElementText
  }
  where noRootFind = error "library defect: root findElement(s) used in fromElement context"

-- | Promote a 'LocateAllFromElementActions' to a 'LocateAllActions' for use
-- with internal helpers that only need the shared (non-finder) fields.
toLocateAllActions :: LocateAllFromElementActions m -> LocateAllActions m
toLocateAllActions MkLocateAllFromElementActions{catch, executeScript, getElementAttribute, getElementText} = MkLocateAllActions
  { catch,
    findElements = error "library defect: root findElements used in fromElement context",
    executeScript,
    getElementAttribute,
    getElementText
  }

-- ---------------------------------------------------------------------------
-- Internal shared helpers
-- ---------------------------------------------------------------------------

type FindElm m = Maybe ElementId -> Selector -> m (Either PreLocateException ElementId)
type FindElms m = Maybe ElementId -> Selector -> m (Either PreLocateException [ElementId])

mkTry :: forall m a. Applicative m => (forall b e. (HasCallStack, Exception e) => m b -> (e -> m b) -> m b) -> (m a -> m (Either PreLocateException a))
mkTry catch action = catch (Right <$> action) (pure . Left . DriverException')

jsFilterDisplayed ::
  forall m.
  (Monad m) =>
  (ElementId -> m (Either PreLocateException Bool)) ->
  [ElementId] ->
  m (Either PreLocateException [ElementId])
jsFilterDisplayed recheckDisplayed' elms = do
  results <- traverse doCheck elms
  pure $ fmap (fmap fst . P.filter snd) (sequence results)
  where
    doCheck elm = fmap (elm,) <$> recheckDisplayed' elm

locateAll :: FindElms m -> Maybe ElementId -> Selector -> m (Either PreLocateException [ElementId])
locateAll findElms' mRoot s = findElms' mRoot s

-- finds leaf without display filtering
locateLeaf ::
  forall m.
  (Monad m) =>
  FindElm m ->
  FindElms m ->
  (ElementId -> Text -> m (Maybe Text)) ->
  (ElementId -> m Text) ->
  (ElementId -> m (Either PreLocateException Bool)) ->
  Maybe ElementId ->
  LeafCardinality ->
  RoleLocateSecondPass ->
  LeafLoc ->
  m (Either PreLocateException [ElementId])
locateLeaf findElm' findElms' getElementAttribute' getElementText' _recheckDisplayed mRoot leafCardinality rolesSecondPass loc = do
  let findFirst = leafCardinality == LeafFirst
  firstPass <-
    ( if findFirst
        then fmap (fmap pure) . findElm' mRoot
        else locateAll findElms' mRoot
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
          else fmap Right $ roleToXPathHttpSecondPass locateAllLenient getElementAttribute' getElementText' mRoot findFirst role
      InnerText {} -> baseResult
  where
    locateAllLenient r s = locateAll findElms' r s >>= pure . either (const []) id

chkRefilterSingleton ::
  forall m.
  (Monad m) =>
  (ElementId -> m (Either PreLocateException Bool)) ->
  [ElementId] ->
  m (Either PreLocateException [ElementId])
chkRefilterSingleton recheckDisplayed' elmIds =
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
              (jsFilterDisplayed recheckDisplayed' xs >>= either (pure . Left) (chkkSingleton' False))

-- single shot base locate (all cardinality)
locateElmsUnchecked ::
  forall m.
  (Monad m) =>
  FindElm m ->
  FindElms m ->
  (ElementId -> Text -> m (Maybe Text)) ->
  (ElementId -> m Text) ->
  (ElementId -> m (Either PreLocateException Bool)) ->
  Maybe ElementId ->
  LeafCardinality ->
  RoleLocateSecondPass ->
  ReducedHttpLoc ->
  m (Either PreLocateException [ElementId])
locateElmsUnchecked findElm' findElms' getAttr getText recheckDisplayed' mRoot leafCardinality rolesSecondPass =
  fmap (fmap LST.nub)
    . \case
      LeafHttp cl ->
        locateLeaf findElm' findElms' getAttr getText recheckDisplayed' mRoot leafCardinality rolesSecondPass cl
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
    locate = locateElmsUnchecked findElm' findElms' getAttr getText recheckDisplayed' mRoot

    locateContained :: [ElementId] -> ReducedHttpLoc -> m (Either PreLocateException [ElementId])
    locateContained containerIds subLoc = do
      containedResults <- traverse (\rootId -> locateElmsUnchecked findElm' findElms' getAttr getText recheckDisplayed' (Just rootId) LeafMany rolesSecondPass subLoc) containerIds
      pure . fmap join . sequence $ containedResults

-- ---------------------------------------------------------------------------
-- Internal locate implementations
-- ---------------------------------------------------------------------------

httpLocateSingleton ::
  forall m.
  (Monad m) =>
  FindElm m ->
  FindElms m ->
  LocateActions m ->
  HttpLocateOpts ->
  ReducedHttpLoc ->
  m (Either PreLocateException [ElementId])
httpLocateSingleton findElm' findElms' actions@MkLocateActions{catch} opts loc = do
  case loc of
    LeafHttp ll -> do
      lr <- locateLeaf findElm' findElms' actions.getElementAttribute actions.getElementText recheckDisplayed' Nothing LeafMany secondPassOnInitial ll
      filtered <- chkElmsSingleton (displayChkAlways || isUnique && displayChkDisambiguate) lr
      case filtered of
        Left e -> pure (Left e)
        Right [] ->
          if opts.extendedRoleLocation == ExtLocateSingletonMiss && isRole
            then do
              missRetryRslt <- locateLeaf findElm' findElms' actions.getElementAttribute actions.getElementText recheckDisplayed' Nothing LeafMany WantSecondPass ll
              retryChked <- chkElmsSingleton (displayChkAlways || displayChkDisambiguate) missRetryRslt
              case retryChked of
                Left e -> pure (Left e)
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
      locateElmsUnchecked findElm' findElms' actions.getElementAttribute actions.getElementText recheckDisplayed' Nothing LeafMany secondPassOnInitial loc
  where
      recheckDisplayed' = isDisplayedHttp catch actions.executeScript

      notFoundErr :: m (Either PreLocateException [ElementId])
      notFoundErr = pure . Left $ (ElementNotFound' "No element found matching locator.")

      throwAmbiguous elms = pure . Left $ (AmbiguousLocator' ("Multiple elements found matching locator: " <> txt elms))
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
              then chkRefilterSingleton recheckDisplayed' elms
              else pure (Right elms)

httpLocateAll ::
  forall m.
  (Monad m) =>
  FindElms m ->
  LocateAllActions m ->
  HttpLocateAllOpts ->
  ReducedHttpLoc ->
  m (Either PreLocateException [ElementId])
httpLocateAll findElms' actions@MkLocateAllActions{catch} opts loc = do
  let recheckDisplayed' = isDisplayedHttp catch actions.executeScript
      mkLocResult = pure . Right
      findElm' _ _ = error "library defect: findElm called in locate-all context"
      secondPassOnInitial = case opts.extendedRoleLocation of
        ExtLocateAllNever -> NoSecondPass
        ExtLocateAllAlways -> WantSecondPass
  result <- locateElmsUnchecked findElm' findElms' actions.getElementAttribute actions.getElementText recheckDisplayed' Nothing LeafMany secondPassOnInitial loc
  case result of
    Left e -> pure (Left e)
    Right elms ->
      if opts.jsRecheckDisplayed == DisplayedCheckAlways
        then jsFilterDisplayed recheckDisplayed' elms >>= either (pure . Left) mkLocResult
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
  m (Either PreLocateException Bool)
isDisplayedHttp catch execScript eid =
  catch
    (Right . toBool <$> execScript MkScript {script = displayedJS, args = [toJSON eid]})
    -- if any error occurs when checking displayed, assume element is displayed
    -- eg. if element becomes stale between finding and checking displayed, or if the driver does not support executeScript
    (pure . Left . DriverException')
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
