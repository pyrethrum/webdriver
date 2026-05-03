module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    Cardinality (..),
    HttpLocateOpts (..),
    BaseLocateActions (..),
    locateHttp
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

data LeafCardinality = LeafFirst | LeafMany deriving (Show, Eq)

data DisplayedCheck = DisplayedCheckNever | DisplayedCheckDisambiguateUnique | DisplayedCheckAlways deriving (Show, Eq)

data ExtendedRoleLocate = ExtLocateNever | ExtLocateSingletonMiss | ExtLocateAlways deriving (Show, Eq)

data RoleLocateSecondPass = WantSecondPass | NoSecondPass deriving (Show, Eq)

data HttpLocateOpts = MkHttpLocateOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocate,
    cardinality :: Cardinality,
    defaultLocator :: Text -> Locator,
    baseElement :: Maybe ElementId
  }

data LocateResult
  = MkLocateResult
  { source :: Locator,
    elmIds :: [ElementId]
  }
  deriving (Show, Eq)

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

data BaseLocateActions m = BaseLocateActions
  { 
    executeScript :: Script -> m Value,
    findElement :: Selector -> m ElementId,
    findElementFromElement :: ElementId -> Selector -> m ElementId,
    findElements :: Selector -> m [ElementId],
    findElementsFromElement :: ElementId -> Selector -> m [ElementId],
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text
  }

locateHttp ::
  forall m.
  (Monad m) =>
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | locate actions
  BaseLocateActions m ->
  -- | locate opts
  HttpLocateOpts ->
  -- | locator
  Locator ->
  m (Either LocateException LocateResult)
locateHttp catch actions opts locator =
  either
    (pure . Left . InvalidLocator)
    httpLocate
    preparedLoc
  where
    preparedLoc :: Either LI.InvalidLocator ReducedHttpLoc
    preparedLoc = prepareSimplify opts.defaultLocator HTTP locator >>= toHttpLocator

    notFoundErr :: m (Either LocateException LocateResult)
    notFoundErr = pure . Left $ ElementNotFound {description = "No element found matching locator.", locator}

    throwAmbiguous :: [ElementId] -> m (Either LocateException LocateResult)
    throwAmbiguous elms = pure . Left $ AmbiguousLocator {description = "Multiple elements found matching locator: " <> txt elms, locator}

    mkLocResult :: [ElementId] -> m (Either LocateException LocateResult)
    mkLocResult = pure . Right . MkLocateResult locator

    withCatch :: forall a. m a -> m (Either LocateException a)
    withCatch action = catch (Right <$> action) (pure . Left . DriverException)

    getAttribute :: ElementId -> Text -> m (Maybe Text)
    getAttribute = actions.getElementAttribute

    findElm :: Maybe ElementId -> Selector -> m (Either LocateException ElementId)
    findElm mRoot sel =
      mRoot
        & maybe
          (withCatch $ actions.findElement sel)
          (\rootId -> withCatch $ actions.findElementFromElement rootId sel)

    findElms :: Maybe ElementId -> Selector -> m (Either LocateException [ElementId])
    findElms mRoot s =
      mRoot
        & maybe
          (withCatch $ actions.findElements s)
          (\rootId -> withCatch $ actions.findElementsFromElement rootId s)

    recheckDisplayed :: ElementId -> m (Either LocateException Bool)
    recheckDisplayed = isDisplayedHttp catch actions.executeScript

    jsFilterDisplayed :: [ElementId] -> m (Either LocateException [ElementId])
    jsFilterDisplayed elms = do
      results <- traverse doCheck elms
      pure $ fmap (fmap fst . P.filter snd) (sequence results)
      where
        doCheck elm = fmap (elm,) <$> recheckDisplayed elm

    locateAll :: Maybe ElementId -> Selector -> m (Either LocateException [ElementId])
    locateAll mRoot s = findElms mRoot s -- >>= filterDisplayedIf Always

    -- finds leaf without display filtering
    locateLeaf :: Maybe ElementId -> LeafCardinality -> RoleLocateSecondPass -> LeafLoc -> m (Either LocateException [ElementId])
    locateLeaf mRoot leafCardinality rolesSecondPass loc = do
      let findFirst = leafCardinality == LeafFirst
      firstPass <-
        ( if findFirst
            then fmap (fmap pure) . findElm mRoot
            else locateAll mRoot
        )
          (toSelector loc)
      let baseResult = pure firstPass
      case loc of
        RL.CSS {} -> baseResult
        RL.XPath {} -> baseResult
        RL.BiDiNative sl -> case sl of
          Role {role} -> do
            if rolesSecondPass == NoSecondPass
              then baseResult
              else fmap Right $ roleToXPathHttpSecondPass locateAllLenient getAttribute actions.getElementText mRoot findFirst role
          InnerText {} -> baseResult
      where
        locateAllLenient r s = locateAll r s >>= pure . either (const []) id

    chkRefilterSingleton ::
      [ElementId] ->
      m (Either LocateException [ElementId])
    chkRefilterSingleton elmIds =
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
                  (jsFilterDisplayed xs >>= either (pure . Left) (chkkSingleton' False))

    -- single shot base locate
    locateElmsUnchecked :: Maybe ElementId -> LeafCardinality -> RoleLocateSecondPass -> ReducedHttpLoc -> m (Either LocateException [ElementId])
    locateElmsUnchecked mRoot leafCardinality rolesSecondPass =
      fmap (fmap LST.nub)
        . \case
          LeafHttp cl ->
            -- need to find all elms for combinator and later checks and retries
            locateLeaf mRoot leafCardinality rolesSecondPass cl
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
        locate = locateElmsUnchecked mRoot

        locateContained :: [ElementId] -> ReducedHttpLoc -> m (Either LocateException [ElementId])
        locateContained containerIds subLoc = do
          -- for each container, locate contained with root of container element, and combine results
          containedResults <- traverse (\rootId -> locateElmsUnchecked (Just rootId) LeafMany rolesSecondPass subLoc) containerIds
          pure . fmap join . sequence $ containedResults

    httpLocate :: ReducedHttpLoc -> m (Either LocateException LocateResult)
    httpLocate loc =
      case loc of
        LeafHttp ll -> do
          lr <- locateLeaf opts.baseElement leafCardinality secondPassOnInitial ll
          filtered <- chkElmsSingleton (displayChkAlways || opts.cardinality == Unique && displayChkDisambiguate) lr

          case filtered of
            Left e -> pure (Left e)
            Right [] ->
              -- rerun with role second pass on miss (try to find one or more matches)
              if wantSingleton && isRole && opts.extendedRoleLocation == ExtLocateSingletonMiss
                then do
                  missRetryRslt <- locateLeaf opts.baseElement leafCardinality WantSecondPass ll
                  retryChked <- chkElmsSingleton (displayChkAlways || displayChkDisambiguate) missRetryRslt
                  case retryChked of
                    Left e -> pure (Left e)
                    Right [] -> notFoundErr
                    Right [x] -> mkLocResult [x]
                    Right (x : xs) ->
                      case opts.cardinality of
                        Unique -> throwAmbiguous xs
                        First -> mkLocResult [x]
                        Many ->
                          -- should never be here due to wantSingleton check
                          error "library defect - locateHttp: unexpected multiple results on singleton retry with extended role location"
                else
                  if wantSingleton
                    then notFoundErr
                    else mkLocResult []
            Right [x] -> mkLocResult [x]
            Right elms@(x : _xs) ->
              case opts.cardinality of
                Unique -> throwAmbiguous elms
                First -> mkLocResult [x]
                Many -> mkLocResult elms
        PostFilterHttpLoc {} ->
          -- will neeed to postfilter &&& all
          postfilterNotImplemented
        CombintorHttp {} ->
          locateElmsUnchecked opts.baseElement leafCardinality secondPassOnInitial loc >>= either (pure . Left) mkLocResult
      where
        chkElmsSingleton doChk eElms =
          case eElms of
            Left e -> pure (Left e)
            Right elms ->
              if doChk
                then chkRefilterSingleton elms
                else pure (Right elms)
        wantSingleton = case opts.cardinality of
          Unique -> True
          First -> True
          Many -> False
        displayChkAlways = opts.jsRecheckDisplayed == DisplayedCheckAlways
        displayChkDisambiguate = opts.jsRecheckDisplayed == DisplayedCheckDisambiguateUnique
        isRole = case loc of
          LeafHttp (RL.BiDiNative (Role {})) -> True
          _ -> False
        secondPassOnInitial =
          case opts.extendedRoleLocation of
            ExtLocateNever -> NoSecondPass
            ExtLocateSingletonMiss -> NoSecondPass
            ExtLocateAlways -> WantSecondPass
        leafCardinality =
          case opts.cardinality of
            First -> LeafFirst
            -- need to find all to check uniqueness
            Unique -> LeafMany
            Many -> LeafMany

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
