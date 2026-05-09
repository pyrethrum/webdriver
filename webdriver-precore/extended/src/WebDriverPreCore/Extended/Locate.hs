module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    SingletonCardinality (..),
    HttpLocateOpts (..),
    HttpLocateAllOpts (..),
    LocateActions (..),
    locateHttp,
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
import Data.Bifunctor (first)

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

-- | Whether to find the unique element (error if multiple match) or just the first.
data SingletonCardinality = Unique | First deriving (Show, Eq)

data LeafCardinality = FindFirst | FindAll deriving (Show, Eq)

data DisplayedCheck = DisplayedCheckNever | DisplayedCheckDisambiguateUnique | DisplayedCheckAlways deriving (Show, Eq)

data ExtendedRoleLocateSingleton = ExtLocateSingletonNever | ExtLocateSingletonMiss | ExtLocateSingletonAlways deriving (Show, Eq)

data ExtendedRoleLocateAll = ExtLocateAllNever | ExtLocateAllAlways deriving (Show, Eq)

data RoleJSSecondPass = DoRoleJSSecondPass | NoRoleJSSecondPass deriving (Show, Eq)

-- | Options for singleton locate functions ('locateHttp', 'locateFromElementHttp').
data HttpLocateOpts = MkHttpLocateOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateSingleton,
    singletonCardinality :: SingletonCardinality,
    mkDefaultLoc :: Text -> Locator
  }

-- | Options for multi-locate functions ('locateAllHttp', 'locateAllFromElementHttp').
data HttpLocateAllOpts = MkHttpLocateAllOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateAll,
    mkDefaultLoc :: Text -> Locator
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
    findElementFromElement :: ElementId -> Selector -> m ElementId,
    findElements :: Selector -> m [ElementId],
    findElementsFromElement :: ElementId -> Selector -> m [ElementId],
    executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text
  }

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
locateHttp actions@MkLocateActions{catch} opts =
  prepareRun catch opts.mkDefaultLoc (httpLocateSingleton actions opts)

-- | Locate all matching elements from the document root.
locateAllHttp ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  HttpLocateAllOpts ->
  Locator ->
  m (Either LocateException [ElementId])
locateAllHttp actions@MkLocateActions{catch} opts =
  prepareRun catch opts.mkDefaultLoc (httpLocateAll actions opts)

-- | Locate a unique or first-matching element rooted at a given element.
locateFromElementHttp ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  HttpLocateOpts ->
  -- | root element
  ElementId ->
  Locator ->
  m (Either LocateException [ElementId])
locateFromElementHttp actions@MkLocateActions{catch} opts rootId =
  prepareRun catch opts.mkDefaultLoc (httpLocateSingleton (setBaseElement rootId actions) opts)
    
-- | Locate all matching elements rooted at a given element.
locateAllFromElementHttp ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  HttpLocateAllOpts ->
  -- | root element
  ElementId ->
  Locator ->
  m (Either LocateException [ElementId])
locateAllFromElementHttp actions@MkLocateActions{catch} opts rootId =
    prepareRun catch opts.mkDefaultLoc (httpLocateAll (setBaseElement rootId actions) opts)


setBaseElement :: ElementId -> LocateActions m -> LocateActions m
setBaseElement rootId act = act {
  findElement = act.findElementFromElement rootId,
  findElements = act.findElementsFromElement rootId
}
   
prepareRun :: forall m. Monad m =>
     (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a)
     -> (Text -> Locator) 
     -> (ReducedHttpLoc -> m (Either PreLocateException [ElementId])) 
     -> Locator 
     -> m (Either LocateException [ElementId])
prepareRun catch mkDefaultLoc locateActn locator =
    either (pure . Left . InvalidLocator) (completeLocException locator . runLoc) preparedLoc
  where 
    preparedLoc :: Either LI.InvalidLocator ReducedHttpLoc
    preparedLoc = prepareSimplify mkDefaultLoc HTTP locator >>= toHttpLocator

    runLoc :: ReducedHttpLoc -> m (Either PreLocateException [ElementId])
    runLoc loc = catch (locateActn loc) (pure . Left . DriverException')

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

-- finds leaf without display filtering
locateLeaf ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  RoleJSSecondPass ->
  LeafCardinality ->
  LeafLoc ->
  m [ElementId]
locateLeaf actions rolesSecondPass lc loc = do
  let 
    simpleLocate :: m [ElementId]
    simpleLocate =
          ( if (lc == FindFirst)
              then fmap LST.singleton . actions.findElement 
              else actions.findElements
          )
          (toSelector loc)
  case loc of
    RL.CSS {} -> simpleLocate
    RL.XPath {} -> simpleLocate
    RL.BiDiNative sl -> case sl of
      Role {role} ->
        case rolesSecondPass of 
          NoRoleJSSecondPass -> simpleLocate
          DoRoleJSSecondPass -> do 
            sr <- simpleLocate
            if lc == FindFirst then 
              case sr of
                [] -> roleToXPathHttpSecondPass actions lc role
                [x] -> pure [x]
                x:_ -> pure [x]
              else
               (sr <>) <$> roleToXPathHttpSecondPass actions lc role
      InnerText {} -> simpleLocate


chkRefilterSingleton ::
  forall m.
  (Monad m) =>
  -- | catch
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | execute script
  (Script -> m Value) ->
  [ElementId] ->
  m (Either PreLocateException [ElementId])
chkRefilterSingleton catch executeScript elmIds =
  chkSingleton' True elmIds
  where
    jsRecheckDisplayed = isDisplayedViaScript catch executeScript
    chkSingleton' recheckAmbiguous' =
      \case
        [] -> pure (Right [])
        [x] -> pure (Right [x])
        xs ->
          recheckAmbiguous'
            & bool
              (pure (Right xs))
              (jsFilterDisplayed jsRecheckDisplayed xs >>= either (pure . Left) (chkSingleton' False))

-- single shot base locate (all cardinality)
locateElmsUnchecked ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  LeafCardinality ->
  RoleJSSecondPass ->
  ReducedHttpLoc ->
  m [ElementId]
locateElmsUnchecked actions leafCardinality rolesSecondPass loc =
  fmap LST.nub $
    case loc of
      LeafHttp cl ->
        locateLeaf actions rolesSecondPass leafCardinality cl
      CombintorHttp cb -> case cb of
        Contains {container, contained} -> do
          containers <- locate FindAll rolesSecondPass container
          locateContained containers contained
        All {elms = locs} -> do
          let (l :| ls) = locs
              step acc loc' =
                if P.null acc
                  then pure []
                  else fmap (LST.intersect acc) (locateElmsUnchecked actions FindAll rolesSecondPass loc')
          initial <- locate FindAll rolesSecondPass l
          foldM step initial ls
        Any {elms = locs} ->
          fmap join $
            traverse (locate FindAll rolesSecondPass) (toList locs)
      PostFilterHttpLoc {} -> postfilterNotImplemented
  where
    locate = locateElmsUnchecked actions

    locateContained :: [ElementId] -> ReducedHttpLoc -> m [ElementId]
    locateContained containerIds subLoc = do
      containedResults <- traverse (\_ -> locateElmsUnchecked actions FindAll rolesSecondPass subLoc) containerIds
      pure $ join containedResults

-- ---------------------------------------------------------------------------
-- Internal locate implementations
-- ---------------------------------------------------------------------------

httpLocateSingleton ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  HttpLocateOpts ->
  ReducedHttpLoc ->
  m (Either PreLocateException [ElementId])
httpLocateSingleton actions@MkLocateActions{catch} opts loc = do
  case loc of
    LeafHttp ll -> do
      lr <- locateLeaf actions secondPassOnInitial FindAll ll
      filtered <- chkElmsSingleton lr
      case filtered of
        Left e -> pure (Left e)
        Right [] ->
          if opts.extendedRoleLocation == ExtLocateSingletonMiss && isRole
            then do
              missRetryRslt <- locateLeaf actions DoRoleJSSecondPass FindAll ll
              retryChked <- chkElmsSingleton missRetryRslt
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
      fmap Right (locateElmsUnchecked actions FindAll secondPassOnInitial loc)
  where

      notFoundErr :: m (Either PreLocateException [ElementId])
      notFoundErr = pure . Left $ (ElementNotFound' "No element found matching locator.")

      throwAmbiguous elms = pure . Left $ (AmbiguousLocator' ("Multiple elements found matching locator: " <> txt elms))
      mkLocResult = pure . Right  
      isUnique = opts.singletonCardinality == Unique
      isRole = case loc of
        LeafHttp (RL.BiDiNative (Role {})) -> True
        _ -> False
      secondPassOnInitial = case opts.extendedRoleLocation of
        ExtLocateSingletonNever -> NoRoleJSSecondPass
        ExtLocateSingletonMiss -> NoRoleJSSecondPass
        ExtLocateSingletonAlways -> DoRoleJSSecondPass

      chkElmsSingleton elms =
        let 
          displayChk = opts.jsRecheckDisplayed
          cardinality = opts.singletonCardinality
          wantRecheck = 
             displayChk == DisplayedCheckAlways 
             || displayChk == DisplayedCheckDisambiguateUnique && cardinality == Unique
        in
        if wantRecheck
          then chkRefilterSingleton catch actions.executeScript elms
          else pure (Right elms)

httpLocateAll ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  HttpLocateAllOpts ->
  ReducedHttpLoc ->
  m (Either PreLocateException [ElementId])
httpLocateAll actions@MkLocateActions{catch} opts loc = do
  let recheckDisplayed' = isDisplayedViaScript catch actions.executeScript
      mkLocResult = pure . Right
      secondPassOnInitial = case opts.extendedRoleLocation of
        ExtLocateAllNever -> NoRoleJSSecondPass
        ExtLocateAllAlways -> DoRoleJSSecondPass
  elms <- locateElmsUnchecked actions FindAll secondPassOnInitial loc
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

isDisplayedViaScript ::
  forall m.
  (Monad m) =>
  -- | catch 
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | execute script
  (Script -> m Value) ->
  -- | element to check
  ElementId ->
  m (Either PreLocateException Bool)
isDisplayedViaScript catch execScript eid =
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
  LocateActions m ->
  LeafCardinality ->
  RoleLocator ->
  m [ElementId]
roleToXPathHttpSecondPass actions lc roleLoc =
    case roleLoc of
      -- role type has no name / label so nothing to do
      RoleType {} -> pure []
      _ -> do
        labelledByElms <- roleToXPathHttpLabeledBy actions lc roleLoc
        if lc == FindFirst && notNull labelledByElms
          then pure labelledByElms
          else do
            forElms <- roleToXPathFor actions lc roleLoc
            pure . nubOrd $ mconcat [labelledByElms, forElms]

roleToXPathHttpLabeledBy ::
  forall m.
  (Monad m) =>
  -- | locate all elements matching a selector
  LocateActions m ->
  LeafCardinality ->
  RoleLocator ->
  m [ElementId]
roleToXPathHttpLabeledBy actions lc roleLoc =
  case roleLoc of
    RoleType {} -> pure []
    _ -> do
      candidates <-
        -- all elms that match role and have an aria-labelledby attribute
        actions.findElements (HTTPP.XPath $ "//*" <> roleXPath roleLoc <> "[@aria-labelledby]")
      filterElms lc labledByMatchesRoleText candidates
      where
        -- Resolve aria-labelledby on @eid@: split on whitespace to get ID-refs,
        -- look up the text of each referenced element, concatenate with spaces,
        -- and compare (after stripping) to @targetName@.
        labledByMatchesRoleText :: ElementId -> m Bool
        labledByMatchesRoleText eid =
          actions.getElementAttribute eid "aria-labelledby"
            >>= \case
              Nothing -> pure False
              Just lblIds -> do
                mappedTxts <- traverse textForId $ T.words lblIds
                pure $ T.strip (T.unwords $ catMaybes mappedTxts) == T.strip roleLoc.name

        -- Find the element whose @id@ matches @idRef@ and return its text, or
        -- 'Nothing' if no such element exists.
        textForId :: Text -> m (Maybe Text)
        textForId idRef = do
          elms <- actions.findElements . HTTPP.XPath $ "//*[@id='" <> idRef <> "']"
          case elms of
            [] -> pure Nothing
            (e : _) -> Just <$> actions.getElementText e

--  use all findElements but limit to 2 results (not supported in standard HTTP WebDriver, but available in BiDi via maxNodeCount).

roleToXPathFor ::
  forall m.
  (Monad m) =>
  -- | locate all elements matching a selector
  LocateActions m ->
  LeafCardinality ->
  RoleLocator ->
  m [ElementId]
roleToXPathFor actions lc roleLoc =
  case roleLoc of
    RoleType {} -> pure []
    _ -> do
      candidates <-
        -- has an @id and matches the role name
        actions.findElements $ HTTPP.XPath $ "//*" <> roleXPath roleLoc <> "[@id]"
      filterElms lc forTxtMatchesId candidates
      where
        forTxtMatchesId :: ElementId -> m Bool
        forTxtMatchesId eid = do
          mId <- actions.getElementAttribute eid "id"
          case mId of
            Nothing -> pure False
            Just idVal -> do
              labels <- actions.findElements . HTTPP.XPath $ "//label[@for='" <> idVal <> "']"
              case labels of
                [] -> pure False
                (lbl : _) -> do
                  labelText <- actions.getElementText lbl
                  pure $ T.strip labelText == T.strip roleLoc.name

roleXPath :: RoleLocator -> Text
roleXPath = \case
  RoleName {} -> "[not(@role='presentation' or @role='none')]"
  r -> LI.roleTypeXPathContent True r.role

filterElms :: forall m. (Monad m) => LeafCardinality -> (ElementId -> m Bool) -> [ElementId] -> m [ElementId]
filterElms lc matcher = recurse []
  where
    recurse :: [ElementId] -> [ElementId] -> m [ElementId]
    recurse acc rem' =
      if lc == FindFirst && notNull acc
        then
          pure acc
        else case rem' of
          [] -> pure $ P.reverse acc
          (e : es) -> do
            matches <- matcher e
            recurse (if matches then e : acc else acc) es
