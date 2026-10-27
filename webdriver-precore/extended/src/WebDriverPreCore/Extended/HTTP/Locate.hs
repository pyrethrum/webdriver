module WebDriverPreCore.Extended.HTTP.Locate
  (
    ExtendedRoleLocateSingleton(..),
    HttpLocateOpts (..),
    LocateActions (..),
    SingletonCardinality (..),
    WDTrace(..),
    locateHttp,
    locateFromElementHttp,
    locateAllHttp,
    locateAllFromElementHttp
  )
where

import Control.Exception (Exception)
import Control.Monad (foldM, join)
import Data.Aeson as A (FromJSON, Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Containers.ListUtils (nubOrd)
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes)
import Data.Text
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import WebDriverPreCore.Extended.Common.Locate (
    DisplayedCheck (..),
    LocateException (..),
    PreLocateException (..),
    LeafCardinality (..),
    addLocToException,
    runLoc,
    displayedJS,
    shouldRunDisplayedCheck,
    filterDisplayed,
    filterDisplayedForSingleton
  )
import WebDriverPreCore.Extended.Common.Locators.Internal (Locator, RoleLocator (..), CompoundLocator, HttpLoc (..), xPathRelativePrefix)
import WebDriverPreCore.Extended.Common.Locators.Internal qualified as LI
import WebDriverPreCore.HTTP.Protocol as HTTPP (ElementId, Script (..), Selector (..))
import Prelude as P hiding (log)
import WebDriverPreCore.Utils.Utils (txt)
import Data.Bifunctor (Bifunctor(..))

-- | Whether to find the unique element (error if multiple match) or just the first.
data SingletonCardinality = Unique | First deriving (Show, Eq)

data ExtendedRoleLocateSingleton = ExtLocateNever | ExtLocateSingletonMiss | ExtLocateAlways deriving (Show, Eq)

data RoleJSSecondPass = DoRoleJSSecondPass | NoRoleJSSecondPass deriving (Show, Eq)

data WDTrace = Prepared {
  loc :: Locator,
  reducedLoc :: CompoundLocator HttpLoc
} |
 PrepareFailed {
  loc :: Locator,
  error :: LI.InvalidLocator
} | 
 JSDisplayedCheck {
  beforeCheck :: [ElementId],
  afterCheck :: [ElementId]
} |
 LeafLocate {
  selector :: Selector,
  cardinality :: LeafCardinality,
  found :: [ElementId]
  } | 
  RoleSecondPassLabeledBy {
    role :: RoleLocator,
    elms :: [ElementId]
  } |
  RoleSecondPassFor {
    role :: RoleLocator,
    elms :: [ElementId]
  }
 deriving (Show, Eq, Generic)

instance ToJSON WDTrace

instance FromJSON WDTrace

-- | Options for singleton locate functions ('locateHttp', 'locateFromElementHttp').
data HttpLocateOpts = MkHttpLocateOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateSingleton,
    singletonCardinality :: SingletonCardinality,
    mkDefaultLoc :: Text -> Locator
  }

-- | Actions for singleton locate functions ('locateHttp', 'locateFirstHttp', 'locateFromElementHttp').
data LocateActions m = MkLocateActions
  { 
    throw :: forall a. HasCallStack => PreLocateException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    trace :: WDTrace -> m (),
    findElement :: Selector -> m ElementId,
    findElementFromElement :: ElementId -> Selector -> m ElementId,
    findElements :: Selector -> m [ElementId],
    findElementsFromElement :: ElementId -> Selector -> m [ElementId],
    executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text
  }
data LocParams m = MkLocParams
  { 
    throw :: forall a. HasCallStack => PreLocateException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    trace :: WDTrace -> m (),
    findElement :: Selector -> m ElementId,
    findElementFromElement :: ElementId -> Selector -> m ElementId,
    findElements :: Selector -> m [ElementId],
    findElementsFromElement :: ElementId -> Selector -> m [ElementId],
    executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text,
    defaultLoc :: Text -> Locator,
    jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateSingleton,
    singletonCardinality :: SingletonCardinality
  }

mkParams :: HttpLocateOpts -> LocateActions m -> LocParams m
mkParams MkHttpLocateOpts{mkDefaultLoc = defaultLoc, ..} MkLocateActions{..} = MkLocParams{..}

-- | Extract a single element from a locate result.
mkSingleton :: Locator -> Either LocateException [ElementId] -> Either LocateException ElementId
mkSingleton loc = \case
  Right (x : _) -> Right x
  Right []      -> Left $ addLocToException loc elementNotFoundError
  Left e        -> Left e

-- | Locate a unique or first-matching element from the document root.
locateHttp :: forall m. Monad m => LocateActions m -> HttpLocateOpts -> Locator -> m (Either LocateException ElementId)
locateHttp actions opts l = mkSingleton l <$> runHttpAction actions opts Nothing httpLocateSingleton l

-- | Locate all matching elements from the document root.
locateAllHttp :: forall m. Monad m => LocateActions m -> HttpLocateOpts -> Locator -> m (Either LocateException [ElementId])
locateAllHttp actions opts = runHttpAction actions opts Nothing httpLocateAll

-- | Locate a unique or first-matching element rooted at a given element.
locateFromElementHttp :: forall m. Monad m => LocateActions m -> HttpLocateOpts -> ElementId -> Locator -> m (Either LocateException ElementId)
locateFromElementHttp actions opts rootId l = mkSingleton l <$> runHttpAction actions opts (Just rootId) httpLocateSingleton l

-- | Locate all matching elements rooted at a given element.
locateAllFromElementHttp :: forall m. Monad m => LocateActions m -> HttpLocateOpts -> ElementId -> Locator -> m (Either LocateException [ElementId])
locateAllFromElementHttp actions opts rootId = runHttpAction actions opts (Just rootId) httpLocateAll

-- | Common implementation for all public HTTP locate functions.
runHttpAction ::
  forall m.
  Monad m =>
  LocateActions m ->
  HttpLocateOpts ->
  -- | root element
  Maybe ElementId ->
  (LocParams m -> CompoundLocator HttpLoc -> m [ElementId]) ->
  Locator ->
  m (Either LocateException [ElementId])
runHttpAction actions@MkLocateActions{catch} opts mRootId locateAction loc = do
  let p = setBaseElement mRootId $ mkParams opts actions
  case LI.transformHttp p.defaultLoc loc of
    -- log failure if transformation failed
    Left err -> do
      p.trace (PrepareFailed loc err)
      pure $ Left (InvalidLocator err)
    -- run locator if transformation succeeded
    Right compoundLoc -> do
      p.trace (Prepared loc compoundLoc)
      first (addLocToException loc) <$> runLoc catch (locateAction p) compoundLoc


setBaseElement :: Maybe ElementId -> LocParams m -> LocParams m
setBaseElement mRootId act@MkLocParams{..} = 
  maybe act (\rootId -> MkLocParams {
  findElement = findElementFromElement rootId,
  findElements = findElementsFromElement rootId,
  ..
}) mRootId

jsFilterDisplayed ::
  forall m.
  (Monad m) =>
  LocParams m ->
  [ElementId] ->
  m [ElementId]
jsFilterDisplayed MkLocParams{trace, executeScript} elms = do
  filtered <- filterDisplayed runScript elms
  trace $
    JSDisplayedCheck
      { beforeCheck = elms,
        afterCheck = filtered
      }
  pure filtered
  where
    runScript :: [ElementId] -> m [Bool]
    runScript elms' = toBools <$> executeScript MkScript {script = displayedJS, args = [toJSON elms']}

    toBools :: Value -> [Bool]
    toBools val = case A.fromJSON val of
      A.Success bs -> bs
      A.Error _ -> error $ "library defect - jsFilterDisplayed: isDisplayed script returned unexpected value (expected [Bool]) - got:\n  " <> P.show val

-- finds leaf without display filtering
locateLeaf ::
  forall m.
  (Monad m) =>
  LocParams m ->
  RoleJSSecondPass ->
  LeafCardinality ->
  HttpLoc ->
  m [ElementId]
locateLeaf prms rolesSecondPass lc loc = do
  case loc of
    CSSHttp {} -> findElms
    XPathHttp {} -> findElms
    RoleHttp {roleSpec} -> 
      case rolesSecondPass of 
        NoRoleJSSecondPass -> findElms
        DoRoleJSSecondPass -> do 
          sr <- findElms
          case lc of
            FindFirst ->
              case sr of
                [] -> indirectRoleElms
                x:_ -> pure [x]
            FindAll ->
              (sr <>) <$> indirectRoleElms
          where 
            indirectRoleElms = findByRoleIndirect prms lc roleSpec
  where
    httpSelector :: Selector
    httpSelector = toSelector loc

    findElms :: m [ElementId]
    findElms = do 
      ids <- case lc of
        FindFirst -> fmap LST.singleton $ prms.findElement httpSelector
        FindAll -> prms.findElements httpSelector
      prms.trace $ LeafLocate httpSelector lc ids
      pure ids
          
chkRefilterSingleton ::
  forall m.
  (Monad m) =>
  LocParams m ->
  [ElementId] ->
  m [ElementId]
chkRefilterSingleton actions =
  filterDisplayedForSingleton (jsFilterDisplayed actions)

-- single shot base locate (all cardinality)
locateElmsUnchecked ::
  forall m.
  (Monad m) =>
  LocParams m ->
  LeafCardinality ->
  RoleJSSecondPass ->
  CompoundLocator HttpLoc ->
  m [ElementId]
locateElmsUnchecked actions leafCardinality rolesSecondPass loc =
   LST.nub <$>
    case loc of
      LI.LeafC cl ->
        locateLeaf actions rolesSecondPass leafCardinality cl
      LI.ContainsC {container, contained} -> do
        containers <- locateAll rolesSecondPass container
        locateContained containers contained
      LI.AllC {elms = locs} -> do
        let (l :| ls) = locs
            step acc loc' =
              if P.null acc
                then pure []
                else LST.intersect acc <$> locateAll rolesSecondPass loc'
        initial <- locateAll rolesSecondPass l
        foldM step initial ls
      LI.AnyC {elms = locs} ->
       join <$> traverse (\l -> locateAll rolesSecondPass l) (toList locs)
  where
    locateAll = locateElmsUnchecked actions FindAll

    locateContained :: [ElementId] -> CompoundLocator HttpLoc -> m [ElementId]
    locateContained containerIds subLoc = do
      containedResults <- 
        traverse 
          (\containerId -> locateElmsUnchecked (setBaseElement (Just containerId) actions) FindAll rolesSecondPass subLoc) 
          containerIds
      pure $ join containedResults

httpLocateSingleton ::
  forall m.
  (Monad m) =>
  LocParams m ->
  CompoundLocator HttpLoc ->
  m [ElementId]
httpLocateSingleton prms@MkLocParams{throw}  loc = do
  case loc of
    LI.LeafC ll -> do
      let leafCard = if prms.singletonCardinality == First
                        && prms.jsRecheckDisplayed /= DisplayedCheckAlways
                     then FindFirst
                     else FindAll
      lr <- locateLeaf prms secondPassOnInitial leafCard ll
      filtered <- chkElmsSingleton lr
      case filtered of
        [] ->
          if prms.extendedRoleLocation == ExtLocateSingletonMiss && isRole
            then do
              missRetryRslt <- locateLeaf prms DoRoleJSSecondPass FindAll ll
              retryChked <- chkElmsSingleton missRetryRslt
              case retryChked of
                [] -> throwNotFound
                [x] -> pure [x]
                (x : xs) ->
                  if isUnique
                    then throwAmbiguous xs
                    else pure [x]
            else throwNotFound
        [x] -> pure [x]
        elms@(x : _xs) ->
          if isUnique
            then throwAmbiguous elms
            else pure [x]
    _ ->
      locateElmsUnchecked prms FindAll secondPassOnInitial loc
  where
      throwNotFound = throw elementNotFoundError
      throwAmbiguous elms = throw (AmbiguousLocator' ("Multiple elements found matching locator: " <> txt elms))
      isUnique = prms.singletonCardinality == Unique
      isRole = case loc of
        LI.LeafC RoleHttp {} -> True
        _ -> False
      secondPassOnInitial = case prms.extendedRoleLocation of
        ExtLocateNever -> NoRoleJSSecondPass
        ExtLocateSingletonMiss -> NoRoleJSSecondPass
        ExtLocateAlways -> DoRoleJSSecondPass

      chkElmsSingleton elms =
        if wantRecheck
          then chkRefilterSingleton prms elms
          else pure elms
        where
          wantRecheck =
            shouldRunDisplayedCheck prms.jsRecheckDisplayed (prms.singletonCardinality == Unique)

elementNotFoundError :: PreLocateException
elementNotFoundError = ElementNotFound' "No element found matching locator."

httpLocateAll ::
  forall m.
  (Monad m) =>
  LocParams m ->
  CompoundLocator HttpLoc ->
  m [ElementId]
httpLocateAll prms loc = do
  let secondPassOnInitial = case prms.extendedRoleLocation of
        ExtLocateNever -> NoRoleJSSecondPass
        ExtLocateSingletonMiss -> NoRoleJSSecondPass
        ExtLocateAlways -> DoRoleJSSecondPass
  elms <- locateElmsUnchecked prms FindAll secondPassOnInitial loc
  if shouldRunDisplayedCheck prms.jsRecheckDisplayed False
    then jsFilterDisplayed prms elms 
    else pure elms

data SingletonCheckResult
  = SingletonSuccess {elms :: [ElementId]}
  | Missing {elms :: [ElementId]}
  | Ambiguous {elms :: [ElementId]}
  deriving (Show, Eq, Ord)

toSelector :: HttpLoc -> Selector
toSelector = \case
  CSSHttp {value} -> HTTPP.CSS value
  XPathHttp {value} -> HTTPP.XPath value
  RoleHttp {xpath} -> HTTPP.XPath xpath
  -- shim BiDiNative locators
  -- BiDiNative sl -> case sl of
  --   Role {role} -> HTTPP.XPath $ roleToXPath role
  --   InnerText {value, matchType, caseSensitivity, maxDepth} -> HTTPP.XPath $ innerTextToXPath value caseSensitivity matchType maxDepth

_locateBiDi :: a
_locateBiDi = undefined

notNull :: [a] -> Bool
notNull = not . P.null

findByRoleIndirect ::
  forall m.
  (Monad m) =>
  LocParams m ->
  LeafCardinality ->
  RoleLocator ->
  m [ElementId]
findByRoleIndirect actions lc roleLoc =
    case roleLoc of
      -- role type has no name / label so nothing to do
      RoleType {} -> pure []
      _ -> do
        labelledByElms <- findRoleByAriaLabledBy actions lc roleLoc
        if lc == FindFirst && notNull labelledByElms
          then pure labelledByElms
          else do
            forElms <- findRoleByForLabel actions lc roleLoc
            pure . nubOrd $ mconcat [labelledByElms, forElms]

findRoleByAriaLabledBy ::
  forall m.
  (Monad m) =>
  LocParams m ->
  LeafCardinality ->
  RoleLocator ->
  m [ElementId]
findRoleByAriaLabledBy prms lc roleLoc =
  case roleLoc of
    RoleType {} -> pure []
    _ -> do
      candidates <-
        -- all elms that match role and have an aria-labelledby attribute
        prms.findElements (HTTPP.XPath $ xPathRelativePrefix <> LI.roleTypeOnlyXPath roleLoc <> "[@aria-labelledby]")
      r <- filterElms lc labledByMatchesRoleText candidates
      prms.trace $ RoleSecondPassLabeledBy roleLoc r
      pure r
      where
        -- Resolve aria-labelledby on @eid@: split on whitespace to get ID-refs,
        -- look up the text of each referenced element, concatenate with spaces,
        -- and compare (after stripping) to @targetName@.
        labledByMatchesRoleText :: ElementId -> m Bool
        labledByMatchesRoleText eid =
          prms.getElementAttribute eid "aria-labelledby"
            >>= \case
              Nothing -> pure False
              Just lblIds -> do
                mappedTxts <- traverse textForId $ T.words lblIds
                pure $ T.strip (T.unwords $ catMaybes mappedTxts) == T.strip roleLoc.name

        -- Find the element whose @id@ matches @idRef@ and return its text, or
        -- 'Nothing' if no such element exists.
        textForId :: Text -> m (Maybe Text)
        textForId idRef = do
          elms <- prms.findElements . HTTPP.XPath $ xPathRelativePrefix <> "*[@id='" <> idRef <> "']"
          case elms of
            [] -> pure Nothing
            (e : _) -> Just <$> prms.getElementText e

findRoleByForLabel ::
  forall m.
  (Monad m) =>
  LocParams m ->
  LeafCardinality ->
  RoleLocator ->
  m [ElementId]
findRoleByForLabel actions lc roleLoc =
  case roleLoc of
    RoleType {} -> pure []
    _ -> do
      candidates <-
        -- has an @id and matches the role name
        actions.findElements $ HTTPP.XPath $ xPathRelativePrefix <> LI.roleTypeOnlyXPath roleLoc <> "[@id]"
      r <- filterElms lc forTxtMatchesId candidates
      actions.trace $ RoleSecondPassFor roleLoc r
      pure r
      where
        forTxtMatchesId :: ElementId -> m Bool
        forTxtMatchesId eid = do
          mId <- actions.getElementAttribute eid "id"
          case mId of
            Nothing -> pure False
            Just idVal -> do
              labels <- actions.findElements . HTTPP.XPath $ xPathRelativePrefix <> "label[@for='" <> idVal <> "']"
              case labels of
                [] -> pure False
                (lbl : _) -> do
                  labelText <- actions.getElementText lbl
                  pure $ T.strip labelText == T.strip roleLoc.name

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
