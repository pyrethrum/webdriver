module WebDriverPreCore.Extended.BiDi.Locate
--   (
--     DisplayedCheck(..),
--     ExtendedRoleLocateSingleton(..),
--     BiDiLocateOpts (..),
--     LocateActions (..),
--     SingletonCardinality (..),
--     locateBiDi,
--     locateFromElementBiDi,
--     -- locateFromElementsBiDi,
--     locateAllBiDI,
--     locateAllFromElementBiDi
--     -- locateAllFromElementsBiDi
--   )
where

import Control.Exception (Exception)
import Control.Monad (foldM, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson as A (Result (..), fromJSON, toJSON, Value)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes)
import Data.Text
import Data.Text qualified as T
import GHC.Stack (HasCallStack)


import WebDriverPreCore.Extended.LocateCommon (
    LocateException (..),
    LocateResult (..),
    LocateTracing (..),
    PreLocateException (..),
    WDTrace (..),
    LeafCardinality (..),
    completeLocException
  )
import WebDriverPreCore.Extended.Locators.Internal (Locator, RoleLocator (..), CompoundLocator, HttpLoc (..), xPathRelativePrefix)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.BiDi.Protocol qualified as BiDiP
import Utils (txt)


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

-- | Whether to find the unique element (error if multiple match) or just the first.
data SingletonCardinality = Unique | First deriving (Show, Eq)

-- | Options for singleton locate functions ('locateHttp', 'locateFromElementHttp').
data BiDiLocateOpts = MkBiDiLocateOpts
  {
    singletonCardinality :: SingletonCardinality,
    mkDefaultLoc :: Text -> Locator,
    locateTracing :: LocateTracing
  }

data LocOpts = MkLocOpts
  { 
    singletonCardinality :: SingletonCardinality
  }

-- | Actions for singleton locate functions ('locateHttp', 'locateFirstHttp', 'locateFromElementHttp').
data LocateActions m = MkLocateActions
  { 
    throw :: forall a. HasCallStack => PreLocateException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    locateNodes :: BiDiP.LocateNodes -> m BiDiP.LocateNodesResult
  }
data LocParams m = MkLocParams
  { 
    throw :: forall a. HasCallStack => PreLocateException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    locateNodes :: BiDiP.LocateNodes -> m BiDiP.LocateNodesResult,
    defaultLoc :: Text -> Locator,
    trace :: WDTrace -> m (),
    locOpts :: LocOpts
  }

-- | Build a 'LocParams m' from 'LocateActions m', writing traces to an 'IORef'.
-- Using IORef instead of WriterT ensures trace entries are preserved even when
-- exceptions are thrown (WriterT state is discarded on exception).
extendActions :: (MonadIO m) => IORef [WDTrace] -> BiDiLocateOpts -> LocateActions m -> LocParams m
extendActions logsRef MkBiDiLocateOpts{..} MkLocateActions{..} = MkLocParams
  {
  -- throw / catch
    throw 
  , catch 

  -- webdriver functions
  , locateNodes

  -- other actions
  , defaultLoc = mkDefaultLoc
  , trace = \traceEntry ->
      case locateTracing of
        LocateTracing -> liftIO $ modifyIORef' logsRef (traceEntry :)
        NoLocateTracing -> pure ()

  -- options
  , locOpts = MkLocOpts {..}
  }

data WDBiDITrace = Prepared {
  loc :: Locator,
  reducedLoc :: CompoundLocator BiDiP.LocateNodes
} |
 PrepareFailed {
  loc :: Locator,
  error :: LI.InvalidLocator
} | 
 LeafLocate {
  selector :: BiDiP.LocateNodes,
  cardinality :: LeafCardinality,
  found :: [BiDiP.NodeRemoteValue]
  } 
 deriving (Show, Eq)

{-
-- | Locate a unique or first-matching element from the document root.
locateBiDi :: forall m. (MonadIO m) => LocateActions m -> BiDiLocateOpts -> Locator -> m (LocateResult BiDiP.NodeRemoteValue WDBiDITrace)
locateBiDi actions opts = runBiDiAction actions opts Nothing httpLocateSingleton

-- | Locate all matching elements from the document root.
locateAllBiDI :: forall m. (MonadIO m) => LocateActions m -> BiDiLocateOpts -> Locator -> m LocateResult
locateAllBiDI actions opts = runBiDiAction actions opts Nothing httpLocateAll

-- | Locate a unique or first-matching element rooted at a given element.
locateFromElementBiDi :: forall m. (MonadIO m) => LocateActions m -> BiDiLocateOpts -> ElementId -> Locator -> m LocateResult
locateFromElementBiDi actions opts rootId = runBiDiAction actions opts (Just rootId) httpLocateSingleton

-- | Locate all matching elements rooted at a given element.
locateAllFromElementBiDi :: forall m. (MonadIO m) => LocateActions m -> BiDiLocateOpts -> ElementId -> Locator -> m LocateResult
locateAllFromElementBiDi actions opts rootId = runBiDiAction actions opts (Just rootId) httpLocateAll

-- | Common implementation for all public HTTP locate functions.
runBiDiAction ::
  forall m.
  (MonadIO m) =>
  LocateActions m ->
  BiDiLocateOpts ->
  -- | root element
  (LocParams m -> CompoundLocator BiDiP.LocateNodes -> m BiDiP.LocateNodesResult) ->
  Locator ->
  m LocateResult
runBiDiAction actions opts mRootId locateAction loc = do
  logsRef <- liftIO $ newIORef []
  let locParams = setBaseElement mRootId $ extendActions logsRef opts actions
  rslt <- prepareRun locParams (locateAction locParams) loc
  logs <- liftIO $ P.reverse <$> readIORef logsRef
  pure $ case opts.locateTracing of
    LocateTracing -> LocateWithTrace rslt logs
    NoLocateTracing -> Locate rslt

setBaseElement :: Maybe ElementId -> LocParams m -> LocParams m
setBaseElement mRootId act@MkLocParams{..} = 
  maybe act (\rootId -> MkLocParams {
  findElement = findElementFromElement rootId,
  findElements = findElementsFromElement rootId,
  ..
}) mRootId


prepareRun :: forall m. Monad m =>
      LocParams m 
     -> (CompoundLocator HttpLoc -> m [ElementId]) 
     -> Locator 
     -> m (Either LocateException [ElementId])
prepareRun MkLocParams{trace, defaultLoc, catch} locateActn locator =
   case preparedLoc of
     Left err -> do
       trace (PrepareFailed locator err)
       pure $ Left (InvalidLocator err)
     Right reduced -> do
       trace (Prepared locator reduced)
       completeLocException locator . runLoc $ reduced
  where 
    preparedLoc :: Either LI.InvalidLocator (CompoundLocator HttpLoc)
    preparedLoc = LI.transform defaultLoc locator

    runLoc :: CompoundLocator HttpLoc -> m (Either PreLocateException [ElementId])
    runLoc loc =
      catch  -- catch PreLocateException thrown via 'throw' (e.g. AmbiguousLocator', ElementNotFound')
        (catch -- catch WebDriverException from underlying HTTP calls
          (Right <$> locateActn loc) 
          (pure . Left . DriverException')
        )
        (pure . Left)

jsFilterDisplayed ::
  forall m.
  (Monad m) =>
  LocParams m ->
  [ElementId] ->
  m [ElementId]
jsFilterDisplayed MkLocParams{throw, catch, trace, executeScript} elms = do
  bools <- catch
    (toBools <$> executeScript MkScript {script = displayedJS, args = [toJSON elms]})
    (throw . DriverException')
  let filtered = fmap fst . P.filter snd $ P.zip elms bools
  trace $ 
    JSDisplayedCheck { 
      beforeCheck = elms, 
      afterCheck = filtered
      }
  pure filtered
  where
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
                [x] -> pure [x]
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
chkRefilterSingleton actions elmIds =
  chkSingleton True elmIds
  where
    chkSingleton recheckAmbiguous =
      \case
        [] -> pure []
        [x] -> pure [x]
        xs ->
          if recheckAmbiguous
            then jsFilterDisplayed actions xs >>= chkSingleton False
            else pure xs

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
  fmap LST.nub $
    case loc of
      LI.Leaf cl ->
        locateLeaf actions rolesSecondPass leafCardinality cl
      LI.ContainsI {container, contained} -> do
        containers <- locate FindAll rolesSecondPass container
        locateContained containers contained
      LI.AllI {elms = locs} -> do
        let (l :| ls) = locs
            step acc loc' =
              if P.null acc
                then pure []
                else fmap (LST.intersect acc) (locate FindAll rolesSecondPass loc')
        initial <- locate FindAll rolesSecondPass l
        foldM step initial ls
      LI.AnyI {elms = locs} ->
        fmap join $
          traverse (locate FindAll rolesSecondPass) (toList locs)
      LI.PostFilterI {} -> postfilterNotImplemented
  where
    locate = locateElmsUnchecked actions

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
httpLocateSingleton prms@MkLocParams{throw, locOpts = opts}  loc = do
  case loc of
    LI.Leaf ll -> do
      lr <- locateLeaf prms secondPassOnInitial FindAll ll
      filtered <- chkElmsSingleton lr
      case filtered of
        [] ->
          if opts.extendedRoleLocation == ExtLocateSingletonMiss && isRole
            then do
              missRetryRslt <- locateLeaf prms DoRoleJSSecondPass FindAll ll
              retryChked <- chkElmsSingleton missRetryRslt
              case retryChked of
                [] -> notFoundErr
                [x] -> pure [x]
                (x : xs) ->
                  if isUnique
                    then throwAmbiguous xs
                    else pure [x]
            else notFoundErr
        [x] -> pure [x]
        elms@(x : _xs) ->
          if isUnique
            then throwAmbiguous elms
            else pure [x]
    LI.PostFilterI {} ->
      postfilterNotImplemented
    _ ->
      locateElmsUnchecked prms FindAll secondPassOnInitial loc
  where

      notFoundErr :: m [ElementId]
      notFoundErr = throw (ElementNotFound' "No element found matching locator.")

      throwAmbiguous elms = throw (AmbiguousLocator' ("Multiple elements found matching locator: " <> txt elms))
      isUnique = opts.singletonCardinality == Unique
      isRole = case loc of
        LI.Leaf RoleHttp {} -> True
        _ -> False
      secondPassOnInitial = case opts.extendedRoleLocation of
        ExtLocateNever -> NoRoleJSSecondPass
        ExtLocateSingletonMiss -> NoRoleJSSecondPass
        ExtLocateAlways -> DoRoleJSSecondPass

      chkElmsSingleton elms =
        let 
          displayChk = opts.jsRecheckDisplayed
          cardinality = opts.singletonCardinality
          wantRecheck = 
             displayChk == DisplayedCheckAlways 
             || displayChk == DisplayedCheckDisambiguateUnique && cardinality == Unique
        in
        if wantRecheck
          then chkRefilterSingleton prms elms
          else pure elms

httpLocateAll ::
  forall m.
  (Monad m) =>
  LocParams m ->
  CompoundLocator HttpLoc ->
  m [ElementId]
httpLocateAll prms loc = do
  let secondPassOnInitial = case prms.locOpts.extendedRoleLocation of
        ExtLocateNever -> NoRoleJSSecondPass
        ExtLocateSingletonMiss -> NoRoleJSSecondPass
        ExtLocateAlways -> DoRoleJSSecondPass
  elms <- locateElmsUnchecked prms FindAll secondPassOnInitial loc
  if prms.locOpts.jsRecheckDisplayed == DisplayedCheckAlways
    then jsFilterDisplayed prms elms
    else pure elms

postfilterNotImplemented :: a
postfilterNotImplemented = error "PostFilter locators are not yet implemented in HTTP WebDriver"

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


displayedJS :: Text
displayedJS =
  "function isDisplayed(el) {\n\
  \  if (!el || !el.isConnected) return false;\n\
  \\n\
  \  const style = getComputedStyle(el);\n\
  \\n\
  \  if (style.display === \"none\") return false;\n\
  \  if (style.visibility === \"hidden\" || style.visibility === \"collapse\") return false;\n\
  \  if (parseFloat(style.opacity) === 0) return false;\n\
  \\n\
  \  if (el.tagName === \"INPUT\" && el.type === \"hidden\")\n\
  \    return false;\n\
  \\n\
  \  if (el.offsetWidth === 0 || el.offsetHeight === 0)\n\
  \    return false;\n\
  \\n\
  \  return true;\n\
  \}\n\
  \return Array.from(arguments[0]).map(isDisplayed);"

  -}
