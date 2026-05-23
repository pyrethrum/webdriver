module WebDriverPreCore.Extended.Locate
  (
    DisplayedCheck(..),
    ExtendedRoleLocateSingleton(..),
    HttpLocateOpts (..),
    LocateActions (..),
    LocateException (..),
    LocateResult (..),
    SingletonCardinality (..),
    WDTrace (..),
    LocateTracing (..),
    locateHttp,
    locateFromElementHttp,
    locateAllHttp,
    locateAllFromElementHttp
  )
where

import Control.Exception (Exception)
import Control.Monad (foldM, join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Strict (WriterT (..), runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Aeson as A (Result (..), Value (Bool), fromJSON, toJSON)
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
import Prelude as P hiding (log)
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
instance Exception PreLocateException

data WDTrace = Prepared {
  loc :: Locator,
  reducedLoc :: ReducedHttpLoc
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
 deriving (Show, Eq)

data LocateTracing = LocateTracing | NoLocateTracing deriving (Show, Eq)

data LocateResult = MkLocateResult
  { result :: Either LocateException [ElementId]
  , logFields :: [WDTrace]
  } deriving (Show, Eq)

-- | Whether to find the unique element (error if multiple match) or just the first.
data SingletonCardinality = Unique | First deriving (Show, Eq)

data LeafCardinality = FindFirst | FindAll deriving (Show, Eq)

data DisplayedCheck = DisplayedCheckNever | DisplayedCheckDisambiguateUnique | DisplayedCheckAlways deriving (Show, Eq)

data ExtendedRoleLocateSingleton = ExtLocateNever | ExtLocateSingletonMiss | ExtLocateAlways deriving (Show, Eq)

data ExtendedRoleLocateAll = ExtLocateAllNever | ExtLocateAllAlways deriving (Show, Eq)

data RoleJSSecondPass = DoRoleJSSecondPass | NoRoleJSSecondPass deriving (Show, Eq)

-- | Options for singleton locate functions ('locateHttp', 'locateFromElementHttp').
data HttpLocateOpts = MkHttpLocateOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateSingleton,
    singletonCardinality :: SingletonCardinality,
    mkDefaultLoc :: Text -> Locator,
    locateTracing :: LocateTracing
  }

data LocOpts = MkLocOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocateSingleton,
    singletonCardinality :: SingletonCardinality
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
  { 
    throw :: forall a. HasCallStack => PreLocateException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
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
    findElement :: Selector -> m ElementId,
    findElementFromElement :: ElementId -> Selector -> m ElementId,
    findElements :: Selector -> m [ElementId],
    findElementsFromElement :: ElementId -> Selector -> m [ElementId],
    executeScript :: Script -> m Value,
    getElementAttribute :: ElementId -> Text -> m (Maybe Text),
    getElementText :: ElementId -> m Text,
    defaultLoc :: Text -> Locator,
    trace :: WDTrace -> m (),
    locOpts :: LocOpts
  }

-- | Lift a 'LocateActions m' into 'LocateActions (WriterT [WDTrace] m)'.
extendActions :: (Monad m) => HttpLocateOpts -> LocateActions m -> LocParams (WriterT [WDTrace] m)
extendActions MkHttpLocateOpts{..} MkLocateActions{..} = MkLocParams
  { 
  -- throw / catch
    throw = lift . throw
  , catch = \ma handler -> WriterT $ catch (runWriterT ma) (runWriterT . handler)

  -- webdriver functions
  , findElement = lift . findElement
  , findElementFromElement = \eid -> lift . findElementFromElement eid
  , findElements = lift . findElements
  , findElementsFromElement = \eid -> lift . findElementsFromElement eid
  , executeScript = lift . executeScript
  , getElementAttribute = \eid -> lift . getElementAttribute eid 
  , getElementText = lift . getElementText

  -- other actions
  , defaultLoc = mkDefaultLoc
  , trace = \logEntry -> 
      case locateTracing of
        LocateTracing -> tell [logEntry]
        NoLocateTracing -> pure ()

  -- options
  , locOpts = MkLocOpts {..}
  }


-- | Locate a unique or first-matching element from the document root.
locateHttp :: forall m. (Monad m) => LocateActions m -> HttpLocateOpts -> Locator -> m LocateResult
locateHttp actions opts = runHttpAction actions opts Nothing httpLocateSingleton

-- | Locate all matching elements from the document root.
locateAllHttp :: forall m. (Monad m) => LocateActions m -> HttpLocateOpts -> Locator -> m LocateResult
locateAllHttp actions opts = runHttpAction actions opts Nothing httpLocateAll

-- | Locate a unique or first-matching element rooted at a given element.
locateFromElementHttp :: forall m. (Monad m) => LocateActions m -> HttpLocateOpts -> ElementId -> Locator -> m LocateResult
locateFromElementHttp actions opts rootId = runHttpAction actions opts (Just rootId) httpLocateSingleton

-- | Locate all matching elements rooted at a given element.
locateAllFromElementHttp :: forall m. (Monad m) => LocateActions m -> HttpLocateOpts -> ElementId -> Locator -> m LocateResult
locateAllFromElementHttp actions opts rootId = runHttpAction actions opts (Just rootId) httpLocateAll

-- | Common implementation for all public HTTP locate functions.
runHttpAction ::
  forall m.
  (Monad m) =>
  LocateActions m ->
  HttpLocateOpts ->
  -- | root element
  Maybe ElementId ->
  (forall m'. Monad m' => LocParams m' -> ReducedHttpLoc -> m' [ElementId]) ->
  Locator ->
  m LocateResult
runHttpAction actions opts mRootId locateAction loc = do
  let locParams = setBaseElement mRootId  $ extendActions opts actions
  (rslt, logs) <- runWriterT $ prepareRun locParams (locateAction locParams) loc
  pure $ MkLocateResult rslt logs

setBaseElement :: Maybe ElementId -> LocParams m -> LocParams m
setBaseElement mRootId act@MkLocParams{..} = 
  maybe act (\rootId -> MkLocParams {
  findElement = findElementFromElement rootId,
  findElements = findElementsFromElement rootId,
  ..
}) mRootId

prepareRun :: forall m. Monad m =>
      LocParams m 
     -> (ReducedHttpLoc -> m [ElementId]) 
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
    preparedLoc :: Either LI.InvalidLocator ReducedHttpLoc
    preparedLoc = prepareSimplify defaultLoc HTTP locator >>= toHttpLocator

    runLoc :: ReducedHttpLoc -> m (Either PreLocateException [ElementId])
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
  LeafLoc ->
  m [ElementId]
locateLeaf prms rolesSecondPass lc loc = do
  let 
    sel = toSelector loc
    trace' :: [ElementId] -> m ()
    trace' ids = prms.trace $ LeafLocate sel lc ids
    simpleLocate :: m [ElementId]
    simpleLocate =
          ( if (lc == FindFirst) then do
             elm <- fmap LST.singleton $ prms.findElement sel
             trace' elm
             pure elm
            else do
              elms <- prms.findElements sel
              trace' elms
              pure elms
          )
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
                [] -> roleToXPathHttpSecondPass prms lc role
                [x] -> pure [x]
                x:_ -> pure [x]
              else
               (sr <>) <$> roleToXPathHttpSecondPass prms lc role
      InnerText {} -> simpleLocate

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
          recheckAmbiguous
            & bool
              (pure xs)
              (jsFilterDisplayed actions xs >>= chkSingleton False)

-- single shot base locate (all cardinality)
locateElmsUnchecked ::
  forall m.
  (Monad m) =>
  LocParams m ->
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
                  else fmap (LST.intersect acc) (locate FindAll rolesSecondPass loc')
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
      containedResults <- traverse (\_ -> locate FindAll rolesSecondPass subLoc) containerIds
      pure $ join containedResults


httpLocateSingleton ::
  forall m.
  (Monad m) =>
  LocParams m ->
  ReducedHttpLoc ->
  m [ElementId]
httpLocateSingleton prms@MkLocParams{throw, locOpts = opts}  loc = do
  case loc of
    LeafHttp ll -> do
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
    PostFilterHttpLoc {} ->
      postfilterNotImplemented
    CombintorHttp {} ->
      locateElmsUnchecked prms FindAll secondPassOnInitial loc
  where

      notFoundErr :: m [ElementId]
      notFoundErr = throw (ElementNotFound' "No element found matching locator.")

      throwAmbiguous elms = throw (AmbiguousLocator' ("Multiple elements found matching locator: " <> txt elms))
      isUnique = opts.singletonCardinality == Unique
      isRole = case loc of
        LeafHttp (RL.BiDiNative (Role {})) -> True
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
  ReducedHttpLoc ->
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

toSelector :: LeafLoc -> Selector
toSelector = \case
  RL.CSS {value} -> HTTPP.CSS value
  RL.XPath {value} -> HTTPP.XPath value
  -- shim BiDiNative locators
  BiDiNative sl -> case sl of
    Role {role} -> HTTPP.XPath $ roleToXPath role
    InnerText {value, matchType, caseSesnsitivity, maxDepth} -> HTTPP.XPath $ innerTextToXPath value caseSesnsitivity matchType maxDepth

_locateBiDi :: a
_locateBiDi = undefined

notNull :: [a] -> Bool
notNull = not . P.null

roleToXPathHttpSecondPass ::
  forall m.
  (Monad m) =>
  LocParams m ->
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
  LocParams m ->
  LeafCardinality ->
  RoleLocator ->
  m [ElementId]
roleToXPathHttpLabeledBy prms lc roleLoc =
  case roleLoc of
    RoleType {} -> pure []
    _ -> do
      candidates <-
        -- all elms that match role and have an aria-labelledby attribute
        prms.findElements (HTTPP.XPath $ "//*" <> roleXPath roleLoc <> "[@aria-labelledby]")
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
          elms <- prms.findElements . HTTPP.XPath $ "//*[@id='" <> idRef <> "']"
          case elms of
            [] -> pure Nothing
            (e : _) -> Just <$> prms.getElementText e

roleToXPathFor ::
  forall m.
  (Monad m) =>
  LocParams m ->
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
  \return Array.from(arguments[0]).map(isDisplayed);"
