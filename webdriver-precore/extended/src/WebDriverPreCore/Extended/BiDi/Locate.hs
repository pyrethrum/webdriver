module WebDriverPreCore.Extended.BiDi.Locate
  (
    SingletonCardinality (..),
    BiDiLocateOpts (..),
    LocateActions (..),
    WDBiDITrace (..),
    BaseLocateOpts (..),
    locatePrimative,
    locateBiDi,
    locateAllBiDi,
    locateFromElementBiDi,
    locateAllFromElementBiDi
  )
where

import Control.Exception (Exception)
import Control.Monad (filterM, foldM, join)
import Data.Bifunctor (first)
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text
import Data.Text qualified as T
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

import WebDriverPreCore.Extended.LocateCommon (
    LocateException (..),
    PreLocateException (..),
    LeafCardinality (..),
    addLocToException,
    runLoc
  )
import WebDriverPreCore.Extended.Locators.Internal (
    Locator,
    RoleLocator (..),
    CompoundLocator,
    BiDiLoc (..),
    MatchType (..),
    CaseSensitivity (..),
    roleLabelText
  )
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.BiDi.Protocol qualified as BiDiP
import WebDriverPreCore.Extended.BiDi.Base.Protocol (JSUInt, SerializationOptions, SharedReference (..))
import Utils (txt)
import Prelude as P hiding (log)


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
    mkDefaultLoc :: Text -> Locator
  } 


-- | Actions for singleton locate functions ('locateHttp', 'locateFirstHttp', 'locateFromElementHttp').
data LocateActions m = MkLocateActions
  { 
    throw :: forall a. HasCallStack => PreLocateException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    trace :: WDBiDITrace -> m (),
    locateNodes :: BiDiP.LocateNodes -> m BiDiP.LocateNodesResult,
    getElementText :: SharedReference -> m Text
  }

data LocParams m = MkLocParams
  { 
    throw :: forall a. HasCallStack => PreLocateException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    trace :: WDBiDITrace -> m (),
    locateNodes :: BiDiP.LocateNodes -> m BiDiP.LocateNodesResult,
    getElementText :: SharedReference -> m Text,
    context :: BiDiP.BrowsingContext,
    startNodes :: Maybe [SharedReference],
    defaultLoc :: Text -> Locator,
    singletonCardinality :: SingletonCardinality
  }


data BaseLocateOpts = MkBaseLocateOpts
  { 
    maxNodeCount :: Maybe JSUInt,
    serializationOptions :: Maybe SerializationOptions,
    startNodes :: Maybe [SharedReference]
  }
  deriving (Show, Eq)

data WDBiDITrace = Prepared {
  loc :: Locator,
  reducedLoc :: CompoundLocator BiDiLoc
} |
 PrepareFailed {
  loc :: Locator,
  error :: LI.InvalidLocator
} | 
 LeafLocate {
  selector :: BiDiP.Locator,
  cardinality :: LeafCardinality,
  found :: [BiDiP.NodeRemoteValue]
  } 
 deriving (Show, Eq)

  -- context :: BrowsingContext,
  --   locator :: Locator,

mkLocParams :: BiDiLocateOpts -> BiDiP.BrowsingContext -> LocateActions m -> LocParams m
mkLocParams MkBiDiLocateOpts{mkDefaultLoc = defaultLoc, singletonCardinality} context MkLocateActions{..} =
  MkLocParams
    { defaultLoc,
      singletonCardinality,
      context,
      startNodes = Nothing,
      ..
    }

locatePrimative :: Applicative m => LocateActions m -> BiDiP.BrowsingContext -> BaseLocateOpts -> BiDiP.Locator -> m (Either PreLocateException BiDiP.LocateNodesResult)
locatePrimative MkLocateActions {catch, locateNodes} context MkBaseLocateOpts {maxNodeCount, serializationOptions, startNodes} locator = 
   runLoc catch locateNodes BiDiP.MkLocateNodes  
         { context,
            locator,
            maxNodeCount,
            serializationOptions,
            startNodes
          }

-- | Locate a unique or first-matching element from the document root.
locateBiDi :: forall m. (Monad m) => LocateActions m -> BiDiLocateOpts -> BiDiP.BrowsingContext -> Locator -> m (Either LocateException BiDiP.NodeRemoteValue)
locateBiDi actions opts context = runBiDiAction actions opts context Nothing biDiLocateSingleton

-- | Locate all matching elements from the document root.
locateAllBiDi :: forall m. (Monad m) => LocateActions m -> BiDiLocateOpts -> BiDiP.BrowsingContext -> Locator -> m (Either LocateException [BiDiP.NodeRemoteValue])
locateAllBiDi actions opts context = runBiDiAction actions opts context Nothing biDiLocateAll

-- | Locate a unique or first-matching element rooted at a given element.
locateFromElementBiDi :: forall m. (Monad m) => LocateActions m -> BiDiLocateOpts -> BiDiP.BrowsingContext -> SharedReference -> Locator -> m (Either LocateException BiDiP.NodeRemoteValue)
locateFromElementBiDi actions opts context sharedRef = runBiDiAction actions opts context (Just sharedRef) biDiLocateSingleton

-- | Locate all matching elements rooted at a given element.
locateAllFromElementBiDi :: forall m. (Monad m) => LocateActions m -> BiDiLocateOpts -> BiDiP.BrowsingContext -> SharedReference -> Locator -> m (Either LocateException [BiDiP.NodeRemoteValue])
locateAllFromElementBiDi actions opts context sharedRef = runBiDiAction actions opts context (Just sharedRef) biDiLocateAll

-- | Common implementation for all public BiDi locate functions. This transforms
--   the user-facing 'Locator' into a 'CompoundLocator' of BiDi leaf locators and
--   runs it via 'locatePrimative', wrapping any driver / pre-locate exceptions.
runBiDiAction ::
  forall m r.
  (Monad m) =>
  LocateActions m ->
  BiDiLocateOpts ->
  BiDiP.BrowsingContext ->
  -- | start node (root element)
  Maybe SharedReference ->
  (LocParams m -> CompoundLocator BiDiLoc -> m r) ->
  Locator ->
  m (Either LocateException r)
runBiDiAction actions@MkLocateActions{catch} opts context mRootId locateAction loc = do
  let p = setBaseElement mRootId $ mkLocParams opts context actions
  case LI.transformBiDi p.defaultLoc loc of
    -- log failure if the transformation failed
    Left err -> do
      p.trace (PrepareFailed loc err)
      pure $ Left (InvalidLocator err)
    -- run the locator if the transformation succeeded
    Right compoundLoc -> do
      p.trace (Prepared loc compoundLoc)
      first (addLocToException loc) <$> runLoc catch (locateAction p) compoundLoc

-- | Set the start node (root element) used by the locate.
setBaseElement :: Maybe SharedReference -> LocParams m -> LocParams m
setBaseElement mRootId act@MkLocParams{..} =
  maybe act (\rootId -> MkLocParams {startNodes = Just [rootId], ..}) mRootId

-- | Recover the underlying 'LocateActions' from the extended params.
mkLocateActions :: LocParams m -> LocateActions m
mkLocateActions MkLocParams{throw, catch, trace, locateNodes, getElementText} = MkLocateActions{throw, catch, trace, locateNodes, getElementText}

-- | Locate a single node. Cardinality checks are deferred until the query
--   completes; for 'First' the driver is asked for at most one node, while for
--   'Unique' all matches are fetched so ambiguity can be reported.
biDiLocateSingleton ::
  forall m.
  (Monad m) =>
  LocParams m ->
  CompoundLocator BiDiLoc ->
  m BiDiP.NodeRemoteValue
biDiLocateSingleton prms@MkLocParams{throw, singletonCardinality, startNodes} loc = do
  let baseOpts =
        MkBaseLocateOpts
          { maxNodeCount = case (singletonCardinality, loc) of
              (First, LI.LeafC leafLoc) | not (needsTextFilter leafLoc) -> Just 1
              _ -> Nothing,
            serializationOptions = Nothing,
            startNodes
          }
  allNodes <- locateElmsBiDi prms baseOpts loc
  case allNodes of
    [] -> throw elementNotFoundError
    (x : xs) ->
      case singletonCardinality of
        Unique
          | not (P.null xs) -> throw (AmbiguousLocator' ("Multiple elements found matching locator: " <> txt (x : xs)))
          | otherwise -> pure x
        First -> pure x

-- | Locate all matching nodes.
biDiLocateAll ::
  forall m.
  (Monad m) =>
  LocParams m ->
  CompoundLocator BiDiLoc ->
  m [BiDiP.NodeRemoteValue]
biDiLocateAll prms@MkLocParams{startNodes} loc = do
  let baseOpts =
        MkBaseLocateOpts
          { maxNodeCount = Nothing,
            serializationOptions = Nothing,
            startNodes
          }
  locateElmsBiDi prms baseOpts loc

-- | Locate all matching nodes for a compound locator, with no cardinality filtering.
locateElmsBiDi ::
  forall m.
  (Monad m) =>
  LocParams m ->
  BaseLocateOpts ->
  CompoundLocator BiDiLoc ->
  m [BiDiP.NodeRemoteValue]
locateElmsBiDi prms baseOpts loc =
  fmap LST.nub $
    case loc of
      LI.LeafC cl -> locateLeafBiDi prms baseOpts cl
      LI.ContainsC {container, contained} -> do
        containers <- locateElmsBiDi prms baseOpts container
        locateContained containers contained
      LI.AllC {elms = locs} -> do
        let (l :| ls) = locs
            step acc loc' =
              if P.null acc
                then pure []
                else fmap (LST.intersect acc) (locateElmsBiDi prms baseOpts loc')
        initial <- locateElmsBiDi prms baseOpts l
        foldM step initial ls
      LI.AnyC {elms = locs} ->
        fmap join $
          traverse (locateElmsBiDi prms baseOpts) (toList locs)
  where
    locateContained :: [BiDiP.NodeRemoteValue] -> CompoundLocator BiDiLoc -> m [BiDiP.NodeRemoteValue]
    locateContained containerIds subLoc = do
      containedResults <-
        traverse
          ( \containerId ->
              case nodeToSharedRef containerId of
                Just ref -> locateElmsBiDi prms (setStartNodes baseOpts (Just [ref])) subLoc
                Nothing -> pure []
          )
          containerIds
      pure $ join containedResults

-- | Run a single BiDi locateNodes command for a leaf locator.
locateLeafBiDi ::
  forall m.
  (Monad m) =>
  LocParams m ->
  BaseLocateOpts ->
  BiDiLoc ->
  m [BiDiP.NodeRemoteValue]
locateLeafBiDi prms baseOpts loc =
  case loc of
    InnerTextBiDi {value, matchType, caseSensitivity, maxDepth} ->
      locateInnerTextBiDi prms baseOpts value matchType caseSensitivity maxDepth
    _ -> runLeafLocator prms baseOpts (toBiDiLocator loc)

-- | Run a single BiDi locateNodes command for a directly-supported BiDi locator.
runLeafLocator ::
  forall m.
  (Monad m) =>
  LocParams m ->
  BaseLocateOpts ->
  BiDiP.Locator ->
  m [BiDiP.NodeRemoteValue]
runLeafLocator prms@MkLocParams{throw, trace, context} baseOpts bidiLoc = do
  rslt <- locatePrimative (mkLocateActions prms) context baseOpts bidiLoc
  case rslt of
    Left e -> throw e
    Right (BiDiP.MkLocateNodesResult found) -> do
      trace $ LeafLocate bidiLoc FindAll found
      pure found

-- | Locate nodes matching an InnerText leaf locator. BiDi natively supports
--   Full and Partial matching; 'Starts' and 'Wildcard' are implemented with a
--   Partial pre-filter followed by a text-based filter over the results.
locateInnerTextBiDi ::
  forall m.
  (Monad m) =>
  LocParams m ->
  BaseLocateOpts ->
  Text ->
  MatchType ->
  CaseSensitivity ->
  Maybe Word8 ->
  m [BiDiP.NodeRemoteValue]
locateInnerTextBiDi prms baseOpts value matchType caseSensitivity maxDepth =
  case matchType of
    Full -> runLeafLocator prms baseOpts $ innerTextLocator caseSensitivity (Just BiDiP.Full) value maxDepth
    Partial -> runLeafLocator prms baseOpts $ innerTextLocator caseSensitivity (Just BiDiP.Partial) value maxDepth
    Starts -> do
      found <- runLeafLocator prms baseOpts $ innerTextLocator caseSensitivity (Just BiDiP.Partial) value maxDepth
      filterByText prms (startsMatch caseSensitivity value) found
    Wildcard -> locateWildcardInnerTextBiDi prms baseOpts caseSensitivity value maxDepth

-- | Locate nodes for a Wildcard InnerText locator. A single fragment resolves
--   to Partial; otherwise the longest fragment is used as a Partial pre-filter
--   and the results are filtered against the full glob pattern. A bare "*"
--   selects every element that has non-empty text content.
locateWildcardInnerTextBiDi ::
  forall m.
  (Monad m) =>
  LocParams m ->
  BaseLocateOpts ->
  CaseSensitivity ->
  Text ->
  Maybe Word8 ->
  m [BiDiP.NodeRemoteValue]
locateWildcardInnerTextBiDi prms baseOpts cs value maxDepth =
  case wildcardFragments value of
    -- "*" / "**" etc: every element that has inner text
    [] -> runLeafLocator prms baseOpts $ BiDiP.XPath {value = allTextXPath maxDepth}
    -- "*Blahh", "Blahh", "Blahh*", "*Blahh*": resolve to Partial
    [single] -> runLeafLocator prms baseOpts $ innerTextLocator cs (Just BiDiP.Partial) single maxDepth
    -- multiple fragments: Partial on the longest, then a text-based glob filter
    _ -> do
      let fragments = wildcardFragments value
          longest = longestFragment fragments
      found <- runLeafLocator prms baseOpts $ innerTextLocator cs (Just BiDiP.Partial) longest maxDepth
      filterByText prms (wildcardMatch cs value) found

-- | Whether locating this leaf requires a text-based post-filter, so the
--   pre-filter query must return every candidate rather than just the first.
needsTextFilter :: BiDiLoc -> Bool
needsTextFilter = \case
  InnerTextBiDi {matchType} -> matchType == Starts || matchType == Wildcard
  _ -> False

-- | Filter located nodes by reading their text and keeping those matching a predicate.
filterByText ::
  forall m.
  (Monad m) =>
  LocParams m ->
  (Text -> Bool) ->
  [BiDiP.NodeRemoteValue] ->
  m [BiDiP.NodeRemoteValue]
filterByText MkLocParams{getElementText} predicate = 
  filterM predicate'
  where
    predicate' :: BiDiP.NodeRemoteValue -> m Bool
    predicate' = maybe (pure False) (fmap predicate . getElementText) . nodeToSharedRef

-- | Build a BiDi InnerText locator with the given matching configuration.
innerTextLocator :: CaseSensitivity -> Maybe BiDiP.MatchType -> Text -> Maybe Word8 -> BiDiP.Locator
innerTextLocator cs mt val mDepth =
  BiDiP.InnerText
    { value = val,
      ignoreCase = case cs of
        CaseSensitive -> Just False
        CaseInsensitive -> Just True,
      matchType = mt,
      maxDepth = fmap fromIntegral mDepth
    }

-- | XPath selecting every element that has non-empty text content.
allTextXPath :: Maybe Word8 -> Text
allTextXPath = \case
  Nothing -> ".//*[normalize-space(.) != '']"
  Just d -> ".//*[normalize-space(.) != '' and count(ancestor::*) <= " <> txt d <> "]"

-- | The non-empty fragments of a wildcard pattern split on '*'.
wildcardFragments :: Text -> [Text]
wildcardFragments = P.filter (not . T.null) . T.splitOn "*"

-- | The longest fragment of a wildcard pattern (used for the Partial pre-filter).
longestFragment :: [Text] -> Text
longestFragment = P.foldr1 (\a b -> if T.length a >= T.length b then a else b)

-- | Apply the locator's case sensitivity to a value for matching.
normText :: CaseSensitivity -> Text -> Text
normText = \case
  CaseSensitive -> id
  CaseInsensitive -> T.toLower

-- | Does @txt@ start with @prefix@ (honouring case sensitivity)?
startsMatch :: CaseSensitivity -> Text -> Text -> Bool
startsMatch cs prefix text = normText cs prefix `T.isPrefixOf` normText cs text

-- | Does @text@ match the wildcard pattern @pat@ ('*' matches any run of characters)?
wildcardMatch :: CaseSensitivity -> Text -> Text -> Bool
wildcardMatch cs pat text = 
  globMatch (normText cs pat) (normText cs text)
  where
    globMatch :: Text -> Text -> Bool
    globMatch pat' txt' =
      case T.splitOn "*" pat' of
        [] -> True
        [single] -> single == txt'
        leading : middle ->
          T.isPrefixOf leading txt'
            && match middle (T.drop (T.length leading) txt')

    match :: [Text] -> Text -> Bool
    match fragments remaining =
      case fragments of
        [] -> True
        [lastFrag] -> T.isSuffixOf lastFrag remaining
        frag : rest
          | T.null frag -> match rest remaining            -- "**" collapses to "*"
          | otherwise ->
              case T.breakOn frag remaining of
                (_, after)
                  | T.null after -> False
                  | otherwise -> match rest (T.drop (T.length frag) after)

-- | Convert an extended BiDi leaf locator to a BiDi protocol locator.
toBiDiLocator :: BiDiLoc -> BiDiP.Locator
toBiDiLocator = \case
  CSSBiDi {value} -> BiDiP.CSS {value}
  XPathBiDi {value} -> BiDiP.XPath {value}
  ContextBiDi {context} -> BiDiP.Context {context}
  RoleBiDi {roleSpec} -> roleToAccessibility roleSpec
  -- 'InnerTextBiDi' is handled by 'locateInnerTextBiDi'; this case is only a
  -- total fallback so 'toBiDiLocator' stays exhaustive.
  InnerTextBiDi {value, caseSensitivity, maxDepth} ->
    innerTextLocator caseSensitivity (Just BiDiP.Partial) value maxDepth

-- | Map a role locator onto a BiDi accessibility locator.
roleToAccessibility :: RoleLocator -> BiDiP.Locator
roleToAccessibility = \case
  RoleFull {role, name} ->
    BiDiP.Accessibility {name = Just name, role = Just (roleLabelText role)}
  RoleType {role} ->
    BiDiP.Accessibility {name = Nothing, role = Just (roleLabelText role)}
  RoleName {name} ->
    BiDiP.Accessibility {name = Just name, role = Nothing}

-- | Update the start nodes used by the base locate options.
setStartNodes :: BaseLocateOpts -> Maybe [SharedReference] -> BaseLocateOpts
setStartNodes MkBaseLocateOpts{..} startNodes' = MkBaseLocateOpts {startNodes = startNodes', ..}

-- | Convert a located node into a 'SharedReference' for use as a start node,
--   returning 'Nothing' if the node has no shared id.
nodeToSharedRef :: BiDiP.NodeRemoteValue -> Maybe SharedReference
nodeToSharedRef (BiDiP.MkNodeRemoteValue {sharedId, handle}) =
  MkSharedReference <$> sharedId <*> pure handle <*> pure Nothing

elementNotFoundError :: PreLocateException
elementNotFoundError = ElementNotFound' "No element found matching locator."
