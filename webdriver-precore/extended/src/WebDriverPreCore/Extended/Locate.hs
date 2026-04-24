module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    Cardinality (..),
    HttpLocateOpts (..),
    -- locateHttp,
    -- displayedJS,
    -- isDisplayedHttp,
  )
where

import Control.Exception (Exception, SomeException)
import Control.Monad (filterM, foldM, (>=>))
import Data.Aeson as A (Result (..), Value (Bool), fromJSON, toJSON)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text
import Data.Text qualified as T
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

data DisplayedCheck = DisplayedCheckNever | DisplayedCheckDisambiguateUnique | DisplayedCheckAlways deriving (Show, Eq)

data ExtendedRoleLocate = ExtLocateNever | ExtLocateSingletonMiss | ExtLocateAlways deriving (Show, Eq)

data HttpLocateOpts = MkHttpLocateOpts
  { jsRecheckDisplayed :: DisplayedCheck,
    extendedRoleLocation :: ExtendedRoleLocate,
    cardinality :: Cardinality
  }
  deriving (Show, Eq)

data LocateResult
  = MkLocateResult
  { source :: LeafLoc,
    elmIds :: [ElementId]
  }
  deriving (Show, Eq)

-- \| ContainsResult
--     { found :: [LocateResult]
--     }
-- \| AndResult
--     { found :: [LocateResult]
--     }
-- \| OrResult
--     { found :: [LocateResult]
--     }
-- \| PostFilterResult
--     { predicate :: LI.Predicate,
--       found :: [LocateResult]
--     }
-- deriving (Show, Eq)

-- foldResult :: (b -> LocateResult -> b) -> b -> LocateResult -> b
-- foldResult f z lr = runIdentity $ traverseResult (\acc x -> Identity (f acc x)) z lr

-- traverseResult :: (Monad m) => (b -> LocateResult -> m b) -> b -> LocateResult -> m b
-- traverseResult f z lr = do
--   z' <- f z lr
--   foldM (traverseResult f) z' children
--   where
--     children = case lr of
--       LeafResult _ -> []
--       ContainsResult {found} -> found
--       AndResult {found} -> found
--       OrResult {found} -> found
--       PostFilterResult {found} -> found

-- TODO - may need to reintroduce locateDirectives param
-- extractIds :: LocateDirectives -> LocateResult -> Either LocateResult [ElementId]
-- extractIds _ lr = recurse lr
-- extractIds :: LocateResult -> [ElementId]
-- extractIds lr = recurse lr
--   where
--     recurse :: LocateResult -> [ElementId]
--     recurse = \case
--       LeafResult (MkLeafResult {elms}) -> elms
--       NodeResult {found} -> recurseConcatAll found
--     recurseConcatAll :: [LocateResult] -> [ElementId]
--     recurseConcatAll = (>>= recurse)

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
  -- | locate opts
  HttpLocateOpts ->
  -- | session
  Session ->
  -- | locator
  Locator ->
  m (Either LocateException ElementId)
locateHttp throw catch runner defLoc MkHttpLocateOpts {jsRecheckDisplayed, extendedRoleLocation, cardinality} ses locator =
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

    getAttribute :: ElementId -> Text -> m (Maybe Text)
    getAttribute eid name = getElementAttribute runner ses eid name

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
    locateLeaf :: Maybe ElementId -> Cardinality -> Bool -> LeafLoc -> m LocateResult
    locateLeaf mRoot cardinality' deepRoles loc = do
      let findFirst = cardinality' == First
      firstPass <-
        ( if findFirst
            then fmap pure . findElm mRoot
            else locateAll mRoot
        )
          (toSelector loc)
      let mkResult = MkLocateResult loc
          baseResult = mkResult firstPass
      case loc of
        RL.CSS {} -> baseResult
        RL.XPath {} -> baseResult
        RL.BiDiNative sl -> case sl of
          Role {role} -> do
            if not deepRoles
              then baseResult
              else do
                mResult <- roleToXPathHttpSecondPass locateAll getAttribute (getElementText runner ses) mRoot findFirst role
                case mResult of
                  Nothing -> throw ElementNotFound {description = "No element found matching role locator.", locator}
                  Just lr -> pure lr
          InnerText {value, matchType, caseSesnsitivity, maxDepth} -> do
            let sel = HTTPP.XPath $ innerTextToXPath value caseSesnsitivity matchType maxDepth
            baseResult <- undefined
            chkedRslt <- chkSingleton (jsRecheckDisplayed `P.elem` [DisambiguateUnique, Always]) baseResult
            either throw pure chkedRslt

    chkSingleton ::
      Bool -> -- recheck ambiguous
      LocateResult ->
      m SingletonCheckResult
    chkSingleton recheckAmbiguous lr =
      chkkSingleton' recheckAmbiguous lr.elmIds
      where
        chkkSingleton' recheckAmbiguous' elmIds =
          do
            let chkResult = baseSingletonChk elmIds
                pureResult = pure chkResult
            case chkResult of
              SingletonSuccess _ -> pureResult
              Missing _ -> pureResult
              Ambiguous _ ->
                if recheckAmbiguous'
                  then
                    jsFilterDisplayed elmIds
                      >>= chkkSingleton' False
                  else
                    pureResult

        baseSingletonChk :: [ElementId] -> SingletonCheckResult
        baseSingletonChk = \case
          [] -> Missing []
          [x] -> SingletonSuccess [x]
          xs -> Ambiguous xs

    -- single shot base locate
    locateUnchecked :: Maybe ElementId -> Bool -> ReducedHttpLoc -> m LocateResult
    locateUnchecked mRoot extendedRoleLocation =
      \case
        LeafHttp cl ->
          -- need to find all elms for combinator and later checks and retries
          locateLeaf mRoot False extendedRoleLocation cl
        CombintorHttp cb -> case cb of
          Contains {container, contained} -> do
            containers <- locate container
            locateContained containers contained
          All {elms = locs} -> do
            found <- andLocs locs
            pure $ AndResult found
          Any {elms = locs} -> undefined
        -- do
        -- results <- traverse locate locs
        -- pure OrResult {found = toList results}
        PostFilterHttpLoc {} -> postfilterNotImplemented
      where
        locate = locateUnchecked mRoot

    -- Short-circuiting fold for All: stops as soon as the running intersection
    -- of element IDs becomes empty, avoiding unnecessary locate calls.
    -- andLocs :: NonEmpty ReducedHttpLoc -> m [LocateResult]
    -- andLocs (l :| ls) = undefined
    -- do
    --   result0 <- locate l
    --   recurse (extractIds result0) [result0] ls
    --   where
    --   recurse :: [ElementId] -> [LocateResult] -> [ReducedHttpLoc] -> m [LocateResult]
    --   recurse rsltIds locRslts remainderLocs =
    --     case (rsltIds, locRslts, remainderLocs) of
    --       (_resultIds, acc, []) -> pure (P.reverse acc)
    --       (intersection, acc, (l' : ls'))
    --         | P.null intersection -> pure (P.reverse acc)
    --         | otherwise ->
    --             do
    --                 result <- locate l'
    --               recurse
    --               (LST.intersect intersection (extractIds result))
    --               (result : acc)
    --               ls'

    locateContained :: LocateResult -> ReducedHttpLoc -> m LocateResult
    locateContained containers subLoc = do
      -- for each container, locate contained with root of container element, and combine results
      let ids = containers.found >>= extractIds
      containedResults <- traverse (\rootId -> locateUnchecked (Just rootId) subLoc) ids
      pure $ ContainsResult containedResults

    httpLocate :: ReducedHttpLoc -> m LocateResult
    httpLocate = \case
      LeafHttp l -> do
        lr <- locateLeaf Nothing cardinality (extendedRoleLocation == ExtLocateAlways) l
        elms <- if jsRecheckDisplayed == DisplayedCheckAlways
          then
            (.elms) <$> chkSingleton True lr
          else
            pure lr.elmIds

        case elms of
          [] -> case cardinality of
            Unique -> 
              if extendedRoleLocation == ExtLocateSingletonMiss
                then HERE 
                else throw ElementNotFound {description = "No element found matching locator.", locator}
            First -> throw ElementNotFound {description = "No elements found matching locator.", locator}
            Many -> pure lr

            throw ElementNotFound {description = "No elements found matching locator.", locator}
          [_] -> pure lr
          _ -> undefined
        chkedRslt <- chkSingleton (jsRecheckDisplayed `P.elem` [DisambiguateUnique, Always]) lr
        either throw pure chkedRslt
      PostFilterHttpLoc {} ->
        -- will neeed to postfilter &&& all
        postfilterNotImplemented
      loc@CombintorHttp {} -> httpLocate' Nothing loc

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
  m (Maybe LocateResult)
roleToXPathHttpSecondPass
  locAll
  getAttr
  getText
  rootElm
  findFirst
  roleLoc =
    case roleLoc of
      RoleType {} -> pure Nothing
      _ -> do
        labelledByElms <- roleToXPathHttpLabeledBy locAll getAttr getText rootElm findFirst roleLoc
        if findFirst && notNull labelledByElms
          then mkResult labelledByElms
          else do
            forElms <- roleToXPathFor locAll getAttr getText rootElm findFirst roleLoc
            mkResult . nubOrd $ mconcat [labelledByElms, forElms]
    where
      mkResult elms =
        pure $
          case elms of
            [] -> Nothing
            _ -> undefined

-- Just . LeafResult $
--   MkLeafResult
--     { source = RL.BiDiNative {loc = RL.Role {role = roleLoc}},
--       elms
--     }

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
