module WebDriverPreCore.Extended.Locators.Internal where

import Control.Exception (Exception)
import Data.Foldable1 (foldl1')
import Data.Functor.Identity (Identity (..))
import Data.List (nub, uncons)
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), groupBy, sortBy, toList)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Text (Text, intercalate, pack, splitOn, toLower, unpack)
import Data.Text qualified as T
import Data.Word (Word8)
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext, NodeProperties)
import Prelude
import Data.Function ((&))
import Control.Monad ((>=>))
import Data.List.NonEmpty (partition)

data MatchFlags = MkMatchFlags
  { ignoreCase :: Bool,
    matchType :: MatchType
  }
  deriving (Show, Eq)

--
data LocatorDirectives = ToDo deriving (Show, Eq)

data RoleLocator
  = RoleFull {role :: AriaRole, name :: Text}
  | RoleType {role :: AriaRole}
  | RoleName {name :: Text}
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )

-- | Locator for use with both HTTP and BiDi protocols.
data Locator
  = -- universal
    CSS {value :: Text}
  | XPath {value :: Text}
  | AllElms
  | ID {value :: Text}
  | Class
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity
      }
  | Attribute
      { name :: Text,
        value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity
      }
  | Tag {value :: Text}
  | Default {value :: Text}
  | -- double shot / difficult -todo:: where is visible text?
    Role {role :: RoleLocator}
  | InnerText
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity,
        maxDepth :: Maybe Word8
      }
  | -- exclusive
    -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
    BiDiContext {context :: BrowsingContext}
  | -- combinators
    Contains {container :: Locator, contained :: Locator}
  | All {elms :: NonEmpty Locator}
  | Any {elms :: NonEmpty Locator}
  | --- PostFilter
    PostFilter
      { predicate :: Predicate,
        locator :: Locator
      }
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )



-- | LocatorI an intermdiate type 
data LocatorI
  = 
    CSSI {value :: Text}
  | XPathI {value :: Text}
  | XPathID {
      tagM :: Maybe Text, 
      body :: Text
      } 
  | TagI {tag :: Text} 
  | RoleI {xpath :: Text}
  | InnerTextI
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity,
        maxDepth :: Maybe Word8
      }
  | -- exclusive
    -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
    BiDiContextI {context :: BrowsingContext}
  | -- combinators
    ContainsI {container :: LocatorI, contained :: LocatorI}
  | AllI {elms :: NonEmpty LocatorI}
  | AnyI {elms :: NonEmpty LocatorI}
  | --- PostFilter
    PostFilterI
      { predicate :: Predicate,
        locator :: LocatorI
      }
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )

transform :: Protocol -> (Text -> Locator) -> Locator -> Either InvalidLocator LocatorI
transform proto defLoc loc = do
  locI <- convertLoc proto defLoc loc
  simplified <- simplify locI
  pure $ derivedAndTagsToXPath simplified
  where
    simplify :: LocatorI -> Either InvalidLocator LocatorI
    simplify current = do
      merged <- mergeContiguous loc $ unnestAnysAlls current
      tagged <- assignTags merged
      let unwrapped = unwrapSingletonCombinators tagged
      if unwrapped == current
        then pure current
        else simplify unwrapped

-----------------------------------------------------------------------------
-- Phase 1: Initial conversion (Locator → LocatorI)
-----------------------------------------------------------------------------

convertLoc :: Protocol -> (Text -> Locator) -> Locator -> Either InvalidLocator LocatorI
convertLoc proto defLoc loc = 
  case loc of
    -- fallable conversions
    Attribute {name, value, matchType, caseSensitivity}
      | T.null name || T.null value ->
          failLocator "Attribute locator has empty name or value"
      | otherwise ->
          Right $ XPathID {tagM = Nothing, body = attrPredX name value matchType caseSensitivity}
    Default {value} ->
      let resolved = defLoc value
      in if hasDefault resolved
        then failLocator "Default locator cannot resolve to another Default"
        else convertLoc proto defLoc resolved
    BiDiContext {context} -> case proto of
      BiDi -> Right $ BiDiContextI {context}
      HTTP -> failLocator "BiDiContext locator cannot be used with HTTP protocol"
      -- combinators / postfilter require higher order conversions
    _ -> case loc of
      Contains {container, contained} ->
        ContainsI <$> convertLoc proto defLoc container <*> convertLoc proto defLoc contained
      All {elms} ->
        AllI <$> traverse (convertLoc proto defLoc) elms
      Any {elms} ->
        AnyI <$> traverse (convertLoc proto defLoc) elms
      PostFilter {predicate, locator} ->
        PostFilterI predicate <$> convertLoc proto defLoc locator
      -- simple pass through conversions / xpath
      _ -> Right $ case loc of
        CSS {value} -> CSSI {value}
        XPath {value} -> XPathI {value}
        AllElms -> XPathID {tagM = Nothing, body = "true()"}
        ID {value} -> XPathID {tagM = Nothing, body = "@id='" <> value <> "'"}
        Class {value, matchType, caseSensitivity} ->
          XPathID {tagM = Nothing, body = classPredX value matchType caseSensitivity}
        Tag {value} -> TagI {tag = value}
        Role {role} -> RoleI {xpath = roleToXPath role}
        InnerText {value, matchType, caseSensitivity, maxDepth} ->
          InnerTextI {value, matchType, caseSensitivity, maxDepth}
    where 
      failLocator msg = Left $ MkInvalidLocator loc msg

-----------------------------------------------------------------------------
-- Phase 2: Simplify loop (fixed-point)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- 2a. Flatten nested AllI/AnyI
-----------------------------------------------------------------------------

unnestAnysAlls :: LocatorI -> LocatorI
unnestAnysAlls = \case
  AllI elms ->
    AllI $ unnestAnysAlls <$> elms >>= (\case 
        AllI xs -> xs
        x -> x :| [] )
  AnyI elms ->
    AnyI $ unnestAnysAlls <$> elms >>= (\case
        AnyI xs -> xs
        x -> x :| [] )
  ContainsI c d -> ContainsI (unnestAnysAlls c) (unnestAnysAlls d)
  PostFilterI p l -> PostFilterI p (unnestAnysAlls l)
  other -> other

-----------------------------------------------------------------------------
-- 2b. Combine contiguous XPathIDs
-----------------------------------------------------------------------------
mergeContiguous :: Locator -> LocatorI -> Either InvalidLocator LocatorI
mergeContiguous srcLoc li = mergeAnys srcLoc <$> mergeAlls srcLoc li

bracket :: Text -> Text
bracket t = "(" <> t <> ")" 

mergeTags :: Locator -> Maybe Text -> Maybe Text -> Either InvalidLocator (Maybe Text)
mergeTags srcLoc = \cases
  Nothing Nothing -> Right Nothing
  (Just t) Nothing -> Right $ Just t
  Nothing (Just t) -> Right $ Just t
  (Just t1) (Just t2)
    | t1 == t2 -> Right (Just t1)
    | otherwise -> Left $ MkInvalidLocator srcLoc ("Contradictory tags in All combinator: " <> txt srcLoc <> "\n " <> t1 <> " and " <> t2)

mergeAlls :: Locator -> LocatorI -> Either InvalidLocator LocatorI
mergeAlls srcLoc = 
  mapOrFailLocIBottomUp (\case 
   AllI elms -> AllI <$> mergeAllElms elms
   other -> Right other)
 where
   mergeAllElms :: NonEmpty LocatorI -> Either InvalidLocator (NonEmpty LocatorI)
   mergeAllElms = \case
       (XPathID tm1 b1 :| XPathID tm2 b2 : rest) -> 
         do 
          tag <- mergeTags srcLoc tm1 tm2
          mergeAllElms $ XPathID {tagM = tag, body = bracket b1 <> " and " <> bracket b2} :| rest 
       x -> Right x

mergeAnys :: Locator -> LocatorI -> LocatorI
mergeAnys srcLoc = 
  mapLocIBottomUp (\case 
   AnyI elms -> AnyI $ mergeAnyElms elms
   other -> other)
 where
   mergeAnyElms :: NonEmpty LocatorI -> NonEmpty LocatorI
   mergeAnyElms = \case
       (XPathID tm1 b1 :| XPathID tm2 b2 : rest) ->
         mergeTags srcLoc tm1 tm2 & either
           (\_ ->
             -- Tags incompatible, keep first as-is, try merging from second onward
             let rest' = mergeAnyElms (XPathID tm2 b2 :| rest)
             in XPathID tm1 b1 :| toList rest')
           (\tag ->
             -- Tags compatible, merge these two and continue
             mergeAnyElms $ XPathID {tagM = tag, body = bracket b1 <> " or " <> bracket b2} :| rest)
       x -> x

-----------------------------------------------------------------------------
-- 2c. Assign tags
-----------------------------------------------------------------------------

assignTags :: Locator -> LocatorI -> Either InvalidLocator LocatorI
assignTags srcLocator = 
    mapLocIBottomUpM (\case
      AllI elms -> AllI <$> processAllI srcLocator elms
      AnyI elms -> AnyI <$> processAnyITags elms
      other -> Right other)

-- | 2c-i: Process TagI constructors within an AnyI.
processAnyITags :: NonEmpty LocatorI -> Either InvalidLocator (NonEmpty LocatorI)
processAnyITags elms = do
  let children = toList elms
      (tags, others) = LST.partition isTagI children
      tagXPaths = case tags of
        [] -> []
        [TagI t] -> [XPathI ("//" <> t)]
        ts -> [XPathI ("//" <> T.intercalate " | " [t | TagI t <- ts])]
      result = tagXPaths <> others
  case result of
    [] -> error "processAnyITags: empty result"
    (x : xs) -> Right (x :| xs)
  where
    isTagI (TagI _) = True
    isTagI _ = False

-- | 2c-ii + 2c-iii + 2c-iv: Process TagI constructors within an AllI.
processAllI :: Locator -> NonEmpty LocatorI -> Either InvalidLocator (NonEmpty LocatorI)
processAllI srcLocator elms = do
  let tagVals = nub . catMaybes $ tagVal <$> toList elms
  
  -- 2c-ii: Contradictory tag detection
  case tagVals of
    [] -> pure elms  -- no tags, nothing to distribute
    _ : _ : _ -> Left . MkInvalidLocator srcLocator $ 
      "Contradictory tags in All combinator: " <> T.intercalate ", " tagVals
    [t] -> do
      let distributed = fmap (distributeTag t) others
      
      -- 2c-iii-b: can we remove the Tag?
      let canRemove = not (any hasNonXPathIDDescendant distributed)
      
      -- 2c-iv: check nested AnyIs for XPathID-only after distribution
      let withNestedAnyCheck = fmap (checkNestedAnyTagRemoval t) distributed
      
      let resultList = if canRemove then withNestedAnyCheck
                       else TagI t : withNestedAnyCheck
      case resultList of
        [] -> error "processAllI: empty result"
        (x : xs) -> pure (x :| xs)

  -- 2c-iii: Tag distribution
  case tagValues of
    [] -> pure elms  -- no tags, nothing to distribute
    (t : _) -> do
      let distributed = fmap (distributeTag t) others
      
      -- 2c-iii-b: can we remove the Tag?
      let canRemove = not (any hasNonXPathIDDescendant distributed)
      
      -- 2c-iv: check nested AnyIs for XPathID-only after distribution
      let withNestedAnyCheck = fmap (checkNestedAnyTagRemoval t) distributed
      
      let resultList = if canRemove then withNestedAnyCheck
                       else TagI t : withNestedAnyCheck
      case resultList of
        [] -> error "processAllI: empty result"
        (x : xs) -> pure (x :| xs)
  where
    isTagI (TagI _) = True
    isTagI _ = False

    tagVal :: LocatorI -> Maybe Text
    tagVal = \case 
      TagI t -> Just t
      -- ar this stage we should not have any other tag types in the AllI
      -- including this fro future proofing
      XPathID {tagM = Just t} -> Just t
      _ -> Nothing

-- | Set tagM = Just t on all reachable XPathID descendants.
-- Traverses through AllI, AnyI, and the contained side of ContainsI.
-- Container side of ContainsI is NOT tagged.
distributeTag :: Text -> LocatorI -> LocatorI
distributeTag t = \case
  XPathID _ body -> XPathID {tagM = Just t, body}
  AllI elms -> AllI $ distributeTag t <$> elms
  AnyI elms -> AnyI $ distributeTag t <$> elms
  ContainsI container contained -> ContainsI container (distributeTag t contained)
  other -> other  -- PostFilterI, RoleI, XPathI, CSSI, InnerTextI, BiDiContextI, TagI

-- | Check if there are non-XPathID constructors reachable through the same
-- traversal as distributeTag. Used to decide if a TagI can be removed.
hasNonXPathIDDescendant :: LocatorI -> Bool
hasNonXPathIDDescendant = \case
  XPathID {} -> False
  TagI {} -> False  -- Tags being processed; don't count as blockers
  AllI elms -> any hasNonXPathIDDescendant elms
  AnyI elms -> any hasNonXPathIDDescendant elms
  ContainsI container contained ->
    hasNonXPathIDDescendant container || hasNonXPathIDDescendant contained
  _ -> True  -- RoleI, XPathI, CSSI, InnerTextI, PostFilterI, BiDiContextI

-- | 2c-iv: If a nested AnyI now contains only XPathID children (all same tag),
-- the Tag in the outer All can be removed. Since the Tag has already been
-- distributed, this checks the state after distribution.
checkNestedAnyTagRemoval :: Text -> LocatorI -> LocatorI
checkNestedAnyTagRemoval t = \case
  AnyI elms
    | all (isXPathIDWithTag t) elms -> 
        -- All children are XPathID with the same tag — the outer Tag is redundant
        -- for this subtree.  We can't remove the outer Tag from here, but we
        -- mark that this AnyI no longer blocks tag removal by returning the
        -- already-tagged children (no-op structurally).
        AnyI elms
  other -> other
  where
    isXPathIDWithTag tag (XPathID {tagM = Just t'}) = t' == tag
    isXPathIDWithTag _ _ = False

-----------------------------------------------------------------------------
-- 2d. Unwrap single-child combinators
-----------------------------------------------------------------------------

unwrapSingletonCombinators :: LocatorI -> LocatorI
unwrapSingletonCombinators = mapLocIBottomUp $ \case
  AllI (x :| []) -> x
  AnyI (x :| []) -> x
  other -> other

-----------------------------------------------------------------------------
-- Phase 3: Final conversion
-----------------------------------------------------------------------------

derivedAndTagsToXPath :: LocatorI -> LocatorI
derivedAndTagsToXPath = convertContains . convertTags . convertXPathIDs

convertXPathIDs :: LocatorI -> LocatorI
convertXPathIDs = mapLocIBottomUp $ \case
  XPathID {tagM, body} ->
    XPathI {value = "//" <> fromMaybe "*" tagM <> body}
  other -> other

convertTags :: LocatorI -> LocatorI
convertTags = mapLocIBottomUp $ \case
  TagI {tag} -> XPathI {value = "//" <> tag}
  other -> other

convertContains :: LocatorI -> LocatorI
convertContains = mapLocIBottomUp $ \case
  ContainsI (XPathI {value = containerXPath}) (XPathI {value = containedXPath}) ->
    XPathI $ containerXPath <> containedXPath
  other -> other

-----------------------------------------------------------------------------
-- Top-level XPath predicate helpers (extracted from locatorToXPathPartial)
-----------------------------------------------------------------------------

-- | XPath predicate for CSS class matching.
classPredX :: Text -> MatchType -> CaseSensitivity -> Text
classPredX val mt cs =
  let classAttr = applyCSText cs "@class"
      matchVal = lowerIfCIText cs val
  in case mt of
       Full ->
         "contains(concat(' ', normalize-space(" <> classAttr <> "), ' '), ' " <> matchVal <> " ')"
       Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
       Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
       Wildcard -> wildcardPredX classAttr matchVal

-- | XPath predicate for named attribute matching.
attrPredX :: Text -> Text -> MatchType -> CaseSensitivity -> Text
attrPredX name val mt cs =
  let attrExpr = applyCSText cs ("@" <> name)
      matchVal = lowerIfCIText cs val
  in case mt of
       Full -> attrExpr <> "='" <> matchVal <> "'"
       Partial -> "contains(" <> attrExpr <> ", '" <> matchVal <> "')"
       Starts -> "starts-with(" <> attrExpr <> ", '" <> matchVal <> "')"
       Wildcard -> wildcardPredX attrExpr matchVal

applyCSText :: CaseSensitivity -> Text -> Text
applyCSText CaseSensitive expr = expr
applyCSText CaseInsensitive expr =
  "translate(" <> expr <> ", '" <> upperAlpha <> "', '" <> lowerAlpha <> "')"

lowerIfCIText :: CaseSensitivity -> Text -> Text
lowerIfCIText CaseSensitive v = v
lowerIfCIText CaseInsensitive v = toLower v

wildcardPredX :: Text -> Text -> Text
wildcardPredX normText val =
  let parts = filter (not . T.null) $ splitOn "*" val
      startsWithWildcard = "*" `T.isPrefixOf` val
      endsWithWildcard = "*" `T.isSuffixOf` val
  in case parts of
       [] -> "true()"
       [single]
         | startsWithWildcard && endsWithWildcard ->
             "contains(" <> normText <> ", '" <> single <> "')"
         | startsWithWildcard ->
             "substring(" <> normText <> ", string-length(" <> normText <> ") - string-length('" <> single <> "') + 1) = '" <> single <> "'"
         | endsWithWildcard ->
             "starts-with(" <> normText <> ", '" <> single <> "')"
         | otherwise -> normText <> "='" <> single <> "'"
       _ ->
         let buildP (preds, curText) (idx, part) =
               let predicate =
                     if idx == (0 :: Int) && not startsWithWildcard
                       then "starts-with(" <> curText <> ", '" <> part <> "')"
                       else "contains(" <> curText <> ", '" <> part <> "')"
                   nextText = "substring-after(" <> curText <> ", '" <> part <> "')"
               in (preds <> [predicate], nextText)
             (predicates, _) = foldl' buildP ([], normText) (zip [0 ..] parts)
         in intercalate " and " predicates

-- | Map over a LocatorI tree bottom-up, with monadic effects.
--   Recursively transforms children first, then applies the function to the
--   reconstructed node.  Short-circuits on the first failure for instances
--   that support it (e.g. 'Either', 'Maybe').
mapLocIBottomUpM :: Monad m => (LocatorI -> m LocatorI) -> LocatorI -> m LocatorI
mapLocIBottomUpM f = (\case 
    ContainsI c d -> ContainsI <$> recurse c <*> recurse d
    AllI elms -> AllI <$> recurseMap elms
    AnyI elms -> AnyI <$> recurseMap elms
    PostFilterI p l -> PostFilterI p <$> recurse l
    l -> pure l)
    >=> f
  where
    recurse = mapLocIBottomUpM f
    recurseMap = traverse recurse

-- | Map over a LocatorI tree bottom-up.
--   Expressed via 'mapLocIBottomUpM' using the 'Identity' monad.
mapLocIBottomUp :: (LocatorI -> LocatorI) -> LocatorI -> LocatorI
mapLocIBottomUp f = runIdentity . mapLocIBottomUpM (Identity . f)

-- | Specialization of 'mapLocIBottomUpM' to 'Either InvalidLocator'.
mapOrFailLocIBottomUp :: (LocatorI -> Either InvalidLocator LocatorI) -> LocatorI -> Either InvalidLocator LocatorI
mapOrFailLocIBottomUp = mapLocIBottomUpM

{-

implement   mergeAnyElms :: NonEmpty LocatorI -> LocatorI
  mergeAnyElms elms. Somewhat similar to #sym:mergeAlls 
it should recursively pattern match on b1 :| XPathID tm2 b2 : rest) -> bracket and or the bodies together IFF merging the tags is valid

## Spec Version 1

As a first step to a large scale refactor implement the transform function (copying and modifying code from related modules as required as well as writing new code)
I need the following:
- apply #sym:defLoc  to get a Locator from #sym:Default  and return a recursive default locator error if the result is a Default locator
- the locators that have been removed  between #sym:Locator  and #sym:LocatorI  need to be coverted into XpathID (D is for Derived) body only tag nothing initially in the xpath
- Role needs to be converted to xpathI (complete with any * tag) and assigned to xpath property
- need to recursively work through combinators working through the lists of locators (elms) as follows 
  - without reordering combine any contiguous series of XPathID  elms as XpathID (copy / modify existing logic to generate the xpath text - with no tag) and nest where there are boundaries between XPathID 
  and other types eg for an AllI locator with 3 XPAthIDs, followed by a Role and 2 XPathIDs would become AllI locaotr XpathID (combining the 3 XPathIDs), a nested RoleI, followed by another nested All XpathID (combining the 2 XPathIDs) 
- when cobining xpaths is complete there should be no adjacent XPathIDs in the result as they should have been combined into a single XpathID locatorS
- any ContainsI for which both the container shoud be converted to an XPathID should be converted into a singleXPAthID (a per existing logic with noting as the tag)
- next assign tags to xpathIDs and convert as follows:
  - if there is a single Tag within an All, this should be the tag for all XPATHIDs within that All and the Tag locator can be removed IFF there are no other tag types (such as ROll or XPathI witihin the all)
  - if there are multiple different tags within an All, return a contradictory tag Invalid locator error
  - if are Tags within an Any, it will just stand alone and be converted to the equivalent XpathI
  - if there is a single tag in an All with a nested Any eg. 
       All [Tag div, Any [XPathID, CSS, XPathID]]  -> then the tag should be distributed across the XPAthIDs in the Any. 
       futher simplify this 
         eg - if there is a matching tag witn the any it can be removed as it is redudant 
            - If there is only XPAthIDs in the Any then the Tag can be removed from the All as it is redundant
- recursively simplify  until no further simplifications can be made (eg. All with a single child, Any with a single child, All or Any with only XPathIDs which can be combined into a single XPathID, etc.) 
   - any Contains should be converted into an XPathID if both the container and contained are XPathIDs and combined as per existing logic
- as a final step when no further simplification is possible convert all XPAthIDs to XPathI with tag as per the tag property or * if the tag is still Nothing also convert stand alone tags to XPathIs
  - note DO not convert RoleI to XPAthI 
- at the end of this simplification there should not be any combinators left that formally contained only xpathID and there should not be any standalone tags or XPAthIDs as they should have been converted to XPathIs 
- other types of locators should be mapped failthfully to the new LocatorI type with minimal changes to their structure  


🔴 Critical Bugs
1. XPathID body semantics: predicate vs. full-path ambiguity

This is the most serious issue and permeates multiple steps. XPathID.body can hold two incompatible kinds of content:

Predicate bodies from ID, Class, Attribute: e.g. [@id='foo'] — the //tag prefix is added in the final step
Full-path bodies from Contains combination: e.g. [@id='foo']//*[contains(@class,'bar')] — the body already contains //* steps
When these two kinds meet (e.g., an All containing a Contains-derived XPathID and a predicate XPathID), the contiguous-combining step would join them with and, producing invalid XPath: "[@id='foo']//*[contains(@class,'bar')] and [@data-x='1']" — syntactically wrong.

The existing convertGroupToXPath in ReducedLocator/Internal.hs (lines 168-270) avoids this via isMultiStepXPath detection and appendPredsToLastStep / ancestorCheckFromMultiStep. The new design needs equivalent multi-step-aware logic.

RESPONSE: 
1. XPathID body semantics: predicate vs. full-path ambiguity
=> the intent is that all XPAthID fields would be generated without tags such as //* from the source locaoter (eg elmClass) and tags added only after all combining and simplification logic is complete. This is different from the existing logic though the logic for generating the xpath bodies should mostly be extracted as is. As the new logic explicitly avoids simplifyinfg user xpath there should be no issue with removing tags or any xpath parsing required. No other Locator types that XPathID and combinators containing XPATHID will be subject to simplification / combination 


2. Missing Protocol parameter

transform's signature has no Protocol parameter, but protocol matters critically:

BiDiContext is valid for BiDi, invalid for HTTP
Role is IsBiDi for BiDi (native), but IsMixed for HTTP (needs XPath + post-filter)
InnerText is IsBiDi for BiDi, IsMixed for HTTP
Without Protocol, transform can't know whether to flag BiDiContext as an error or whether Role/InnerText need special HTTP handling. The current classify function (lines 290-317) already captures this logic.

RESPONSE:
add parameter but only implement for http for now throwing an error for bidi only locators tthat hve no existing special http conversion logic such as bidiContext

3. PostFilterI's inner type is still Locator, not LocatorI

Should be locator :: LocatorI for consistency — otherwise the transformation is incomplete and downstream code must handle both Locator and LocatorI.

RESPONSE:
=> FIX this its a typo

4. AllElms→XPathID body representation

AllElms means "match every element". toXPathStr produces "//*". If stored as XPathID with body = "", tag = Nothing → final "//*". But if tag gets assigned (e.g., inside an All with Tag div), the result becomes "//div" — meaning "all divs". While semantically close, it's not identical: the original AllElms inside an All is a tautology (true()), while Tag div is a concrete constraint. The existing system preserves this distinction.


RESPONSE:
=> update as per your suggestion: AllElms must produce XPathID {tag=Nothing, body="true()"}, not body="". The spec should state this explicitly. 
The empty-body alternative produces invalid XPath whenever AllElms participates in contiguous combining with other XPathIDs. 
The true() choice is also consistent with the existing toPred AllElms = "true()"


🟡 Logic & Ambiguity Bugs
5. Nesting of combined XPathIDs is unnecessary and complicates simplification

The spec says the trailing 2 XPathIDs (after RoleI) should be in a "nested All". But the outer AllI already provides and semantics. If the nested AllI gets a single child, the later "unwrap single-child" simplification would eliminate it anyway — so the nesting is transient and adds complexity for no gain.

RESPONSE: Agreed, the nesting is unnecessary. The spec should be revised to say that after combining contiguous XPathIDs, the resulting XPathID should be placed directly in the parent combinator (All or Any) without adding an extra All layer. The simplification logic can then focus on combining adjacent XPathIDs without needing to handle nested structures created solely for grouping.

6. "Other tag types" definition is underspecified

The spec says a Tag can be removed from an All "IFF there are no other tag types (such as Role or XPathI within the all)". But:

Is XPathI always a "tag type"? What if the XPathI is "//*[contains(@class,'foo')]" — it has no tag constraint
Is InnerTextI a "tag type"?
The term "within the all" — does it mean direct children only, or recursively including nested All/Any?

RESPONSE:  This is a typo. It should say somthing such like: IFF there are no sibling or nested non XPathID type constructors (such as Role or XPathI) within the All, because the tag could not be distrbuted to them

7. Contradictory tag detection scope

"if there are multiple different tags within an All, return a contradictory tag error." Does "within" mean direct children only? If All [Tag div, All [Tag span, XPathID]], the tags are at different nesting levels — should this be an error? Probably not, since the inner All forms its own scope. But the spec doesn't clarify.

RESPONSE: Contradictory tags in a nested All should still be an error because it would still be a logical contradiction. In fact, this is the overarching principle for this error condition. Are the tags contradictory?
eg. All [Tag div, All [Tag span, XPathID]] == All [Tag div, Tag span, XPathID] ==> Contradictory tags. 
    Any [Tag span, All [Tag div, XPathID]] ==> not Contradictory
    All [Tag div, All [Tag div, XPathID]]  ==> second tag redundant

8. Tag distribution across nested boundary interacts with Contains

Consider: All [Tag div, Contains (XPathID {body="[@id='a']"}) (XPathID {body="[contains(@class,'b')]"})]. After Contains→XPathID conversion (step 3), the body is "[@id='a']//*[contains(@class,'b')]" with tag=Nothing. Then tag assignment sets tag=Just "div". Final XPathI: "//div[@id='a']//*[contains(@class,'b')]". This means "div with id 'a' containing any descendant with class 'b'" — the tag only constrains the container, not the contained. Is this the intended semantics? The spec doesn't address which side of a Contains the tag should apply to.

RESPONSE:  The overiding law should be that if the tag is not distributed and the logic run the outcome should be the same so in this case the tag could be distributed to the child but not the container. 
  IE if these filters were run separately the result would be the intersection of all divs and (all class b under id a) so the effect is the same as all divs with class b under id a

9. Tag from outer All distributed into nested Any with non-XPathID children

All [Tag div, Any [XPathID, CSS, XPathID]] — the spec says tag is distributed to XPathIDs in the Any. CSS doesn't get tagged. Then: "If there is only XPathIDs in the Any then the Tag can be removed from the All as it is redundant." But CSS is still in the Any, so the Tag stays. Later, after the Tag remains, TagI→XPathI produces XPathI "//div" sitting alongside the XPathIDs-turned-XPathIs. The All then has [XPathI "//div", Any [XPathI "//div[...]", CSSI, XPathI "//div[...]"]] — the XPathI "//div" as a separate element seems semantically redundant/odd.

RESPONSE: since the div tag is part of the all, any elm that satsisfies the criteria will be a div. This being the case it makes sense to inline the Tag into the XPAthIDs as there is a chance they will run faster than if they are being applied to all tags *. The CSS, however, will not be changed so we need to keep the Tag in the All to ensure the correct semantics. For this reason the exisitng spec is correct.

10. Order sensitivity between steps

The spec orders steps as: contiguous combine → Contains→XPathID → tag assignment → recursive simplify → final conversion. But:

Contiguous combining must run after Contains→XPathID conversion too, because that conversion can create new XPathIDs adjacent to existing ones
Tag assignment must run before "unwrap single-child" simplification, otherwise the tag context is lost
"Contains→XPathID" appears in two places: step 3 (initial) and step 5 (recursive). Are these the same pass or two separate passes? If separate, Contains→XPathID in step 5 would need tag assignment to have already happened for its children, creating a phase ordering problem.

RESPONSE: I think it needs to be:  contiguous combine → (Contains → PathID) → tag assignment → simplify (unwrap single child) -> repeat until no change -> final conversion.

🟢 Minor Issues & Edge Cases
11. Default resolution: top-level vs. recursive check

The spec says "return a recursive default locator error if the result is a Default locator." The existing hasDefault (line ~414) uses anyLoc which checks recursively. But the spec wording "the result is a Default" could be interpreted as only checking the top-level constructor. If defLoc returns All [Default "x", CSS "y"], the top-level is All, not Default. Clarify whether recursive checking is intended.

RESPONSE: The intent is to check recursively, as any presence of a Default in the resolved locator would indicate an unresolved default and thus an error. The spec should be clarified to say "return a recursive default locator error if any part of the resolved locator is a Default locator." This ensures that the check is comprehensive and prevents any Default from slipping through, even if it's nested within other combinators.

12. Tag within Any with multiple Tags

Any [Tag div, Tag span, XPathID] — each Tag becomes a standalone XPathI. But logically, two different XPathIs in an Any is equivalent to a single XPath union "//div | //span". The spec doesn't address whether standalone Tags within the same Any should be merged into one XPathI union.

RESPONSE: The spec should be updated to say that if there are multiple Tags within the same Any, they should be combined into a single XPathI with a union of the tags. For example, Any [Tag div, Tag span, XPathID] would become Any [XPathID "//div | //span", XPathID "..."] which then could be reduced furhter to a single XPathID.

13. Attribute with empty name/value

Attribute {name = "", value = "", ...} — attrPred would produce @='' which is invalid XPath. This is pre-existing in the current codebase but worth noting as the transform pipeline doesn't add any validation.

RESPONSE: add check and return invalid locator error with description.

14. InnerText conversion during transform

InnerText→InnerTextI is a faithful mapping. But the existing classify treats InnerText as IsMixed for HTTP (needs XPath + post-filter). The transform spec doesn't mention any special handling — so InnerText passes through as InnerTextI and the HTTP double-shot would need to happen in a later stage. This is probably intentional but should be documented.

RESPONSE: this is correct add short note to spec.

15. CaseSensitivity field name typo in existing code

InnerText has caseSesnsitivity (misspelled) — this propagates to InnerTextI and BiDiNativeLoc. Not introduced by the spec, but worth fixing during the refactor.

RESPONSE: Fix 

16. XPathID combining within Any uses or, not and

The spec describes contiguous combining only in terms of AllI, but AnyI needs the same treatment with or semantics (matching the existing toPred logic). This is implied but not stated.
## Spec Version 2
RESPONSE: Add a short note to spec.

Summary
#	Severity	Issue
1	🔴 Critical	XPathID body: predicate vs full-path ambiguity breaks combining
2	🔴 Critical	Missing Protocol parameter
3	🔴 Critical	PostFilterI.locator still Locator, not LocatorI
4	🔴 Critical	AllElms→XPathID with tag assignment changes semantics
5	🟡 Medium	Unnecessary nesting of combined XPathIDs
6	🟡 Medium	"Other tag types" definition underspecified
7	🟡 Medium	Contradictory tag scope ambiguity
8	🟡 Medium	Tag application to Contains (container vs contained)
9	🟡 Medium	Tag distribution with mixed Any children
10	🟡 Medium	Phase ordering (Contains→XPathID appears twice)
11	🟢 Minor	Default resolution: top-level vs recursive
12	🟢 Minor	Multiple standalone Tags in Any
13	🟢 Minor	Empty Attribute name/value → invalid XPath
14	🟢 Minor	InnerText HTTP handling deferred
15	🟢 Minor	caseSesnsitivity typo
16	🟢 Minor	AnyI contiguous combining with or not stated
The core architectural question is Bug #1: whether XPathID.body should always be a pure predicate (no // prefix), or whether it can hold full-path expressions. The existing convertGroupToXPath code in ReducedLocator/Internal.hs (lines 168-340) handles this via isMultiStepXPath, splitMultiStepXPath, extractDescendantPreds, ancestorCheckFromMultiStep, and appendPredsToLastStep — all of which would need equivalents in the LocatorI simplification passes if full-path bodies are allowed. The simpler alternative is to restrict XPathID.body to predicates only and defer all //* concatenation to the final XPathID→XPathI conversion step, treating Contains differently (not collapsing it to XPathID until after tag assignment).

## Spec Version 2

### Signature

```haskell
transform :: Protocol -> (Text -> Locator) -> Locator -> Either InvalidLocator LocatorI
```

The `Protocol` parameter is required. Initial implementation targets HTTP only;
BiDi-only constructors (e.g. `BiDiContext`) return an `InvalidLocator` error.
(BiDi support is deferred to a later change.)

### Design principle

XPathID bodies are always pure predicates — no `//*` or `//tag` prefix.
Tags are stored separately in `tagM` and are only materialised in the final
Phase 3 conversion.  ContainsI is NOT collapsed to XPathID during the
simplification loop; instead tags cascade *through* ContainsI to the contained
side during tag assignment, and the ContainsI → XPathI concatenation happens
as the very last step in Phase 3, using the already-fully-tagged XPathI values.

### Phase 0 — Type fixes before implementation

- Fix `PostFilterI.locator` from `Locator` to `LocatorI` (typo).
- Fix `caseSesnsitivity` → `caseSensitivity` throughout `InnerText`, `InnerTextI`,
  and any downstream copies (typo).

### Phase 1 — Initial conversion (`Locator` → `LocatorI`)

Map each `Locator` constructor to `LocatorI` as follows. Recurse through
combinators bottom-up (convert children first, then rebuild the parent).

| `Locator` constructor | `LocatorI` result |
|---|---|
| `CSS {value}` | `CSSI {value}` |
| `XPath {value}` | `XPathI {value}` (preserved as-is; never merged) |
| `AllElms` | `XPathID {tagM = Nothing, body = "true()"}` |
| `ID {value}` | `XPathID {tagM = Nothing, body = "@id='" <> value <> "'"}` |
| `Class {value, matchType, caseSensitivity}` | `XPathID {tagM = Nothing, body = classPred value matchType caseSensitivity}` |
| `Attribute {name, value, matchType, caseSensitivity}` | `XPathID {tagM = Nothing, body = attrPred name value matchType caseSensitivity}`. Validate: if `name` or `value` is empty, return `InvalidLocator`. |
| `Tag {value}` | `TagI {tag = value}` (will be resolved in Phase 3) |
| `Default {value}` | Resolve via `defLoc value`. If *any* node in the resolved tree is a `Default`, return `InvalidLocator` (recursive check). Otherwise, recursively convert the resolved locator. |
| `Role {role}` | `RoleI {xpath = roleToXPath role}` (pre-compute the full `//*[...]` XPath string using existing `roleToXPath`) |
| `InnerText {value, matchType, caseSensitivity, maxDepth}` | `InnerTextI {value, matchType, caseSensitivity, maxDepth}`. Note: InnerText passes through unchanged; HTTP double-shot handling belongs in a later pipeline stage. |
| `BiDiContext {context}` | `BiDiContextI {context}`. For HTTP: return `InvalidLocator`. |
| `Contains {container, contained}` | `ContainsI {container = convert container, contained = convert contained}` |
| `All {elms}` | `AllI {elms = convert <$> elms}` |
| `Any {elms}` | `AnyI {elms = convert <$> elms}` |
| `PostFilter {predicate, locator}` | `PostFilterI {predicate, locator = convert locator}` |

Predicate helper functions (`classPred`, `attrPred`) are extracted from the existing
`locatorToXPathPartial` / `toPred` logic. They produce *bare predicates* without
any `//*` or `//tag` prefix.

### Phase 2 — Simplify loop (fixed-point)

Apply the following passes in order, repeating the entire sequence until no
further changes occur:

```
loop:
  2a. flatten nested AllI/AnyI
  2b. combine contiguous XPathIDs
  2c. assign tags
  2d. unwrap single-child combinators
  2e. combine XPathID-only AllI/AnyI into a single XPathID
```

Note: ContainsI → XPathID conversion is **not** done here. Tags are cascaded
*through* ContainsI to the contained side. ContainsI is resolved in Phase 3.

#### 2a. Flatten nested AllI/AnyI

Same semantics as existing `flattenLoc` but on `LocatorI`:
- `AllI [..., AllI [a,b], ...]` → `AllI [..., a, b, ...]`
- `AnyI [..., AnyI [a,b], ...]` → `AnyI [..., a, b, ...]`
- Flattening recurses: `AllI [AllI [AllI [x]]]` → `AllI [x]`
- If after flattening an AllI/AnyI has a single child, it is **not** unwrapped
  here — that is done in step 2d.

#### 2b. Combine contiguous XPathIDs

Within each `AllI` and `AnyI`, scan the child list for runs of adjacent `XPathID`
constructors. For each run, combine them into a single `XPathID` and place the
result directly in the parent combinator (no extra nesting).

- Within **AllI**: join bodies with `" and "`.
  `XPathID {body="a"}, XPathID {body="b"}` → `XPathID {tagM=Nothing, body="a and b"}`
- Within **AnyI**: join bodies with `" or "`.
  `XPathID {body="a"}, XPathID {body="b"}` → `XPathID {tagM=Nothing, body="a or b"}`
- Parenthesise each original body before joining if it contains `and`/`or` to
  preserve precedence: `"(a or b) and c"`.
- The combined XPathID inherits `tagM = Nothing` (tags are assigned in step 2c).
- After this pass, no combinator should contain adjacent XPathID children.

Example: `AllI [XPathID{body="p1"}, XPathID{body="p2"}, XPathID{body="p3"}, RoleI{...}, XPathID{body="p4"}, XPathID{body="p5"}]`
→ `AllI [XPathID{body="p1 and p2 and p3"}, RoleI{...}, XPathID{body="p4 and p5"}]`

#### 2c. Assign tags

Recurse through the tree. At each `AllI` and `AnyI`, process Tags as follows.

##### 2c-i. Tags within AnyI

Each `TagI` within an `AnyI` is converted to a standalone `XPathI`. If there
are multiple `TagI` children within the same `AnyI`, they are merged into a
single `XPathI` with a union value:

- `AnyI [TagI "div", TagI "span", ...]` →
  `AnyI [XPathI "//div | //span", ...]`

After this step there should be no `TagI` constructors remaining inside any
`AnyI`. If the resulting `XPathI` is the only child of the `AnyI`, unwrapping
will happen in step 2d.

##### 2c-ii. Contradictory tag detection within AllI

Scan an `AllI` for `TagI` constructors in its *direct children*. Also consider
`TagI` constructors from nested `AllI` children (but NOT from nested `AnyI`
children — those are handled independently in their own scope).

If two `TagI` constructors at the same effective level (after considering
nested-All flattening) have different tag values, return
`InvalidLocator "Contradictory tags in All combinator: div, span"`.

Examples:
- `AllI [TagI "div", AllI [TagI "span", XPathID{...}]]` → **error** (nested All
  contributes a conflicting tag)
- `AllI [TagI "div", AnyI [TagI "span", XPathID{...}]]` → **ok** (Any's tag is
  in its own scope)
- `AllI [TagI "div", AllI [TagI "div", XPathID{...}]]` → **ok** (same tag;
  inner one is redundant and will be removed below)

##### 2c-iii. Tag distribution within AllI

For an `AllI`:
1. Collect all `TagI` direct children.
2. If zero Tags → nothing to do.
3. If exactly one Tag value `t`:
   a. Set `tagM = Just t` on every `XPathID` descendant that is reachable by
      traversing through `AllI`, `AnyI`, **and `ContainsI`** combinators.
      When crossing a `ContainsI`, the tag cascades to the **contained** side
      only (and recursively through any AllI/AnyI beneath it). The container
      side of a `ContainsI` is NOT tagged.
      Do not cross `PostFilterI` or any leaf non-XPathID constructor
      (RoleI, XPathI, CSSI, InnerTextI, BiDiContextI).
   b. Remove the `TagI` from the AllI's children **iff** there are no non-XPathID
      constructors (RoleI, XPathI, CSSI, InnerTextI, ContainsI, PostFilterI,
      BiDiContextI) among the AllI's direct or nested descendants (traversing
      through AllI/AnyI and the contained side of ContainsI). If any such
      constructor exists, the Tag stays to preserve correct semantics for
      elements it could not be distributed to.
4. If multiple Tags with the same value → keep one, remove the rest (redundant).
   This case is reached after contradictory-tag check (2c-ii) has already filtered
   out differing Tags.

##### 2c-iv. Tag removal when nested Any becomes XPathID-only

After tag distribution, if a nested `AnyI` within the `AllI` now contains only
`XPathID` children (all of which have the same `tagM`), the `TagI` in the outer
`AllI` can be removed (it is redundant — all elements in the Any already carry
the tag).

#### 2d. Unwrap single-child combinators

- `AllI [x]` → `x`
- `AnyI [x]` → `x`
- `ContainsI {container = x, contained = y}` where `x` or `y` is a
  single-child combinator → unwrap first, then re-check ContainsI.

This pass is intentionally placed *after* tag assignment (2c) so that tag
distribution is not lost by premature unwrapping.

#### 2e. Combine XPathID-only AllI/AnyI

If an `AllI` or `AnyI` contains *only* `XPathID` children (after all previous
passes), combine them into a single `XPathID`:

- For `AllI`: join bodies with `" and "` (parenthesise as in 2b).
- For `AnyI`: join bodies with `" or "` (parenthesise as in 2b).
- tagM = the common `tagM` if all children have the same tag, otherwise
  `Nothing`.

After this pass, if the result is a single XPathID, the outer combinator has
been eliminated. Apply 2d in the next iteration to unwrap any new single-child
combinators.

#### Loop termination

The fixed-point loop terminates when a full pass (2a→2e) produces no changes.
Since every pass either reduces the number of constructors or eliminates a
constructor type, termination is guaranteed.

### Phase 3 — Final conversion

When the loop terminates (no further simplifications possible), convert all
remaining intermediate constructors to their final forms, in this order:

**3a. XPathID → XPathI**:
   `XPathI {value = "//" <> tagStr <> body}`
   where `tagStr = fromMaybe "*" tagM`.
   Since all XPathID bodies are pure predicates (ContainsI was not collapsed
   in the loop), the `//tagStr` prefix is always correct.

**3b. TagI → XPathI**:
   `XPathI {value = "//" <> tag}`

**3c. ContainsI → XPathI** *(new — replaces the old Phase 2 step)*:
   For each `ContainsI {container, contained}` where both sides are now
   `XPathI` (after steps 3a/3b), replace with a single `XPathI` by
   concatenating the already-resolved values:
   `XPathI {value = container.value <> contained.value}`

   This naturally places the container's tag (if any) on the container step
   and the contained's tag on the contained step, since tags were already
   materialised in 3a.  Example:
   - container = `XPathI "//*[@id='foo']"` (no tag, so `*`)
   - contained = `XPathI "//div[contains(@class,'bar')]"` (tag=div)
   - result = `XPathI "//*[@id='foo']//div[contains(@class,'bar')]"`

   After this pass apply step 3d to clean up any redundant neighbouring
   XPathIs.

**3d. Cleanup redundant Tag-derived XPathIs**:
   After 3c, an `AllI` may contain a standalone `XPathI "//t"` (from a TagI
   that could not be removed in 2c because a ContainsI blocked it) alongside
   the newly-created Contains-derived `XPathI` which already encodes `t` on
   the contained step.  These are logically redundant but harmless; they may
   be left as-is for the downstream consumer to handle, or optionally removed
   if `"//t"` is a suffix of another child's value.

**3e. RoleI** stays as `RoleI` — do NOT convert to XPathI.
   (The xpath is already pre-computed in its `xpath` field.)

### Phase 4 — Post-conditions

After Phase 3, the following invariants hold:
- No `XPathID` or `TagI` constructors remain.
- No `AllI` or `AnyI` contains only `XPathI` children that could have been
  combined (they were combined in 2e).
- No `ContainsI` has both sides as `XPathI` (they were converted in 3c).
- `RoleI` is preserved (not converted to `XPathI`).
- User-provided `XPath` values are preserved as-is in `XPathI` and were never
  merged with auto-generated XPath.

### Notes from review

- **Contains resolution**: Deferred to Phase 3. Tags cascade through ContainsI
  to the contained side during the loop (2c-iii). The XPath concatenation uses
  already-tagged XPathI values, so each side carries its own tag naturally.
- **InnerText**: Passes through as `InnerTextI`. HTTP double-shot (XPath +
  post-filter) is handled in a downstream pipeline stage, not in `transform`.
- **validate `Attribute`**: Empty `name` or `value` returns `InvalidLocator`.
- **`tagM` field**: The `XPathID` constructor uses `tagM :: Maybe Text` (not
  `tag`), matching the current type definition.
-

-}
-------

data Predicate
  = BiDiPredicate
      { description :: Text,
        -- TODO: fix this when merged
        nodePredicate :: NodeProperties -> Bool
      }
  | HttpPredicate
      { description :: Text,
        -- TODO: fix this when merged
        httpCommand :: Either Text Text
      }
  | JSPredicate
      { description :: Text,
        -- TODO: fix this when merged
        js :: Text
      }
  | ValuePredicate
      { description :: Text,
        value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity
      }
  | ValueFuncPredicate
      { description :: Text,
        valPredicate :: Text -> Bool
      }

instance Show Predicate where
  show :: Predicate -> String
  show p =
    prefix <> unpack p.description
    where
      prefix = case p of
        BiDiPredicate {} -> "BiDiPredicate: "
        HttpPredicate {} -> "HttpPredicate: "
        JSPredicate {} -> "JSPredicate: "
        ValuePredicate {} -> "ValuePredicate: "
        ValueFuncPredicate {} -> "ValueFuncPredicate: "

instance Eq Predicate where
  (==) :: Predicate -> Predicate -> Bool
  (==) p p1 = p.description == p1.description

instance Ord Predicate where
  compare :: Predicate -> Predicate -> Ordering
  compare p p1 = compare p.description p1.description

-- | ARIA roles from https://www.w3.org/TR/wai-aria-1.2/#role_definitions
data AriaRole
  = Article
  | Banner
  | Button
  | Cell
  | Checkbox
  | ColumnHeader
  | Complementary
  | ContentInfo
  | Definition
  | Dialog
  | Figure
  | Form
  | Group
  | Heading
  | Img
  | Link
  | List
  | ListItem
  | Main
  | Navigation
  | Option
  | ProgressBar
  | Radio
  | Region
  | Row
  | RowHeader
  | Search
  | Separator
  | Slider
  | SpinButton
  | Status
  | Table
  | Term
  | Textbox
  deriving (Show, Eq, Ord, Enum, Bounded)

data MatchType = Full | Starts | Partial | Wildcard deriving (Show, Eq, Ord)

data CaseSensitivity = CaseSensitive | CaseInsensitive deriving (Show, Eq, Ord)

displayAriaRole :: AriaRole -> Text
displayAriaRole = toLower . pack . show

roleToXPath :: RoleLocator -> Text
roleToXPath = \case
  RoleFull {role, name} -> "//*" <> role' role <> name' name
  RoleType {role} -> "//*" <> role' role
  RoleName {name} -> "//*[not(@role='presentation' or @role='none')]" <> name' name
  where
    role' = roleTypeXPathContent True 

    name' n =
      "["
        <> intercalate
          " or "
          [ "@aria-label='" <> n <> "'",
            "@placeholder='" <> n <> "'",
            "@alt='" <> n <> "'",
            "normalize-space(text())='" <> n <> "'",
            -- title is a last-resort fallback: only matches when no higher-priority source exists
            "@title='" <> n <> "' and not(@aria-label) and not(@placeholder) and not(@alt) and not(normalize-space(text()))"
          ]
        <> "]"

-- | the content of the role type xpath no: //*[ ]
roleTypeXPathContent :: Bool -> AriaRole -> Text
roleTypeXPathContent wantBrackets r = 
  if wantBrackets
    then "[" <> content <> "]"
    else content
  where 
    content = implicitRoleXPath r <> " or @role='" <> displayAriaRole r <> "'"


-- | Maps an ARIA role to an XPath predicate matching elements that have
--   that role implicitly (i.e. without an explicit role= attribute).
--   Based on the ARIA in HTML spec: https://www.w3.org/TR/html-aria/
implicitRoleXPath :: AriaRole -> Text
implicitRoleXPath =
  ("self::" <>) . \case
    Article -> "article"
    Banner -> "header[not(ancestor::article) and not(ancestor::section)]"
    Button -> "button or self::input[@type='button'] or self::input[@type='submit'] or self::input[@type='reset'] or self::input[@type='image'] or self::summary"
    Cell -> "td"
    Checkbox -> "input[@type='checkbox']"
    ColumnHeader -> "th[@scope='col' or not(@scope)]"
    Complementary -> "aside"
    ContentInfo -> "footer[not(ancestor::article) and not(ancestor::section)]"
    Definition -> "dd"
    Dialog -> "dialog"
    Figure -> "figure"
    Form -> "form[@aria-label or @aria-labelledby]"
    Group -> "fieldset or self::optgroup"
    Heading -> "h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6"
    Img -> "img[@alt and string-length(@alt)>0]"
    Link -> "a[@href] or self::area[@href]"
    List -> "ul or self::ol"
    ListItem -> "li"
    Main -> "main"
    Navigation -> "nav"
    Option -> "option"
    ProgressBar -> "progress"
    Radio -> "input[@type='radio']"
    Region -> "section[@aria-label or @aria-labelledby]"
    Row -> "tr"
    RowHeader -> "th[@scope='row']"
    Search -> "search"
    Separator -> "hr"
    Slider -> "input[@type='range']"
    SpinButton -> "input[@type='number']"
    Status -> "output"
    Table -> "table"
    Term -> "dt"
    Textbox -> "input[not(@type) or @type='text' or @type='email' or @type='tel' or @type='url' or @type='search'] or self::textarea"

innerTextToXPath :: Text -> CaseSensitivity -> MatchType -> Maybe Word8 -> Text
innerTextToXPath val cs matchType mMaxDepth =
  "//*" <> depthPred <> "[" <> hiddenPred <> " and " <> textPred <> "]"
  where
    normalisedText = case cs of
      CaseInsensitive -> "translate(normalize-space(.), '" <> upperAlpha <> "', '" <> lowerAlpha <> "')"
      CaseSensitive -> "normalize-space(.)"

    matchVal = case cs of
      CaseInsensitive -> toLower val
      CaseSensitive -> val

    textPred = case matchType of
      Full -> normalisedText <> "='" <> matchVal <> "'"
      Partial -> "contains(" <> normalisedText <> ", '" <> matchVal <> "')"
      Starts -> "starts-with(" <> normalisedText <> ", '" <> matchVal <> "')"
      Wildcard -> buildWildcardPredicate normalisedText matchVal

    buildWildcardPredicate normText val' =
      let parts = filter (not . T.null) $ splitOn "*" val'
          startsWithWildcard = "*" `T.isPrefixOf` val'
          endsWithWildcard = "*" `T.isSuffixOf` val'
       in case parts of
            [] -> "true()" -- "*" or "**" etc. matches everything
            [single]
              | startsWithWildcard && endsWithWildcard -> "contains(" <> normText <> ", '" <> single <> "')"
              | startsWithWildcard -> "substring(" <> normText <> ", string-length(" <> normText <> ") - string-length('" <> single <> "') + 1) = '" <> single <> "'"
              | endsWithWildcard -> "starts-with(" <> normText <> ", '" <> single <> "')"
              | otherwise -> normText <> "='" <> single <> "'" -- No wildcards
            _ ->
              -- For multiple parts, use substring-after to ensure order
              let buildPred (preds, currentText) (idx, part) =
                    let predicate =
                          if idx == 0 && not startsWithWildcard
                            then "starts-with(" <> currentText <> ", '" <> part <> "')"
                            else "contains(" <> currentText <> ", '" <> part <> "')"
                        nextText = "substring-after(" <> currentText <> ", '" <> part <> "')"
                     in (preds <> [predicate], nextText)
                  (predicates, _) = foldl' buildPred ([], normText) (zip [0 ..] parts)
               in intercalate " and " predicates

    -- Partial visibility filter: catches @hidden, aria-hidden, and inline styles only.
    -- Cannot detect hiding via CSS classes or ancestor cascade.
    hiddenPred =
      "not(@hidden)"
        <> " and not(@aria-hidden='true')"
        <> " and not(contains(@style,'display:none'))"
        <> " and not(contains(@style,'visibility:hidden'))"

    depthPred = maybe "" (\d -> "[count(ancestor::*)<=" <> pack (show d) <> "]") mMaxDepth

data Protocol = HTTP | BiDi deriving (Show, Eq)

data InvalidLocator = MkInvalidLocator {loc :: Locator, description :: Text} deriving (Show, Eq, Ord)

instance Exception InvalidLocator

prepare :: (Text -> Locator) -> Protocol -> Locator -> Either InvalidLocator Locator
prepare defLoc proto =
  toEither . sortGroupChildLocs defLoc proto . flattenLoc
  where
    toEither :: Locator -> Either InvalidLocator Locator
    toEither l = case classify defLoc proto l of
      Invalid err -> Left err
      _ -> Right l

data Classification = IsXPath | IsXPathConvertable | IsCSS | IsBiDi | Invalid InvalidLocator | IsMixed deriving (Show, Eq, Ord)

mergeClassification :: Classification -> Classification -> Classification
mergeClassification i ii
  -- if equal return that info
  | i == ii = i
  -- if either invalid then invalid (first)
  | invalid i = i
  | invalid ii = ii
  -- else mixed
  | otherwise = IsMixed
  where
    invalid = \case
      Invalid _ -> True
      _ -> False

data LocPlus = MkLocPlus {accLoc :: Locator, info :: Classification}

classify :: (Text -> Locator) -> Protocol -> Locator -> Classification
classify defLoc proto =
  \case
    CSS {} -> IsCSS
    XPath {} -> IsXPath
    AllElms -> IsXPathConvertable
    ID {} -> IsXPathConvertable
    Class {} -> IsXPathConvertable
    Attribute {} -> IsXPathConvertable
    Tag {} -> IsXPathConvertable
    d@Default {value} ->
      let nxtLoc = defLoc value
          nestedDefault = hasDefault nxtLoc
       in if nestedDefault
            then Invalid $ MkInvalidLocator d "Invalid Default locator - Default locator cannot resolve to another Default"
            else classifyNxt nxtLoc
    Role {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> IsMixed -- requires double shot Xpath + post filter
    InnerText {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> IsMixed -- requires double shot Xpath + post filter
    c@BiDiContext {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> Invalid $ MkInvalidLocator c "BiDiContext locator cannot be used with HTTP protocol"
    Contains {container, contained} ->
      mergeClassification (classifyNxt container) (classifyNxt contained)
    All {elms} -> clasifyElms elms
    Any {elms} -> clasifyElms elms
    PostFilter {} -> IsMixed
  where
    classifyNxt :: Locator -> Classification
    classifyNxt = classify defLoc proto

    clasifyElms :: NonEmpty Locator -> Classification
    clasifyElms = foldl1' mergeClassification . fmap classifyNxt

sortGroupChildLocs :: (Text -> Locator) -> Protocol -> Locator -> Locator
sortGroupChildLocs defLoc proto =
  mapLocBottomUp sortGroupChildLocs'
  where
    sortGroupChildLocs' :: Locator -> Locator
    sortGroupChildLocs' l =
      case l of
        CSS {} -> l
        XPath {} -> l
        AllElms -> l
        ID {} -> l
        Class {} -> l
        Attribute {} -> l
        Tag {} -> l
        Default {} -> l
        Role {} -> l
        InnerText {} -> l
        BiDiContext {} -> l
        Contains {} -> l
        All {elms} -> All $ sortAndGroup All elms
        Any {elms} -> Any $ sortAndGroup Any elms
        PostFilter {} -> l
      where
        clasify' = classify defLoc proto
        sortAndGroup groupCons = regroup groupCons . sortLocList

        sortLocList :: NonEmpty Locator -> NonEmpty Locator
        sortLocList = sortBy (\a b -> compare (clasify' a) (clasify' b))

        regroup :: (NonEmpty Locator -> Locator) -> NonEmpty Locator -> NonEmpty Locator
        regroup constr elms =
          -- as the source is a non-empty list, fromJust is safe here as uncons will always return a head and tail
          uncurry (:|) . fromJust . uncons $ rewrapGroup <$> grouped
          where
            grouped = groupBy (\a b -> clasify' a == clasify' b) elms
            -- here careful of not ~ may need 2 construcots ???
            rewrapGroup :: NonEmpty Locator -> Locator
            rewrapGroup = \case
              l' :| [] -> l'
              multi -> constr multi

locatorToXPathPartial :: Locator -> Locator
locatorToXPathPartial = XPath . toXPathStr
  where
    -- \| Convert a Locator to a full XPath expression string.
    toXPathStr :: Locator -> Text
    toXPathStr loc = case loc of
      XPath {value} -> value
      AllElms -> "//*"
      ID {value} -> "//*[@id='" <> value <> "']"
      Class {value, matchType, caseSensitivity} ->
        "//*[" <> classPred value matchType caseSensitivity <> "]"
      Attribute {name, value, matchType, caseSensitivity} ->
        "//*[" <> attrPred name value matchType caseSensitivity <> "]"
      Tag {value} -> "//" <> value
      -- Contains: concatenate container and contained XPath — contained's leading // creates a
      -- descendant-axis step from the container result set, e.g. //form//input.
      Contains {container, contained} -> toXPathStr container <> toXPathStr contained
      All {elms} -> "//*[" <> intercalate " and " (toList $ toPred <$> elms) <> "]"
      Any {elms} -> "//*[" <> intercalate " or " (toList $ toPred <$> elms) <> "]"
      CSS {} -> locErr loc
      Default {} -> locErr loc
      Role {} -> locErr loc
      InnerText {} -> locErr loc
      BiDiContext {} -> locErr loc
      PostFilter {} -> locErr loc

    -- \| Convert a Locator to an XPath predicate expression for use inside [...].
    --   Combinators are recursively inlined; Parent uses the ancestor:: axis.
    toPred :: Locator -> Text
    toPred loc = case loc of
      XPath {value} ->
        -- Try to unwrap //*[pred] to get just the inner predicate; fall back to a boolean test.
        let stripped = T.stripPrefix "//*[" value
            unwrapped = stripped >>= \s -> if "]" `T.isSuffixOf` s then Just (T.dropEnd 1 s) else Nothing
         in maybe ("boolean(" <> value <> ")") id unwrapped
      AllElms -> "true()"
      ID {value} -> "@id='" <> value <> "'"
      Class {value, matchType, caseSensitivity} -> classPred value matchType caseSensitivity
      Attribute {name, value, matchType, caseSensitivity} -> attrPred name value matchType caseSensitivity
      Tag {value} -> "self::" <> value
      -- Contains as predicate: "I match contained AND I have an ancestor matching container"
      Contains {container, contained} ->
        toPred contained <> " and ancestor::*[" <> toPred container <> "]"
      All {elms} -> "(" <> intercalate " and " (toList $ toPred <$> elms) <> ")"
      Any {elms} -> "(" <> intercalate " or " (toList $ toPred <$> elms) <> ")"
      CSS {} -> locErr loc
      Default {} -> locErr loc
      Role {} -> locErr loc
      InnerText {} -> locErr loc
      BiDiContext {} -> locErr loc
      PostFilter {} -> locErr loc

    -- \| XPath predicate for CSS class matching.
    --   Full uses the space-padding token trick to match whole class names.
    --   Other match types operate directly on the raw @class attribute value.
    classPred :: Text -> MatchType -> CaseSensitivity -> Text
    classPred val mt cs =
      let classAttr = applyCS cs "@class"
          matchVal = lowerIfCI cs val
       in case mt of
            Full ->
              -- Pad the class attribute with spaces so each token is surrounded by spaces,
              -- then check for ' token '. Case folding applied inside concat.
              -- normalize-space() collapses multiple spaces and trims leading/trailing whitespace.
              "contains(concat(' ', normalize-space(" <> classAttr <> "), ' '), ' " <> matchVal <> " ')"
            Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
            Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
            Wildcard -> wildcardPred classAttr matchVal

    -- \| XPath predicate matching elements that have the named attribute satisfying the condition.
    --   @name@ is the HTML attribute name (e.g. "href", "data-testid").
    attrPred :: Text -> Text -> MatchType -> CaseSensitivity -> Text
    attrPred name val mt cs =
      let attrExpr = applyCS cs ("@" <> name)
          matchVal = lowerIfCI cs val
       in case mt of
            Full -> attrExpr <> "='" <> matchVal <> "'"
            Partial -> "contains(" <> attrExpr <> ", '" <> matchVal <> "')"
            Starts -> "starts-with(" <> attrExpr <> ", '" <> matchVal <> "')"
            Wildcard -> wildcardPred attrExpr matchVal

    -- \| Wrap an XPath string expression with a translate() call to fold it to lower-case,
    --   for CaseInsensitive matching.
    applyCS :: CaseSensitivity -> Text -> Text
    applyCS CaseSensitive expr = expr
    applyCS CaseInsensitive expr =
      "translate(" <> expr <> ", '" <> upperAlpha <> "', '" <> lowerAlpha <> "')"

    lowerIfCI :: CaseSensitivity -> Text -> Text
    lowerIfCI CaseSensitive v = v
    lowerIfCI CaseInsensitive v = toLower v

    -- \| Build a wildcard predicate from a normalised text expression and pattern.
    --   Mirrors the logic in innerTextToXPath's buildWildcardPredicate.
    wildcardPred :: Text -> Text -> Text
    wildcardPred normText val =
      let parts = filter (not . T.null) $ splitOn "*" val
          startsWithWildcard = "*" `T.isPrefixOf` val
          endsWithWildcard = "*" `T.isSuffixOf` val
       in case parts of
            [] -> "true()" -- "*" or "**" etc. matches everything
            [single]
              | startsWithWildcard && endsWithWildcard ->
                  "contains(" <> normText <> ", '" <> single <> "')"
              | startsWithWildcard ->
                  "substring(" <> normText <> ", string-length(" <> normText <> ") - string-length('" <> single <> "') + 1) = '" <> single <> "'"
              | endsWithWildcard ->
                  "starts-with(" <> normText <> ", '" <> single <> "')"
              | otherwise -> normText <> "='" <> single <> "'"
            _ ->
              let buildP (preds, curText) (idx, part) =
                    let predicate =
                          if idx == (0 :: Int) && not startsWithWildcard
                            then "starts-with(" <> curText <> ", '" <> part <> "')"
                            else "contains(" <> curText <> ", '" <> part <> "')"
                        nextText = "substring-after(" <> curText <> ", '" <> part <> "')"
                     in (preds <> [predicate], nextText)
                  (predicates, _) = foldl' buildP ([], normText) (zip [0 ..] parts)
               in intercalate " and " predicates

    locErr :: Locator -> a
    locErr loc =
      error . unpack $
        "Locator "
          <> txt loc
          <> " conversion not implemented - should not be called - this is a library defect - check classify or locatorToXPathPartial"

-- | Fold over a Locator tree with an accumulator, similar to foldl.
--   Processes the parent node first (top-down), then recursively folds over children,
--   threading the accumulator through the entire tree.
--   Useful for counting, collecting, or accumulating information from the Locator tree.
foldLoc :: (a -> Locator -> a) -> a -> Locator -> a
foldLoc f acc loc =
  case loc of
    Contains p c -> foldLoc f (foldLoc f acc' p) c
    All locs -> foldList locs
    Any locs -> foldList locs
    -- WithOptions base _ -> foldLoc f acc' base
    PostFilter {} -> acc'
    _ -> acc' -- Leaf locators
  where
    acc' = f acc loc -- Apply function to parent first
    foldList = foldl' (foldLoc f) acc' . toList

-- | Fold over a Locator tree with an accumulator, bottom-up (post-order).
--   Recursively folds over children first, then applies the function to the current node.
--   Useful when the result at a node depends on the already-folded results of its children.
foldLocBottomUp :: (a -> Locator -> a) -> a -> Locator -> a
foldLocBottomUp f acc loc =
  case loc of
    Contains p c -> f (foldLocBottomUp f (foldLocBottomUp f acc p) c) loc
    All locs -> f (foldList locs) loc
    Any locs -> f (foldList locs) loc
    -- WithOptions base _ -> f (foldLocBottomUp f acc base) loc
    PostFilter {} -> f acc loc
    _ -> f acc loc -- Leaf locators
  where
    foldList = foldl' (foldLocBottomUp f) acc . toList

-- | Map over a Locator tree bottom-up (post-order).
--   Recursively transforms children first, rebuilds the node with the new children,
--   then applies the function to the reconstructed node.
--   Useful for rewriting or normalising a Locator tree.
mapLocBottomUp :: (Locator -> Locator) -> Locator -> Locator
mapLocBottomUp f loc = f $
  case loc of
    Contains p c -> Contains (recurse p) (recurse c)
    All locs -> All $ recurseMap locs
    Any locs -> Any $ recurseMap locs
    _ -> loc -- Leaf locators and Predicate
  where
    recurse = mapLocBottomUp f
    recurseMap = fmap (mapLocBottomUp f)

-- | Returns 'True' if the predicate holds for any node in the locator tree.
anyLoc :: (Locator -> Bool) -> Locator -> Bool
anyLoc p = foldLoc (\acc loc -> acc || p loc) False

-- | Returns 'True' if the locator tree contains any node that is invalid
--   for the given protocol.
hasInvalidLoc :: (Text -> Locator) -> Protocol -> Locator -> Bool
hasInvalidLoc defLoc proto =
  anyLoc
    ( \l -> case classify defLoc proto l of
        Invalid _ -> True
        _ -> False
    )

-- | Returns 'True' if a 'Default' constructor appears anywhere within the locator tree.
hasDefault :: Locator -> Bool
hasDefault = 
  anyLoc $
    \case 
       Default {} -> True
       _ -> False

-- | Recursively flattens and simplifies Match* locators while maintaining logical correctness.
-- Flattens nested Match* of the same type and applies De Morgan's laws where applicable.
flattenLoc :: Locator -> Locator
flattenLoc = \case
  -- Flatten All: All [All [a,b], c] -> All [a,b,c]
  All locs ->
    let reduced = flattenLoc <$> locs
        flattened = concatMap flattenAll reduced
     in case flattened of
          [single] -> single
          (x : xs) -> All (x :| xs)
          [] -> error "flattenLoc: All produced empty list (impossible with NonEmpty input)"
    where
      flattenAll (All xs) = toList xs
      flattenAll x = [x]

  -- Flatten Any: Any [Any [a,b], c] -> Any [a,b,c]
  Any locs ->
    let reduced = flattenLoc <$> locs
        flattened = concatMap flattenAny reduced
     in case flattened of
          [single] -> single
          (x : xs) -> Any (x :| xs)
          [] -> error "flattenLoc: Any produced empty list (impossible with NonEmpty input)"
    where
      flattenAny (Any xs) = toList xs
      flattenAny x = [x]

  -- Recurse into other composite locators
  Contains p c -> Contains (flattenLoc p) (flattenLoc c)
  -- WithOptions base opts -> WithOptions (flattenLoc base) opts
  -- Leaf locators and Predicate have no children to recurse into
  other -> other

upperAlpha :: Text
upperAlpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowerAlpha :: Text
lowerAlpha = "abcdefghijklmnopqrstuvwxyz"