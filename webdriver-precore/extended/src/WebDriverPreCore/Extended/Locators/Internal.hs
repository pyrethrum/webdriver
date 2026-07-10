{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module WebDriverPreCore.Extended.Locators.Internal (
    CompoundLocator(..),
    Locator(..),
    HttpLoc(..),
    Predicate(..),
    InvalidLocator(..),
    Protocol(..),

    -- * Re-exports from Internal
    AriaRole (..),
    MatchType (..),
    CaseSensitivity(..),
    RoleLocator(..),
    roleLabelText,
    transform,
    xPathRelativePrefix 
) where

import Control.Exception (Exception)
import Data.Functor.Identity (Identity (..))
import Data.List (nub)
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Text (Text, intercalate, pack, splitOn, toLower, unpack)
import Data.Text qualified as T
import Data.Word (Word8)
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext, NodeProperties)
import Prelude
import Control.Monad ((>=>))
import Data.Function ((&))

-----------------------------------------------------------------------------
-- Constants
-----------------------------------------------------------------------------

-- | Relative XPath prefix for all library-generated (derived) XPath expressions.
--
-- Uses './/'' (relative descendant-or-self) instead of '//' (absolute) because:
--
-- 1. **Semantic correctness in Contains**: When a locator appears inside a Contains
--    combinator, the contained locator should search relative to the container element,
--    not from the document root. Using './/'' ensures proper scoping.
--
-- 2. **Works at top level too**: When called on the driver (document root context),
--    './/tag' and '//tag' produce identical results, so using './/'' everywhere is safe.
--
-- 3. **Simplifies locateFromElementHttp**: When searching from a base element,
--    relative paths work correctly without needing text manipulation in setBaseElement
--    (Locate.hs). Absolute '//' paths would escape the intended container scope.
--
-- 4. **User-supplied XPath unchanged**: This only affects library-derived XPath from
--    constructors like Tag, Class, Attribute, etc. User-supplied XPath via the 'xpath'
--    constructor is passed through unchanged, giving users full control when needed.
xPathRelativePrefix :: Text
xPathRelativePrefix = ".//"

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

-- | LocatorI an intermediate type representing leaf locators.
--   Compound locators are represented by 'CompoundLocator'.
data LocatorI
  = 
    CSSI {value :: Text}
  | XPathI {value :: Text}
  | XPathID {
      tagM :: Maybe Text, 
      body :: Text
      } 
  | TagI {tag :: Text} 
  | RoleI {roleSpec :: RoleLocator, xpath :: Text}
  -- | -- exclusive
  --   -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
  --   BiDiContextI {context :: BrowsingContext}
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )

{-
data LocatorFinal
  = 
    CSSF {value :: Text}
  | XPathF {value :: Text} 
  | RoleF {xpath :: Text}
  | InnerTextF
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity,
        maxDepth :: Maybe Word8
      }
  | -- exclusive
    -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
    BiDiContextF {context :: BrowsingContext}
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )
  -}

data HttpLoc
  = 
    CSSHttp {value :: Text}
  | XPathHttp {value :: Text} 
  | RoleHttp {roleSpec :: RoleLocator, xpath :: Text}
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )

-- | CompoundLocator represents the tree structure of composed locators.
data CompoundLocator a
  = Leaf {getLeaf :: a}
  | ContainsI {container :: CompoundLocator a, contained :: CompoundLocator a}
  | AllI {elms :: NonEmpty (CompoundLocator a)}
  | AnyI {elms :: NonEmpty (CompoundLocator a)}
  | PostFilterI {predicate :: Predicate, locator :: CompoundLocator a}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

transform :: (Text -> Locator) -> Locator -> Either InvalidLocator (CompoundLocator HttpLoc)
transform defLoc loc = do
  locI <- toIntermediate defLoc loc
  simplified <- simplify locI
  Right $ derivedAndTagsToXPath simplified
  where
    simplify :: CompoundLocator LocatorI -> Either InvalidLocator (CompoundLocator LocatorI)
    simplify current = do
      merged <- mergeContiguous loc $ unnestAnysAlls current
      tagged <- distributeTagsInAll loc merged
      let unwrapped = unwrapSingletonCombinators tagged
      if unwrapped == current
        then pure current
        else simplify unwrapped

-----------------------------------------------------------------------------
-- Phase 1: Initial conversion (Locator → CompoundLocator LocatorI)
-----------------------------------------------------------------------------

toIntermediate :: (Text -> Locator) -> Locator -> Either InvalidLocator (CompoundLocator LocatorI)
toIntermediate defLoc loc = 
  case loc of
    -- fallable conversions
    Attribute {name, value, matchType, caseSensitivity}
      | T.null name || T.null value ->
          failLocator "Attribute locator has empty name or value"
      | otherwise ->
          leaf $ XPathID {tagM = Nothing, body = attrPredX name value matchType caseSensitivity}
    Default {value} ->
      let resolved = defLoc value
      in if hasDefault resolved
        then failLocator "Default locator cannot resolve to another Default"
        else toIntermediate defLoc resolved
    BiDiContext {} -> 
      failLocator "BiDiContext locator cannot be used with HTTP protocol"
    Contains {container, contained} ->
      ContainsI <$> toIntermediate defLoc container <*> toIntermediate defLoc contained
    All {elms} ->
      AllI <$> traverse (toIntermediate defLoc) elms
    Any {elms} ->
      AnyI <$> traverse (toIntermediate defLoc) elms
    PostFilter {predicate, locator} ->
      PostFilterI predicate <$> toIntermediate defLoc locator
    -- leaf conversions
    _ -> leaf $ case loc of
          CSS {value} -> CSSI {value}
          XPath {value} -> XPathI {value}
          AllElms -> XPathID {tagM = Nothing, body = "true()"}
          ID {value} -> XPathID {tagM = Nothing, body = "@id='" <> value <> "'"}
          Class {value, matchType, caseSensitivity} ->
            XPathID {tagM = Nothing, body = classPredX value matchType caseSensitivity}
          Tag {value} -> TagI {tag = value}
          Role {role} -> RoleI {roleSpec = role, xpath = roleToXPath role}
          InnerText {value, matchType, caseSensitivity, maxDepth} ->
            XPathID {tagM = Nothing, body = innerTextPredX value caseSensitivity matchType maxDepth}
  where 
    leaf = Right . Leaf
    failLocator msg = Left $ MkInvalidLocator loc msg

-----------------------------------------------------------------------------
-- Phase 2: Simplify loop (fixed-point)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- 2a. Flatten nested AllI/AnyI
-----------------------------------------------------------------------------

unnestAnysAlls :: CompoundLocator LocatorI -> CompoundLocator LocatorI
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
  Leaf a -> Leaf a

-----------------------------------------------------------------------------
-- 2b. Combine contiguous XPathIDs
-----------------------------------------------------------------------------
mergeContiguous :: Locator -> CompoundLocator LocatorI -> Either InvalidLocator (CompoundLocator LocatorI)
mergeContiguous srcLoc li = mergeAnys <$> mergeAlls srcLoc li

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

mergeAlls :: Locator -> CompoundLocator LocatorI -> Either InvalidLocator (CompoundLocator LocatorI)
mergeAlls srcLoc = 
  mapCompoundLocBottomUpM (\case 
   AllI elms -> AllI <$> mergeAllElms elms
   other -> Right other)
 where
   mergeAllElms :: NonEmpty (CompoundLocator LocatorI) -> Either InvalidLocator (NonEmpty (CompoundLocator LocatorI))
   mergeAllElms = \case
       (Leaf (XPathID tm1 b1) :| Leaf (XPathID tm2 b2) : rest) -> 
         do 
          tag <- mergeTags srcLoc tm1 tm2
          mergeAllElms $ Leaf (XPathID {tagM = tag, body = bracket b1 <> " and " <> bracket b2}) :| rest 
       x -> Right x

mergeAnys ::  CompoundLocator LocatorI -> CompoundLocator LocatorI
mergeAnys  = 
  mapCompoundLocBottomUp (\case 
   AnyI elms -> AnyI $ mergeAnyElms elms
   other -> other)
 where
   mergeAnyElms :: NonEmpty (CompoundLocator LocatorI) -> NonEmpty (CompoundLocator LocatorI)
   mergeAnyElms = \case
       -- Merge XPathIDs with identical tags
       ((Leaf (XPathID tm1 b1)) :| (Leaf (XPathID tm2 b2)) : rest) ->
        -- if tags are identical then they can be merged 
        -- Nothing cannot be merged with Just because Nothing represents any tag
        if tm1 == tm2 then 
          mergeAnyElms $ Leaf (XPathID {tagM = tm1, body = bracket b1 <> " or " <> bracket b2}) :| rest
        else
          mergeAnyElms $ Leaf (XPathID Nothing xpath1or2Txt) :| rest
        where
            xpath1or2Txt = xpathTxt1 <> " or " <> xpathTxt2
            xpathTxt1 = xpathTxt tm1 b1
            xpathTxt2 = xpathTxt tm2 b2
            xpathTxt tagM body =
              tagM & maybe 
                (bracket body)
                \t -> "(" <> selfTag t <> " and " <> bracket body <> ")"
              

                   
       -- Tag overrides XPathID with the same tag (eg div or div && class => div as all div && class will satisfy div)
       (Leaf (TagI tag) :| Leaf (XPathID {tagM = Just tm}) : rest)  | tag == tm ->
          mergeAnyElms $ Leaf (TagI tag) :| rest

       -- as above flipped
       (Leaf (XPathID {tagM = Just tm}) :| Leaf (TagI tag) : rest) | tag == tm ->
         mergeAnyElms $ Leaf (TagI tag) :| rest
       
       -- TagI followed by XPathID with wildcard (no tag)
       -- Any(Tag "div", Class "foo") → .//*[self::div or (...)]
       (Leaf (TagI tag) :| Leaf (XPathID {tagM = Nothing, body}) : rest) ->
         mergeAnyElms $ Leaf (XPathID {tagM = Nothing, body = selfTag tag <> " or " <> bracket body}) :| rest
       
       -- as above flipped
       (Leaf (XPathID {tagM = Nothing, body}) :| Leaf (TagI tag) : rest) ->
         mergeAnyElms $ Leaf (XPathID {tagM = Nothing, body = bracket body <> " or " <> selfTag tag}) :| rest
       
       x -> x
    where 
      selfTag :: Text -> Text
      selfTag tag = "self::" <> tag
           


  

-----------------------------------------------------------------------------
-- 2c. Assign tags
-----------------------------------------------------------------------------

tagTxt :: CompoundLocator LocatorI -> Maybe Text
tagTxt  = \case 
  Leaf (TagI t) -> Just t
  _ -> Nothing

isTagI :: CompoundLocator LocatorI -> Bool
isTagI  = isJust . tagTxt

isNotTagI :: CompoundLocator LocatorI -> Bool
isNotTagI = not . isTagI

-- | Collect all tag values from TagI and tagged XPathID leaves at all depths,
--   including inside nested AnyI/ContainsI/AllI/PostFilterI combinators.
collectTags :: CompoundLocator LocatorI -> [Text]
collectTags = \case
  Leaf (TagI t) -> [t]
  Leaf (XPathID {tagM = Just t}) -> [t]
  Leaf _ -> []
  ContainsI c d -> collectTags c ++ collectTags d
  AllI xs -> toList xs >>= collectTags
  AnyI xs -> toList xs >>= collectTags
  PostFilterI _ l -> collectTags l

-- copy tags from TagI and XPathID to all reachable XPathID descendants, and remove the TagI if possible.
distributeTagsInAll :: Locator -> CompoundLocator LocatorI -> Either InvalidLocator (CompoundLocator LocatorI)
distributeTagsInAll srcLocator = 
    mapCompoundLocBottomUpM (\case
      AllI elms -> AllI <$> distributeTagsToAllElms srcLocator elms
      other -> Right other)

-- | 2c-ii + 2c-iii: Process TagI constructors within an AllI.
distributeTagsToAllElms :: Locator -> NonEmpty (CompoundLocator LocatorI) -> Either InvalidLocator (NonEmpty (CompoundLocator LocatorI))
distributeTagsToAllElms srcLocator elms = do
  let tagVals = nub $ toList elms >>= collectTags
      -- Tags from direct children (TagI or tagged XPathID only, no recursion).
      -- Used to decide whether to distribute: if all tags come from nested
      -- combinators (AnyI/ContainsI etc.) with no direct child tag source,
      -- skip distribution to avoid corrupting unrelated branches.
      directTagVals = nub . catMaybes $ toList elms >>= \case
        Leaf (TagI t) -> [Just t]
        Leaf (XPathID {tagM = Just t}) -> [Just t]
        _ -> [Nothing]
  
  -- 2c-ii: Contradictory tag detection (all descendants)
  case tagVals of
     -- no tags, nothing to check
    [] -> pure elms 
     -- contradictory tags
    _ : _ : _ -> Left . MkInvalidLocator srcLocator $ 
      "Contradictory tags in All combinator: " <> T.intercalate ", " tagVals
     -- singleton tag -> check if removable
    [t] ->
      case directTagVals of
        -- Tag only from nested sources (Any/Contains/etc.), not a direct constraint.
        -- Don't distribute — it would corrupt unrelated branches.
        [] -> pure elms
        _ -> case result of
          -- empty list wont happen unless there is a bug
          [] -> error "processAllI: empty result"
          x : xs -> Right (x :| xs)
      where
          elements = toList elms
          distributed = fmap (distributeTagToLeaf t) <$> elms
          canRemove = all allDescendantsXPathIDOrTag distributed && not (all isTagI elements)
          result = if canRemove 
                   then filter isNotTagI $ toList distributed
                   else toList distributed
  where
    -- | Set tagM = Just t on XPathID leaves, no-op on all other leaves.
    --   Since 'LocatorI' is now leaf-only, we only need to handle XPathID.
    distributeTagToLeaf :: Text -> LocatorI -> LocatorI
    distributeTagToLeaf t = \case
      XPathID _ body -> XPathID {tagM = Just t, body}
      other -> other

    -- | Check if a leaf is an XPathID or TagI.
    --   Since 'LocatorI' is leaf-only, this just checks the leaf itself.
    allDescendantsXPathIDOrTag :: CompoundLocator LocatorI -> Bool
    allDescendantsXPathIDOrTag = \case
      Leaf (XPathID {}) -> True
      Leaf (TagI {}) -> True
      _ -> False


-----------------------------------------------------------------------------
-- 2d. Unwrap single-child combinators
-----------------------------------------------------------------------------

unwrapSingletonCombinators :: CompoundLocator LocatorI -> CompoundLocator LocatorI
unwrapSingletonCombinators = mapCompoundLocBottomUp $ \case
  AllI (x :| []) -> x
  AnyI (x :| []) -> x
  other -> other

-----------------------------------------------------------------------------
-- Phase 3: Final conversion
-----------------------------------------------------------------------------

derivedAndTagsToXPath :: CompoundLocator LocatorI -> CompoundLocator HttpLoc
derivedAndTagsToXPath = convertTagsXPathIDs . convertContains

-- | Merge adjacent user-provided XPath locators in Contains at the LocatorI stage.
--   This avoids text manipulation since the relative prefix ".//", brackets, etc.
--   haven't been added yet. Only handles XPathI (user-provided XPath).
convertContains :: CompoundLocator LocatorI -> CompoundLocator LocatorI
convertContains = mapCompoundLocBottomUp $ \case
  -- Merge two user-provided XPaths
  ContainsI (Leaf (XPathI {value = containerXPath})) (Leaf (XPathI {value = containedXPath})) ->
    Leaf $ XPathI {value = containerXPath <> "//" <> containedXPath}
  -- Keep other combinations as ContainsI - they'll be handled after conversion to HttpLoc
  other -> other

-- | Convert LocatorI leaves to LocatorFinal leaves — uses 'fmap' via 'Functor'.
convertTagsXPathIDs :: CompoundLocator LocatorI -> CompoundLocator HttpLoc
convertTagsXPathIDs = fmap $ \case
  XPathID {tagM, body} ->
    XPathHttp {value = xPathIDTxt tagM body}
  TagI {tag} -> XPathHttp {value = xPathRelativePrefix <> tag}
  CSSI {..} -> CSSHttp {..}
  XPathI {..} -> XPathHttp {..}
  RoleI {..} -> RoleHttp {..}

xPathIDTxt :: Maybe Text -> Text -> Text
xPathIDTxt tagM body = xPathRelativePrefix <> fromMaybe "*" tagM <> "[" <> body <> "]"

-----------------------------------------------------------------------------
-- Top-level XPath predicate helpers (extracted from locatorToXPathPartial)
-----------------------------------------------------------------------------

-- | XPath predicate for CSS class matching.
classPredX :: Text -> MatchType -> CaseSensitivity -> Text
classPredX val mt cs =
  case mt of
    Full ->
      "contains(concat(' ', normalize-space(" <> classAttr <> "), ' '), ' " <> matchVal <> " ')"
    Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
    Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
    Wildcard -> wildcardPredX classAttr matchVal
  where
    classAttr = applyCaseSensitivity "@class" cs
    matchVal = lwrCaseInsensitive val cs

-- | XPath predicate for named attribute matching.
attrPredX :: Text -> Text -> MatchType -> CaseSensitivity -> Text
attrPredX name val mt cs =
  case mt of
    Full -> attrExpr <> "='" <> matchVal <> "'"
    Partial -> "contains(" <> attrExpr <> ", '" <> matchVal <> "')"
    Starts -> "starts-with(" <> attrExpr <> ", '" <> matchVal <> "')"
    Wildcard -> wildcardPredX attrExpr matchVal
  where
    attrExpr = applyCaseSensitivity ("@" <> name) cs
    matchVal = lwrCaseInsensitive val cs

-- | XPath predicate for inner text matching (without the leading '//*').
--   Returns predicate brackets that can be used in XPathID body.
innerTextPredX :: Text -> CaseSensitivity -> MatchType -> Maybe Word8 -> Text
innerTextPredX val cs matchType mMaxDepth =
  depthPred <> "[" <> hiddenPred <> " and " <> textPred <> "]"
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

applyCaseSensitivity :: Text -> CaseSensitivity -> Text
applyCaseSensitivity expr = \case
  CaseSensitive -> expr
  CaseInsensitive -> "translate(" <> expr <> ", '" <> upperAlpha <> "', '" <> lowerAlpha <> "')"

lwrCaseInsensitive :: Text -> CaseSensitivity -> Text
lwrCaseInsensitive v = \case
  CaseSensitive -> v
  CaseInsensitive -> toLower v

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

-- | Map over a 'CompoundLocator' tree bottom-up, with monadic effects.
--   Recursively transforms children first, then applies the function to the
--   reconstructed node.  Short-circuits on the first failure for instances
--   that support it (e.g. 'Either', 'Maybe').
mapCompoundLocBottomUpM :: Monad m => (CompoundLocator a -> m (CompoundLocator a)) -> CompoundLocator a -> m (CompoundLocator a)
mapCompoundLocBottomUpM f = recurse >=> f
  where
    mapf = mapCompoundLocBottomUpM f
    recurse = \case
      ContainsI c d -> ContainsI <$> mapf c <*> mapf d
      AllI elms -> AllI <$> traverse mapf elms
      AnyI elms -> AnyI <$> traverse mapf elms
      PostFilterI p l -> PostFilterI p <$> mapf l
      leaf@(Leaf _) -> pure leaf
 
-- | Map over a 'CompoundLocator' tree bottom-up.
--   Expressed via 'mapCompoundLocBottomUpM' using the 'Identity' monad.
mapCompoundLocBottomUp :: (CompoundLocator a -> CompoundLocator a) -> CompoundLocator a -> CompoundLocator a
mapCompoundLocBottomUp f = runIdentity . mapCompoundLocBottomUpM (Identity . f)

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

roleLabelText :: AriaRole -> Text
roleLabelText = toLower . pack . show

roleToXPath :: RoleLocator -> Text
roleToXPath = \case
  RoleFull {role, name} -> xPathRelativePrefix <> "*" <> role' role <> name' name
  RoleType {role} -> xPathRelativePrefix <> "*" <> role' role
  RoleName {name} -> xPathRelativePrefix <> "*[not(@role='presentation' or @role='none')]" <> name' name
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
    content = implicitRoleXPath r <> " or @role='" <> roleLabelText r <> "'"


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

data Protocol = HTTP | BiDi deriving (Show, Eq)

data InvalidLocator = MkInvalidLocator {loc :: Locator, description :: Text} deriving (Show, Eq, Ord)

instance Exception InvalidLocator

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

-- | Returns 'True' if the predicate holds for any node in the locator tree.
anyLoc :: (Locator -> Bool) -> Locator -> Bool
anyLoc p = foldLoc (\acc loc -> acc || p loc) False

-- | Returns 'True' if a 'Default' constructor appears anywhere within the locator tree.
hasDefault :: Locator -> Bool
hasDefault = 
  anyLoc $
    \case 
       Default {} -> True
       _ -> False

upperAlpha :: Text
upperAlpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowerAlpha :: Text
lowerAlpha = "abcdefghijklmnopqrstuvwxyz"