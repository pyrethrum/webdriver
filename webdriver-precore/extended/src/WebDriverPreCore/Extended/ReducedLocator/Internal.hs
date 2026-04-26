module WebDriverPreCore.Extended.ReducedLocator.Internal
  ( ReducedLoc (..),
    ReducedHttpLoc (..),
    LeafLoc (..),
    BiDiNativeLoc (..),
    CombinatorLoc (..),
    PostFilterLoc (..),
    BiDiOnlyLeafLoc (..),
    isXPath,
    toHttpLocator,
    prepareSimplify,
  )
where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import Prelude

data LeafLoc
  = -- universal
    CSS {value :: Text}
  | XPath {value :: Text}
  | -- bidi native locators that can be approximated in HTTP
    BiDiNative {loc :: BiDiNativeLoc}
  deriving
    ( Show,
      Eq
    )

data PostFilterLoc a = PostFilter {predicate :: LI.Predicate, locator :: a}
  deriving
    ( Show,
      Eq
    )

data CombinatorLoc a
  = Contains {container :: a, contained :: a}
  | All {elms :: NonEmpty a}
  | Any {elms :: NonEmpty a}
  deriving
    ( Show,
      Eq
    )

data BiDiNativeLoc
  = Role {role :: LI.RoleLocator}
  | InnerText
      { value :: Text,
        matchType :: LI.MatchType,
        caseSesnsitivity :: LI.CaseSensitivity,
        maxDepth :: Maybe Word8
      }
  deriving
    ( Show,
      Eq
    )

data BiDiOnlyLeafLoc
  = BiDiContext {context :: BrowsingContext}
  deriving
    ( Show,
      Eq
    )

-- | Simplified/resolved form of 'LI.Locator', where leaf locators expressible
--   as XPath have been folded in and 'LI.Default' has been resolved.
--   Produced by 'prepareSimplify'.
data ReducedLoc
  = Leaf LeafLoc
  | PostFilterLoc (PostFilterLoc ReducedLoc)
  | Combintor (CombinatorLoc ReducedLoc)
  | BiDiOnlyLeaf BiDiOnlyLeafLoc
  deriving
    ( Show,
      Eq
    )

-- | Simplified/resolved form of 'LI.Locator', where leaf locators expressible
--   as XPath have been folded in and 'LI.Default' has been resolved.
--   Produced by 'prepareSimplify'.
data ReducedHttpLoc
  = LeafHttp LeafLoc
  | PostFilterHttpLoc (PostFilterLoc ReducedHttpLoc)
  | CombintorHttp (CombinatorLoc ReducedHttpLoc)
  deriving
    ( Show,
      Eq
    )

toHttpLocator :: ReducedLoc -> Either LI.InvalidLocator ReducedHttpLoc
toHttpLocator = \case
  Leaf cl -> Right $ LeafHttp cl
  PostFilterLoc (PostFilter {predicate, locator}) ->
      PostFilterHttpLoc . PostFilter predicate <$> toHttpLocator locator
  Combintor cl ->
      CombintorHttp <$> case cl of
        Contains {container, contained} -> Contains <$> toHttpLocator container <*> toHttpLocator contained
        ccl -> case ccl of
          All {} -> nested All
          Any {} -> nested Any

        where 
          nested ctr = ctr <$> traverse toHttpLocator cl.elms
  BiDiOnlyLeaf (BiDiContext {context}) ->
    Left $ LI.MkInvalidLocator (LI.BiDiContext {context}) "BiDi-only locator cannot be used with HTTP protocol"



isXPath :: ReducedLoc -> Bool
isXPath = \case
  Leaf XPath {} -> True
  _ -> False


prepareSimplify :: (Text -> LI.Locator) -> LI.Protocol -> LI.Locator -> Either LI.InvalidLocator ReducedLoc
prepareSimplify defLoc proto l =
  simplify <$> xPathSub defLoc proto l
  where
    simplify :: LI.Locator -> ReducedLoc
    simplify = \case
      LI.CSS {..} -> Leaf CSS {..}
      LI.XPath {..} -> Leaf XPath {..}
      LI.Role {..} -> Leaf . BiDiNative $ Role {..}
      LI.InnerText {..} -> Leaf . BiDiNative $ InnerText {..}
      LI.BiDiContext {..} -> BiDiOnlyLeaf BiDiContext {..}
      LI.PostFilter {predicate, locator} -> PostFilterLoc $ PostFilter {predicate, locator = simplify locator}
      LI.Contains {container, contained} -> Combintor $ Contains {container = simplify container, contained = simplify contained}
      LI.All {elms} -> Combintor . All $ simplify <$> elms
      LI.Any {elms} -> Combintor . Any $ simplify <$> elms
      LI.AllElms -> shouldNotExistAfterXPathSub "AllElms"
      LI.ID {} -> shouldNotExistAfterXPathSub "ID"
      LI.Class {} -> shouldNotExistAfterXPathSub "Class"
      LI.Attribute {} -> shouldNotExistAfterXPathSub "Attribute"
      LI.Tag {} -> shouldNotExistAfterXPathSub "Tag"
      LI.Default {} -> shouldNotExistAfterXPathSub "Default"
    shouldNotExistAfterXPathSub name = error . T.unpack $ name <> " should not exist after xPathSub - this is a library defect"

xPathSub :: (Text -> LI.Locator) -> LI.Protocol -> LI.Locator -> Either LI.InvalidLocator LI.Locator
xPathSub defLoc proto l =
  -- after prepare comboinator ocators such as Parent, All, Any are already be grouped correctly with all XPaths together,
  --  so we can just recursively convert to XPath if the grup is XPath Convertable
  coreToFullXPath . convertXPath <$> LI.prepare defLoc proto l
  where
    convertXPath :: LI.Locator -> LI.Locator
    convertXPath loc =
      case loc of
        LI.CSS {} -> loc
        LI.InnerText {} -> loc
        LI.Role {} -> loc
        LI.BiDiContext {} -> loc
        LI.PostFilter {} -> loc
        LI.XPath {} -> xpathLoc
        LI.AllElms -> xpathLoc
        LI.ID {} -> xpathLoc
        LI.Class {} -> xpathLoc
        LI.Attribute {} -> xpathLoc
        LI.Tag {} -> xpathLoc
        LI.Default {value} -> convertXPath (defLoc value)
        LI.Contains {container, contained} -> tryConvert $ LI.Contains (convertXPath container) (convertXPath contained)
        LI.All {elms} -> tryConvert $ LI.All (convertXPath <$> elms)
        LI.Any {elms} -> tryConvert $ LI.Any (convertXPath <$> elms)
      where
        xpathLoc = toXPathCore loc
        convertable l' = LI.classify defLoc proto l' == LI.IsXPath
        tryConvert :: LI.Locator -> LI.Locator
        tryConvert l' =
          if convertable l'
            then
              toXPathCore l'
            else
              l'

coreToFullXPath :: LI.Locator -> LI.Locator
coreToFullXPath l' =
  LI.mapLocBottomUp prefixSuffix l'
  where
    prefixSuffix :: LI.Locator -> LI.Locator
    prefixSuffix = \case
      LI.XPath {value} -> LI.XPath $ if T.null value then "//*" else "//*[" <> value <> "]"
      other -> other

toXPathCore :: LI.Locator -> LI.Locator
toXPathCore = LI.XPath . toXPathCoreTxt
  where
    -- \| Convert a Locator to an XPath predicate expression for use inside [...].
    --   Combinators are recursively inlined; Parent uses the ancestor:: axis.
    toXPathCoreTxt :: LI.Locator -> Text
    toXPathCoreTxt loc =
      case loc of
        LI.XPath {value} ->
          -- Try to unwrap //*[pred] to get just the inner predicate; fall back to a boolean test.
          let stripped = T.stripPrefix "//*[" value
              unwrapped = stripped >>= \s -> if "]" `T.isSuffixOf` s then Just (T.dropEnd 1 s) else Nothing
           in maybe ("boolean(" <> value <> ")") id unwrapped
        LI.AllElms -> "true()"
        LI.ID {value} -> "@id='" <> value <> "'"
        LI.Class {value, matchType, caseSensitivity} -> classToXPathCoreTxt value matchType caseSensitivity
        LI.Attribute {value, matchType, caseSensitivity} -> attrToXPathCoreTxt value matchType caseSensitivity
        LI.Tag {value} -> "self::" <> value
        -- Contains as predicate: "I match contained AND I have an ancestor matching container"
        LI.Contains {container, contained} ->
          toXPathCoreTxt contained <> " and ancestor::*[" <> toXPathCoreTxt container <> "]"
        LI.All {elms} -> elmsTxt "and" elms
        LI.Any {elms} -> elmsTxt "or" elms
        LI.CSS {} -> locErr loc
        LI.Default {} -> locErr loc
        LI.Role {} -> locErr loc
        LI.InnerText {} -> locErr loc
        LI.BiDiContext {} -> locErr loc
        LI.PostFilter {} -> locErr loc
      where
        elmsTxt conjunctive elms = "(" <> T.intercalate (" " <> conjunctive <> " ") (toList $ toXPathCoreTxt <$> elms) <> ")"

    -- \| XPath predicate for CSS class matching.
    --   Full uses the space-padding token trick to match whole class names.
    --   Other match types operate directly on the raw @class attribute value.
    classToXPathCoreTxt :: Text -> LI.MatchType -> LI.CaseSensitivity -> Text
    classToXPathCoreTxt val mt cs =
      let classAttr = applyCS cs "@class"
          matchVal = lowerIfCI cs val
       in case mt of
            LI.Full ->
              -- Pad the class attribute with spaces so each token is surrounded by spaces,
              -- then check for ' token '. Case folding applied inside concat.
              "contains(concat(' ', " <> classAttr <> ", ' '), ' " <> matchVal <> " ')"
            LI.Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
            LI.Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
            LI.Wildcard -> wildcardToXPathCoreTxt classAttr matchVal

    -- \| XPath predicate matching elements that have any attribute satisfying the condition.
    --   Uses @*[...] predicate syntax so the condition is applied to each attribute node.
    attrToXPathCoreTxt :: Text -> LI.MatchType -> LI.CaseSensitivity -> Text
    attrToXPathCoreTxt val mt cs =
      let attrExpr = applyCS cs "." -- '.' refers to the attribute node's string value
          matchVal = lowerIfCI cs val
       in case mt of
            LI.Full -> "@*[" <> attrExpr <> "='" <> matchVal <> "']"
            LI.Partial -> "@*[contains(" <> attrExpr <> ", '" <> matchVal <> "')]"
            LI.Starts -> "@*[starts-with(" <> attrExpr <> ", '" <> matchVal <> "')]"
            LI.Wildcard -> "@*[" <> wildcardToXPathCoreTxt attrExpr matchVal <> "]"

    -- \| Wrap an XPath string expression with a translate() call to fold it to lower-case,
    --   for CaseInsensitive matching.
    applyCS :: LI.CaseSensitivity -> Text -> Text
    applyCS cs t = case cs of
      LI.CaseSensitive -> t
      LI.CaseInsensitive -> "translate(" <> t <> ", '" <> LI.upperAlpha <> "', '" <> LI.lowerAlpha <> "')"

    lowerIfCI :: LI.CaseSensitivity -> Text -> Text
    lowerIfCI cs t = case cs of
      LI.CaseSensitive -> t
      LI.CaseInsensitive -> T.toLower t

    -- \| Build a wildcard predicate from a normalised text expression and pattern.
    --   Mirrors the logic in innerTextToXPath's buildWildcardPredicate.
    wildcardToXPathCoreTxt :: Text -> Text -> Text
    wildcardToXPathCoreTxt normText val =
      let parts = filter (not . T.null) $ T.splitOn "*" val
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
               in T.intercalate " and " predicates

    locErr :: LI.Locator -> a
    locErr loc =
      error . T.unpack $
        "Locator\n"
          <> txt loc
          <> "\nconversion not implemented - should not be called - this is a library defect - check classify or locatorToXPathPartial"
