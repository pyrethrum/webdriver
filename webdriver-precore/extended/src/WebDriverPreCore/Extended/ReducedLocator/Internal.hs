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

import Data.List (foldl', nub)
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
  coreToFullXPath <$> (LI.prepare defLoc proto l >>= convertXPath)
  where
    convertXPath :: LI.Locator -> Either LI.InvalidLocator LI.Locator
    convertXPath loc =
      case loc of
        LI.CSS {} -> rLoc
        LI.InnerText {} -> rLoc
        LI.Role {} -> rLoc
        LI.BiDiContext {} -> rLoc
        LI.PostFilter {} -> rLoc
        LI.XPath {} -> xpathLoc
        LI.AllElms -> xpathLoc
        LI.ID {} -> xpathLoc
        LI.Class {} -> xpathLoc
        LI.Attribute {} -> xpathLoc
        LI.Tag {} -> xpathLoc
        LI.Default {value} -> convertXPath (defLoc value)
        LI.Contains {container, contained} ->
          -- Keep Contains structure, just convert sub-locators
          LI.Contains <$> (convertXPath container) <*> (convertXPath contained)
        LI.All {elms} -> LI.All <$> traverse convertXPath elms >>= tryConvert
        LI.Any {elms} -> LI.Any <$> traverse convertXPath elms >>= tryConvert
      where
        rLoc = Right loc
        xpathLoc = toXPathCore loc
        convertable l' = LI.classify defLoc proto l' == LI.IsXPath
        
        tryConvert :: LI.Locator -> Either LI.InvalidLocator LI.Locator
        tryConvert l' =
          if convertable l'
            then
              toXPathCore l'
            else
              Right l'

-- | Structured representation of an XPath location step: //tag[pred1][pred2]...
--   Using a structured ADT avoids the fragile wrap/unwrap of raw predicate text.
data XPathNode = MkXPathNode
  { tag :: Text
  , predicates :: [Text]
  }
  deriving (Show, Eq)

-- | Render an 'XPathNode' to a full XPath expression.
renderXPathNode :: XPathNode -> Text
renderXPathNode MkXPathNode {tag, predicates} =
  "//" <> tag <> foldMap (\p -> "[" <> p <> "]") predicates

coreToFullXPath :: LI.Locator -> LI.Locator
coreToFullXPath = LI.mapLocBottomUp prefixSuffix
  where
    prefixSuffix :: LI.Locator -> LI.Locator
    prefixSuffix = \case
      -- Preserve explicit XPath unions; parsing them as a single node drops branches.
      LI.XPath {value}
        | T.isInfixOf " | " value -> LI.XPath {value}
        | otherwise -> LI.XPath . renderXPathNode $ parseXPathNode value
      other -> other

-- | Parse a raw XPath value that may or may not have been produced by this
--   library into an 'XPathNode'.  Only the simple //tag[pred] form produced
--   internally is parsed structurally; anything else is preserved verbatim as
--   a single predicate on the wildcard step via boolean().
parseXPathNode :: Text -> XPathNode
parseXPathNode value =
  case T.stripPrefix "//" value of
    Nothing -> MkXPathNode {
      tag = "*", 
      predicates = ["boolean(" <> value <> ")"]
      }
    Just rest ->
      let (t, remainder) = T.break (== '[') rest
          preds = parsePreds remainder
       in MkXPathNode {
             tag = if T.null t then "*" else t, 
             predicates = preds
          }
  where
    parsePreds :: Text -> [Text]
    parsePreds txt'
      | T.null txt' = []
      | otherwise =
          case T.stripPrefix "[" txt' of
            Nothing -> []
            Just inner ->
              let (p, after) = T.breakOn "]" inner
               in p : parsePreds (T.drop 1 after)

toXPathCore :: LI.Locator -> Either LI.InvalidLocator LI.Locator
toXPathCore loc =
  case loc of
    LI.XPath {value}
      | T.isInfixOf " | " value -> Right LI.XPath {value}
    LI.Any {elms} -> do
      convertedBranches <- traverse toXPathCore elms
      let branches =
            toList $
              fmap
                (\case
                    LI.XPath {value} -> value
                    _ -> error "toXPathCore: expected XPath branch for LI.Any"
                )
                convertedBranches
      Right $ LI.XPath {value = T.intercalate " | " branches}
    _ -> LI.XPath . renderXPathNode <$> toXPathNode loc
  where
    -- | Convert a Locator to a structured 'XPathNode'.
    --   Tag sets the node-test; all other predicates accumulate in the predicate list.
    --   For combinators: All merges predicates (intersection), Any uses | union.
    toXPathNode :: LI.Locator -> Either LI.InvalidLocator XPathNode
    toXPathNode l = 
      case l of
        LI.All {elms} -> do
          nodes <- traverse toXPathNode elms
          let explicitTags = filter (/= "*") . toList $ (.tag) <$> nodes
              mergedPreds = concatMap (.predicates) (toList nodes)
          mergedTag <- case nub explicitTags of
            []  -> Right "*"
            [t] -> Right t
            xs  -> Left $ LI.MkInvalidLocator l ("Conflicting tags in All combinator - cannot convert to XPath: " <> txt xs)
          Right $ MkXPathNode {tag = mergedTag, predicates = mergedPreds}
        -- Any is handled at toXPathCore top-level to preserve XPath unions.
        LI.Any {} -> locErr l
        -- Contains: should not reach here since it's handled specially in convertXPath
        -- If it does, it means both parts weren't XPath and we can't convert
        LI.Contains {} -> 
          Left $ LI.MkInvalidLocator l "Contains locator reached toXPathNode unexpectedly"
        _ -> Right $ case l of
                LI.XPath {value}
                  | T.isInfixOf " | " value ->
                      -- Union XPath produced by a nested Any combinator. Extract
                      -- predicates from every branch and combine with 'or' so the
                      -- whole union merges cleanly as a single predicate inside All.
                      let branches = T.splitOn " | " value
                          allPreds = concatMap (.predicates) $ parseXPathNode . T.strip <$> branches
                          orPred   = "(" <> T.intercalate " or " allPreds <> ")"
                      in  MkXPathNode {tag = "*", predicates = [orPred]}
                LI.XPath {value} -> parseXPathNode value
                LI.AllElms -> MkXPathNode {tag = "*", predicates = []}
                LI.Tag {value} -> MkXPathNode {tag = value, predicates = []}
                LI.ID {value} -> MkXPathNode {tag = "*", predicates = ["@id='" <> value <> "'"]}
                LI.Class {value, matchType, caseSensitivity} ->
                  MkXPathNode {tag = "*", predicates = [classToXPathPred value matchType caseSensitivity]}
                LI.Attribute {name, value, matchType, caseSensitivity} ->
                  MkXPathNode {tag = "*", predicates = [namedAttrToXPathPred name value matchType caseSensitivity]}
          
                -- Error cases - should have been converted to XPath or removed by prepareSimplify
                LI.CSS {} -> locErr l
                LI.Default {} -> locErr l
                LI.Role {} -> locErr l
                LI.InnerText {} -> locErr l
                LI.BiDiContext  {} -> locErr l
                LI.PostFilter  {} -> locErr l

    -- | XPath predicate for CSS class matching.
    classToXPathPred :: Text -> LI.MatchType -> LI.CaseSensitivity -> Text
    classToXPathPred val mt cs =
      let classAttr = applyCS cs "@class"
          matchVal = lowerIfCI cs val
       in case mt of
            LI.Full -> "contains(concat(' ', normalize-space(" <> classAttr <> "), ' '), ' " <> matchVal <> " ')"
            LI.Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
            LI.Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
            LI.Wildcard -> wildcardToXPathPred classAttr matchVal

    -- | XPath predicate matching elements with a specific named attribute satisfying the condition.
    namedAttrToXPathPred :: Text -> Text -> LI.MatchType -> LI.CaseSensitivity -> Text
    namedAttrToXPathPred name val mt cs =
      let attrExpr = applyCS cs ("@" <> name)
          matchVal = lowerIfCI cs val
       in case mt of
            LI.Full -> attrExpr <> "='" <> matchVal <> "'"
            LI.Partial -> "contains(" <> attrExpr <> ", '" <> matchVal <> "')" 
            LI.Starts -> "starts-with(" <> attrExpr <> ", '" <> matchVal <> "')" 
            LI.Wildcard -> wildcardToXPathPred attrExpr matchVal 

    applyCS :: LI.CaseSensitivity -> Text -> Text
    applyCS cs t = case cs of
      LI.CaseSensitive -> t
      LI.CaseInsensitive -> "translate(" <> t <> ", '" <> LI.upperAlpha <> "', '" <> LI.lowerAlpha <> "')"

    lowerIfCI :: LI.CaseSensitivity -> Text -> Text
    lowerIfCI cs t = case cs of
      LI.CaseSensitive -> t
      LI.CaseInsensitive -> T.toLower t

    wildcardToXPathPred :: Text -> Text -> Text
    wildcardToXPathPred normText val =
      let parts = filter (not . T.null) $ T.splitOn "*" val
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
              let buildP (preds', curText) (idx, part) =
                    let predicate =
                          if idx == (0 :: Int) && not startsWithWildcard
                            then "starts-with(" <> curText <> ", '" <> part <> "')"
                            else "contains(" <> curText <> ", '" <> part <> "')"
                        nextText = "substring-after(" <> curText <> ", '" <> part <> "')"
                     in (preds' <> [predicate], nextText)
                  (preds, _) = foldl' buildP ([], normText) (zip [0 ..] parts)
               in T.intercalate " and " preds

    locErr :: LI.Locator -> a
    locErr l =
      error . T.unpack $
        "Locator\n"
          <> txt l
          <> "\nconversion not implemented - should not be called - this is a library defect - check classify or locatorToXPathPartial"
