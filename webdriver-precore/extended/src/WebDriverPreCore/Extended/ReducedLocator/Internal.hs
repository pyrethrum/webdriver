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

import Control.Monad (when)
import Data.List (foldl', nub)
import Data.List qualified as LST
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
        caseSensitivity :: LI.CaseSensitivity,
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
xPathSub defLoc proto l = do
  -- prepare: flatten, sort/group by classification, validate
  prepared <- LI.prepare defLoc proto l
  -- Top-down conversion: IsXPathConvertable subtrees are converted to XPath
  -- in one shot; user XPath nodes are left untouched.
  convertTopDown defLoc proto prepared

-- | Top-down conversion of IsXPathConvertable subtrees to XPath.
--   Uses 'LI.classify' to detect IsXPathConvertable groups (which after
--   'LI.prepare' / 'sortGroupChildLocs' are pure groups of convertable nodes).
--   User-provided XPath nodes are preserved as-is so they are never merged
--   with auto-generated XPath — instead a multi-step combinator is produced.
convertTopDown :: (Text -> LI.Locator) -> LI.Protocol -> LI.Locator -> Either LI.InvalidLocator LI.Locator
convertTopDown defLoc proto loc
  | LI.classify defLoc proto loc == LI.IsXPathConvertable =
    convertGroupToXPath loc
  | otherwise = case loc of
      LI.Contains {container, contained} ->
        LI.Contains <$> convertTopDown defLoc proto container <*> convertTopDown defLoc proto contained
      LI.All {elms} ->
        LI.All <$> traverse (convertTopDown defLoc proto) elms
      LI.Any {elms} ->
        LI.Any <$> traverse (convertTopDown defLoc proto) elms
      LI.PostFilter {predicate, locator} ->
        LI.PostFilter predicate <$> convertTopDown defLoc proto locator
      LI.Default {value} ->
        convertTopDown defLoc proto (defLoc value)
      _ -> Right loc

-- | Convert an IsXPathConvertable subtree directly to an XPath locator.
--   Uses structured conversion: leaf nodes render to known-format XPath strings,
--   All merges tags and predicates, Any produces unions, Contains concatenates.
convertGroupToXPath :: LI.Locator -> Either LI.InvalidLocator LI.Locator
convertGroupToXPath = fmap LI.XPath . go
  where
    go :: LI.Locator -> Either LI.InvalidLocator Text
    go = \case
      -- Leaf IsXPathConvertable nodes: render directly to XPath
      LI.AllElms -> Right "//*"
      LI.ID {value} -> Right $ "//*[@id='" <> value <> "']"
      LI.Tag {value} -> Right $ "//" <> value
      LI.Class {value, matchType, caseSensitivity} ->
        Right $ "//*[" <> classXPathPred value matchType caseSensitivity <> "]"
      LI.Attribute {name, value, matchType, caseSensitivity} ->
        Right $ "//*[" <> attrXPathPred name value matchType caseSensitivity <> "]"

      -- All (intersection): merge tags and predicates from all children.
      -- Single-step children are parsed and merged.  Multi-step children
      -- (from Contains) get extra predicates appended to their last step.
      LI.All {elms} -> do
        children <- traverse go (toList elms)
        let (multiSteps, singleSteps) = LST.partition isMultiStepXPath children
            extraParsed = parseKnownXPath <$> singleSteps
            extraPreds = concatMap (.predicates) extraParsed
            extraTags = nub $ (.tag) <$> extraParsed
        case multiSteps of
          [] -> do
            -- All single-step: merge tags and predicates
            let parsed = parseKnownXPath <$> children
                tags = nub $ (.tag) <$> parsed
                allPreds = concatMap (.predicates) parsed
            mergedTag <- case tags of
              []  -> Right "*"
              ["*"] -> Right "*"
              [t] -> Right t
              _   -> Left $ LI.MkInvalidLocator (LI.All elms) $
                       "Conflicting tags in All combinator: " <> T.intercalate ", " tags
            Right $ case allPreds of
              [] -> "//" <> mergedTag
              _  -> "//" <> mergedTag <> "[" <> T.intercalate " and " allPreds <> "]"
          _ -> do
            -- Has multi-step children: use the first multi-step for the
            -- structural container//descendant path.  For each remaining
            -- multi-step, extract descendant predicates and add an
            -- ancestor check on its container, then append all extra
            -- predicates to the last step.
            when (not (null extraTags || extraTags == ["*"])) $
              Left $ LI.MkInvalidLocator (LI.All elms) $
                "Cannot merge single-step tags " <> txt extraTags <> " into multi-step XPaths"
            let (baseStep : restSteps) = multiSteps
                base = appendPredsToLastStep extraPreds baseStep
                restDescPreds = concatMap extractDescendantPreds restSteps
                ancestorChecks = map ancestorCheckFromMultiStep restSteps
                allExtraPreds = restDescPreds <> ancestorChecks
            Right $ appendPredsToLastStep allExtraPreds base

      -- Any (union): produce XPath union with |
      LI.Any {elms} -> do
        children <- traverse go (toList elms)
        Right $ T.intercalate " | " children

      -- Contains (descendant): concatenate container and contained XPaths.
      -- When either side is a union, take the cartesian product so every
      -- container branch prefixes every descendant branch.
      LI.Contains {container, contained} -> do
        cv <- go container
        dv <- go contained
        let cvBranches = T.splitOn " | " cv
            dvBranches = T.splitOn " | " dv
        Right $ T.intercalate " | " $
          [c <> d | c <- cvBranches, d <- dvBranches]

      -- Should not happen for IsXPathConvertable subtrees
      other -> Left $ LI.MkInvalidLocator other $
        "Unexpected locator in convertGroupToXPath: " <> txt other

-- | Simple structured representation of an XPath location step.
--   Used only for merging XPath strings that were produced by this module
--   (known format: //tag[pred1][pred2]...).
data SimpleXPath = SimpleXPath {tag :: Text, predicates :: [Text]}
  deriving (Show, Eq)

-- | Parse an XPath string in the known format produced by this module.
--   Since we only parse strings WE generated, there are no edge cases:
--   the format is always //tag or //tag[pred1][pred2]...
parseKnownXPath :: Text -> SimpleXPath
parseKnownXPath value =
  case T.stripPrefix "//" value of
    Nothing -> SimpleXPath "*" []
    Just rest ->
      let (t, remainder) = T.break (== '[') rest
          preds = parsePreds remainder
      in SimpleXPath {tag = if T.null t then "*" else t, predicates = preds}
  where
    parsePreds :: Text -> [Text]
    parsePreds txt
      | T.null txt = []
      | otherwise =
          case T.stripPrefix "[" txt of
            Nothing -> []
            Just inner ->
              let (p, after) = T.breakOn "]" inner
              in p : parsePreds (T.drop 1 after)

-- | Does the XPath string contain more than one location step?
--   A multi-step XPath has // appearing after the initial //.
isMultiStepXPath :: Text -> Bool
isMultiStepXPath v = "//" `T.isInfixOf` T.drop 2 v

-- | Split a multi-step XPath into (containerPart, lastStep).
--   \"//a//b\" -> (\"//a\", \"//b\")
--   \"//a//b//c\" -> (\"//a//b\", \"//c\")
splitMultiStepXPath :: Text -> (Text, Text)
splitMultiStepXPath v =
  let steps = T.splitOn "//" v
      -- steps[0] is \"\" (before leading //), steps[1..] are the actual steps
      actualSteps = drop 1 steps
  in case reverse actualSteps of
      (lastStep : revInit) ->
        let containerSteps = reverse revInit
            container = "//" <> T.intercalate "//" containerSteps
            descendant = "//" <> lastStep
        in (container, descendant)
      [] -> ("//*", v)  -- shouldn't happen for valid multi-step

-- | Extract predicates from the descendant (last step) of a multi-step XPath.
--   Used when merging multiple Contains within an All:
--   additional Contains' descendant conditions are folded onto the last step.
extractDescendantPreds :: Text -> [Text]
extractDescendantPreds multiStep =
  let (_, descendant) = splitMultiStepXPath multiStep
  in (.predicates) $ parseKnownXPath descendant

-- | Build an \"count(ancestor::tag[preds]) > 0\" predicate from the container
--   part of a multi-step XPath.  This ensures the target element has an
--   ancestor matching the container specification of an additional Contains.
ancestorCheckFromMultiStep :: Text -> Text
ancestorCheckFromMultiStep multiStep =
  let (container, _) = splitMultiStepXPath multiStep
      cNode = parseKnownXPath container
      ancestorMatch = cNode.tag <> foldMap (\p -> "[" <> p <> "]") cNode.predicates
  in "count(ancestor::" <> ancestorMatch <> ") > 0"

-- | Append extra predicates to the last location step of a multi-step XPath.
--   The XPath must have the form //step1//step2//...//lastStep.
--   Extra predicates are added to lastStep: //step1//...//lastStep[extra1 and extra2].
appendPredsToLastStep :: [Text] -> Text -> Text
appendPredsToLastStep [] v = v
appendPredsToLastStep extraPreds v =
  let -- Split into steps, keeping the // delimiters
      steps = T.splitOn "//" v
      -- steps[0] is "" (before leading //), steps[1..] are the actual steps
      (initSteps, lastStep) = case reverse steps of
        []     -> ([], "*")
        (l:ls) -> (reverse ls, l)
      -- Add extra predicates to the last step
      extraPred = "[" <> T.intercalate " and " extraPreds <> "]"
      lastStep' = case T.break (== '[') lastStep of
        (tag, "")  -> tag <> extraPred
        (tag, preds) -> tag <> preds <> extraPred
  in T.intercalate "//" (initSteps <> [lastStep'])

-- | XPath predicate for CSS class matching.
classXPathPred :: Text -> LI.MatchType -> LI.CaseSensitivity -> Text
classXPathPred val mt cs =
  let classAttr = applyCS cs "@class"
      matchVal = lowerIfCI cs val
  in case mt of
        LI.Full -> "contains(concat(' ', normalize-space(" <> classAttr <> "), ' '), ' " <> matchVal <> " ')"
        LI.Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
        LI.Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
        LI.Wildcard -> wildcardXPathPred classAttr matchVal

-- | XPath predicate for named attribute matching.
attrXPathPred :: Text -> Text -> LI.MatchType -> LI.CaseSensitivity -> Text
attrXPathPred name val mt cs =
  let attrExpr = applyCS cs ("@" <> name)
      matchVal = lowerIfCI cs val
  in case mt of
        LI.Full -> attrExpr <> "='" <> matchVal <> "'"
        LI.Partial -> "contains(" <> attrExpr <> ", '" <> matchVal <> "')"
        LI.Starts -> "starts-with(" <> attrExpr <> ", '" <> matchVal <> "')"
        LI.Wildcard -> wildcardXPathPred attrExpr matchVal

applyCS :: LI.CaseSensitivity -> Text -> Text
applyCS cs t = case cs of
  LI.CaseSensitive -> t
  LI.CaseInsensitive -> "translate(" <> t <> ", '" <> LI.upperAlpha <> "', '" <> LI.lowerAlpha <> "')"

lowerIfCI :: LI.CaseSensitivity -> Text -> Text
lowerIfCI cs t = case cs of
  LI.CaseSensitive -> t
  LI.CaseInsensitive -> T.toLower t

wildcardXPathPred :: Text -> Text -> Text
wildcardXPathPred normText val =
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
