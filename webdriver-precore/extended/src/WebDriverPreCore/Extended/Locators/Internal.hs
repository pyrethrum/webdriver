module WebDriverPreCore.Extended.Locators.Internal where

import Control.Exception (Exception)
import Data.Foldable1 (foldl1')
import Data.Functor ((<&>))
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty (..), groupBy, sortBy, toList)
import Data.Maybe (fromJust)
import Data.Text (Text, intercalate, pack, splitOn, toLower, unpack)
import Data.Text qualified as T
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext, JSUInt, NodeProperties)
import Prelude


data MatchFlags = MkMatchFlags
  { ignoreCase :: Bool,
    matchType :: MatchType
  }
  deriving (Show, Eq)

--
data LocatorDirectives = ToDo deriving (Show, Eq)

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
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity
      }
  | Tag {value :: Text}
  | Default {value :: Text}
  | -- double shot / difficult
    Role {role :: Maybe AriaRole, name :: Maybe Text}
  | InnerText
      { value :: Text,
        matchType :: MatchType,
        caseSesnsitivity :: CaseSensitivity,
        maxDepth :: Maybe JSUInt
      }
  | -- exclusive
    -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
    BiDiContext {context :: BrowsingContext}
  | -- combinators
    Parent {parent :: Locator, child :: Locator}
  | All {elms :: NonEmpty Locator}
  | Any {elms :: NonEmpty Locator}
  | None {elms :: NonEmpty Locator}
  | --- postfilter
    PostFilter PostFilter
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq
    )

data PostFilter
  = BiDiPostFilter
      { description :: Text,
        -- TODO: fix this when merged
        nodePredicate :: NodeProperties -> Bool
      }
  | HttpPostFilter
      { description :: Text,
        -- TODO: fix this when merged
        httpCommand :: Either Text Text
      }
  | JSPostFilter
      { description :: Text,
        -- TODO: fix this when merged
        js :: Text
      }
  | ValuePostFilter
      { description :: Text,
        value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity
      }
  | ValueFuncPostFilter
      { description :: Text,
        valPredicate :: Text -> Bool
      }

instance Show PostFilter where
  show :: PostFilter -> String
  show = \case
    BiDiPostFilter desc _ -> "BiDiPostFilter: " <> unpack desc
    HttpPostFilter desc _ -> "HttpPostFilter: " <> unpack desc
    JSPostFilter desc _ -> "JSPostFilter: " <> unpack desc
    ValuePostFilter desc _ _ _ -> "ValuePostFilter: " <> unpack desc
    ValueFuncPostFilter desc _ -> "ValueFuncPostFilter: " <> unpack desc

instance Eq PostFilter where
  (==) :: PostFilter -> PostFilter -> Bool
  (==) = \cases
    (BiDiPostFilter desc1 _) (BiDiPostFilter desc2 _) -> desc1 == desc2
    (HttpPostFilter desc1 _) (HttpPostFilter desc2 _) -> desc1 == desc2
    (JSPostFilter desc1 _) (JSPostFilter desc2 _) -> desc1 == desc2
    (ValuePostFilter desc1 val1 mt1 cs1) (ValuePostFilter desc2 val2 mt2 cs2) ->
      desc1 == desc2 && val1 == val2 && mt1 == mt2 && cs1 == cs2
    (ValueFuncPostFilter desc1 _) (ValueFuncPostFilter desc2 _) -> desc1 == desc2
    _ _ -> False

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

data MatchType = Full | Starts | Partial | Wildcard deriving (Show, Eq)

data CaseSensitivity = CaseSensitive | CaseInsensitive deriving (Show, Eq)

displayAriaRole :: AriaRole -> Text
displayAriaRole = toLower . pack . show

roleToXPath :: Maybe AriaRole -> Maybe Text -> Maybe Text
roleToXPath = \cases
  Nothing Nothing -> Nothing
  mRole mName -> Just $ "//*" <> rle mRole <> name mName
  where
    rle = maybe "" \r -> "[" <> implicitRoleXPath r <> " or @role='" <> displayAriaRole r <> "']"

    name = maybe "" \n ->
      "["
        <> intercalate
          " or "
          [ "@aria-label='" <> n <> "'",
            "@placeholder='" <> n <> "'",
            "@title='" <> n <> "'",
            "@alt='" <> n <> "'",
            "normalize-space(text())='" <> n <> "'"
          ]
        <> "]"

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


innerTextToXPath :: Text -> CaseSensitivity -> MatchType -> Maybe JSUInt -> Text
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

data Cardinality = One | Many deriving (Show, Eq)

data InvalidLocator = InvalidLocator Text deriving (Show, Eq, Ord)

instance Exception InvalidLocator

prepare :: (Text -> Locator) -> Protocol ->  Locator -> Either InvalidLocator Locator
prepare defLoc proto = 
   toEither . sortGroupChildLocs defLoc proto . flattenLoc
   where 
    toEither :: Locator -> Either InvalidLocator Locator
    toEither l = case classify defLoc proto l of
      Invalid err -> Left err
      _ -> Right l
      

data Classification = IsXPath | IsCSS | IsBiDi | Invalid InvalidLocator | IsMixed deriving (Show, Eq, Ord)

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
    AllElms -> IsXPath
    ID {} -> IsXPath
    Class {} -> IsXPath
    Attribute {} -> IsXPath
    Tag {} -> IsXPath
    Default {value} ->
      let nxtLoc = defLoc value
          nestedDefault = hasDefault nxtLoc
       in if nestedDefault
            then Invalid $ InvalidLocator "Invalid Default locator - Default locator cannot resolve to another Default"
            else classifyNxt nxtLoc
    Role {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> IsMixed -- requires double shot Xpath + post filter
    InnerText {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> IsMixed -- requires double shot Xpath + post filter
    BiDiContext {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> Invalid $ InvalidLocator "BiDiContext locator cannot be used with HTTP protocol"
    Parent {parent, child} ->
      mergeClassification (classifyNxt parent) (classifyNxt child)
    All {elms} -> clasifyElms elms
    Any {elms} -> clasifyElms elms
    None {elms} -> clasifyElms elms
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
        Parent {} -> l
        All {elms} -> All $ sortAndGroup All elms
        Any {elms} -> Any $ sortAndGroup Any elms
        --- None a1, a2, b1, b2, c => None ( any (a1, a2), any (b1, b2), any (c))
        None {elms} -> None $ sortAndGroup Any elms
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
      Attribute {value, matchType, caseSensitivity} ->
        "//*[" <> attrPred value matchType caseSensitivity <> "]"
      Tag {value} -> "//" <> value
      -- Parent: concatenate parent and child XPath — child's leading // creates a
      -- descendant-axis step from the parent result set, e.g. //form//input.
      Parent {parent, child} -> toXPathStr parent <> toXPathStr child
      All {elms} -> "//*[" <> intercalate " and " (toList $ toPred <$> elms) <> "]"
      Any {elms} -> "//*[" <> intercalate " or " (toList $ toPred <$> elms) <> "]"
      None {elms} -> "//*[not(" <> intercalate " or " (toList $ toPred <$> elms) <> ")]"
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
      Attribute {value, matchType, caseSensitivity} -> attrPred value matchType caseSensitivity
      Tag {value} -> "self::" <> value
      -- Parent as predicate: "I match child AND I have an ancestor matching parent"
      Parent {parent, child} ->
        toPred child <> " and ancestor::*[" <> toPred parent <> "]"
      All {elms} -> "(" <> intercalate " and " (toList $ toPred <$> elms) <> ")"
      Any {elms} -> "(" <> intercalate " or " (toList $ toPred <$> elms) <> ")"
      None {elms} -> "not(" <> intercalate " or " (toList $ toPred <$> elms) <> ")"
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
              "contains(concat(' ', " <> classAttr <> ", ' '), ' " <> matchVal <> " ')"
            Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
            Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
            Wildcard -> wildcardPred classAttr matchVal

    -- \| XPath predicate matching elements that have any attribute satisfying the condition.
    --   Uses @*[...] predicate syntax so the condition is applied to each attribute node.
    attrPred :: Text -> MatchType -> CaseSensitivity -> Text
    attrPred val mt cs =
      let attrExpr = applyCS cs "." -- '.' refers to the attribute node's string value
          matchVal = lowerIfCI cs val
       in case mt of
            Full -> "@*[" <> attrExpr <> "='" <> matchVal <> "']"
            Partial -> "@*[contains(" <> attrExpr <> ", '" <> matchVal <> "')]"
            Starts -> "@*[starts-with(" <> attrExpr <> ", '" <> matchVal <> "')]"
            Wildcard -> "@*[" <> wildcardPred attrExpr matchVal <> "]"

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
    Parent p c -> foldLoc f (foldLoc f acc' p) c
    All locs -> foldList locs
    Any locs -> foldList locs
    None locs -> foldList locs
    -- WithOptions base _ -> foldLoc f acc' base
    PostFilter _ -> acc'
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
    Parent p c -> f (foldLocBottomUp f (foldLocBottomUp f acc p) c) loc
    All locs -> f (foldList locs) loc
    Any locs -> f (foldList locs) loc
    None locs -> f (foldList locs) loc
    -- WithOptions base _ -> f (foldLocBottomUp f acc base) loc
    PostFilter _ -> f acc loc
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
    Parent p c -> Parent (recurse p) (recurse c)
    All locs -> All $ recurseMap locs
    Any locs -> Any $ recurseMap locs
    None locs -> None $ recurseMap locs
    _ -> loc -- Leaf locators and PostFilter
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
hasDefault = anyLoc isDefault
  where
    isDefault (Default _) = True
    isDefault _ = False

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

  -- Apply De Morgan's laws and flatten None
  None locs ->
    case toList reduced of
      -- Double negation: None [None [x]] -> Any [x]
      [None xs] -> flattenLoc $ Any xs
      -- De Morgan: None [All [x,y]] -> Any [None [x], None [y]]
      [All xs] -> flattenLoc . Any $ negateAll xs
      -- De Morgan: None [Any [x,y]] -> All [None [x], None [y]]
      [Any xs] -> flattenLoc . All $ negateAll xs
      -- Single non-Match* locator - already reduced
      [single] -> None (single :| [])
      -- Multiple locators - check for nested None and apply De Morgan
      -- None [a, None [b], c] -> All [None [a], b, None [c]]
      (x : xs) ->
        if any isNone (x : xs)
          then flattenLoc . All $ (x :| xs) <&> applyDoubleNegation
          else None (x :| xs)
      [] -> error "flattenLoc: None produced empty list (impossible with NonEmpty input)"
    where
      reduced = flattenLoc <$> locs
      negateAll = fmap (\x -> None (x :| []))
      isNone (None _) = True
      isNone _ = False
      -- Apply double negation to unwrap None, or negate non-None
      applyDoubleNegation (None (y :| [])) = y -- None [y] becomes y
      applyDoubleNegation (None ys) = Any ys -- None [y1, y2, ...] becomes Any [y1, y2, ...]
      applyDoubleNegation y = None (y :| []) -- y becomes None [y]

  -- Recurse into other composite locators
  Parent p c -> Parent (flattenLoc p) (flattenLoc c)
  -- WithOptions base opts -> WithOptions (flattenLoc base) opts
  -- Leaf locators and PostFilter have no children to recurse into
  other -> other

upperAlpha :: Text
upperAlpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowerAlpha :: Text
lowerAlpha = "abcdefghijklmnopqrstuvwxyz"