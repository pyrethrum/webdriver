module WebDriverPreCore.Extended.Locators.Internal where

import Control.Exception (Exception)
import Data.Foldable1 (Foldable1 (..), foldl1')
import Data.Functor ((<&>))
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty (..), groupBy, sortBy, toList)
import Data.Maybe (fromJust)
import Data.Text (Text, intercalate, pack, splitOn, toLower, unpack)
import Data.Text qualified as T
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext, Command, JSUInt, NodeProperties)
import Prelude

-- TODO : use bidi type when merged
{-
From specs:
## HTTP Selector Types
- css selector
- xpath
// easily derivable
- link text
- partial link text
- tag name

## BiDI Locator Types (CDDL)
browsingContext.Locator = (
   browsingContext.CssLocator /
   browsingContext.XPathLocator /
   ---
   browsingContext.AccessibilityLocator /
   browsingContext.ContextLocator /
   browsingContext.InnerTextLocator /
)

browsingContext.AccessibilityLocator = {
   type: "accessibility",
   value: {
    ? name: text,
    ? role: text,
   }
}

browsingContext.CssLocator = {
   type: "css",
   value: text
}

browsingContext.ContextLocator = {
  type: "context",
  value: {
    context: browsingContext.BrowsingContext,
  }
}

browsingContext.InnerTextLocator = {
   type: "innerText",
   value: text,
   ? ignoreCase: bool
   ? matchType: "full" / "partial",
   ? maxDepth: js-uint,
}

browsingContext.XPathLocator = {
   type: "xpath",
   value: text
}

locator visible to user includes

-}

data MatchFlags = MkMatchFlags
  { ignoreCase :: Bool,
    matchType :: MatchType
  }
  deriving (Show, Eq)

{- strategy for implementation:

- xpath / css universal
- bidi  classify
- combinators sorting
-> tests
- edge
    - HTTP + accesibility
    - accessibility + composition
- postfilter

-}

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

-- | Best-effort XPath approximation of a BiDi accessibility locator.
--   Covers explicit @role attributes, implicit roles for common HTML elements,
--   and common accessible name sources. Cannot cover aria-labelledby,
--   label[for=...], or shadow DOM.
-- TODO: cover edge cases in execution

{-
User: -- | Best-effort XPath approximation of a BiDi accessibility locator.
--   Covers explicit @role attributes, implicit roles for common HTML elements,
--   and common accessible name sources. Cannot cover aria-labelledby,
--   label[for=...], or shadow DOM.

expand on edge cases missede by this function

GitHub Copilot: Based on the function `accessibilityToXPath` and the ARIA/HTML specifications, here are the edge cases this XPath approximation misses:

## Edge Cases Not Covered

### 1. **aria-labelledby References**
The function cannot resolve `aria-labelledby` which references other element IDs to construct the accessible name. XPath 1.0 has no way to follow ID references and concatenate text from multiple elements.

```html
<!-- Cannot handle this: -->
<h2 id="dialog-title">Confirm Action</h2>
<button aria-labelledby="dialog-title">OK</button>
<!-- Accessible name is "Confirm Action", but XPath won't resolve the idref -->
```

### 2. **`<label for="...">` Associations**
HTML `<label>` elements that reference form controls via the `for` attribute create accessible names, but XPath cannot make this reverse lookup.

```html
<!-- Cannot handle this: -->
<label for="email-input">Email Address</label>
<input id="email-input" type="email">
<!-- Accessible name is "Email Address", but XPath can't follow the for/id link -->
```

### 3. **Shadow DOM**
Elements inside shadow roots are completely invisible to XPath queries from the light DOM.

```html
<!-- Cannot handle this: -->
<custom-button>
  #shadow-root
    <button role="button">Click Me</button>
</custom-button>
```

### 4. **aria-describedby (Description, Not Name)**
While the function focuses on accessible *names*, `aria-describedby` contributes to the accessible *description* and is similarly unsupported.

### 5. **Accessible Name Computation Order**
The [ARIA spec's name computation algorithm](https://www.w3.org/TR/accname-1.2/#mapping_additional_nd_te) has a specific precedence order. This function checks all sources with `or`, but the real algorithm stops at the first match:

1. `aria-labelledby` (highest priority)
2. `aria-label`
3. Native HTML labeling (`<label for>`, `alt`, `title`, etc.)
4. Inner text content (lowest priority)

The XPath treats them as equals, potentially matching elements that wouldn't match in real accessibility APIs.

### 6. **Hidden Text in Accessible Names**
CSS-hidden text (`visibility: hidden`, `display: none`) can still contribute to accessible names via `aria-label` or `aria-labelledby`, but those elements wouldn't match the XPath visibility filters you use elsewhere.

### 7. **Recursive/Nested aria-labelledby**
Elements referenced by `aria-labelledby` can themselves have `aria-labelledby`, creating chains. Real browsers flatten these recursively; XPath cannot.

### 8. **Title Attribute Fallback Conditions**
The `title` attribute only becomes the accessible name when no other labeling mechanism exists. The XPath uses `or`, so it might match elements where `title` shouldn't be considered.

### 9. **Role Presentation/None Suppression**
Elements with `role="presentation"` or `role="none"` have their semantics (and often their names) suppressed in the accessibility tree, but this function still tries to match them.

```html
<!-- Should not expose accessible name: -->
<img src="decorative.png" alt="Decorative" role="none">
```

### 10. **Form Control Implicit Labels**
Wrapping a form control inside a `<label>` without using `for=` creates an implicit association:

```html
<!-- Cannot handle this: -->
<label>
  Username
  <input type="text">
</label>
```

### 11. **ARIA 1.3 Name-from-Content Rules**
Some roles prohibit name-from-content (e.g., `textbox`, `combobox`). Others require it (e.g., `button`, `link`). The function doesn't enforce these constraints.

### 12. **Internationalization/Whitespace**
`normalize-space()` handles basic whitespace, but doesn't account for RTL text, zero-width characters, or Unicode normalization differences that might affect accessible name matching.

### Recommendation

For production use, consider documenting these limitations prominently and/or falling back to BiDi's native `browsingContext.Locator` with `type: "accessibility"` when the session supports it,
using this XPath only as a best-effort HTTP fallback.
-}
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

-- | Best-effort XPath approximation of a BiDi inner text locator.
--   Uses normalize-space(.) as a proxy for innerText. Case-insensitive
--   matching uses the translate() alphabet hack. maxDepth is approximated
--   via count(ancestor::*).
--   Visibility filtering: excludes elements with the HTML @hidden attribute,
--   @aria-hidden='true', or inline style display:none / visibility:hidden.
-- TODO: check strategy for visibility filtering in execution - may need to add more cases or use script.callFunction instead
--   Does NOT catch hiding via CSS classes or inherited/cascaded styles —
--   only the BiDi innerText locator handles those correctly.
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

-- locator ready to run
-- css or xpath

-- ===== Parent =====
-- parent
-- xpath
-- xpath
-- => use xpath

-- xpath
-- css | bidi only
-- => double shot [BiDi start nodes | HTTP post filter - Find Element From Element]
-- css child
-- bidi child

-- ==== Depth - inner text only ====
-- bidi use direct
-- HTTP Xpath - use xpath
-- css - use js function

-- ==== inner text ====
-- bidi
-- use direct
-- HTTP Xpath - use xpath + conditional post attempt check for duplicate visible elements - consider js function
-- css - use js function

-- ==== Role ====
-- BiDi - use native
-- HTTP - use xpath approximation + post filter for duplicates - consider js function
-- CSS use xpath + multi shot

data InvalidLocator = InvalidLocator Text deriving (Show, Eq, Ord)

instance Exception InvalidLocator

prepare :: (Text -> Locator) -> Protocol -> Cardinality -> Locator -> Either InvalidLocator Locator
prepare defLoc proto card loc = undefined

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

prepare' :: (Text -> Locator) -> Protocol -> Cardinality -> Locator -> (Locator, Classification)
prepare' defLoc proto card loc = undefined
  where
    flattenned = flattenLoc loc

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

sortCombinatorChildLocs :: (Text -> Locator) -> Protocol -> Locator -> Locator
sortCombinatorChildLocs defLoc proto = mapLocBottomUp (sortCombinatorChildLocs' defLoc proto)

sortCombinatorChildLocs' :: (Text -> Locator) -> Protocol -> Locator -> Locator
sortCombinatorChildLocs' defLoc proto l =
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
    None {elms} -> None $ sortAndGroup All elms
    PostFilter {} -> l
  where
    sortAndGroup groupCons = regroup groupCons . sortLocList
    clasify' = classify defLoc proto
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

--  readers
--  gt fail info - failinfo
-- postcheck when multiple locators detected
-- hybrid locators
-- wildcard text matching - js function
-- could RunTimeFilter &&& RunTimeFilter
-- reduce to filter
-- run
-- list of modifiers

--  readLoc @bool - isChecked Locator -> Reader Bool
--  readLoc
-- Getter a / Setter a

{-
HTTP ::
GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute

GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
  -- Returns a *JavaScript property* of the element (reflects live DOM state,
  -- not the original HTML attribute). Reading `value` gives the current content
  -- of an input; reading `checked` gives the current checked state of a checkbox.
  -- Unlike getAttribute, this returns the typed JS value coerced to a string.
  -- Example: GET .../property/value  on <input value="hello"> that the user has
  --          since cleared → returns "" (the live JS property), not "hello"
  --          (which getAttribute("value") would still return).
  --
  -- Property names are JS DOM properties, *not* CSS properties (those belong to
  -- /css/{property name}).  There is no single enumerated list — names come from
  -- the IDL interfaces defined in two WHATWG Living Standards:
  --
  --   WebDriver endpoint spec:
  --     https://w3c.github.io/webdriver/#get-element-property
  --   HTML Living Standard — element IDL interfaces (primary source):
  --     https://html.spec.whatwg.org/multipage/indices.html#element-interfaces
  --   DOM Standard — base Element/Node properties:
  --     https://dom.spec.whatwg.org/#interface-element
  --
  -- Common properties by category:
  --   All elements (DOM/HTML):
  --     id, className, tagName, innerHTML, outerHTML, innerText, textContent,
  --     hidden, tabIndex, title, lang, dir, accessKey
  --   Form controls (HTMLInputElement, HTMLTextAreaElement, HTMLSelectElement):
  --     value, checked, disabled, readOnly, required, multiple, type, name,
  --     placeholder, defaultValue, defaultChecked, selectedIndex,
  --     valueAsNumber, valueAsDate, min, max, step, pattern, maxLength, minLength
  --   Links / media (HTMLAnchorElement, HTMLImageElement, etc.):
  --     href, src, alt, width, height
  --   Select-specific:
  --     options, selectedOptions, size
  --
  -- BiDi: NO direct protocol message. NodeProperties.attributes is *not* the
  -- equivalent — it mirrors getAttribute() (original HTML attributes only).
  -- To read a live JS property, use script.callFunction with the element's
  -- sharedId as a SharedReference argument:
  --   script.callFunction {
  --     functionDeclaration: "(el, name) => el[name]",
  --     arguments: [{ sharedId: <id> }, { type: "string", value: "value" }]
  --   }

GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
  -- Returns the *computed* CSS value for the given property name, as the browser
  -- has resolved it (after applying the cascade, inheritance, and defaults).
  -- The property name should be in camelCase or hyphenated form.
  -- Colours are typically returned in rgba() notation; lengths in px.
  -- Example: GET .../css/background-color  → "rgba(255, 99, 71, 1)"
  --          GET .../css/font-size         → "16px"
  --
  -- BiDi: No direct message. Use script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el, p) => window.getComputedStyle(el).getPropertyValue(p)",
  --     arguments: [{ sharedId: <id> }, { type: "string", value: "background-color" }]
  --   }
  -- Returns a script.StringRemoteValue.

GET 	/session/{session id}/element/{element id}/text 	Get Element Text
  -- Returns the *visible rendered text* of the element and all its descendants,
  -- whitespace-collapsed the same way a user sees it on screen (mirrors the
  -- JS `element.innerText` semantics).  Hidden elements contribute no text.
  -- Example: GET .../text  on <button>  Submit </button> → "Submit"
  --          GET .../text  on <p>Hello <span>world</span></p> → "Hello world"
  --
  -- BiDi: NodeProperties.nodeValue only holds the raw value of a *text node*,
  -- not the rendered text of an element. For innerText semantics use
  -- script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el) => el.innerText",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- Alternatively, serialize the node with NodeProperties.children and
  -- recursively concatenate nodeValue of visible text-node descendants —
  -- but this misses CSS-driven hiding, so script.callFunction is preferred.

GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
  -- Returns the *lowercase HTML tag name* of the element (equivalent to the JS
  -- `element.tagName.toLowerCase()`).  Useful for asserting that you have
  -- matched the right kind of element before interacting with it.
  -- Example: GET .../name  on <INPUT type="text"> → "input"
  --          GET .../name  on <BUTTON>Click</BUTTON> → "button"
  --
  -- BiDi: This is the *only* endpoint with a direct structural equivalent.
  -- NodeProperties.localName in the script.NodeRemoteValue returned by
  -- browsingContext.locateNodes already contains the lowercase tag name —
  -- no extra round-trip needed.
  --   nodeRemoteValue.value.localName  ≡  "button"

GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
  -- Returns a JSON object describing the element's *bounding rectangle* in the
  -- viewport coordinate system: { x, y, width, height } (all in CSS pixels).
  -- x/y are the top-left corner relative to the document origin.
  -- Useful for checking visibility, position, or size without running JS.
  -- Example: GET .../rect  → {"x": 120.5, "y": 340.0, "width": 200.0, "height": 48.0}
  --
  -- BiDi: No direct message. Use script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el) => { const r = el.getBoundingClientRect();
  --                            return {x:r.x, y:r.y, width:r.width, height:r.height}; }",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- Returns a script.ObjectRemoteValue with number-typed entries.

GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
  -- Returns true/false indicating whether the element is currently *interactive*.
  -- An element is disabled (false) when it carries the HTML `disabled` attribute
  -- or is a form control inside a disabled <fieldset>.  Non-form elements (e.g.
  -- a plain <div>) always return true regardless of any aria-disabled attribute.
  -- Example: GET .../enabled  on <button disabled> → false
  --          GET .../enabled  on <input type="text"> → true
  --
  -- BiDi: No direct message. Use script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el) => !el.disabled",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- Note: NodeProperties.attributes may contain a "disabled" key, but only if
  -- the attribute is present in the HTML source — it won't reflect a <fieldset>
  -- disabled ancestor, making the JS property check more reliable.

GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
  -- Returns the *accessibility role* the browser has resolved for the element,
  -- taking both implicit (native HTML) roles and explicit `role=` attributes into
  -- account.  This is what an assistive technology actually sees, so it is more
  -- reliable for a11y assertions than checking the `role` attribute directly.
  -- Example: GET .../computedrole  on <button> → "button"
  --          GET .../computedrole  on <div role="alert"> → "alert"
  --          GET .../computedrole  on <div> (no role) → "generic"
  --
  -- BiDi: No direct message. Use script.callFunction via the Accessibility
  -- Object Model (AOM) property el.computedRole (Chrome 90+, Firefox preview):
  --   script.callFunction {
  --     functionDeclaration: "(el) => el.computedRole",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- NodeProperties.attributes["role"] only gives the *explicit* attribute value,
  -- missing all implicit roles from native HTML semantics.

GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
  -- Returns the *accessible name* of the element as computed by the browser's
  -- accessible name and description computation algorithm (ARIA spec).  This
  -- aggregates aria-label, aria-labelledby, the element's own text content,
  -- associated <label> elements, alt text, title, placeholder, etc. — the same
  -- string a screen reader would announce.
  -- Example: GET .../computedlabel  on <button aria-label="Close dialog"> → "Close dialog"
  --          GET .../computedlabel  on <input id="x"><label for="x">Email</label> → "Email"
  --
  -- BiDi: No direct message. Use script.callFunction via the AOM property
  -- el.computedLabel (Chrome 90+):
  --   script.callFunction {
  --     functionDeclaration: "(el) => el.computedLabel",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- NodeProperties.attributes can surface "aria-label" directly, but that is
  -- only one of many labelling sources — it misses aria-labelledby (requires
  -- following an idref), inner text, <label for=...>, alt, title, etc.
  -- el.computedLabel is the only way to get the fully resolved accessible name
  -- without reimplementing the entire ARIA naming algorithm in the client.

BiDi ::
browsingContext.LocateNodes = (
  method: "browsingContext.locateNodes",
  params: browsingContext.LocateNodesParameters
)

browsingContext.LocateNodesParameters = {
   context: browsingContext.BrowsingContext,
   locator: browsingContext.Locator,
   ? maxNodeCount: (js-uint .ge 1),
   ? serializationOptions: script.SerializationOptions,
   ? startNodes: [ + script.SharedReference ]
}

=>

browsingContext.LocateNodesResult = {
    nodes: [ * script.NodeRemoteValue ]
}

script.NodeRemoteValue = {
  type: "node",
  ? sharedId: script.SharedId,
  ? handle: script.Handle,
  ? internalId: script.InternalId,
  ? value: script.NodeProperties,
}

script.NodeProperties = {
  nodeType: js-uint,
  childNodeCount: js-uint,
  ? attributes: {*text => text},
  ? children: [*script.NodeRemoteValue],
  ? localName: text,
  ? mode: "open" / "closed",
  ? namespaceURI: text,
  ? nodeValue: text,
  ? shadowRoot: script.NodeRemoteValue / null,
}

script.WindowProxyRemoteValue = {
  type: "window",
  value: script.WindowProxyProperties,
  ? handle: script.Handle,
  ? internalId: script.InternalId
}

-}

{- https://chatgpt.com/c/69ab3adc-94bc-8324-bf32-218569fcaf86
function bidiIsVisible(el) {
  if (!el || !el.isConnected) return false;

  const style = getComputedStyle(el);

  if (style.display === "none") return false;
  if (style.visibility === "hidden" || style.visibility === "collapse") return false;mplification,
          test_flatenning_no

  if (el.tagName === "INPUT" && el.type === "hidden")
    return false;

  const rect = el.getBoundingClientRect();

  if (rect.width === 0 || rect.height === 0)
    return false;

  const vpW = window.innerWidth;
  const vpH = window.innerHeight;

  if (
    rect.bottom < 0 ||
    rect.right < 0 ||
    rect.top > vpH ||
    rect.left > vpW
  )
    return false;

  const points = [
    [rect.left + rect.width / 2, rect.top + rect.height / 2],
    [rect.left + 1, rect.top + 1],
    [rect.right - 1, rect.bottom - 1]
  ];

  for (const [x, y] of points) {
    if (x < 0 || y < 0 || x > vpW || y > vpH)
      continue;

    const hit = document.elementFromPoint(x, y);

    if (hit === el || el.contains(hit))
      return true;
  }

  return false;
}

Performance tip (important)

For automation frameworks, inject the helper once per page:

window.__bidiIsVisible = bidiIsVisible

Then call:

__bidiIsVisible(element)

This avoids re-sending the function each time

-}
-- Looks like we need a GADT

{- riffing
  ### locator

- `Form` type - polymorphic on selector or sting bool so record types are paired (possible use for associated types)
	- not sure how this would work (how to do reflection)
	- `setForm`
	- could setform even be an effect?
	- how would this differ from palin old do notation

- `ensure` selectors - wait till condition is satisfied
- or make locators a predicate at heart with monoid and alternative instances so you could have `byId` or `withClass` or `containsText` or `withText` or `withText'`  `withTextFilter`  `elmFilter` ...

  use default string rules and a typeclass and a newtype around text to get default string treatment
- l

  ```haskell
  IsLocator

  -- locators lieient by default ??

  Locator == id
  LocatorText
  ```

  ```haskell
   click $
     byCSS ".highlight"
      <> placeholder "enter here"
     <|> button "Submit"

```haskell
 click $
   byCSS ".highlight"
    && placeholder "enter here"
    || button "Submit"
```

    submitButton = button <> hasText "Submit"
    buttonTxt = (<>) button . hasText

    submitButton = buttonTxt "Submit"

    clickById
    clickByRoll
    fillByCSS

    or
     byId s = byCSS $ "#" <> s
     button = (<>) button_ . hasText
     submitButton = button "Submit"

  -- default string treatment
  click submit_button
  set "given_name" "John"

  -- globs by default

  welcomeMessage = hasText "Hello * welcome to Myer"

  hasText' [NoGlobs, CaseSensitive, FullMatch] "Hello * welcome to Myer"

  hasParent Locator
  hasDirectParent Locator

  -- an edit type which is an id and a value and has a monoid instance

  Edit type

  set EDIT
  set $
   "given_name" ~> "John"
   <> "last_name" "Walker"
   <> byPlaceholder "Title" "Sir"

-- I doubt this would work ??
-- use GADTs ? or just a parameterised data type wiith one param type for Locators and another for values

perhaps a data type with selector -> a -> m () that could be partially applied

----
Yes, you can select elements based on their `value` in Playwright, though the approach differs slightly between standard HTML elements and custom components.

### 🎯 For Standard HTML Elements

The most direct way is to use standard CSS attribute selectors. This is particularly effective for `<input>`, `<button>`, and `<option>` elements where the `value` is set as an HTML attribute .

```javascript
// Select an input button by its value
await page.locator('input[value="Log in"]').click();

// Select any element with a specific value attribute
const element = page.locator('[value="your-value-here"]');
```

For `<select>` dropdowns, Playwright provides the dedicated `selectOption()` method, which can select by `value`, visible `label`, or `index` .

```javascript
// Select by the option's value attribute
await page.locator('select#country').selectOption('us');

// Select by the option's visible text
await page.locator('select#country').selectOption({ label: 'United States' });
```

### ⚠️ For Custom Components

Modern frontend frameworks often build custom dropdowns and inputs that don't use standard HTML `<select>` or simple `<input>` elements. In these cases, the `value` you see may be a property of the JavaScript object, not an HTML attribute, so CSS selectors won't work directly .

You have a couple of options to handle this:

1.  **Interact by visible text**: Click the custom component to open it, then select the option by its visible text.
    ```javascript
    await page.getByTestId('custom-dropdown-trigger').click();
    await page.getByText('Desired Option', { exact: true }).click();
    ```
2.  **Use a more resilient locator strategy**: Rely on `getByTestId()`, `getByRole()`, or other stable attributes provided by the component, rather than the element's state .

### 🔍 Checking Values After Selection

Once you've interacted with an element, you'll often want to verify it has the correct value. Playwright provides specific methods for this :

- **`toHaveValue()`**: An assertion to verify the value of an `<input>`, `<textarea>`, or `<select>` element.
    ```javascript
    await expect(page.locator('input#name')).toHaveValue('John Doe');
    ```
- **`inputValue()`**: To retrieve the current value of an input element without making an assertion.
    ```javascript
    const currentValue = await page.locator('input#name').inputValue();
    ```

I hope this helps you effectively select and interact with elements based on their values. If you're dealing with a particularly tricky component, feel free to share the HTML structure, and I might be able to offer a more specific suggestion

  -}
