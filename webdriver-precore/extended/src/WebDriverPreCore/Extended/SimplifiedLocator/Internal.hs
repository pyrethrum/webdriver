module WebDriverPreCore.Extended.SimplifiedLocator.Internal where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Text qualified as T
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext, JSUInt)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import Prelude

--  HERE
-- nesting
-- process and or based on loc type - note should already be grouped properly due to classification

-- | Simplified/resolved form of 'LI.Locator', where leaf locators expressible
--   as XPath have been folded in and 'LI.Default' has been resolved.
--   Produced by 'prepareSimplify'.
data SimplifiedLocator
  = -- universal
    CSS {value :: Text}
  | XPath {value :: Text}
  | -- double shot / difficult
    Role {role :: Maybe LI.AriaRole, name :: Maybe Text}
  | InnerText
      { value :: Text,
        matchType :: LI.MatchType,
        caseSesnsitivity :: LI.CaseSensitivity,
        maxDepth :: Maybe JSUInt
      }
  | -- exclusive
    -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
    BiDiContext {context :: BrowsingContext}
  | -- combinators
    Parent {parent :: SimplifiedLocator, child :: SimplifiedLocator}
  | All {elms :: NonEmpty SimplifiedLocator}
  | Any {elms :: NonEmpty SimplifiedLocator}
  | None {elms :: NonEmpty SimplifiedLocator}
  | --- postfilter
    PostFilter LI.PostFilter
  deriving
    ( Show,
      Eq
    )

isXPath :: SimplifiedLocator -> Bool
isXPath = \case
  XPath _ -> True
  _ -> False

prepareSimplify :: (Text -> LI.Locator) -> LI.Protocol -> LI.Locator -> Either LI.InvalidLocator SimplifiedLocator
prepareSimplify defLoc proto l =
  LI.prepare defLoc proto l <&> simplify
  where
    simplifyAll = (simplify <$>)
    simplify :: LI.Locator -> SimplifiedLocator
    simplify loc =
      case loc of
        LI.CSS {..} -> CSS {..}
        LI.XPath {..} -> XPath {..}
        LI.AllElms -> xpathLoc
        LI.ID {} -> xpathLoc
        LI.Class {} -> xpathLoc
        LI.Attribute {} -> xpathLoc
        LI.Tag {} -> xpathLoc
        LI.Default {value} -> simplify (defLoc value)
        LI.Role {..} -> Role {..}
        LI.InnerText {..} -> InnerText {..}
        LI.BiDiContext {..} -> BiDiContext {..}
        LI.Parent {parent, child} -> mergeXPaths $ Parent {parent = simplify parent, child = simplify child}
        -- here mergeIfXPath
        LI.All {..} -> -- mergeXPaths $ All $ simplifyAll elms
        LI.Any {..} -> -- mergeXPaths $ Any $ simplifyAll elms
        LI.None {..} -> -- mergeXPaths $ None $ simplifyAll elms
        LI.PostFilter pf -> PostFilter pf
      where
        xpathLoc = toXPath loc
        mergeIfXPath = undefined 
         where 
           classification = classify loc

toXPath :: LI.Locator -> SimplifiedLocator
toXPath = XPath . toXPathTxt
  where
    -- \| Convert a Locator to a full XPath expression string.
    toXPathTxt :: LI.Locator -> Text
    toXPathTxt loc = case loc of
      LI.XPath {value} -> value
      LI.AllElms -> "//*"
      LI.ID {value} -> "//*[@id='" <> value <> "']"
      LI.Class {value, matchType, caseSensitivity} ->
        "//*[" <> classToXPathCoreTxt value matchType caseSensitivity <> "]"
      LI.Attribute {value, matchType, caseSensitivity} ->
        "//*[" <> attrToXPathCoreTxt value matchType caseSensitivity <> "]"
      LI.Tag {value} -> "//" <> value
      -- Parent: concatenate parent and child XPath — child's leading // creates a
      -- descendant-axis step from the parent result set, e.g. //form//input.
      LI.Parent {parent, child} -> toXPathTxt parent <> toXPathTxt child
      LI.All {elms} -> "//*[" <> T.intercalate " and " (toList $ toXPathCoreTxt <$> elms) <> "]"
      LI.Any {elms} -> "//*[" <> T.intercalate " or " (toList $ toXPathCoreTxt <$> elms) <> "]"
      LI.None {elms} -> "//*[not(" <> T.intercalate " or " (toList $ toXPathCoreTxt <$> elms) <> ")]"
      LI.CSS {} -> locErr loc
      LI.Default {} -> locErr loc
      LI.Role {} -> locErr loc
      LI.InnerText {} -> locErr loc
      LI.BiDiContext {} -> locErr loc
      LI.PostFilter {} -> locErr loc

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
        -- Parent as predicate: "I match child AND I have an ancestor matching parent"
        LI.Parent {parent, child} ->
          toXPathCoreTxt child <> " and ancestor::*[" <> toXPathCoreTxt parent <> "]"
        LI.All {elms} -> elmsTxt "and" elms
        LI.Any {elms} -> elmsTxt "or" elms
        LI.None {elms} -> "not" <> elmsTxt "or" elms
        LI.CSS {} -> locErr loc
        LI.Default {} -> locErr loc
        LI.Role {} -> locErr loc
        LI.InnerText {} -> locErr loc
        LI.BiDiContext {} -> locErr loc
        LI.PostFilter {} -> locErr loc
      where
        elmsTxt conjunctive = "(" <> T.intercalate (" " <> conjunctive <> " ") (toList $ toXPathCoreTxt <$> elms) <> ")"

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
            LI.Wildcard -> "@*[" <> wildcardPred attrExpr matchVal <> "]"

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
