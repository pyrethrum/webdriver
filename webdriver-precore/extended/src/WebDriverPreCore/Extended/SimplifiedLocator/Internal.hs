module WebDriverPreCore.Extended.SimplifiedLocator.Internal where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Text qualified as T
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
    simplify = \case
      LI.CSS {..} -> CSS {..}
      LI.XPath {..} -> XPath {..}
      loc@LI.AllElms -> toXPath loc
      loc@(LI.ID {}) -> toXPath loc
      loc@(LI.Class {}) -> toXPath loc
      loc@(LI.Attribute {}) -> toXPath loc
      loc@(LI.Tag {}) -> toXPath loc
      LI.Default {value} -> simplify (defLoc value)
      LI.Role {..} -> Role {..}
      LI.InnerText {..} -> InnerText {..}
      LI.BiDiContext {..} -> BiDiContext {..}
      LI.Parent {parent, child} -> Parent {parent = simplify parent, child = simplify child}
      LI.All {..} -> mergeXPaths $ All $ simplifyAll elms
      LI.Any {..} -> mergeXPaths $ Any $ simplifyAll elms
      LI.None {..} -> mergeXPaths $ None $ simplifyAll elms
      LI.PostFilter pf -> PostFilter pf

    -- Convert leaf locators that map to XPath via the existing locatorToXPathPartial logic.
    toXPath :: LI.Locator -> SimplifiedLocator
    toXPath loc = case LI.locatorToXPathPartial loc of
      LI.XPath {value} -> XPath {value}
      _ -> error "impossible: locatorToXPathPartial did not return XPath"

    mergeXPaths :: SimplifiedLocator -> SimplifiedLocator
    mergeXPaths sl = case sl of
      All {elms} ->
        xpathVals elms & maybe sl (XPath . combineWith " and ")
      Any {elms} ->
        xpathVals elms & maybe sl (XPath . combineWith " or ")
      None {elms} ->
        xpathVals elms & maybe sl (XPath . (\preds -> "//*[not(" <> preds <> ")]") . combineWith " or ")
      other -> other
      where
        xpathVals l' =
          if all isXPath l'
            then Just $ (.value) <$> l'
            else Nothing
        combineWith sep vals =
          "//*[" <> T.intercalate sep (toList $ toPred <$> vals) <> "]"
        toPred v =
          let unwrapped = T.stripPrefix "//*[" v >>= \s ->
                if T.isSuffixOf "]" s then Just (T.dropEnd 1 s) else Nothing
           in maybe ("boolean(" <> v <> ")") id unwrapped
