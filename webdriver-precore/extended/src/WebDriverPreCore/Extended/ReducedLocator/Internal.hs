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

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word8)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext)
import WebDriverPreCore.Extended.Locators.Internal (
  Locator,
  CompoundLocator,
  HttpLoc(..),
  Predicate,
  RoleLocator,
  MatchType,
  CaseSensitivity,
  InvalidLocator(..), 
  Protocol)
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

data PostFilterLoc a = PostFilter {predicate :: Predicate, locator :: a}
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
  = Role {role :: RoleLocator}
  | InnerText
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity,
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

-- | Simplified/resolved form of 'Locator', where leaf locators expressible
--   as XPath have been folded in and 'Default' has been resolved.
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

-- | Simplified/resolved form of 'Locator', where leaf locators expressible
--   as XPath have been folded in and 'Default' has been resolved.
--   Produced by 'prepareSimplify'.
data ReducedHttpLoc
  = LeafHttp LeafLoc
  | PostFilterHttpLoc (PostFilterLoc ReducedHttpLoc)
  | CombintorHttp (CombinatorLoc ReducedHttpLoc)
  deriving
    ( Show,
      Eq
    )

toHttpLocator :: ReducedLoc -> Either InvalidLocator ReducedHttpLoc
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
    Left $ MkInvalidLocator (LI.BiDiContext {context}) "BiDi-only locator cannot be used with HTTP protocol"

isXPath :: ReducedLoc -> Bool
isXPath = \case
  Leaf XPath {} -> True
  _ -> False

prepareSimplify :: (Text -> Locator) -> Protocol -> Locator -> Either InvalidLocator ReducedLoc
prepareSimplify defLoc proto l =
  simplify <$> LI.transform proto defLoc l
  where
    simplify :: CompoundLocator HttpLoc -> ReducedLoc
    simplify = \case
      LI.Leaf (CSSF {..}) -> Leaf CSS {..}
      LI.Leaf (XPathF {..}) -> Leaf XPath {..}
      LI.Leaf (RoleF {xpath}) -> Leaf XPath {value = xpath}
      LI.Leaf (InnerTextF {..}) -> Leaf . BiDiNative $ InnerText {..}
      LI.Leaf (BiDiContextF {..}) -> BiDiOnlyLeaf BiDiContext {..}
      LI.PostFilterI {predicate, locator} -> PostFilterLoc $ PostFilter {predicate, locator = simplify locator}
      LI.ContainsI {container, contained} -> Combintor $ Contains {container = simplify container, contained = simplify contained}
      LI.AllI {elms} -> Combintor . All $ simplify <$> elms
      LI.AnyI {elms} -> Combintor . Any $ simplify <$> elms

