module WebDriverPreCore.Extended.Locators.Internal (
    CompoundLocator(..),
    Locator(..),
    HttpLoc(..),
    InvalidLocator(..),
    Protocol(..),

    -- * HTTP
    transformHttp,

    -- * internal xpath utils
    roleTypeOnlyXPath,
    roleLabelText,
    xPathRelativePrefix,

    -- * BiDi
    transformBiDi,
    BiDiLoc(..),

    -- * Re-exports from Internal
    AriaRole (..),
    MatchType (..),
    CaseSensitivity(..),
    RoleLocator(..)
) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity (..))
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text, intercalate, pack, splitOn, toLower)
import Data.Text qualified as T
import Data.Word (Word8)
import GHC.Generics (Generic)
import Utils (txt, db)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext)
import Prelude
import Control.Monad ((>=>))
import Data.Function ((&))

-- ###################################### Top-level transform functions #####################################

transformHttp :: (Text -> Locator) -> Locator -> Either InvalidLocator (CompoundLocator HttpLoc)
transformHttp  = transform' toIntermediate derivedAndTagsToXPath

transformBiDi :: (Text -> Locator) -> Locator -> Either InvalidLocator (CompoundLocator BiDiLoc)
transformBiDi = transform' toIntermediateBiDi derivedAndTagsToXPathBiDi

transform' :: forall a b. (Show a, Eq a, Show b) =>
  ((Text -> Locator) -> Locator -> Either InvalidLocator (CompoundLocatorI a))
  -> (CompoundLocatorI a -> CompoundLocatorI b)
  -> (Text -> Locator)
  -> Locator
  -> Either InvalidLocator (CompoundLocator b)
transform' locToIntermediate eliminateDrived defLoc loc = do
  locI <- locToIntermediate defLoc loc
  simplified <- simplify loc locI
  Right 
    . db "!!!!!!!!! FINAL !!!!!!!" 
    . intermediateToFinal 
    . db "!!!!!!!!! eliminateDrived !!!!!!!" 
    $ eliminateDrived simplified 
    & db "!!!!!!!!! SiMplIfIED !!!!!!!"

-- ###################################### Locator types (and subtypes) #####################################

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
      Ord,
      Generic
    )

instance ToJSON RoleLocator

instance FromJSON RoleLocator

-- | Locator for use with both HTTP and BiDi protocols.
data Locator
  = -- universal
  AllElms
  | CSS {value :: Text}
  | XPath {value :: Text}
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
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord,
      Generic
    )

instance ToJSON Locator

instance FromJSON Locator

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
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON AriaRole

instance FromJSON AriaRole

data MatchType = Full | Starts | Partial | Wildcard deriving (Show, Eq, Ord, Generic)

instance ToJSON MatchType

instance FromJSON MatchType

data CaseSensitivity = CaseSensitive | CaseInsensitive deriving (Show, Eq, Ord, Generic)

instance ToJSON CaseSensitivity

instance FromJSON CaseSensitivity

data Protocol = HTTP | BiDi deriving (Show, Eq)

data InvalidLocator = MkInvalidLocator {loc :: Locator, description :: Text} deriving (Show, Eq, Ord, Generic)

instance ToJSON InvalidLocator

instance FromJSON InvalidLocator

instance Exception InvalidLocator

-- ###################################### HTTP types #####################################

-- | LocatorI an intermediate type representing leaf locators.
--   Compound locators are represented by 'CompoundLocator'.
data LocatorI
  = CSSI {value :: Text}
  | XPathI {value :: Text}
  | RoleI {roleSpec :: RoleLocator, xpath :: Text}
  deriving (Show, Eq, Ord)
data HttpLoc
  = 
    CSSHttp {value :: Text}
  | XPathHttp {value :: Text} 
  | RoleHttp {roleSpec :: RoleLocator, xpath :: Text}
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

instance ToJSON HttpLoc

instance FromJSON HttpLoc

-- ###################################### HTTP functions #####################################

-----------------------------------------------------------------------------
-- 1: Initial conversion (Locator → CompoundLocator LocatorI)
-----------------------------------------------------------------------------

toIntermediate :: (Text -> Locator) -> Locator -> Either InvalidLocator (CompoundLocatorI LocatorI)
toIntermediate defLoc loc = 
  case loc of
    -- fallable conversions
    Attribute {name, value, matchType, caseSensitivity}
      | T.null name || T.null value ->
          failLocator "Attribute locator has empty name or value"
      | otherwise ->
          Right $ XPathDerivedLeaf {tagM = Nothing, body = attrPredX name value matchType caseSensitivity}
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
    -- leaf conversions
    CSS {value} -> leaf $ CSSI {value}
    XPath {value} -> leaf $ XPathI {value}
    AllElms -> Right $ XPathDerivedLeaf {tagM = Nothing, body = "true()"}
    ID {value} -> Right $ XPathDerivedLeaf {tagM = Nothing, body = "@id='" <> value <> "'"}
    Class {value, matchType, caseSensitivity} ->
      Right $ XPathDerivedLeaf {tagM = Nothing, body = classPredX value matchType caseSensitivity}
    Tag {value} -> Right $ TagLeaf {tag = value}
    Role {role} -> leaf $ RoleI {roleSpec = role, xpath = roleToXPath role}
    InnerText {..} ->
      Right $ XPathDerivedLeaf {tagM = Nothing, body = innerTextPredX value caseSensitivity matchType maxDepth}
  where 
    leaf = Right . LeafI
    failLocator msg = Left $ MkInvalidLocator loc msg

-----------------------------------------------------------------------------
-- 3: Final conversion (HTTP)
-----------------------------------------------------------------------------

derivedAndTagsToXPath :: CompoundLocatorI LocatorI -> CompoundLocatorI HttpLoc
derivedAndTagsToXPath = convertTagsXPathIDs . convertContains

-- | Merge adjacent user-provided XPath locators in Contains at the LocatorI stage.
--   This avoids text manipulation since the relative prefix ".//", brackets, etc.
--   haven't been added yet. Only handles XPathI (user-provided XPath).
convertContains :: CompoundLocatorI LocatorI -> CompoundLocatorI LocatorI
convertContains = mapCompoundLocBottomUp $ \case
  -- Merge two user-provided XPaths
  ContainsI (LeafI (XPathI {value = containerXPath})) (LeafI (XPathI {value = containedXPath})) ->
    LeafI $ XPathI {value = containerXPath <> "//" <> containedXPath}
  -- Keep other combinations as ContainsI - they'll be handled after conversion to HttpLoc
  other -> other

-- | Convert LocatorI leaves and XPathDerivedLeaf/TagLeaf to HttpLoc.
convertTagsXPathIDs :: CompoundLocatorI LocatorI -> CompoundLocatorI HttpLoc
convertTagsXPathIDs = go
  where
    go = \case
      XPathDerivedLeaf {tagM, body} -> LeafI $ XPathHttp {value = xPathIDTxt tagM body}
      TagLeaf {tag} -> LeafI $ XPathHttp {value = xPathRelativePrefix <> tag}
      LeafI (CSSI {..}) -> LeafI $ CSSHttp {..}
      LeafI (XPathI {..}) -> LeafI $ XPathHttp {..}
      LeafI (RoleI {..}) -> LeafI $ RoleHttp {..}
      ContainsI c d -> ContainsI (go c) (go d)
      AllI elms -> AllI (go <$> elms)
      AnyI elms -> AnyI (go <$> elms)

xPathIDTxt :: Maybe Text -> Text -> Text
xPathIDTxt tagM body = xPathRelativePrefix <> fromMaybe "*" tagM <> "[" <> body <> "]"

-- ###################################### BiDi types #####################################

data LocatorIBiDi
  = CSSIBiDI {value :: Text}
  | XPathIBiDI {value :: Text}
  | RoleIBiDI {roleSpec :: RoleLocator}
  | BiDiContextI {context :: BrowsingContext}
  | InnerTextIBiDI
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity,
        maxDepth :: Maybe Word8
      }
  deriving (Show, Eq, Ord)

data BiDiLoc
  = 
    CSSBiDi {value :: Text}
  | XPathBiDi {value :: Text}
  | RoleBiDi {roleSpec :: RoleLocator}
  | ContextBiDi {context :: BrowsingContext}
  | InnerTextBiDi
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity,
        maxDepth :: Maybe Word8
      }
  deriving
    ( -- | WithOptions {base :: LocatLocatorIor, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )

-- ###################################### BiDi functions #####################################

-----------------------------------------------------------------------------
-- 1: Initial conversion (Locator → CompoundLocator LocatorIBiDi)
-----------------------------------------------------------------------------

toIntermediateBiDi :: (Text -> Locator) -> Locator -> Either InvalidLocator (CompoundLocatorI LocatorIBiDi)
toIntermediateBiDi defLoc loc = 
  case loc of
    -- fallable conversions
    Attribute {name, value, matchType, caseSensitivity}
      | T.null name || T.null value ->
          failLocator "Attribute locator has empty name or value"
      | otherwise ->
          Right $ XPathDerivedLeaf {tagM = Nothing, body = attrPredX name value matchType caseSensitivity}
    Default {value} ->
      let resolved = defLoc value
      in if hasDefault resolved
        then failLocator "Default locator cannot resolve to another Default"
        else toIntermediateBiDi defLoc resolved
    BiDiContext {context} -> 
      leaf $ BiDiContextI {context}
    Contains {container, contained} ->
      ContainsI <$> toIntermediateBiDi defLoc container <*> toIntermediateBiDi defLoc contained
    All {elms} ->
      AllI <$> traverse (toIntermediateBiDi defLoc) elms
    Any {elms} ->
      AnyI <$> traverse (toIntermediateBiDi defLoc) elms
    -- leaf conversions
    CSS {value} -> leaf $ CSSIBiDI {value}
    XPath {value} -> leaf $ XPathIBiDI {value}
    AllElms -> Right $ XPathDerivedLeaf {tagM = Nothing, body = "true()"}
    ID {value} -> Right $ XPathDerivedLeaf {tagM = Nothing, body = "@id='" <> value <> "'"}
    Class {value, matchType, caseSensitivity} ->
      Right $ XPathDerivedLeaf {tagM = Nothing, body = classPredX value matchType caseSensitivity}
    Tag {value} -> Right $ TagLeaf {tag = value}
    Role {role} -> leaf $ RoleIBiDI {roleSpec = role}
    InnerText {value, matchType, caseSensitivity, maxDepth} ->
      leaf $ InnerTextIBiDI {value, matchType, caseSensitivity, maxDepth}
  where 
    leaf = Right . LeafI
    failLocator msg = Left $ MkInvalidLocator loc msg

-----------------------------------------------------------------------------
-- 3: Final conversion (BiDi)
-----------------------------------------------------------------------------

derivedAndTagsToXPathBiDi :: CompoundLocatorI LocatorIBiDi -> CompoundLocatorI BiDiLoc
derivedAndTagsToXPathBiDi = convertTagsXPathIDsBiDi . convertContainsBiDi

-- | Merge adjacent user-provided XPath locators in Contains at the LocatorIBiDi stage.
convertContainsBiDi :: CompoundLocatorI LocatorIBiDi -> CompoundLocatorI LocatorIBiDi
convertContainsBiDi = mapCompoundLocBottomUp $ \case
  ContainsI (LeafI (XPathIBiDI {value = containerXPath})) (LeafI (XPathIBiDI {value = containedXPath})) ->
    LeafI $ XPathIBiDI {value = containerXPath <> "//" <> containedXPath}
  other -> other

-- | Convert LocatorIBiDi leaves and XPathDerivedLeaf/TagLeaf to BiDiLoc.
convertTagsXPathIDsBiDi :: CompoundLocatorI LocatorIBiDi -> CompoundLocatorI BiDiLoc
convertTagsXPathIDsBiDi = go
  where
    go = \case
      XPathDerivedLeaf {tagM, body} -> LeafI $ XPathBiDi {value = xPathIDTxt tagM body}
      TagLeaf {tag} -> LeafI $ XPathBiDi {value = xPathRelativePrefix <> tag}
      LeafI (CSSIBiDI {..}) -> LeafI $ CSSBiDi {..}
      LeafI (XPathIBiDI {..}) -> LeafI $ XPathBiDi {..}
      LeafI (RoleIBiDI {..}) -> LeafI $ RoleBiDi {..}
      LeafI (BiDiContextI {..}) -> LeafI $ ContextBiDi {..}
      LeafI (InnerTextIBiDI {..}) -> LeafI $ InnerTextBiDi {..}
      ContainsI c d -> ContainsI (go c) (go d)
      AllI elms -> AllI (go <$> elms)
      AnyI elms -> AnyI (go <$> elms)

-- ###################################### Common transformation types #####################################

-- | CompoundLocator represents the tree structure of composed locators.
data CompoundLocatorI a
  = LeafI {getLeaf :: a}
  | TagLeaf {tag :: Text}
  | XPathDerivedLeaf {tagM :: Maybe Text, body :: Text}
  | ContainsI {container :: CompoundLocatorI a, contained :: CompoundLocatorI a}
  | AllI {elms :: NonEmpty (CompoundLocatorI a)}
  | AnyI {elms :: NonEmpty (CompoundLocatorI a)}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data CompoundLocator a
  = LeafC {getLeaf :: a}
  | ContainsC {container :: CompoundLocator a, contained :: CompoundLocator a}
  | AllC {elms :: NonEmpty (CompoundLocator a)}
  | AnyC {elms :: NonEmpty (CompoundLocator a)}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance ToJSON a => ToJSON (CompoundLocator a)

instance FromJSON a => FromJSON (CompoundLocator a)

-- ###################################### Common transformation functions #####################################

-- | Convert from the intermediate representation (which may contain
--   'TagLeaf' and 'XPathDerivedLeaf') to the final representation.
--   These constructors should have been eliminated during simplification;
--   if encountered, they indicate a bug.
intermediateToFinal :: CompoundLocatorI a -> CompoundLocator a
intermediateToFinal = \case
  LeafI a -> LeafC a
  TagLeaf {} -> error "toCompoundLocator: unexpected TagLeaf — should have been eliminated during simplification"
  XPathDerivedLeaf {} -> error "toCompoundLocator: unexpected XPathDerivedLeaf — should have been eliminated during simplification"
  ContainsI c d -> ContainsC (intermediateToFinal c) (intermediateToFinal d)
  AllI elms -> AllC (intermediateToFinal <$> elms)
  AnyI elms -> AnyC (intermediateToFinal <$> elms)

-- | Shared simplification pipeline: flatten, merge, distribute tags, unwrap.
--   Fixed-point loop — repeats until no more simplifications apply.
simplify :: (Show a, Eq a )=> Locator -> CompoundLocatorI a -> Either InvalidLocator (CompoundLocatorI a)
simplify srcLoc current = do
  merged <- mergeContiguous srcLoc $ unnestAnysAlls current
  tagged <- distributeTagsInAll srcLoc merged & db "!!!!!!!!! merged !!!!!!!"
  let unwrapped = unwrapSingletonCombinators tagged
  if unwrapped == current
    then pure current  & db "!!!!!!!!! final simplify !!!!!!!"
    else simplify srcLoc unwrapped

-----------------------------------------------------------------------------
-- 2a. Flatten nested AllI/AnyI
-----------------------------------------------------------------------------

unnestAnysAlls :: CompoundLocatorI a -> CompoundLocatorI a
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
  other -> other

-----------------------------------------------------------------------------
-- 2b. Combine contiguous XPathIDs
-----------------------------------------------------------------------------
mergeContiguous :: Show a => Locator -> CompoundLocatorI a -> Either InvalidLocator (CompoundLocatorI a)
mergeContiguous srcLoc li = db "!!!!!!!!! mergeContiguous !!!!!!!" . mergeAnys <$> mergeAlls srcLoc li

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

mergeAlls :: Locator -> CompoundLocatorI a -> Either InvalidLocator (CompoundLocatorI a)
mergeAlls srcLoc = 
  mapCompoundLocBottomUpM (\case 
   AllI elms -> AllI <$> mergeAllElms elms
   other -> Right other)
 where
   mergeAllElms :: NonEmpty (CompoundLocatorI a) -> Either InvalidLocator (NonEmpty (CompoundLocatorI a))
   mergeAllElms = \case
       (XPathDerivedLeaf tm1 b1 :| XPathDerivedLeaf tm2 b2 : rest) -> 
         do 
          tag <- mergeTags srcLoc tm1 tm2
          mergeAllElms $ XPathDerivedLeaf {tagM = tag, body = bracket b1 <> " and " <> bracket b2} :| rest 
       x -> Right x

mergeAnys :: CompoundLocatorI a -> CompoundLocatorI a
mergeAnys  = 
  mapCompoundLocBottomUp (\case 
   AnyI elms -> AnyI $ mergeAnyElms elms
   other -> other)
 where
   mergeAnyElms :: NonEmpty (CompoundLocatorI a) -> NonEmpty (CompoundLocatorI a)
   mergeAnyElms = \case
       -- Merge XPathIDs with identical tags
       (XPathDerivedLeaf tm1 b1 :| XPathDerivedLeaf tm2 b2 : rest) ->
        -- if tags are identical then they can be merged 
        -- Nothing cannot be merged with Just because Nothing represents any tag
         mergeAnyElms $ 
          if tm1 == tm2 then 
            XPathDerivedLeaf tm1 (bracket b1 <> " or " <> bracket b2) :| rest
          else
            XPathDerivedLeaf Nothing (xpathTxt1 <> " or " <> xpathTxt2) :| rest
          where
            xpathTxt1 = inlineTag tm1 b1
            xpathTxt2 = inlineTag tm2 b2
            inlineTag tagM body =
              tagM & maybe 
                (bracket body)
                \t -> "(" <> selfTag t <> " and " <> bracket body <> ")"
                          
       -- Tag overrides XPathDerived with the same tag (eg div or div && class => div as all div && class will satisfy div)
       (TagLeaf tag :| XPathDerivedLeaf {tagM = Just tm} : rest)  | tag == tm ->
          mergeAnyElms $ TagLeaf tag :| rest
       -- as above flipped
       (XPathDerivedLeaf {tagM = Just tm} :| TagLeaf tag : rest) | tag == tm ->
         mergeAnyElms $ TagLeaf tag :| rest
       
       -- TagLeaf followed by XPathDerived with wildcard (no tag)
       -- Any(Tag "div", Class "foo") → .//*[self::div or (...)]
       (TagLeaf tag :| XPathDerivedLeaf {tagM = Nothing, body} : rest) ->
         mergeAnyElms $ XPathDerivedLeaf {tagM = Nothing, body = selfTag tag <> " or " <> bracket body} :| rest
       -- as above flipped
       (XPathDerivedLeaf {tagM = Nothing, body} :| TagLeaf tag : rest) ->
         mergeAnyElms $ XPathDerivedLeaf {tagM = Nothing, body = bracket body <> " or " <> selfTag tag} :| rest

       (TagLeaf tag1  :| TagLeaf tag2 : rest) ->
         mergeAnyElms $ XPathDerivedLeaf {tagM = Nothing, body = selfTag tag1 <> " or " <> selfTag tag2} :| rest
       
       x -> x
    where 
      selfTag :: Text -> Text
      selfTag tag = "self::" <> tag
           

-----------------------------------------------------------------------------
-- 2c. Assign tags
-----------------------------------------------------------------------------

-- | Collect all tag values from TagLeaf and tagged XPathDerivedLeaf leaves at all depths,
--   including inside nested AnyI/ContainsI/AllI combinators.
collectTags :: CompoundLocatorI a -> [Text]
collectTags = \case
  TagLeaf t -> [t]
  XPathDerivedLeaf {tagM = Just t} -> [t]
  XPathDerivedLeaf {} -> []
  LeafI _ -> []
  ContainsI c d -> collectTags c <> collectTags d
  AllI xs -> toList xs >>= collectTags
  AnyI xs -> toList xs >>= collectTags

-- copy tags from TagLeaf and XPathDerivedLeaf to all reachable XPathDerivedLeaf descendants, and remove the TagLeaf if possible.
distributeTagsInAll :: Locator -> CompoundLocatorI a -> Either InvalidLocator (CompoundLocatorI a)
distributeTagsInAll srcLocator = 
    mapCompoundLocBottomUpM (\case
      AllI elms -> AllI <$> distributeTagsToAllElms  elms
      other -> Right other)
    where
      distributeTagsToAllElms ::  NonEmpty (CompoundLocatorI a) -> Either InvalidLocator (NonEmpty (CompoundLocatorI a))
      distributeTagsToAllElms elms = do
        let tagVals = nub $ toList elms >>= collectTags
            -- Tags from direct children (TagLeaf or tagged XPathDerivedLeaf only, no recursion).
            -- Used to decide whether to distribute: if all tags come from nested
            -- combinators (AnyI/ContainsI etc.) with no direct child tag source,
            -- skip distribution to avoid corrupting unrelated branches.
            directTagVals = nub . catMaybes $ toList elms >>= \case
              TagLeaf t -> [Just t]
              XPathDerivedLeaf {tagM = Just t} -> [Just t]
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
                [] -> error "distributeTagsInAll: empty result"
                x : xs -> Right (x :| xs)
            where
                elements = toList elms
                distributed = copyTagToXPath t <$> elms
                canRemove = all isTagOrXPathID distributed && not (all isTag elements)
                result = if canRemove 
                        then filter isNotTag $ toList distributed
                        else toList distributed
      
      copyTagToXPath :: Text -> CompoundLocatorI a -> CompoundLocatorI a
      copyTagToXPath t = \case
          XPathDerivedLeaf _ body -> XPathDerivedLeaf {tagM = Just t, body}
          other -> other

      isTagOrXPathID :: CompoundLocatorI a -> Bool
      isTagOrXPathID = \case
          XPathDerivedLeaf {} -> True
          TagLeaf {} -> True
          _ -> False

      isTag :: CompoundLocatorI a -> Bool
      isTag = \case
          TagLeaf {} -> True
          _ -> False

      isNotTag :: CompoundLocatorI a -> Bool
      isNotTag = not . isTag


-----------------------------------------------------------------------------
-- 2d. Unwrap single-child combinators
-----------------------------------------------------------------------------

unwrapSingletonCombinators :: CompoundLocatorI a -> CompoundLocatorI a
unwrapSingletonCombinators = mapCompoundLocBottomUp $ \case
  AllI (x :| []) -> x
  AnyI (x :| []) -> x
  other -> other

-- | Map over a 'CompoundLocator' tree bottom-up, with monadic effects.
--   that support it (e.g. 'Either', 'Maybe').
mapCompoundLocBottomUpM :: Monad m => (CompoundLocatorI a -> m (CompoundLocatorI a)) -> CompoundLocatorI a -> m (CompoundLocatorI a)
mapCompoundLocBottomUpM f = recurse >=> f
  where
    mapf = mapCompoundLocBottomUpM f
    recurse = \case
      ContainsI c d -> ContainsI <$> mapf c <*> mapf d
      AllI elms -> AllI <$> traverse mapf elms
      AnyI elms -> AnyI <$> traverse mapf elms
      other -> pure other
 
-- | Map over a 'CompoundLocator' tree bottom-up.
--   Expressed via 'mapCompoundLocBottomUpM' using the 'Identity' monad.
mapCompoundLocBottomUp :: (CompoundLocatorI a -> CompoundLocatorI a) -> CompoundLocatorI a -> CompoundLocatorI a
mapCompoundLocBottomUp f = runIdentity . mapCompoundLocBottomUpM (Identity . f)

-- ###################################### Common utils #####################################

xPathRelativePrefix :: Text
xPathRelativePrefix = ".//"

-- | XPath predicate to exclude elements with presentation or none roles.
-- Used in RoleName locators to match only elements that are not explicitly
-- excluded from the accessibility tree.
excludedRolesTxt :: Text
excludedRolesTxt = "*[not(@role='presentation' or @role='none')]"

-----------------------------------------------------------------------------
-- XPath predicate helpers
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
--   Returns predicate brackets that can be used in XPathIDerived body.
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

-----------------------------------------------------------------------------
-- Role helpers
-----------------------------------------------------------------------------

roleLabelText :: AriaRole -> Text
roleLabelText = toLower . pack . show

-- | Generate XPath for role type predicates only (without .// prefix or name matching).
-- Used as a building block for constructing role-based XPath expressions.
roleTypeOnlyXPath :: RoleLocator -> Text
roleTypeOnlyXPath = \case
  RoleFull {role} -> "*" <> roleTypeXPathContent True role
  RoleName {} -> excludedRolesTxt
  RoleType {role} -> "*" <> roleTypeXPathContent True role

roleToXPath :: RoleLocator -> Text
roleToXPath roleLoc = case roleLoc of
  RoleFull {name} -> xPathRelativePrefix <> roleTypeOnlyXPath roleLoc <> name' name
  RoleType {} -> xPathRelativePrefix <> roleTypeOnlyXPath roleLoc
  RoleName {name} -> xPathRelativePrefix <> roleTypeOnlyXPath roleLoc <> name' name
  where
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

-----------------------------------------------------------------------------
-- Locator tree utilities
-----------------------------------------------------------------------------

-- | Fold top-down over a Locator tree with an accumulator, similar to foldl.
foldLoc :: (a -> Locator -> a) -> a -> Locator -> a
foldLoc f acc loc =
  case loc of
    Contains p c -> foldLoc f (foldLoc f acc' p) c
    All locs -> foldList locs
    Any locs -> foldList locs
    -- WithOptions base _ -> foldLoc f acc' base
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