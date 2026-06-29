module WebDriverPreCore.Extended.Locators.Internal where

import Control.Exception (Exception)
import Data.Foldable1 (foldl1')
import Data.Functor.Identity (Identity (..))
import Data.List (nub, uncons)
import Data.List qualified as LST
import Data.List.NonEmpty (NonEmpty (..), groupBy, sortBy, toList)
import Data.Maybe (fromJust, fromMaybe, catMaybes, isJust)
import Data.Text (Text, intercalate, pack, splitOn, toLower, unpack)
import Data.Text qualified as T
import Data.Word (Word8)
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext, NodeProperties)
import Prelude
import Data.Function ((&))
import Control.Monad ((>=>))
import Data.List.NonEmpty (partition)

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

-- | LocatorI an intermdiate type 
data LocatorI
  = 
    CSSI {value :: Text}
  | XPathI {value :: Text}
  | XPathID {
      tagM :: Maybe Text, 
      body :: Text
      } 
  | TagI {tag :: Text} 
  | RoleI {xpath :: Text}
  | InnerTextI
      { value :: Text,
        matchType :: MatchType,
        caseSensitivity :: CaseSensitivity,
        maxDepth :: Maybe Word8
      }
  | -- exclusive
    -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
    BiDiContextI {context :: BrowsingContext}
  | -- combinators
    ContainsI {container :: LocatorI, contained :: LocatorI}
  | AllI {elms :: NonEmpty LocatorI}
  | AnyI {elms :: NonEmpty LocatorI}
  | --- PostFilter
    PostFilterI
      { predicate :: Predicate,
        locator :: LocatorI
      }
  deriving
    ( -- | WithOptions {base :: Locator, options :: [LocatorDirectives]}
      Show,
      Eq,
      Ord
    )

transform :: Protocol -> (Text -> Locator) -> Locator -> Either InvalidLocator LocatorI
transform proto defLoc loc = do
  locI <- convertLoc proto defLoc loc
  simplified <- simplify locI
  Right $ derivedAndTagsToXPath simplified
  where
    simplify :: LocatorI -> Either InvalidLocator LocatorI
    simplify current = do
      merged <- mergeContiguous loc $ unnestAnysAlls current
      tagged <- assignTags loc merged
      let unwrapped = unwrapSingletonCombinators tagged
      if unwrapped == current
        then pure current
        else simplify unwrapped

-----------------------------------------------------------------------------
-- Phase 1: Initial conversion (Locator → LocatorI)
-----------------------------------------------------------------------------

convertLoc :: Protocol -> (Text -> Locator) -> Locator -> Either InvalidLocator LocatorI
convertLoc proto defLoc loc = 
  case loc of
    -- fallable conversions
    Attribute {name, value, matchType, caseSensitivity}
      | T.null name || T.null value ->
          failLocator "Attribute locator has empty name or value"
      | otherwise ->
          Right $ XPathID {tagM = Nothing, body = attrPredX name value matchType caseSensitivity}
    Default {value} ->
      let resolved = defLoc value
      in if hasDefault resolved
        then failLocator "Default locator cannot resolve to another Default"
        else convertLoc proto defLoc resolved
    BiDiContext {context} -> case proto of
      BiDi -> Right $ BiDiContextI {context}
      HTTP -> failLocator "BiDiContext locator cannot be used with HTTP protocol"
      -- combinators / postfilter require higher order conversions
    _ -> case loc of
      Contains {container, contained} ->
        ContainsI <$> convertLoc proto defLoc container <*> convertLoc proto defLoc contained
      All {elms} ->
        AllI <$> traverse (convertLoc proto defLoc) elms
      Any {elms} ->
        AnyI <$> traverse (convertLoc proto defLoc) elms
      PostFilter {predicate, locator} ->
        PostFilterI predicate <$> convertLoc proto defLoc locator
      -- simple pass through conversions / xpath
      _ -> Right $ case loc of
        CSS {value} -> CSSI {value}
        XPath {value} -> XPathI {value}
        AllElms -> XPathID {tagM = Nothing, body = "true()"}
        ID {value} -> XPathID {tagM = Nothing, body = "@id='" <> value <> "'"}
        Class {value, matchType, caseSensitivity} ->
          XPathID {tagM = Nothing, body = classPredX value matchType caseSensitivity}
        Tag {value} -> TagI {tag = value}
        Role {role} -> RoleI {xpath = roleToXPath role}
        InnerText {value, matchType, caseSensitivity, maxDepth} ->
          InnerTextI {value, matchType, caseSensitivity, maxDepth}
    where 
      failLocator msg = Left $ MkInvalidLocator loc msg

-----------------------------------------------------------------------------
-- Phase 2: Simplify loop (fixed-point)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- 2a. Flatten nested AllI/AnyI
-----------------------------------------------------------------------------

unnestAnysAlls :: LocatorI -> LocatorI
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
  other -> other

-----------------------------------------------------------------------------
-- 2b. Combine contiguous XPathIDs
-----------------------------------------------------------------------------
mergeContiguous :: Locator -> LocatorI -> Either InvalidLocator LocatorI
mergeContiguous srcLoc li = mergeAnys srcLoc <$> mergeAlls srcLoc li

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

mergeAlls :: Locator -> LocatorI -> Either InvalidLocator LocatorI
mergeAlls srcLoc = 
  mapOrFailLocIBottomUp (\case 
   AllI elms -> AllI <$> mergeAllElms elms
   other -> Right other)
 where
   mergeAllElms :: NonEmpty LocatorI -> Either InvalidLocator (NonEmpty LocatorI)
   mergeAllElms = \case
       (XPathID tm1 b1 :| XPathID tm2 b2 : rest) -> 
         do 
          tag <- mergeTags srcLoc tm1 tm2
          mergeAllElms $ XPathID {tagM = tag, body = bracket b1 <> " and " <> bracket b2} :| rest 
       x -> Right x

mergeAnys :: Locator -> LocatorI -> LocatorI
mergeAnys srcLoc = 
  mapLocIBottomUp (\case 
   AnyI elms -> AnyI $ mergeAnyElms elms
   other -> other)
 where
   mergeAnyElms :: NonEmpty LocatorI -> NonEmpty LocatorI
   mergeAnyElms = \case
       (XPathID tm1 b1 :| XPathID tm2 b2 : rest) ->
         mergeTags srcLoc tm1 tm2 & either
           (\_ ->
             -- Tags incompatible, keep first as-is, try merging from second onward
             let rest' = mergeAnyElms (XPathID tm2 b2 :| rest)
             in XPathID tm1 b1 :| toList rest')
           (\tag ->
             -- Tags compatible, merge these two and continue
             mergeAnyElms $ XPathID {tagM = tag, body = bracket b1 <> " or " <> bracket b2} :| rest)
       x -> x

-----------------------------------------------------------------------------
-- 2c. Assign tags
-----------------------------------------------------------------------------

tagTxt :: LocatorI -> Maybe Text
tagTxt  = \case 
  TagI t -> Just t
  _ -> Nothing

isTagI :: LocatorI -> Bool
isTagI  = isJust . tagTxt

isNotTage :: LocatorI -> Bool
isNotTage = not . isTagI

-- copy tags from TagI and XPathID to all reachable XPathID descendants, and remove the TagI if possible.
assignTags :: Locator -> LocatorI -> Either InvalidLocator LocatorI
assignTags srcLocator = 
    mapLocIBottomUpM (\case
      AllI elms -> AllI <$> distributeTagsToAllElms srcLocator elms
      AnyI elms -> AnyI <$> mergeTagsInAny elms
      other -> Right other)

-- | 2c-i: Process TagI constructors within an AnyI.
mergeTagsInAny :: NonEmpty LocatorI -> Either InvalidLocator (NonEmpty LocatorI)
mergeTagsInAny elms = do
  let children = toList elms
      (tags, others) = LST.partition isTagI children
      tagXPaths = 
        case tags of
          [] -> []
          [TagI t] -> [XPathI ("//" <> t)]
          ts -> [XPathI ("//" <> T.intercalate " | " (catMaybes (tagTxt <$> ts)))]
      result = tagXPaths <> others
  case result of
    [] -> error "mergeTagsInAny: empty result"
    (x : xs) -> Right (x :| xs)


-- | 2c-ii + 2c-iii: Process TagI constructors within an AllI.
distributeTagsToAllElms :: Locator -> NonEmpty LocatorI -> Either InvalidLocator (NonEmpty LocatorI)
distributeTagsToAllElms srcLocator elms = do
  let tagVals = nub . catMaybes $ tagVal <$> toList elms
  
  -- 2c-ii: Contradictory tag detection
  case tagVals of
     -- no tags, nothing to check
    [] -> pure elms 
     -- contradictory tags
    _ : _ : _ -> Left . MkInvalidLocator srcLocator $ 
      "Contradictory tags in All combinator: " <> T.intercalate ", " tagVals
     -- singleton tag -> check if removable
    [t] ->
      case result of
        -- empty list wont happen unless there is a bug
        [] -> error "processAllI: empty result"
        x : xs -> Right (x :| xs)
      where
          elements = toList elms
          distributed = distributeTagToDescendantsOfAll t <$> elms
          canRemove = all allDescendantsXPathIDOrTag distributed && not (all isTagI elements)
          result = if canRemove 
                   then filter isNotTage $ toList distributed
                   else toList distributed
  where
    tagVal :: LocatorI -> Maybe Text
    tagVal = \case 
      TagI t -> Just t
      -- not expected that there would be any XPathID with a tag at this point
      -- but include here for future proofing
      XPathID {tagM = Just t} -> Just t
      _ -> Nothing

    -- | Set tagM = Just t on all reachable XPathID descendants.
    -- Traverses through AllI, AnyI, and the contained side of ContainsI.
    -- Container side of ContainsI is NOT tagged.
    distributeTagToDescendantsOfAll :: Text -> LocatorI -> LocatorI
    distributeTagToDescendantsOfAll t = \case
      XPathID _ body -> XPathID {tagM = Just t, body}
      AllI elms' -> AllI $ cascade <$> elms'
      AnyI elms' -> AnyI $ cascade <$> elms'
      ContainsI container contained -> ContainsI container (cascade contained)
      other -> case other of 
        PostFilterI {} -> other
        RoleI {} -> other
        XPathI {} -> other
        CSSI {} -> other
        InnerTextI {} -> other
        BiDiContextI {} -> other
        TagI {} -> other
     where 
      cascade :: LocatorI -> LocatorI
      cascade = distributeTagToDescendantsOfAll t 

    -- | Check if all reachable descendants are XPathID or TagI constructors.
    -- Traverses through AllI, AnyI, and the contained side of ContainsI.
    allDescendantsXPathIDOrTag :: LocatorI -> Bool
    allDescendantsXPathIDOrTag = \case
      XPathID {} -> True
      TagI {} -> True
      AllI elms' -> all allDescendantsXPathIDOrTag elms'
      AnyI elms' -> all allDescendantsXPathIDOrTag elms'
      ContainsI _container contained -> allDescendantsXPathIDOrTag contained
      PostFilterI {} -> False
      RoleI {} -> False
      XPathI {} -> False
      CSSI {} -> False
      InnerTextI {} -> False
      BiDiContextI {} -> False


-----------------------------------------------------------------------------
-- 2d. Unwrap single-child combinators
-----------------------------------------------------------------------------

unwrapSingletonCombinators :: LocatorI -> LocatorI
unwrapSingletonCombinators = mapLocIBottomUp $ \case
  AllI (x :| []) -> x
  AnyI (x :| []) -> x
  other -> other

-----------------------------------------------------------------------------
-- Phase 3: Final conversion
-----------------------------------------------------------------------------

derivedAndTagsToXPath :: LocatorI -> LocatorI
derivedAndTagsToXPath = convertContains . convertTags . convertXPathIDs

convertXPathIDs :: LocatorI -> LocatorI
convertXPathIDs = mapLocIBottomUp $ \case
  XPathID {tagM, body} ->
    XPathI {value = "//" <> fromMaybe "*" tagM <> body}
  other -> other

convertTags :: LocatorI -> LocatorI
convertTags = mapLocIBottomUp $ \case
  TagI {tag} -> XPathI {value = "//" <> tag}
  other -> other

convertContains :: LocatorI -> LocatorI
convertContains = mapLocIBottomUp $ \case
  ContainsI (XPathI {value = containerXPath}) (XPathI {value = containedXPath}) ->
    XPathI $ containerXPath <> containedXPath
  other -> other

-----------------------------------------------------------------------------
-- Top-level XPath predicate helpers (extracted from locatorToXPathPartial)
-----------------------------------------------------------------------------

-- | XPath predicate for CSS class matching.
classPredX :: Text -> MatchType -> CaseSensitivity -> Text
classPredX val mt cs =
  let classAttr = applyCSText cs "@class"
      matchVal = lowerIfCIText cs val
  in case mt of
       Full ->
         "contains(concat(' ', normalize-space(" <> classAttr <> "), ' '), ' " <> matchVal <> " ')"
       Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
       Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
       Wildcard -> wildcardPredX classAttr matchVal

-- | XPath predicate for named attribute matching.
attrPredX :: Text -> Text -> MatchType -> CaseSensitivity -> Text
attrPredX name val mt cs =
  let attrExpr = applyCSText cs ("@" <> name)
      matchVal = lowerIfCIText cs val
  in case mt of
       Full -> attrExpr <> "='" <> matchVal <> "'"
       Partial -> "contains(" <> attrExpr <> ", '" <> matchVal <> "')"
       Starts -> "starts-with(" <> attrExpr <> ", '" <> matchVal <> "')"
       Wildcard -> wildcardPredX attrExpr matchVal

applyCSText :: CaseSensitivity -> Text -> Text
applyCSText CaseSensitive expr = expr
applyCSText CaseInsensitive expr =
  "translate(" <> expr <> ", '" <> upperAlpha <> "', '" <> lowerAlpha <> "')"

lowerIfCIText :: CaseSensitivity -> Text -> Text
lowerIfCIText CaseSensitive v = v
lowerIfCIText CaseInsensitive v = toLower v

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

-- | Map over a LocatorI tree bottom-up, with monadic effects.
--   Recursively transforms children first, then applies the function to the
--   reconstructed node.  Short-circuits on the first failure for instances
--   that support it (e.g. 'Either', 'Maybe').
mapLocIBottomUpM :: Monad m => (LocatorI -> m LocatorI) -> LocatorI -> m LocatorI
mapLocIBottomUpM f = (\case 
    ContainsI c d -> ContainsI <$> recurse c <*> recurse d
    AllI elms -> AllI <$> recurseMap elms
    AnyI elms -> AnyI <$> recurseMap elms
    PostFilterI p l -> PostFilterI p <$> recurse l
    l -> pure l)
    >=> f
  where
    recurse = mapLocIBottomUpM f
    recurseMap = traverse recurse

-- | Map over a LocatorI tree bottom-up.
--   Expressed via 'mapLocIBottomUpM' using the 'Identity' monad.
mapLocIBottomUp :: (LocatorI -> LocatorI) -> LocatorI -> LocatorI
mapLocIBottomUp f = runIdentity . mapLocIBottomUpM (Identity . f)

-- | Specialization of 'mapLocIBottomUpM' to 'Either InvalidLocator'.
mapOrFailLocIBottomUp :: (LocatorI -> Either InvalidLocator LocatorI) -> LocatorI -> Either InvalidLocator LocatorI
mapOrFailLocIBottomUp = mapLocIBottomUpM

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

displayAriaRole :: AriaRole -> Text
displayAriaRole = toLower . pack . show

roleToXPath :: RoleLocator -> Text
roleToXPath = \case
  RoleFull {role, name} -> "//*" <> role' role <> name' name
  RoleType {role} -> "//*" <> role' role
  RoleName {name} -> "//*[not(@role='presentation' or @role='none')]" <> name' name
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
    content = implicitRoleXPath r <> " or @role='" <> displayAriaRole r <> "'"


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

innerTextToXPath :: Text -> CaseSensitivity -> MatchType -> Maybe Word8 -> Text
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

data InvalidLocator = MkInvalidLocator {loc :: Locator, description :: Text} deriving (Show, Eq, Ord)

instance Exception InvalidLocator

prepare :: (Text -> Locator) -> Protocol -> Locator -> Either InvalidLocator Locator
prepare defLoc proto =
  toEither . sortGroupChildLocs defLoc proto . flattenLoc
  where
    toEither :: Locator -> Either InvalidLocator Locator
    toEither l = case classify defLoc proto l of
      Invalid err -> Left err
      _ -> Right l

data Classification = IsXPath | IsXPathConvertable | IsCSS | IsBiDi | Invalid InvalidLocator | IsMixed deriving (Show, Eq, Ord)

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
    AllElms -> IsXPathConvertable
    ID {} -> IsXPathConvertable
    Class {} -> IsXPathConvertable
    Attribute {} -> IsXPathConvertable
    Tag {} -> IsXPathConvertable
    d@Default {value} ->
      let nxtLoc = defLoc value
          nestedDefault = hasDefault nxtLoc
       in if nestedDefault
            then Invalid $ MkInvalidLocator d "Invalid Default locator - Default locator cannot resolve to another Default"
            else classifyNxt nxtLoc
    Role {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> IsMixed -- requires double shot Xpath + post filter
    InnerText {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> IsMixed -- requires double shot Xpath + post filter
    c@BiDiContext {} ->
      case proto of
        BiDi -> IsBiDi
        HTTP -> Invalid $ MkInvalidLocator c "BiDiContext locator cannot be used with HTTP protocol"
    Contains {container, contained} ->
      mergeClassification (classifyNxt container) (classifyNxt contained)
    All {elms} -> clasifyElms elms
    Any {elms} -> clasifyElms elms
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
        Contains {} -> l
        All {elms} -> All $ sortAndGroup All elms
        Any {elms} -> Any $ sortAndGroup Any elms
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
      Attribute {name, value, matchType, caseSensitivity} ->
        "//*[" <> attrPred name value matchType caseSensitivity <> "]"
      Tag {value} -> "//" <> value
      -- Contains: concatenate container and contained XPath — contained's leading // creates a
      -- descendant-axis step from the container result set, e.g. //form//input.
      Contains {container, contained} -> toXPathStr container <> toXPathStr contained
      All {elms} -> "//*[" <> intercalate " and " (toList $ toPred <$> elms) <> "]"
      Any {elms} -> "//*[" <> intercalate " or " (toList $ toPred <$> elms) <> "]"
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
      Attribute {name, value, matchType, caseSensitivity} -> attrPred name value matchType caseSensitivity
      Tag {value} -> "self::" <> value
      -- Contains as predicate: "I match contained AND I have an ancestor matching container"
      Contains {container, contained} ->
        toPred contained <> " and ancestor::*[" <> toPred container <> "]"
      All {elms} -> "(" <> intercalate " and " (toList $ toPred <$> elms) <> ")"
      Any {elms} -> "(" <> intercalate " or " (toList $ toPred <$> elms) <> ")"
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
              -- normalize-space() collapses multiple spaces and trims leading/trailing whitespace.
              "contains(concat(' ', normalize-space(" <> classAttr <> "), ' '), ' " <> matchVal <> " ')"
            Partial -> "contains(" <> classAttr <> ", '" <> matchVal <> "')"
            Starts -> "starts-with(normalize-space(" <> classAttr <> "), '" <> matchVal <> "')"
            Wildcard -> wildcardPred classAttr matchVal

    -- \| XPath predicate matching elements that have the named attribute satisfying the condition.
    --   @name@ is the HTML attribute name (e.g. "href", "data-testid").
    attrPred :: Text -> Text -> MatchType -> CaseSensitivity -> Text
    attrPred name val mt cs =
      let attrExpr = applyCS cs ("@" <> name)
          matchVal = lowerIfCI cs val
       in case mt of
            Full -> attrExpr <> "='" <> matchVal <> "'"
            Partial -> "contains(" <> attrExpr <> ", '" <> matchVal <> "')"
            Starts -> "starts-with(" <> attrExpr <> ", '" <> matchVal <> "')"
            Wildcard -> wildcardPred attrExpr matchVal

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
    Contains p c -> foldLoc f (foldLoc f acc' p) c
    All locs -> foldList locs
    Any locs -> foldList locs
    -- WithOptions base _ -> foldLoc f acc' base
    PostFilter {} -> acc'
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
    Contains p c -> f (foldLocBottomUp f (foldLocBottomUp f acc p) c) loc
    All locs -> f (foldList locs) loc
    Any locs -> f (foldList locs) loc
    -- WithOptions base _ -> f (foldLocBottomUp f acc base) loc
    PostFilter {} -> f acc loc
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
    Contains p c -> Contains (recurse p) (recurse c)
    All locs -> All $ recurseMap locs
    Any locs -> Any $ recurseMap locs
    _ -> loc -- Leaf locators and Predicate
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
hasDefault = 
  anyLoc $
    \case 
       Default {} -> True
       _ -> False

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

  -- Recurse into other composite locators
  Contains p c -> Contains (flattenLoc p) (flattenLoc c)
  -- WithOptions base opts -> WithOptions (flattenLoc base) opts
  -- Leaf locators and Predicate have no children to recurse into
  other -> other

upperAlpha :: Text
upperAlpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowerAlpha :: Text
lowerAlpha = "abcdefghijklmnopqrstuvwxyz"