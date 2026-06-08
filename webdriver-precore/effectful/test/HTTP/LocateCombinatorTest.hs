module HTTP.LocateCombinatorTest where

import Control.Monad (replicateM)
import Data.Function ((&))
import Data.List (intersect, nub, sort)
import Data.Text qualified as T
import Data.Text (Text, unpack)
import HTTP.Runner (WDSession, closeWDSession, getWDSession)
import Prelude
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Interactive (sample)
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource, defaultMain)
import System.Environment (withArgs)
import Test.Tasty.HUnit (assertEqual, testCase)
import Utils (txt)
import qualified Data.Text.IO as T


tests :: TestTree
tests =
  withResource getWDSession closeWDSession runSessionTests
  where
  runSessionTests :: IO WDSession -> TestTree
  runSessionTests _ses =
    testGroup "Locate Combinator Tests"
      [ testGroup "sanity checks for abstract selector" [
          sanitySimpleA,
          sanityAOrB,
          sanityAandC,
          sanityAUnderB,
          sanityAandCUnderC,
          sanityAandCUnderB],
        inOrderTestGroup "Locate Combinator Tests" []
      ]


data DOMClass = A | B | C | D | E deriving (Eq, Show)
data Node = Div {
                  autoId :: Text,
                  classes :: [DOMClass],
                  children :: [Node] 
                } | 
              Span {
                autoId :: Text,
                classes :: [DOMClass]
              }
              deriving (Eq, Show)

nodeToHtml :: Node -> Text
nodeToHtml node = case node of
  Div {children} -> "<div " <> idAndClasses <> ">" <> (T.concat $ nodeToHtml <$> children) <> "</div>"
  Span {} -> "<span " <> idAndClasses <> "></span>" 
  where
    id' = " auto-id=\"" <> node.autoId <> "\""
    classes = if null node.classes then "" else " class=\"" <> T.intercalate " " (txt <$> node.classes) <> "\""
    idAndClasses = id' <> " " <> classes

genNode :: Gen Node
genNode = 
  genDivNodeAt nodeRootDepth rootAutoId
  where
    nodeMaxDepth = 6
    nodeRootDepth = 1
    rootAutoId = "1"
    autoIdSeparator = "-"
    minChildrenPerLevel = 1
    maxChildrenPerLevel = 10
    spanNodeWeight = 3
    divNodeWeight = 1
    maxDomClassesPerNode = 5

    domClassCountWeights :: [(Word, Int)]
    domClassCountWeights =
      (\classCount -> (fromIntegral classCount, classCount)) <$> [1 .. maxDomClassesPerNode]

    genNodeAt :: Int -> Text -> Gen Node
    genNodeAt depth parentAutoId
      | depth >= nodeMaxDepth = genSpanNode parentAutoId
      | otherwise =
          frequency
            [ (spanNodeWeight, genSpanNode parentAutoId),
              (divNodeWeight, genDivNodeAt depth parentAutoId)
            ]

    genDivNodeAt :: Int -> Text -> Gen Node
    genDivNodeAt depth parentAutoId = do
      nodeClasses <- genDomClasses
      nodeChildren <- genChildrenAt depth parentAutoId
      pure $ Div {autoId = parentAutoId, classes = nodeClasses, children = nodeChildren}

    genSpanNode :: Text -> Gen Node
    genSpanNode parentAutoId = do
      nodeClasses <- genDomClasses
      pure $ Span {autoId = parentAutoId, classes = nodeClasses}

    genChildrenAt :: Int -> Text -> Gen [Node]
    genChildrenAt depth parentAutoId = do
      childCount <- G.integral $ R.between (minChildrenPerLevel, maxChildrenPerLevel)
      traverse (\childIndex -> genNodeAt (depth + 1) (mkChildAutoId parentAutoId childIndex)) [1 .. childCount]

    mkChildAutoId :: Text -> Int -> Text
    mkChildAutoId parentAutoId childIndex = parentAutoId <> autoIdSeparator <> txt childIndex

    genDomClasses :: Gen [DOMClass]
    genDomClasses = do
      classCount <- genDomClassCount
      genUniqueDomClasses classCount [A, B, C, D, E]

    genUniqueDomClasses :: Int -> [DOMClass] -> Gen [DOMClass]
    genUniqueDomClasses classCount availableClasses
      | classCount <= 0 = pure []
      | null availableClasses = pure []
      | otherwise = do
          nextClass <- genDomClassFrom availableClasses
          restClasses <- genUniqueDomClasses (classCount - 1) (filter (/= nextClass) availableClasses)
          pure $ nextClass : restClasses

    genDomClassCount :: Gen Int
    genDomClassCount = frequency ((\(w, c) -> (w, pure c)) <$> domClassCountWeights)

    genDomClassFrom :: [DOMClass] -> Gen DOMClass
    genDomClassFrom availableClasses =
      frequency $ ((\dc -> (1, pure dc)) <$> availableClasses)

prettyNode :: Int -> Node -> Text
prettyNode idx node = "Node " <> txt idx <> ":\n" <> txt node

data Selection = 
  Or' {selection:: [Selection] }|
  And' {selection:: [Selection] }|
  Under {
    parent :: Selection,
    selection :: [Selection]
  } |
  Match {
    domClass :: DOMClass
  } deriving (Eq, Show)

genLocator :: Selection -> Locator
genLocator sel = undefined

genSelection :: Gen Selection
genSelection = genSelectionAtDepth 1
  where
    maxSelectionDepth :: Int
    maxSelectionDepth = 5

    maxSelectionChildren :: Int
    maxSelectionChildren = 5

    genSelectionDomClass :: Gen DOMClass
    genSelectionDomClass = frequency $ (\nodeClass -> (1, pure nodeClass)) <$> [A, B, C, D, E]

    genSelectionAtDepth :: Int -> Gen Selection
    genSelectionAtDepth depth
      | depth >= maxSelectionDepth = Match <$> genSelectionDomClass
      | otherwise = frequency
          [ (matchWeight, Match <$> genSelectionDomClass),
            (parentWeight, genParentSelectionAtDepth depth)
          ]
      where
        matchWeight = 3
        parentWeight = 2

    genParentSelectionAtDepth :: Int -> Gen Selection
    genParentSelectionAtDepth depth = do
      childSelections <- genChildSelectionsAtDepth depth
      frequency
        [ (1, pure $ Or' childSelections),
          (1, pure $ And' childSelections),
          (1, do
              parentSelection <- genSelectionAtDepth (depth + 1)
              pure $ Under {parent = parentSelection, selection = childSelections}
          )
        ]

    genChildSelectionsAtDepth :: Int -> Gen [Selection]
    genChildSelectionsAtDepth depth = do
      childCount <- G.integral $ R.between (0, maxSelectionChildren)
      replicateM childCount (genSelectionAtDepth (depth + 1))

match :: Node -> Selection -> [Text]
match root sel =
  let matchingIds = selectIds root sel
   in
    flatten root
      & fmap (.autoId)
      & filter (`elem` matchingIds)

-- Evaluate a selection against a subtree and return matching node ids.
selectIds :: Node -> Selection -> [Text]
selectIds root sel =
  case sel of
    Match {domClass} ->
      flatten root
        & filter (elem domClass . (.classes))
        & fmap (.autoId)
    Or' {selection} -> nub . concat $ selectIds root <$> selection
    And' {selection} -> foldSelections root selection
    Under {parent, selection} ->
      let parentIds = selectIds root parent
          parentNodes = flatten root & filter (\n -> n.autoId `elem` parentIds)
       in nub . concat $ selectUnder selection <$> parentNodes

-- Evaluate all child selections under a specific parent node.
selectUnder :: [Selection] -> Node -> [Text]
selectUnder selections parentNode =
  let validScope = descendants parentNode & fmap (.autoId)
      selected = foldSelections parentNode selections
   in validScope `intersect` selected

foldSelections :: Node -> [Selection] -> [Text]
foldSelections root selections =
  case selections of
    [] -> []
    firstSel : rest ->
      foldl' intersect (selectIds root firstSel) $ selectIds root <$> rest

flatten :: Node -> [Node]
flatten = \case
  divNode@Div {children} -> divNode : concatMap flatten children
  spanNode@Span {} -> [spanNode]

descendants :: Node -> [Node]
descendants = \case
  Div {children} -> concatMap flatten children
  Span {} -> []

chkListContentEq :: (Ord a, Show a) => Text -> [a] -> [a] -> IO ()
chkListContentEq message expected actual =
  assertEqual (unpack message) (sort expected) (sort actual)

sanityChk :: Text -> [Text] -> Selection -> TestTree
sanityChk message expected selection =
  testCase (unpack message) $ do
    let actual = match sanityNode selection
    chkListContentEq message expected actual

sanityNode :: Node
sanityNode =
  Div
    { autoId = "1",
      classes = [A, B],
      children =
        [ Span {autoId = "1-1", classes = [A]},
          Div
            { autoId = "1-2",
              classes = [B],
              children =
                [ Span {autoId = "1-2-1", classes = [A, C]},
                  Span {autoId = "1-2-2", classes = [C]}
                ]
            },
          Div
            { autoId = "1-3",
              classes = [C],
              children =
                [ Span {autoId = "1-3-1", classes = [A, C]},
                  Span {autoId = "1-3-2", classes = [C]}
                ]
            }
        ]
    }

sanitySimpleA :: TestTree
sanitySimpleA = sanityChk "Simple Match A" ["1", "1-1", "1-2-1", "1-3-1"] $ Match A

sanityAOrB :: TestTree
sanityAOrB = sanityChk "A or B"  ["1", "1-1", "1-2-1", "1-3-1", "1-2"] $ Or' [Match A, Match B]

sanityAandC :: TestTree
sanityAandC = sanityChk "A and C"  ["1-2-1", "1-3-1"] $ And' [Match A, Match C]

sanityAandCUnderC :: TestTree
sanityAandCUnderC = sanityChk "A and C under C"  ["1-3-1"] $ Under {parent = Match C, selection = [And' [Match A, Match C]]}

sanityAandCUnderB :: TestTree
sanityAandCUnderB = sanityChk "A and C under B"  ["1-2-1", "1-3-1"] $ Under {parent = Match B, selection = [And' [Match A, Match C]]}

sanityAUnderB :: TestTree
sanityAUnderB = sanityChk "A under B"  ["1-1", "1-2-1", "1-3-1"] $ Under {parent = Match B, selection = [Match A]}




-- locators mixture of css and xpath

_pattern :: Maybe Text
-- _pattern = Just "roleType Option"
_pattern = Nothing

_eval :: Maybe Text -> TestTree -> IO ()
_eval mPattern = withArgs (maybe [] (\pat -> ["-p", (unpack pat)]) mPattern) . defaultMain

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess

