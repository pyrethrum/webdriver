module HTTP.LocateCombinatorTest where

import Control.Monad (forM_, replicateM, when)
import Data.Function ((&))
import Data.List ((\\), intersect, nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text (Text, unpack)
import Effectful (Eff, IOE, (:>), liftIO)
import Common.Utils (defOpts, locateAllHttp)
import HTTP.Runner (WDSession, closeWDSession, getWDSession, runHttpTest)
import Prelude
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Interactive (sample)
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource, defaultMain)
import System.Environment (withArgs)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import System.IO (hClose, openTempFile)
import Utils (txt)
import qualified Data.Text.IO as T
import WebDriver.Effectful (WebDriverHttp)
import WebDriver.Effectful.HTTP.Base.Actions (getElementAttribute, navigateTo)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL (..))
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator, css, elmClass, (&&&), (>>>), (|||))


tests :: TestTree
tests =
  withResource getWDSession closeWDSession runSessionTests
  where
  runSessionTests :: IO WDSession -> TestTree
  runSessionTests ses =
    testGroup "Locate Combinator Tests"
      [ testGroup "sanity checks for abstract selector" [
          sanitySimpleA,
          sanityAOrB,
          sanityAandC,
          sanityAUnderB,
          sanityAandCUnderC,
          sanityAandCUnderB],
        inOrderTestGroup "Locate Combinator Tests"
          [ locateCombinatorProperty ses
          ]
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

data LocateExpectation = MkLocateExpectation {
  locator :: Selection,
  expectedMatches :: [Text]
}
data LocatorTestCase = MkLocatorTestCase {
  testNode :: Node,
  testCases :: [LocateExpectation]
}

genCase :: Gen LocatorTestCase
genCase = do
  node <- genNode
  rawSelections <- replicateM 1000 genSelection
  let expectations =
       take 100
       . filter (not . null . (.expectedMatches))
       . fmap (mkExpectation node)
       . nub
       $ rawSelections
  pure $ MkLocatorTestCase {testNode = node, testCases = expectations}
  where
    mkExpectation :: Node -> Selection -> LocateExpectation
    mkExpectation node selection =
      MkLocateExpectation
        { locator = selection,
          expectedMatches = nub $ match node selection
        }

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
  Or' {selection:: NonEmpty Selection }|
  And' {selection:: NonEmpty Selection }|
  Under {
    parent :: Selection,
    descendant :: Selection
  } |
  Match {
    domClass :: DOMClass
  } deriving (Eq, Show)

genLocator :: Selection -> Gen Locator
genLocator = \case
  Match {domClass} -> matchLocator domClass
  Or' {selection} -> foldNonEmpty1 (|||) <$> traverse genLocator selection
  And' {selection} -> foldNonEmpty1 (&&&) <$> traverse genLocator selection
  Under {parent, descendant} -> do
    parentLoc <- genLocator parent
    descendantLoc <- genLocator descendant
    pure $ parentLoc >>> descendantLoc
 where
    foldNonEmpty1 :: (a -> a -> a) -> NonEmpty a -> a
    foldNonEmpty1 f (x :| xs) = foldl' f x xs

    -- Prefer elmClass because it is simpler than raw css.
    matchLocator :: DOMClass -> Gen Locator
    matchLocator domClass =
      let className = txt domClass in
      frequency
        [ (7, pure $ elmClass className),
          (3, pure $ css $ "." <> className)
        ]

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
              descendantSelection <- genSelectionAtDepth (depth + 1)
              pure $ Under {parent = parentSelection, descendant = descendantSelection}
          )
        ]

    genChildSelectionsAtDepth :: Int -> Gen (NonEmpty Selection)
    genChildSelectionsAtDepth depth = do
      childCount <- G.integral $ R.between (1, maxSelectionChildren)
      firstSelection <- genSelectionAtDepth (depth + 1)
      restSelections <- replicateM (childCount - 1) (genSelectionAtDepth (depth + 1))
      pure $ firstSelection :| restSelections

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
    Under {parent, descendant} ->
      let parentIds = selectIds root parent
          parentNodes = flatten root & filter (\n -> n.autoId `elem` parentIds)
       in nub . concat $ selectUnder descendant <$> parentNodes

-- Evaluate all child selections under a specific parent node.
selectUnder :: Selection -> Node -> [Text]
selectUnder descendantSelection parentNode =
  let validScope = descendants parentNode & fmap (.autoId)
      selected = selectIds parentNode descendantSelection
   in validScope `intersect` selected

foldSelections :: Node -> NonEmpty Selection -> [Text]
foldSelections root (firstSel :| rest) =
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
sanityAOrB = sanityChk "A or B"  ["1", "1-1", "1-2-1", "1-3-1", "1-2"] $ Or' (Match A :| [Match B])

sanityAandC :: TestTree
sanityAandC = sanityChk "A and C"  ["1-2-1", "1-3-1"] $ And' (Match A :| [Match C])

sanityAandCUnderC :: TestTree
sanityAandCUnderC = sanityChk "A and C under C"  ["1-3-1"] $ Under {parent = Match C, descendant = And' (Match A :| [Match C])}

sanityAandCUnderB :: TestTree
sanityAandCUnderB = sanityChk "A and C under B"  ["1-2-1", "1-3-1"] $ Under {parent = Match B, descendant = And' (Match A :| [Match C])}

sanityAUnderB :: TestTree
sanityAUnderB = sanityChk "A under B"  ["1-1", "1-2-1", "1-3-1"] $ Under {parent = Match B, descendant = Match A}


data LocatorTestFailure = MkLocatorTestFailure
  { node :: Node,
    html :: Text,
    selection :: Selection,
    generatedLocator :: Locator,
    expectedMatches :: [Text],
    actualMatches :: [Text],
    missingFromActual :: [Text],
    extraInActual :: [Text]
  }
  deriving (Show)

locateCombinatorProperty :: IO WDSession -> TestTree
locateCombinatorProperty ses =
  runHttpTest ses "Generated locate combinator property (10 cases)" $ do
    forM_ [1 .. propertyRuns] $ \runIdx -> do
      locCase <- liftIO $ sample genCase
      let caseHtml = wrapHtml locCase.testNode
      htmlFilePath <- liftIO $ writeTempHtmlFile caseHtml
      navigateTo $ filePathToUrl htmlFilePath

      forM_ (zip [1 :: Int ..] locCase.testCases) $ \(expectationIdx, expectation) -> do
        generatedLoc <- liftIO $ sample $ genLocator expectation.locator
        locateRslt <- locateAll generatedLoc
        actual <- locateRslt.result & either
          (\err -> liftIO $ assertFailure . unpack $
            "locateAll failed in generated locate property, run=" <> txt runIdx <> ", expectation=" <> txt expectationIdx
              <> "\nSelection: " <> txt expectation.locator
              <> "\nGenerated locator: " <> txt generatedLoc
              <> "\nError: " <> txt err
          )
          (\elms -> nub . catMaybes <$> traverse (`getElementAttribute` "auto-id") elms)

        let expected = nub expectation.expectedMatches
        when (sort expected /= sort actual) $ do
          let failure = mkLocatorTestFailure locCase.testNode caseHtml expectation.locator generatedLoc expected actual
          liftIO $ assertFailure . unpack $
            "Generated locate mismatch in run=" <> txt runIdx <> ", expectation=" <> txt expectationIdx
              <> "\n" <> txt failure
  where
    propertyRuns :: Int
    propertyRuns = 10

    wrapHtml :: Node -> Text
    wrapHtml testNode =
      "<!doctype html><html><head><meta charset=\"utf-8\"></head><body>"
        <> nodeToHtml testNode
        <> "</body></html>"

    locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
    locateAll = locateAllHttp defOpts

writeTempHtmlFile :: Text -> IO FilePath
writeTempHtmlFile html = do
  (fp, h) <- openTempFile "/tmp" "webdriver-locate-combinator-XXXX.html"
  hClose h
  T.writeFile fp html
  pure fp

filePathToUrl :: FilePath -> URL
filePathToUrl fp = MkUrl $ "file://" <> txt fp

mkLocatorTestFailure :: Node -> Text -> Selection -> Locator -> [Text] -> [Text] -> LocatorTestFailure
mkLocatorTestFailure testNode caseHtml selected generatedLoc expected actual =
  let missing = expected \\ actual
      extra = actual \\ expected
   in MkLocatorTestFailure
      { node = testNode,
        html = caseHtml,
        selection = selected,
        generatedLocator = generatedLoc,
        expectedMatches = expected,
        actualMatches = actual,
        missingFromActual = missing,
        extraInActual = extra
      }


-- locators mixture of css and xpath

_pattern :: Maybe Text
-- _pattern = Just "roleType Option"
_pattern = Nothing

_eval :: Maybe Text -> TestTree -> IO ()
_eval mPattern = withArgs (maybe [] (\pat -> ["-p", (unpack pat)]) mPattern) . defaultMain

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess



