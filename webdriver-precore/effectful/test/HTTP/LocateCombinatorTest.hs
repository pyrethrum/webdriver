module HTTP.LocateCombinatorTest where

import Control.Monad (replicateM)
import Control.Exception (SomeException, displayException, throwIO, try)
import Data.Base64.Types qualified as B64T
import Data.ByteString.Base64 qualified as B64
import Data.Function ((&))
import Data.List ((\\), find, intersect, nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Effectful (Eff, IOE, (:>), liftIO)
import Common.Utils (defOpts, locateAllHttp)
import HTTP.Runner (WDSession, closeWDSession, getWDSession, runHttp)
import Prelude
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource, defaultMain)
import System.Environment (withArgs)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), Verbose (..), genWith, info, testFailed, testPropertyWith)
import System.IO.Unsafe (unsafePerformIO)
import Utils (txt)
import WebDriver.Effectful (WebDriverHttp, sleep)
import WebDriver.Effectful.HTTP.Base.Actions (getElementAttribute, navigateTo)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL (..))
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator, css, elmClass, (&&&), (>>>), (|||), elmClass', MatchType (Partial), CaseSensitivity (..))
import WebDriverPreCore.Extended.Protocol (milliseconds)
import Data.Bifunctor (Bifunctor(first))


tests :: TestTree
tests =
  testGroup "Locate Combinator Tests" [
    testGroup "sanity checks for abstract selector" [
          sanitySimpleA,
          sanityAOrB,
          sanityAandC,
          sanityAUnderB,
          sanityAandCUnderC,
          sanityAandCUnderB 
          ],
    withResource getWDSession closeWDSession runSessionTests
  ]
  where
  runSessionTests :: IO WDSession -> TestTree
  runSessionTests ses =
        inOrderTestGroup "Combinator tests"
          [ locateCombinatorProperty ses,
             inOrderTestGroup "Combinator singleton tests" [
             locTest "simple OR" $ Matched
                { testNode =
                    Div
                      { autoId = "1"
                      , classes = [ A ]
                      , children =
                          [ Span { autoId = "1-1" , classes = [ A ] }
                          , Span { autoId = "1-2" , classes = [ A ] }
                          , Span { autoId = "1-3" , classes = [ A ] }
                          , Span { autoId = "1-4" , classes = [ B ] }
                          ]
                      }
                , abstractLocator =
                    Or'
                      { selection = Match { domClass = A } :| [ Match { domClass = B } ]
                      }
                , expectedMatches = [ "1" , "1-1" , "1-2" , "1-3" , "1-4" ]
                , locator = classLoc "A" ||| classLoc "B"
                }
             ]
          ]
        where 
          classLoc = elmClass' Partial CaseInsensitive
          locTest :: Text -> LocatorTestCase -> TestTree
          locTest name = testCase (unpack name) .  evaluateCase ses


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

data LocatorTestCase = Matched {
  testNode :: Node,
  abstractLocator :: Selection,
  expectedMatches :: [Text],
  locator :: Locator
} |
 Unmatched {
  testNode :: Node,
  abstractLocator :: Selection,
  locator :: Locator
}
 deriving (Show, Eq)

genCase :: Gen LocatorTestCase
genCase = do
  node <- genNode
  locatorCases <- replicateM 100 $ genLocatorTestCase node
  case pickLocatorTestCase locatorCases of
    matched@Matched {} -> pure matched
    Unmatched {} -> genCase
  where
    genLocatorTestCase :: Node -> Gen LocatorTestCase
    genLocatorTestCase node = do
      abstractLocator <- genSelection
      locator <- genLocator abstractLocator
      let expectedMatches = nub $ match node abstractLocator
      pure $ if null expectedMatches
        then Unmatched {testNode = node, abstractLocator, locator}
        else Matched
          { testNode = node,
            abstractLocator,
            expectedMatches,
            locator
          }

    pickLocatorTestCase :: [LocatorTestCase] -> LocatorTestCase
    pickLocatorTestCase locatorCases =
      case find isMatched locatorCases of
        Just matchedCase -> matchedCase
        Nothing -> case locatorCases of
          firstCase : _ -> firstCase
          [] -> error "genCase: expected at least one locator case"

    isMatched :: LocatorTestCase -> Bool
    isMatched = \case
      Matched {} -> True
      Unmatched {} -> False

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

countNodes :: Node -> Int
countNodes = \case
  Div {children} -> 1 + sum (countNodes <$> children)
  Span {} -> 1

countSelectionNodes :: Selection -> Int
countSelectionNodes = \case
  Match {} -> 1
  Or' {selection} -> 1 + sum (countSelectionNodes <$> selection)
  And' {selection} -> 1 + sum (countSelectionNodes <$> selection)
  Under {parent, descendant} -> 1 + countSelectionNodes parent + countSelectionNodes descendant

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
locateCombinatorProperty getSes =
  testPropertyWith propertyOptions "Generated locate combinator property" $ do
    locCase@Matched {} <- genWith (const Nothing) genCase
    info $ "LocateCombinatorTest generator node count (test node): " <> show (countNodes locCase.testNode)
    info $ "LocateCombinatorTest generator node count (generator selection): " <> show (countSelectionNodes locCase.abstractLocator)

    let evaluation = unsafeRunIO $ evaluateCase getSes locCase
    info $ "Test Case:\n" <> unpack (txt locCase)
    evaluation & either
      (testFailed . unpack)
      pure

  where
    propertyOptions :: TestOptions
    propertyOptions =
      TestOptions
        { expectFailure = DontExpectFailure,
          overrideVerbose = Just Verbose,
          overrideNumTests = Just 10,
          overrideMaxShrinks = Nothing,
          overrideMaxRatio = Nothing
        }

unsafeRunIO :: IO a -> Either Text a
unsafeRunIO action =
  let rslt = unsafePerformIO $ try action
  in
  first (("Unexpected exception during property IO:\n " <>) . txt . displayException @SomeException) rslt

{-# NOINLINE unsafeRunIO #-}


evaluateCase :: IO WDSession -> LocatorTestCase -> IO ()
evaluateCase getSession locCase  = 
  case locCase of
    Matched {testNode, abstractLocator, expectedMatches} -> do
      putStrLn $ "LocateCombinatorTest pre-IO node count (test node): " <> show (countNodes testNode)
      putStrLn $ "LocateCombinatorTest pre-IO node count (generator selection): " <> show (countSelectionNodes abstractLocator)
      wdSession <- getSession
      runHttp wdSession $ do
        let dataUrl = htmlToDataUrl html
        navigateTo dataUrl
        -- TODO FIX
        sleep $ 100 * milliseconds
        evaluateExpectation
      where
        locator = locCase.locator
        locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
        locateAll = locateAllHttp $ defOpts {L.jsRecheckDisplayed = L.DisplayedCheckNever}
        html = "<!doctype html><html><head><meta charset=\"utf-8\"></head><body>"
              <> nodeToHtml testNode
              <> "</body></html>"
        evaluateExpectation :: forall es. (IOE :> es, WebDriverHttp :> es) => Eff es ()
        evaluateExpectation = do
          locateRslt <- locateAll locator
          actual <- locateRslt.result & either
            (\err -> liftIO . throwIO . userError $
              "locateAll failed in generated locate property"
                <> "\nSelection: " <> unpack (txt abstractLocator)
                <> "\nGenerated locator: " <> unpack (txt locator)
                <> "\nError: " <> unpack (txt err)
            )
            (\elms -> nub . catMaybes <$> traverse (`getElementAttribute` "auto-id") elms)

          let expected = nub expectedMatches
          if sort expected == sort actual
            then pure ()
            else do
              let failure = mkLocatorTestFailure testNode html abstractLocator locator expected actual
              liftIO . throwIO . userError $ "Failure generated" <> "\n" <> unpack (txt failure)

    Unmatched {} ->
      liftIO $ throwIO $ userError "evaluateCase called with an unmatched locator case"

htmlToDataUrl :: Text -> URL
htmlToDataUrl html =
  let htmlBytes = encodeUtf8 html
      encoded = B64T.extractBase64 $ B64.encodeBase64 htmlBytes
      dataUrl = "data:text/html;base64," <> encoded
  in MkUrl dataUrl

mkLocatorTestFailure :: Node -> Text -> Selection -> Locator -> [Text] -> [Text] -> LocatorTestFailure
mkLocatorTestFailure node html selection generatedLocator expectedMatches actualMatches =
  let missingFromActual = expectedMatches \\ actualMatches
      extraInActual = actualMatches \\ expectedMatches
   in MkLocatorTestFailure
      { node,
        html,
        selection,
        generatedLocator,
        expectedMatches,
        actualMatches,
        missingFromActual,
        extraInActual
      }


-- locators mixture of css and xpath

_pattern :: Maybe Text
_pattern = Just "simple OR"
-- _pattern = Nothing

_eval :: Maybe Text -> TestTree -> IO ()
_eval mPattern = withArgs (maybe [] (\pat -> ["-p", (unpack pat)]) mPattern) . defaultMain

--- >>> _eval _pattern tests
-- *** Exception: ExitFailure 1

