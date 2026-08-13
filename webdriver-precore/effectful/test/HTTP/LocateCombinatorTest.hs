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
import Common.Utils (defAllOpts, locateAllHttp)
import HTTP.Runner (WDSession, closeWDSession, getWDSession, runHttp)
import Prelude
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource, defaultMain)
import System.Environment (withArgs)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), info, testFailed, testPropertyWith)
import System.IO.Unsafe (unsafePerformIO)
import Utils (txt)
import WebDriver.Effectful (WebDriverHttp)
import WebDriver.Effectful.HTTP.Base.Actions (getElementAttribute, navigateTo)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, URL (..))
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator, css, elmClass, (&&&), (>>>), (|||), elmClass', MatchType (..), CaseSensitivity (..))
import Data.Bifunctor (Bifunctor(first))
import Test.Falsify.Property (gen)
import WebDriverPreCore.Extended.HTTP.Locate (DisplayedCheck(..))

tests :: TestTree
tests =
  testGroup "Locate Combinator Tests" [
    testGroup "sanity checks for abstract locator" [
          sanitySimpleA,
          sanityAOrB,
          sanityAandC,
          sanityAUnderB,
          sanityAandCUnderC,
          sanityAandCUnderB,
          sanityNested,
          sanityNested2
          ],
    withResource getWDSession closeWDSession runSessionTests
  ]
  where
  runSessionTests :: IO WDSession -> TestTree
  runSessionTests ses =
        inOrderTestGroup "Combinator tests"
          [ 
             inOrderTestGroup "Combinator property tests" 
               [locateCombinatorProperty ses],
              --  known failures from property tests
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
                        { locs = Match A :| [ Match { domClass = B } ]
                        }
                  , expectedMatches = [ "1" , "1-1" , "1-2" , "1-3" , "1-4" ]
                  , locator = classLoc A ||| classLoc B
                  },
              locTest "simple under" $
                 Matched
                      { testNode =
                          Div
                            { autoId = "1"
                            , classes = [ A ]
                            , children = [ Span { autoId = "1-1" , classes = [ A ] } ]
                            }
                      , abstractLocator =
                          Under
                            { parentLoc = Match A
                            , descendantLoc =
                                Or'
                                  { locs = Match A :| [ Match A ]
                                  }
                            }
                      , expectedMatches = [ "1-1" ]
                      , locator = classLoc A  >>> (classLoc A ||| classLoc A)
                      },
               locTest "nested under with OR remains scoped" $
                    Matched
                       { testNode =
                         Div
                           { autoId = "1"
                           , classes = [ A ]
                           , children =
                             [ Div
                               { autoId = "1-1"
                               , classes = [ B ]
                               , children =
                                 [ Span { autoId = "1-1-1", classes = [ C ] }
                                 , Span { autoId = "1-1-2", classes = [ D ] }
                                 ]
                               }
                             , Div
                               { autoId = "1-2"
                               , classes = [ B ]
                               , children =
                                 [ Span { autoId = "1-2-1", classes = [ C ] }
                                 ]
                               }
                             ]
                           }
                       , abstractLocator =
                         Under
                           { parentLoc = Match A
                           , descendantLoc =
                             Under
                             { parentLoc = Match { domClass = B }
                             , descendantLoc =
                               Or'
                                 { locs = Match { domClass = C } :| [ Match { domClass = D } ]
                                 }
                             }
                           }
                       , expectedMatches = [ "1-1-1", "1-1-2", "1-2-1" ]
                       , locator = classLoc A >>> (classLoc B >>> (classLoc C ||| classLoc D))
                       },
          locTest "Or in And" $  Matched
          { testNode =
              Div
                { autoId = "1"
                , classes = [ A ]
                , children = [ Span { autoId = "1-1" , classes = [ A ] } ]
                }
          , abstractLocator =
              And'
                { locs =
                    Match A :|
                      [ Or'
                          { locs = Match { domClass = B } :| [ Match A ]
                          }
                      ]
                }
          , expectedMatches = [ "1" , "1-1" ]
          , locator = classLoc A &&& (classLoc B ||| classLoc A)
          },
          locTest "Nested Contains" $ Matched
          { testNode =
              Div
                { autoId = "1"
                , classes = [ A ]
                , children =
                    [ Div
                        { autoId = "1-1"
                        , classes = [ A ]
                        , children =
                            [ Span { autoId = "1-1-1" , classes = [ A ] }
                            , Span { autoId = "1-1-2" , classes = [ A ] }
                            , Div
                                { autoId = "1-1-3"
                                , classes = [ A ]
                                , children =
                                    [ Span { autoId = "1-1-3-1" , classes = [ A ] }
                                    , Span { autoId = "1-1-3-2" , classes = [ A ] }
                                    , Span { autoId = "1-1-3-3" , classes = [ A ] }
                                    , Span { autoId = "1-1-3-4" , classes = [ A ] }
                                    , Span { autoId = "1-1-3-5" , classes = [ A ] }
                                    , Div
                                        { autoId = "1-1-3-6"
                                        , classes = [ A , B ]
                                        , children = [ Span { autoId = "1-1-3-6-1" , classes = [ B ] } ]
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
          , abstractLocator =
              Under
                { parentLoc = Match { domClass = B }
                , descendantLoc =
                    Under
                      { parentLoc = Match A
                      , descendantLoc = Or' { locs = Match { domClass = B } :| [] }
                      }
                }
          , expectedMatches = []
          , locator = classLoc B >>>
                        classLoc A >>> 
                          classLoc B
          },
          locTest "Or and contains" $ Matched
          { testNode =
              Div
                { autoId = "1"
                , classes = [ A ]
                , children = [ Span { autoId = "1-1" , classes = [ A ] } ]
                }
          , abstractLocator =
              Or'
                { locs =
                    Or'
                      { locs =
                          Match A  :|
                            [ Match A 
                            , Or'
                                { locs =
                                    Under
                                      { parentLoc = Match A
                                      , descendantLoc = Match A
                                      } :|
                                      []
                                }
                            ]
                      } :|
                      []
                }
          , expectedMatches = [ "1" , "1-1" ]
          , locator = classLoc A ||| classLoc A ||| (classLoc A 
                                                        >>> classLoc A)  
          },
          locTest "Or and contains ii" $ Matched
          { testNode =
              Div
                { autoId = "1"
                , classes = [ A ]
                , children = [ Span { autoId = "1-1" , classes = [ A ] } ]
                }
          , abstractLocator =
              Or'
                { locs =
                    Match A :|
                      [ Match A
                      , Match A
                      , And'
                          { locs =
                              Match A :|
                                [ Or'
                                    { locs =
                                        Under
                                          { parentLoc = Match A
                                          , descendantLoc = Match A
                                          } :|
                                          []
                                    }
                                ]
                          }
                      ]
                }
          , expectedMatches = [ "1" , "1-1" ]
          , locator = (classLoc A ||| classLoc A ||| classLoc A)
                        ||| (classLoc A &&& (classLoc A >>> classLoc A))
          },
          locTest "complex nested OR-AND-Under" $ Matched
          { testNode =
              Div
                { autoId = "1"
                , classes = [ A ]
                , children = [ Span { autoId = "1-1" , classes = [ A ] } ]
                }
          , abstractLocator =
              Or'
                { locs =
                    Match A :|
                      [ Match A
                      , Match A
                      , Match A
                      , And'
                          { locs =
                              Match A :|
                                [ Match A
                                , Under
                                    { parentLoc = Match A
                                    , descendantLoc = Match A
                                    }
                                , Match A
                                , Under
                                    { parentLoc = Match A
                                    , descendantLoc = Match A
                                    }
                                ]
                          }
                      ]
                }
          , expectedMatches = [ "1" , "1-1" ]
          , locator =
              let a = elmClass' Partial CaseInsensitive "A"
              in (((a ||| a) ||| a) ||| a) ||| ((((a &&& a) &&& (a >>> a)) &&& a) &&& (a >>> a))
          },
          -- 
          locTest "OR with contains under" $ Matched
          { testNode =
              Div
                { autoId = "1"
                , classes = [ B ]
                , children = [ Span { autoId = "1-1" , classes = [ A ] } ]
                }
          , abstractLocator =
              Or'
                { locs =
                    Match A :|
                      [ Under
                          { parentLoc = Match A
                          , descendantLoc =
                              Or'
                                { locs =
                                    Match A :|
                                      [ Match A , Match { domClass = B } ]
                                }
                          }
                      ]
                }
          , expectedMatches = [ "1-1" ]
          , locator = classLoc A ||| (classLoc A >>> classLoc A ||| classLoc A ||| classLoc B)
          }
             ]
          ]
        where 
          locTest :: Text -> LocatorTestCase -> TestTree
          locTest name = testCase (unpack name) .  evaluateCase ses

classLoc :: DOMClass -> Locator
classLoc = elmClass' Full CaseInsensitive . txt

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
  abstractLocator :: AbsLoc,
  expectedMatches :: [Text],
  locator :: Locator
} |
 Unmatched {
  testNode :: Node,
  abstractLocator :: AbsLoc,
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
    genLocatorTestCase testNode = do
      abstractLocator <- genSelection
      locator <- genLocator abstractLocator
      let expectedMatches = nub $ match testNode abstractLocator
      pure $ if null expectedMatches
        then Unmatched {testNode, abstractLocator, locator}
        else Matched
          { testNode,
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
  genDivNodeAt nodeRootDepth rootAutoId maxNodeBudget
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
    maxNodeBudget = 1000

    domClassCountWeights :: [(Word, Int)]
    domClassCountWeights =
      (\classCount -> (fromIntegral classCount, classCount)) <$> [1 .. maxDomClassesPerNode]

    genNodeAt :: Int -> Text -> Int -> Gen Node
    genNodeAt depth parentAutoId remaining
      | depth >= nodeMaxDepth || remaining <= 1 = genSpanNode parentAutoId
      | otherwise =
          frequency
            [ (spanNodeWeight, genSpanNode parentAutoId),
              (divNodeWeight, genDivNodeAt depth parentAutoId remaining)
            ]

    genDivNodeAt :: Int -> Text -> Int -> Gen Node
    genDivNodeAt depth parentAutoId remaining = do
      nodeClasses <- genDomClasses
      -- This div costs 1 from the budget.
      let afterDiv = remaining - 1
      nodeChildren <- genChildrenAt depth parentAutoId afterDiv
      pure $ Div {autoId = parentAutoId, classes = nodeClasses, children = nodeChildren}

    genSpanNode :: Text -> Gen Node
    genSpanNode parentAutoId = do
      nodeClasses <- genDomClasses
      pure $ Span {autoId = parentAutoId, classes = nodeClasses}

    genChildrenAt :: Int -> Text -> Int -> Gen [Node]
    genChildrenAt depth parentAutoId remaining = do
      -- Cap child count so the budget isn't exceeded:
      -- each child gets at most @remaining `div` childCount@ nodes.
      let budgetMax = min maxChildrenPerLevel remaining
          budgetMin = min minChildrenPerLevel budgetMax
      childCount <- if budgetMax < budgetMin
                    then pure 0
                    else G.integral $ R.between (budgetMin, budgetMax)
      let perChild = if childCount == 0 then 0 else max 1 (remaining `div` childCount)
      traverse (\childIndex -> genNodeAt (depth + 1) (mkChildAutoId parentAutoId childIndex) perChild) [1 .. childCount]

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

data AbsLoc =
  Or' {locs:: NonEmpty AbsLoc }|
  And' {locs:: NonEmpty AbsLoc }|
  Under {
    parentLoc :: AbsLoc,
    descendantLoc :: AbsLoc
  } |
  Match {
    domClass :: DOMClass
  } deriving (Eq, Show)

genLocator :: AbsLoc -> Gen Locator
genLocator = \case
  Match {domClass} -> matchLocator domClass
  Or' {locs} -> foldNonEmpty1 (|||) <$> traverse genLocator locs
  And' {locs} -> foldNonEmpty1 (&&&) <$> traverse genLocator locs
  Under {parentLoc, descendantLoc} -> do
    parent <- genLocator parentLoc
    descendant <- genLocator descendantLoc
    pure $ parent >>> descendant
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

genSelection :: Gen AbsLoc
genSelection = genSelectionAtDepth 1 maxSelectionBudget
  where
    maxSelectionDepth :: Int
    maxSelectionDepth = 5

    maxSelectionChildren :: Int
    maxSelectionChildren = 5

    maxSelectionBudget :: Int
    maxSelectionBudget = 100

    genSelectionDomClass :: Gen DOMClass
    genSelectionDomClass = frequency $ (\nodeClass -> (1, pure nodeClass)) <$> [A, B, C, D, E]

    genSelectionAtDepth :: Int -> Int -> Gen AbsLoc
    genSelectionAtDepth depth remaining
      | depth >= maxSelectionDepth || remaining <= 1 = Match <$> genSelectionDomClass
      | otherwise = frequency
          [ (matchWeight, Match <$> genSelectionDomClass),
            (parentWeight, genParentSelectionAtDepth depth remaining)
          ]
      where
        matchWeight = 3
        parentWeight = 2

    genParentSelectionAtDepth :: Int -> Int -> Gen AbsLoc
    genParentSelectionAtDepth depth remaining = do
      let afterSelf = remaining - 1  -- this combinator node costs 1
      childSelections <- genChildSelectionsAtDepth depth afterSelf
      frequency
        [ (1, pure $ Or' childSelections),
          (1, pure $ And' childSelections),
          (1, do
              -- Split remaining budget between parent and descendant subtrees.
              let half = max 1 (afterSelf `div` 2)
              parentSelection <- genSelectionAtDepth (depth + 1) half
              descendantSelection <- genSelectionAtDepth (depth + 1) half
              pure $ Under {parentLoc = parentSelection, descendantLoc = descendantSelection}
          )
        ]

    genChildSelectionsAtDepth :: Int -> Int -> Gen (NonEmpty AbsLoc)
    genChildSelectionsAtDepth depth remaining = do
      let budgetMax = min maxSelectionChildren remaining
          budgetMin = min 1 budgetMax
      childCount <- if budgetMax < budgetMin
                    then pure 1
                    else G.integral $ R.between (budgetMin, budgetMax)
      let perChild = max 1 (remaining `div` childCount)
      firstSelection <- genSelectionAtDepth (depth + 1) perChild
      restSelections <- replicateM (childCount - 1) (genSelectionAtDepth (depth + 1) perChild)
      pure $ firstSelection :| restSelections
      pure $ firstSelection :| restSelections

match :: Node -> AbsLoc -> [Text]
match root sel =
  let matchingIds = selectIds root sel
   in
    flatten root
      & fmap (.autoId)
      & filter (`elem` matchingIds)

-- Evaluate a selection against a subtree and return matching node ids.
selectNodes :: Node -> AbsLoc -> [Node]
selectNodes root sel =
  case sel of
    Match {domClass} ->
      flatten root
        & filter (elem domClass . (.classes))
    Or' {locs} -> nub . concat $ selectNodes root <$> locs
    And' {locs} -> foldSelections root locs
    Under {parentLoc, descendantLoc} ->
       nub . concat $ selectUnder descendantLoc <$> (selectNodes root parentLoc)

-- Evaluate a selection against a subtree and return matching node ids.
selectIds :: Node -> AbsLoc -> [Text]
selectIds root sel = (.autoId) <$> selectNodes root sel

-- Evaluate all child selections under a specific parent node.
selectUnder :: AbsLoc -> Node -> [Node]
selectUnder descendantLoc parentNode =
  descendants parentNode >>= flip selectNodes descendantLoc

foldSelections :: Node -> NonEmpty AbsLoc -> [Node]
foldSelections root (firstSel :| rest) =
  foldl' intersect (selectNodes root firstSel) $ selectNodes root <$> rest

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

countSelectionNodes :: AbsLoc -> Int
countSelectionNodes = \case
  Match {} -> 1
  Or' {locs} -> 1 + sum (countSelectionNodes <$> locs)
  And' {locs} -> 1 + sum (countSelectionNodes <$> locs)
  Under {parentLoc, descendantLoc} -> 1 + countSelectionNodes parentLoc + countSelectionNodes descendantLoc

chkListContentEq :: (Ord a, Show a) => Text -> [a] -> [a] -> IO ()
chkListContentEq message expected actual =
  assertEqual (unpack message) (sort expected) (sort actual)

sanityChk' :: Node ->Text -> [Text] -> AbsLoc -> TestTree
sanityChk' node message expected selection =
  testCase (unpack message) $ do
    let actual = match node selection
    chkListContentEq message expected actual

sanityChk :: Text -> [Text] -> AbsLoc -> TestTree
sanityChk message expected selection =
  sanityChk' sanityNode message expected selection

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

-- Tests run against sanityNode (above)
sanitySimpleA :: TestTree
sanitySimpleA = sanityChk "Simple Match A" ["1", "1-1", "1-2-1", "1-3-1"] $ Match A

sanityAOrB :: TestTree
sanityAOrB = sanityChk "A or B"  ["1", "1-1", "1-2-1", "1-3-1", "1-2"] $ Or' (Match A :| [Match B])

sanityAandC :: TestTree
sanityAandC = sanityChk "A and C"  ["1-2-1", "1-3-1"] $ And' (Match A :| [Match C])

sanityAandCUnderC :: TestTree
sanityAandCUnderC = sanityChk "A and C under C"  ["1-3-1"] $ Under {parentLoc = Match C, descendantLoc = And' (Match A :| [Match C])}

sanityAandCUnderB :: TestTree
sanityAandCUnderB = sanityChk "A and C under B"  ["1-2-1", "1-3-1"] $ Under {parentLoc = Match B, descendantLoc = And' (Match A :| [Match C])}

sanityAUnderB :: TestTree
sanityAUnderB = sanityChk "A under B"  ["1-1", "1-2-1", "1-3-1"] $ Under {parentLoc = Match B, descendantLoc = Match A}

sanityNode2 :: Node
sanityNode2 =
   Div
    { autoId = "1-1-3"
    , classes = [ A ]
    , children =
        [ 
          Div
            { autoId = "1-1-3-6"
            , classes = [ A , B ]
            , children = [ Span { autoId = "1-1-3-6-1" , classes = [ B ] } ]
            }
        ]
    }

sanityNested :: TestTree
sanityNested =  sanityChk' sanityNode2 "Double nested" [] $ 
                    Under
                      { parentLoc = Match B 
                      , descendantLoc =
                          Under
                            { parentLoc = Match A 
                            , descendantLoc = Match B 
                            }
                      }


sanityNode3 :: Node
sanityNode3 =
   Div
    { autoId = "1"
    , classes = [ C ]
    , children =
        [ 
          Div
            { autoId = "1-1-3-6"
            , classes = [ A ]
            , children = [ 
              Span { autoId = "1-1-3-6-1" , classes = [ B ] }, 
              Div
                { autoId = "1-1-3-6-2"
                , classes = [ B ]
                , children = [ Span { autoId = "1-1-3-6-2-1" , classes = [ C ] } ]
                }
               ]
            }
        ]
    }

sanityNested2 :: TestTree
sanityNested2 =  sanityChk' sanityNode3 "Double nested 2" ["1-1-3-6-2-1"] $ 
                    Under
                      { parentLoc = Match C 
                      , descendantLoc =
                          Under
                            { parentLoc = Match B 
                            , descendantLoc = Match C
                            }
                      }

sanityNode4 :: Node
sanityNode4 =
  Div
    { autoId = "1"
    , classes = [ A ]
    , children =
        [ Div
            { autoId = "1-1"
            , classes = [ A ]
            , children =
                [ Span { autoId = "1-1-1" , classes = [ A ] }
                , Span { autoId = "1-1-2" , classes = [ A ] }
                , Div
                    { autoId = "1-1-3"
                    , classes = [ A ]
                    , children =
                        [ Span { autoId = "1-1-3-1" , classes = [ A ] }
                        , Span { autoId = "1-1-3-2" , classes = [ A ] }
                        , Span { autoId = "1-1-3-3" , classes = [ A ] }
                        , Span { autoId = "1-1-3-4" , classes = [ A ] }
                        , Span { autoId = "1-1-3-5" , classes = [ A ] }
                        , Div
                            { autoId = "1-1-3-6"
                            , classes = [ A , B ]
                            , children = [ Span { autoId = "1-1-3-6-1" , classes = [ B ] } ]
                            }
                        ]
                    }
                ]
            }
        ]
    }
  
nestedLoc :: AbsLoc
nestedLoc = 
    Under
      { parentLoc = Match { domClass = B }
      , descendantLoc =
          Under
            { parentLoc = Match A
            , descendantLoc = Or' { locs = Match { domClass = B } :| [] }
            }
      }


data LocatorTestFailure = MkLocatorTestFailure
  { node :: Node,
    html :: Text,
    selection :: AbsLoc,
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
    locCase@Matched {} <- gen genCase
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
          -- overrideVerbose = Just Verbose,
          overrideVerbose = Nothing,
          overrideNumTests = Just 1000,
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
      -- putStrLn $ "LocateCombinatorTest pre-IO node count (test node): " 
      --   <> show (countNodes testNode) 
      --   <> " (generator selection): " 
      --   <> show (countSelectionNodes abstractLocator)
      wdSession <- getSession
      runHttp wdSession $ do
        let dataUrl = htmlToDataUrl html
        navigateTo dataUrl
        evaluateExpectation
      where
        locator = locCase.locator
        locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es (Either L.LocateException [ElementId])
        locateAll = locateAllHttp $ defAllOpts {L.jsRecheckDisplayed = DisplayedCheckNever}
        html = "<!doctype html><html><head><meta charset=\"utf-8\"></head><body>"
              <> nodeToHtml testNode
              <> "</body></html>"
        evaluateExpectation :: forall es. (IOE :> es, WebDriverHttp :> es) => Eff es ()
        evaluateExpectation = do
          locateRslt <- locateAll locator
          actual <- locateRslt & either
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
              liftIO . throwIO . userError $ "Failure generated" 
                                                <> "\n" 
                                                <> unpack (txt failure)

    Unmatched {} ->
      liftIO $ throwIO $ userError "evaluateCase called with an unmatched locator case"

htmlToDataUrl :: Text -> URL
htmlToDataUrl html =
  let htmlBytes = encodeUtf8 html
      encoded = B64T.extractBase64 $ B64.encodeBase64 htmlBytes
      dataUrl = "data:text/html;base64," <> encoded
  in MkUrl dataUrl

mkLocatorTestFailure :: Node -> Text -> AbsLoc -> Locator -> [Text] -> [Text] -> LocatorTestFailure
mkLocatorTestFailure node html selection generatedLocator expectedMatches actualMatches =
  let missingFromActual = expectedMatches \\ actualMatches
      extraInActual = actualMatches \\ expectedMatches
    in
      MkLocatorTestFailure
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
-- _pattern = Just "OR with contains under"
_pattern = Nothing

_eval :: Maybe Text -> TestTree -> IO ()
_eval mPattern = withArgs (maybe [] (\pat -> ["-p", (unpack pat)]) mPattern) . defaultMain

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess



