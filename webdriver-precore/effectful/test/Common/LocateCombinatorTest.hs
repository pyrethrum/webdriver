module Common.LocateCombinatorTest
  ( tests,
  )
where

import Common.LocatorCombinatorSanityTests
  ( AbsLoc (And', Match, Or', Under, descendantLoc, domClass, locs, parentLoc),
    DOMClass (..),
    Node (Div, Span, autoId, children, classes),
    match,
  )
import Common.WebDriver.Effect (WebDriver, executeScript, getAttribute, locateAll, navigateTo)
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (replicateM)
import Data.Aeson (toJSON)
import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import Data.List (find, nub, sort, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Property (gen)
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, inOrderTestGroup)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), info, testFailed, testPropertyWith)
import Test.Tasty.HUnit (testCase)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL (..))
import WebDriverPreCore.Extended.Locators (CaseSensitivity (..), Locator, MatchType (..), css, elmClass, elmClass', (&&&), (>>>), (|||))
import WebDriverPreCore.Utils.Utils (txt)
import Prelude

tests ::
  forall elm es.
  (IOE :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
tests run interp =
  inOrderTestGroup
    "Combinator tests"
    [ inOrderTestGroup
        "Combinator property tests"
        [locateCombinatorProperty run interp],
      --  known failures from property tests
      inOrderTestGroup
        "Combinator singleton tests"
        [ locTest "simple OR" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [A],
                      children =
                        [ Span {autoId = "1-1", classes = [A]},
                          Span {autoId = "1-2", classes = [A]},
                          Span {autoId = "1-3", classes = [A]},
                          Span {autoId = "1-4", classes = [B]}
                        ]
                    },
                abstractLocator =
                  Or'
                    { locs = Match A :| [Match {domClass = B}]
                    },
                expectedMatches = ["1", "1-1", "1-2", "1-3", "1-4"],
                locator = classLoc A ||| classLoc B
              },
          locTest "simple under" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [A],
                      children = [Span {autoId = "1-1", classes = [A]}]
                    },
                abstractLocator =
                  Under
                    { parentLoc = Match A,
                      descendantLoc =
                        Or'
                          { locs = Match A :| [Match A]
                          }
                    },
                expectedMatches = ["1-1"],
                locator = classLoc A >>> (classLoc A ||| classLoc A)
              },
          locTest "nested under with OR remains scoped" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [A],
                      children =
                        [ Div
                            { autoId = "1-1",
                              classes = [B],
                              children =
                                [ Span {autoId = "1-1-1", classes = [C]},
                                  Span {autoId = "1-1-2", classes = [D]}
                                ]
                            },
                          Div
                            { autoId = "1-2",
                              classes = [B],
                              children =
                                [ Span {autoId = "1-2-1", classes = [C]}
                                ]
                            }
                        ]
                    },
                abstractLocator =
                  Under
                    { parentLoc = Match A,
                      descendantLoc =
                        Under
                          { parentLoc = Match {domClass = B},
                            descendantLoc =
                              Or'
                                { locs = Match {domClass = C} :| [Match {domClass = D}]
                                }
                          }
                    },
                expectedMatches = ["1-1-1", "1-1-2", "1-2-1"],
                locator = classLoc A >>> (classLoc B >>> (classLoc C ||| classLoc D))
              },
          locTest "Or in And" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [A],
                      children = [Span {autoId = "1-1", classes = [A]}]
                    },
                abstractLocator =
                  And'
                    { locs =
                        Match A
                          :| [ Or'
                                 { locs = Match {domClass = B} :| [Match A]
                                 }
                             ]
                    },
                expectedMatches = ["1", "1-1"],
                locator = classLoc A &&& (classLoc B ||| classLoc A)
              },
          locTest "Nested Contains" $
            Matched
              { testNode =
                  Div 
                    { autoId = "1",
                      classes = [A],
                      children =
                        [ Div
                            { autoId = "1-1",
                              classes = [A],
                              children =
                                [ Span {autoId = "1-1-1", classes = [A]},
                                  Span {autoId = "1-1-2", classes = [A]},
                                  Div
                                    { autoId = "1-1-3",
                                      classes = [A],
                                      children =
                                        [ Span {autoId = "1-1-3-1", classes = [A]},
                                          Span {autoId = "1-1-3-2", classes = [A]},
                                          Span {autoId = "1-1-3-3", classes = [A]},
                                          Span {autoId = "1-1-3-4", classes = [A]},
                                          Span {autoId = "1-1-3-5", classes = [A]},
                                          Div
                                            { autoId = "1-1-3-6",
                                              classes = [A, B],
                                              children = [Span {autoId = "1-1-3-6-1", classes = [B]}]
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    },
                abstractLocator =
                  Under
                    { parentLoc = Match {domClass = B},
                      descendantLoc =
                        Under
                          { parentLoc = Match A,
                            descendantLoc = Or' {locs = Match {domClass = B} :| []}
                          }
                    },
                expectedMatches = [],
                locator =
                  classLoc B
                    >>> classLoc A
                    >>> classLoc B
              },
          locTest "Or and contains" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [A],
                      children = [Span {autoId = "1-1", classes = [A]}]
                    },
                abstractLocator =
                  Or'
                    { locs =
                        Or'
                          { locs =
                              Match A
                                :| [ Match A,
                                     Or'
                                       { locs =
                                           Under
                                             { parentLoc = Match A,
                                               descendantLoc = Match A
                                             }
                                             :| []
                                       }
                                   ]
                          }
                          :| []
                    },
                expectedMatches = ["1", "1-1"],
                locator =
                  classLoc A
                    ||| classLoc A
                    ||| ( classLoc A
                            >>> classLoc A
                        )
              },
          locTest "Or and contains ii" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [A],
                      children = [Span {autoId = "1-1", classes = [A]}]
                    },
                abstractLocator =
                  Or'
                    { locs =
                        Match A
                          :| [ Match A,
                               Match A,
                               And'
                                 { locs =
                                     Match A
                                       :| [ Or'
                                              { locs =
                                                  Under
                                                    { parentLoc = Match A,
                                                      descendantLoc = Match A
                                                    }
                                                    :| []
                                              }
                                          ]
                                 }
                             ]
                    },
                expectedMatches = ["1", "1-1"],
                locator =
                  (classLoc A ||| classLoc A ||| classLoc A)
                    ||| (classLoc A &&& (classLoc A >>> classLoc A))
              },
          locTest "complex nested OR-AND-Under" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [A],
                      children = [Span {autoId = "1-1", classes = [A]}]
                    },
                abstractLocator =
                  Or'
                    { locs =
                        Match A
                          :| [ Match A,
                               Match A,
                               Match A,
                               And'
                                 { locs =
                                     Match A
                                       :| [ Match A,
                                            Under
                                              { parentLoc = Match A,
                                                descendantLoc = Match A
                                              },
                                            Match A,
                                            Under
                                              { parentLoc = Match A,
                                                descendantLoc = Match A
                                              }
                                          ]
                                 }
                             ]
                    },
                expectedMatches = ["1", "1-1"],
                locator =
                  let a = elmClass' Partial CaseInsensitive "A"
                   in (((a ||| a) ||| a) ||| a) ||| ((((a &&& a) &&& (a >>> a)) &&& a) &&& (a >>> a))
              },
          --
          locTest "OR with contains under" $
            Matched
              { testNode =
                  Div
                    { autoId = "1",
                      classes = [B],
                      children = [Span {autoId = "1-1", classes = [A]}]
                    },
                abstractLocator =
                  Or'
                    { locs =
                        Match A
                          :| [ Under
                                 { parentLoc = Match A,
                                   descendantLoc =
                                     Or'
                                       { locs =
                                           Match A
                                             :| [Match A, Match {domClass = B}]
                                       }
                                 }
                             ]
                    },
                expectedMatches = ["1-1"],
                locator = classLoc A ||| (classLoc A >>> classLoc A ||| classLoc A ||| classLoc B)
              }
        ]
    ]
  where
    locTest :: Text -> LocatorTestCase -> TestTree
    locTest name = testCase (unpack name) . evaluateCase run interp

classLoc :: DOMClass -> Locator
classLoc = elmClass' Full CaseInsensitive . txt

nodeToHtml :: Node -> Text
nodeToHtml node = case node of
  Div {children} -> "<div " <> idAndClasses <> ">" <> (T.concat $ nodeToHtml <$> children) <> "</div>"
  Span {} -> "<span " <> idAndClasses <> "></span>"
  where
    id' = " auto-id=\"" <> node.autoId <> "\""
    classes = if null node.classes then "" else " class=\"" <> T.intercalate " " (txt <$> node.classes) <> "\""
    idAndClasses = id' <> " " <> classes

data LocatorTestCase
  = Matched
      { testNode :: Node,
        abstractLocator :: AbsLoc,
        expectedMatches :: [Text],
        locator :: Locator
      }
  | Unmatched
      { testNode :: Node,
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
      pure $
        if null expectedMatches
          then Unmatched {testNode, abstractLocator, locator}
          else
            Matched
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
      childCount <-
        if budgetMax < budgetMin
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
      let className = txt domClass
       in frequency
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
      | otherwise =
          frequency
            [ (matchWeight, Match <$> genSelectionDomClass),
              (parentWeight, genParentSelectionAtDepth depth remaining)
            ]
      where
        matchWeight = 3
        parentWeight = 2

    genParentSelectionAtDepth :: Int -> Int -> Gen AbsLoc
    genParentSelectionAtDepth depth remaining = do
      let afterSelf = remaining - 1 -- this combinator node costs 1
      childSelections <- genChildSelectionsAtDepth depth afterSelf
      frequency
        [ (1, pure $ Or' childSelections),
          (1, pure $ And' childSelections),
          ( 1,
            do
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
      childCount <-
        if budgetMax < budgetMin
          then pure 1
          else G.integral $ R.between (budgetMin, budgetMax)
      let perChild = max 1 (remaining `div` childCount)
      firstSelection <- genSelectionAtDepth (depth + 1) perChild
      restSelections <- replicateM (childCount - 1) (genSelectionAtDepth (depth + 1) perChild)
      pure $ firstSelection :| restSelections
      pure $ firstSelection :| restSelections

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

locateCombinatorProperty ::
  forall elm es.
  (IOE :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
locateCombinatorProperty run interp =
  testPropertyWith propertyOptions "Generated locate combinator property" $ do
    locCase@Matched {} <- gen genCase
    info $ "LocateCombinatorTest generator node count (test node): " <> show (countNodes locCase.testNode)
    info $ "LocateCombinatorTest generator node count (generator selection): " <> show (countSelectionNodes locCase.abstractLocator)

    let evaluation = unsafeRunIO $ evaluateCase run interp locCase
    info $ "Test Case:\n" <> unpack (txt locCase)
    evaluation
      & either
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

-- used to shoehorn IO actions into Falsify which at time of writing does not support IO actions
unsafeRunIO :: IO a -> Either Text a
unsafeRunIO action =
  let rslt = unsafePerformIO $ try action
   in first (("Unexpected exception during property IO:\n " <>) . txt . displayException @SomeException) rslt
{-# NOINLINE unsafeRunIO #-}

evaluateCase ::
  forall elm es.
  (IOE :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  LocatorTestCase ->
  IO ()
evaluateCase run interp locCase =
  case locCase of
    Matched {testNode, abstractLocator, expectedMatches} ->
      run . interp $ do
        navigateTo @elm (MkUrl "about:blank")
        executeScript @elm script [toJSON html]
        locateRslt <- locateAll @elm locator
        actual <-
          locateRslt
            & either
              ( \err ->
                  liftIO . throwIO . userError $
                    "locateAll failed in generated locate property"
                      <> "\nSelection: "
                      <> unpack (txt abstractLocator)
                      <> "\nGenerated locator: "
                      <> unpack (txt locator)
                      <> "\nError: "
                      <> unpack (txt err)
              )
              (\elms -> nub . catMaybes <$> traverse (\el -> getAttribute @elm el "auto-id") elms)

        let expected = nub expectedMatches
        if sort expected == sort actual
          then pure ()
          else do
            let failure = mkLocatorTestFailure testNode html abstractLocator locator expected actual
            liftIO . throwIO . userError $
              "Failure generated"
                <> "\n"
                <> unpack (txt failure)
      where
        html =
          "<!doctype html><html><head><meta charset=\"utf-8\"></head><body>"
            <> nodeToHtml testNode
            <> "</body></html>"
    Unmatched {} ->
      liftIO . throwIO $ userError "evaluateCase called with an unmatched locator case"
  where
    locator = locCase.locator
    script = "document.open(); document.write(arguments[0]); document.close();"

mkLocatorTestFailure :: Node -> Text -> AbsLoc -> Locator -> [Text] -> [Text] -> LocatorTestFailure
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
