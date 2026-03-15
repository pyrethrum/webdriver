module LocatorsTest (tests) where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, unpack)
import Data.Text.IO (putStrLn)
import System.Environment (withArgs)
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Predicate (dot, expect, fn, (.$))
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), Verbose (..), gen, info, testPropertyWith)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (txt)
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locators.Internal (CaseSensitive, Locator (..), flattenLoc, foldLoc, foldLocBottomUp)
import Prelude hiding (putStrLn)

-- >>> _eval tests

-- *** Exception: ExitSuccess

tests :: TestTree
tests =
  testGroup
    "Locators"
    [ testGroup
        "Show"
        [ testCase "CSS shows correctly" $
            show (CSS "button") @?= "CSS {value = \"button\"}"
        ],
      testGroup
        "flattenLoc"
        [ flattenNestedAnd,
          flattenNestedOr,
          reduceSingleAnd,
          reduceSingleOr,
          applyDoubleNegation,
          applyDeMorganAnd,
          applyDeMorganOr,
          preserveNonMatchLocators,
          recursiveReduceParent,
          complexNestedFlattening
        ],
      testGroup
        "foldLoc traversal order"
        [ foldLocTopDown,
          foldLocBottomUpTest
        ],
      testGroup
        "Property Tests"
        [ test_mock_logic_preserved_on_flattenning,
          test_flatenning_simplification,
          test_nested_none_match,
          test_infix_precedence_i,
          test_infix_precedence_ii,
          test_parent_infix_precedence
        ]
    ]

logPretty :: (Show a) => a -> IO ()
logPretty = putStrLn . txt

-- http_login_navigation_demo :: IO ()
-- http_login_navigation_demo = do
--   undefined
--   where
--     loginButton = button "Submit"
--     navBar = navigation "Main Navigation"
--     absurdLoc = notLoc (button "Submit" &&& navBar) ||| navBar

logging :: Bool
logging = True

data FlattenCase = MkFlattenCase
  { unflattened :: Locator,
    flattenned :: Locator
  }

chkFlatten :: Text -> FlattenCase -> TestTree
chkFlatten description MkFlattenCase {unflattened, flattenned = expected} =
  testCase (unpack description) $ do
    when logging $ do
      logPretty unflattened
      logPretty "--->"
      logPretty actual
    expected @?= actual
  where
    actual = flattenLoc unflattened

_eval :: TestTree -> IO ()
_eval = withArgs [] . defaultMain

-- >>> _eval flattenNestedAnd
flattenNestedAnd :: TestTree
flattenNestedAnd =
  chkFlatten
    "flattens nested And"
    MkFlattenCase
      { unflattened = And (And (CSS "a" :| [CSS "b"]) :| [CSS "c"]),
        flattenned = And (CSS "a" :| [CSS "b", CSS "c"])
      }

flattenNestedOr :: TestTree
flattenNestedOr =
  chkFlatten
    "flattens nested Or"
    MkFlattenCase
      { unflattened = Or (Or (CSS "a" :| [CSS "b"]) :| [CSS "c"]),
        flattenned = Or (CSS "a" :| [CSS "b", CSS "c"])
      }

reduceSingleAnd :: TestTree
reduceSingleAnd =
  chkFlatten
    "reduces single element And to the element"
    MkFlattenCase
      { unflattened = And (CSS "button" :| []),
        flattenned = CSS "button"
      }

-- >>> _eval reduceSingleOr

-- *** Exception: ExitSuccess

reduceSingleOr :: TestTree
reduceSingleOr =
  chkFlatten
    "reduces single element Or to the element"
    MkFlattenCase
      { unflattened = Or (CSS "button" :| []),
        flattenned = CSS "button"
      }

applyDoubleNegation :: TestTree
applyDoubleNegation =
  chkFlatten
    "applies double negation: Not [Not [x]] -> x"
    MkFlattenCase
      { unflattened = Not (Not (CSS "button" :| []) :| []),
        flattenned = CSS "button"
      }

-- >>> _eval applyDeMorganAnd

-- *** Exception: ExitSuccess

applyDeMorganAnd :: TestTree
applyDeMorganAnd =
  chkFlatten
    "applies De Morgan: Not [And [x, y]] -> Or [Not [x], Not [y]]"
    MkFlattenCase
      { unflattened = Not (And (CSS "a" :| [CSS "b"]) :| []),
        flattenned = Or (Not (CSS "a" :| []) :| [Not (CSS "b" :| [])])
      }

applyDeMorganOr :: TestTree
applyDeMorganOr =
  chkFlatten
    "applies De Morgan: Not [Or [x, y]] -> And [Not [x], Not [y]]"
    MkFlattenCase
      { unflattened = Not (Or (CSS "a" :| [CSS "b"]) :| []),
        flattenned = And (Not (CSS "a" :| []) :| [Not (CSS "b" :| [])])
      }

preserveNonMatchLocators :: TestTree
preserveNonMatchLocators =
  chkFlatten
    "preserves non-Match* locators"
    MkFlattenCase
      { unflattened = CSS "button",
        flattenned = CSS "button"
      }

recursiveReduceParent :: TestTree
recursiveReduceParent =
  chkFlatten
    "recursively reduces Parent locators"
    MkFlattenCase
      { unflattened = Parent (And (CSS "a" :| [])) (Or (CSS "b" :| [])),
        flattenned = Parent (CSS "a") (CSS "b")
      }

complexNestedFlattening :: TestTree
complexNestedFlattening =
  chkFlatten
    "complex nested flattening"
    MkFlattenCase
      { unflattened = And (And (CSS "a" :| [And (CSS "b" :| [CSS "c"])]) :| [CSS "d"]),
        flattenned = And (CSS "a" :| [CSS "b", CSS "c", CSS "d"])
      }

-- | Shared nested locator used by fold traversal tests.
-- Tree shape (4 different constructors, 3 levels deep):
--
--   Parent
--   ├── And
--   │   ├── CSS "a"
--   │   └── XPath "//b"
--   └── Not
--       └── Tag "div"
nestedLoc :: Locator
nestedLoc = Parent (And (CSS "a" :| [XPath "//b"])) (Not (Tag "div" :| []))

-- | Collect txt of each node in the order visited, using snoc.
collectLoc :: (([Locator] -> Locator -> [Locator]) -> [Locator] -> Locator -> [Locator]) -> [Locator]
collectLoc fold' = fold' (\acc loc -> acc `snoc` loc) [] nestedLoc
  where
    snoc xs x = xs ++ [x]

-- >>> _eval foldLocTopDown

-- *** Exception: ExitSuccess

foldLocTopDown :: TestTree
foldLocTopDown =
  testCase "foldLoc visits nodes top-down (pre-order)" $
    collectLoc foldLoc
      @?= [ nestedLoc,
            And (CSS "a" :| [XPath "//b"]),
            CSS "a",
            XPath "//b",
            Not (Tag "div" :| []),
            Tag "div"
          ]

-- >>> _eval foldLocBottomUpTest

-- *** Exception: ExitSuccess

foldLocBottomUpTest :: TestTree
foldLocBottomUpTest =
  testCase "foldLocBottomUp visits nodes bottom-up (post-order)" $
    collectLoc foldLocBottomUp
      @?= [ CSS "a",
            XPath "//b",
            And (CSS "a" :| [XPath "//b"]),
            Tag "div",
            Not (Tag "div" :| []),
            nestedLoc
          ]

trueLoc :: Locator
trueLoc = css "True"

falseLoc :: Locator
falseLoc = button "False"

mockLocated :: Locator -> Bool
mockLocated = \case
  CSS "True" -> True
  Role (Just Button) (Just "False") -> False
  And locs -> all mockLocated locs
  Or locs -> any mockLocated locs
  Not locs -> not (any mockLocated locs)
  Parent parent child -> mockLocated parent && mockLocated child
  _ -> error "Locator not Mocked"

-- | Falsify generator for Locator with depth and node count limits.
-- Only generates Parent, And, Or, Not, and singletons (trueLoc, falseLoc).
-- Layers 0-1: Equal probability for all constructors (20% each)
-- Singleton selection: 80% trueLoc, 20% falseLoc
-- After layer 1: Increase singleton probability by 5% per layer
-- Terminates at max 10 layers or approximately 1000 nodes
genLocator :: Gen Locator
genLocator = genLocatorWithLimits genTrueFalseLoc 0 1000

-- Internal generator that tracks depth and remaining node budget
genLocatorWithLimits :: Gen Locator -> Int -> Int -> Gen Locator
genLocatorWithLimits genSingleton depth remainingNodes
  | depth >= 10 || remainingNodes <= 0 = genSingleton
  | otherwise = frequency weights
  where
    -- Calculate singleton probability
    -- Layers 0-1: 20% singleton
    -- Layer 2: 25% singleton (20% + 5%)
    -- Layer 3: 30% singleton (20% + 10%), etc.
    baseSingletonProb = 20 :: Word
    extraProb = if depth <= 1 then 0 else fromIntegral (depth - 1) * 5
    singletonProb = min 100 (baseSingletonProb + extraProb)

    -- Remaining probability distributed evenly among 4 constructors
    nonSingletonProb = 100 - singletonProb
    perConstructorProb = nonSingletonProb `div` 4

    -- Small adjustment for rounding
    remainder = nonSingletonProb `mod` 4

    weights =
      [ (singletonProb, genSingleton),
        (perConstructorProb, genParentAt genSingleton (depth + 1) (remainingNodes - 1)),
        (perConstructorProb, genAndAt genSingleton (depth + 1) (remainingNodes - 1)),
        (perConstructorProb, genOrAt genSingleton (depth + 1) (remainingNodes - 1)),
        (perConstructorProb + remainder, genNotAt genSingleton (depth + 1) (remainingNodes - 1))
      ]

-- | Generate a singleton locator (80% trueLoc, 20% falseLoc)
genTrueFalseLoc :: Gen Locator
genTrueFalseLoc =
  frequency
    [ (80, pure trueLoc),
      (20, pure falseLoc)
    ]

uniformFrequency :: [a] -> Gen a
uniformFrequency = frequency . fmap (1,) . fmap pure

xPathOnlySample :: Gen Locator
xPathOnlySample = uniformFrequency xPathOnlyLocs

xPathOnlyLocs :: [Locator]
xPathOnlyLocs =
  [ XPath "//div",
    All,
    ID "my-id",
    Class "my-class" Contains CaseSensitive,
    Attribute "data-test" Contains CaseInsensitive,
    Tag "button"
  ]

invalidHTTPLocs :: Gen Locator
invalidHTTPLocs = uniformFrequency
  [ Default $ Default "nested",
    BiDiContext $ BrowsingContext "context-id"
  ]

-- | Generate a Parent locator at a given depth
genParentAt :: Gen Locator -> Int -> Int -> Gen Locator
genParentAt genSingleton depth remainingNodes = do
  -- Split remaining nodes between parent and child
  let halfNodes = remainingNodes `div` 2
  parent <- genLocatorWithLimits genSingleton depth halfNodes
  child <- genLocatorWithLimits genSingleton depth (remainingNodes - halfNodes - 1)
  pure $ Parent parent child

-- | Generate a And locator at a given depth
genAndAt :: Gen Locator -> Int -> Int -> Gen Locator
genAndAt genSingleton depth remainingNodes = do
  locs <- genNonEmptyLocators genSingleton depth remainingNodes
  pure $ And locs

-- | Generate a Or locator at a given depth
genOrAt :: Gen Locator -> Int -> Int -> Gen Locator
genOrAt genSingleton depth remainingNodes = do
  locs <- genNonEmptyLocators genSingleton depth remainingNodes
  pure $ Or locs

-- | Generate a Not locator at a given depth
genNotAt :: Gen Locator -> Int -> Int -> Gen Locator
genNotAt genSingleton depth remainingNodes = do
  locs <- genNonEmptyLocators genSingleton depth remainingNodes
  pure $ Not locs

-- | Generate a non-empty list of locators, distributing the node budget
genNonEmptyLocators :: Gen Locator -> Int -> Int -> Gen (NonEmpty Locator)
genNonEmptyLocators genSingleton depth remainingNodes = do
  -- Generate 1 to min(5, remainingNodes) locators
  let maxCount = min 5 (max 1 remainingNodes)
  count <- G.integral $ R.between (1, maxCount)
  let nodesPerLoc = max 1 (remainingNodes `div` count)
  case count of
    1 -> do
      loc <- genLocatorWithLimits genSingleton depth remainingNodes
      pure $ loc :| []
    n -> do
      first <- genLocatorWithLimits genSingleton depth nodesPerLoc
      rest <- sequence [genLocatorWithLimits genSingleton depth nodesPerLoc | _ <- [2 .. n]]
      pure $ first :| rest

-- Property test options for 100 tests
genLocatorOptions :: TestOptions
genLocatorOptions =
  TestOptions
    { expectFailure = DontExpectFailure,
      overrideVerbose = Just NotVerbose,
      overrideMaxShrinks = Nothing,
      overrideNumTests = Just 1000,
      overrideMaxRatio = Nothing
    }

-- >>> _eval test_flatenning_simplification

-- *** Exception: ExitSuccess

test_flatenning_simplification :: TestTree
test_flatenning_simplification = testPropertyWith genLocatorOptions "Flattening simplification" $ do
  loc <- gen genLocator
  let unflattenedComplexity = complexity loc
      flatloc = flattenLoc loc
      flattenedComplexity = complexity flatloc
  info $ "Unflattened complexity: " <> show unflattenedComplexity
  info $ "Original locator:\n" <> unpack (txt loc)
  info $ "Flattened complexity: " <> show flattenedComplexity
  info $ "Flattened locator:\n" <> unpack (txt flatloc)
  F.assert $ expect True `dot` fn ("flattenLoc simplifies or maintains complexity", \_l -> complexity flatloc <= complexity loc) .$ ("loc", loc)
  where
    -- Calculate complexity score: singleton/leaf = 1, nesting constructors = 2
    complexity :: Locator -> Int
    complexity = \case
      -- trueLoc
      CSS "True" -> 1
      -- falseLoc
      Role (Just Button) (Just "False") -> 1
      Not (x :| []) -> complexity x --- Singleton Not leaf - complexity of child
      Not locs -> plus2Map locs
      And locs -> plus2Map locs
      And (_ :| []) -> error "Singleton And should be flattenned"
      Or locs -> plus2Map locs
      Or (_ :| []) -> error "Singleton Or should be flattenned"
      Parent parent child -> 2 + complexity parent + complexity child
      _ -> error "Locator not Mocked"

    plus2Map :: NonEmpty Locator -> Int
    plus2Map locs = 2 + (sum $ complexity <$> locs)

-- Mock property test that generates locators and logs them

-- >>> _eval test_mock_logic_preserved_on_flattenning

-- *** Exception: ExitSuccess

test_mock_logic_preserved_on_flattenning :: TestTree
test_mock_logic_preserved_on_flattenning = testPropertyWith genLocatorOptions "Generate and log locators" $ do
  loc <- gen genLocator
  F.assert $ expect True `dot` fn ("flattenLoc preserves mockLocated", \l -> mockLocated l == mockLocated (flattenLoc l)) .$ ("loc", loc)

-- >>> _eval test_fail

-- *** Exception: ExitSuccess

test_nested_none_match :: TestTree
test_nested_none_match = testCase "This test fails" $ do
  let loc = Not (Not (Not (falseLoc :| [trueLoc]) :| []) :| [])
  -- logPretty loc
  -- logPretty "--->"
  -- logPretty (flattenLoc loc)
  mockLocated loc @?= mockLocated (flattenLoc loc)

-- >>> _eval test_infix_precedence

-- *** Exception: ExitSuccess

test_infix_precedence_i :: TestTree
test_infix_precedence_i =
  testCase "Test operator precedence i" $
    expected @?= actual
  where
    expected = True || False && False
    actual = mockLocated $ trueLoc ||| falseLoc &&& falseLoc

-- >>> _eval test_infix_precedence_ii

-- *** Exception: ExitSuccess

test_infix_precedence_ii :: TestTree
test_infix_precedence_ii =
  testCase "Test operator precedence ii" $
    expected @?= actual
  where
    expected = False || True && False || True
    actual = mockLocated $ falseLoc ||| trueLoc &&& falseLoc ||| trueLoc

-- >>> _eval test_parent_infix_precedence

-- *** Exception: ExitSuccess

test_parent_infix_precedence :: TestTree
test_parent_infix_precedence =
  testCase "Test Parent operator precedence" $
    expected @?= actual
  where
    expected = Parent (falseLoc ||| trueLoc) (trueLoc &&& falseLoc)
    actual = falseLoc ||| trueLoc >>> trueLoc &&& falseLoc
