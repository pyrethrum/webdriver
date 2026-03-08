module LocatorsTest (tests) where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, unpack)
import Data.Text.IO (putStrLn)
import System.Environment (withArgs)
import Test.Falsify.Generator as G (Gen, bool, choose, frequency, integral, list)
import Test.Falsify.Predicate (dot, expect, fn, (.$))
import Test.Falsify.Range as R (between)
import Test.Tasty
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), Property, TestOptions (..), Verbose (..), gen, info, testPropertyWith)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit
import Utils (txt)
import WebDriverPreCore.Extended.Locators
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
        [ flattenNestedMatchAll,
          flattenNestedMatchAny,
          reduceSingleMatchAll,
          reduceSingleMatchAny,
          applyDoubleNegation,
          applyDeMorganMatchAll,
          applyDeMorganMatchAny,
          preserveNonMatchLocators,
          recursiveReduceParent,
          complexNestedFlattening
        ],
      testGroup
        "Property Tests"
        [ test_mock_logic_preserved_on_flattenning,
          test_nested_none_match
        ]
    ]

logPretty :: (Show a) => a -> IO ()
logPretty = putStrLn . txt

http_login_navigation_demo :: IO ()
http_login_navigation_demo = do
  undefined
  where
    loginButton = button "Submit"
    navBar = navigation "Main Navigation"
    absurdLoc = notLoc (button "Submit" &&& navBar) ||| navBar

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

-- >>> _eval flattenNestedMatchAll
flattenNestedMatchAll :: TestTree
flattenNestedMatchAll =
  chkFlatten
    "flattens nested MatchAll"
    MkFlattenCase
      { unflattened = MatchAll (MatchAll (CSS "a" :| [CSS "b"]) :| [CSS "c"]),
        flattenned = MatchAll (CSS "a" :| [CSS "b", CSS "c"])
      }

flattenNestedMatchAny :: TestTree
flattenNestedMatchAny =
  chkFlatten
    "flattens nested MatchAny"
    MkFlattenCase
      { unflattened = MatchAny (MatchAny (CSS "a" :| [CSS "b"]) :| [CSS "c"]),
        flattenned = MatchAny (CSS "a" :| [CSS "b", CSS "c"])
      }

reduceSingleMatchAll :: TestTree
reduceSingleMatchAll =
  chkFlatten
    "reduces single element MatchAll to the element"
    MkFlattenCase
      { unflattened = MatchAll (CSS "button" :| []),
        flattenned = CSS "button"
      }

-- >>> _eval reduceSingleMatchAny

-- *** Exception: ExitSuccess

reduceSingleMatchAny :: TestTree
reduceSingleMatchAny =
  chkFlatten
    "reduces single element MatchAny to the element"
    MkFlattenCase
      { unflattened = MatchAny (CSS "button" :| []),
        flattenned = CSS "button"
      }

applyDoubleNegation :: TestTree
applyDoubleNegation =
  chkFlatten
    "applies double negation: MatchNone [MatchNone [x]] -> x"
    MkFlattenCase
      { unflattened = MatchNone (MatchNone (CSS "button" :| []) :| []),
        flattenned = CSS "button"
      }

applyDeMorganMatchAll :: TestTree
applyDeMorganMatchAll =
  chkFlatten
    "applies De Morgan: MatchNone [MatchAll [x, y]] -> MatchAny [MatchNone [x], MatchNone [y]]"
    MkFlattenCase
      { unflattened = MatchNone (MatchAll (CSS "a" :| [CSS "b"]) :| []),
        flattenned = MatchAny (MatchNone (CSS "a" :| []) :| [MatchNone (CSS "b" :| [])])
      }

applyDeMorganMatchAny :: TestTree
applyDeMorganMatchAny =
  chkFlatten
    "applies De Morgan: MatchNone [MatchAny [x, y]] -> MatchAll [MatchNone [x], MatchNone [y]]"
    MkFlattenCase
      { unflattened = MatchNone (MatchAny (CSS "a" :| [CSS "b"]) :| []),
        flattenned = MatchAll (MatchNone (CSS "a" :| []) :| [MatchNone (CSS "b" :| [])])
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
      { unflattened = Parent (MatchAll (CSS "a" :| [])) (MatchAny (CSS "b" :| [])),
        flattenned = Parent (CSS "a") (CSS "b")
      }

complexNestedFlattening :: TestTree
complexNestedFlattening =
  chkFlatten
    "complex nested flattening"
    MkFlattenCase
      { unflattened = MatchAll (MatchAll (CSS "a" :| [MatchAll (CSS "b" :| [CSS "c"])]) :| [CSS "d"]),
        flattenned = MatchAll (CSS "a" :| [CSS "b", CSS "c", CSS "d"])
      }

trueLoc :: Locator
trueLoc = css "True"

falseLoc :: Locator
falseLoc = button "False"

mockLocated :: Locator -> Bool
mockLocated = \case
  CSS "True" -> True
  Role (Just Button) (Just "False") -> False
  MatchAll locs -> all mockLocated locs
  MatchAny locs -> any mockLocated locs
  MatchNone locs -> not (any mockLocated locs)
  Parent parent child -> mockLocated parent && mockLocated child
  _ -> error "Locator not Mocked"

-- | Falsify generator for Locator with depth and node count limits.
-- Only generates Parent, MatchAll, MatchAny, MatchNone, and singletons (trueLoc, falseLoc).
-- Layers 0-1: Equal probability for all constructors (20% each)
-- Singleton selection: 80% trueLoc, 20% falseLoc
-- After layer 1: Increase singleton probability by 5% per layer
-- Terminates at max 10 layers or approximately 1000 nodes
genLocator :: Gen Locator
genLocator = genLocatorWithLimits 0 1000

-- Internal generator that tracks depth and remaining node budget
genLocatorWithLimits :: Int -> Int -> Gen Locator
genLocatorWithLimits depth remainingNodes
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
        (perConstructorProb, genParentAt (depth + 1) (remainingNodes - 1)),
        (perConstructorProb, genMatchAllAt (depth + 1) (remainingNodes - 1)),
        (perConstructorProb, genMatchAnyAt (depth + 1) (remainingNodes - 1)),
        (perConstructorProb + remainder, genMatchNoneAt (depth + 1) (remainingNodes - 1))
      ]

-- | Generate a singleton locator (80% trueLoc, 20% falseLoc)
genSingleton :: Gen Locator
genSingleton = frequency [(80 :: Word, pure trueLoc), (20, pure falseLoc)]

-- | Generate a Parent locator at a given depth
genParentAt :: Int -> Int -> Gen Locator
genParentAt depth remainingNodes = do
  -- Split remaining nodes between parent and child
  let halfNodes = remainingNodes `div` 2
  parent <- genLocatorWithLimits depth halfNodes
  child <- genLocatorWithLimits depth (remainingNodes - halfNodes - 1)
  pure $ Parent parent child

-- | Generate a MatchAll locator at a given depth
genMatchAllAt :: Int -> Int -> Gen Locator
genMatchAllAt depth remainingNodes = do
  locs <- genNonEmptyLocators depth remainingNodes
  pure $ MatchAll locs

-- | Generate a MatchAny locator at a given depth
genMatchAnyAt :: Int -> Int -> Gen Locator
genMatchAnyAt depth remainingNodes = do
  locs <- genNonEmptyLocators depth remainingNodes
  pure $ MatchAny locs

-- | Generate a MatchNone locator at a given depth
genMatchNoneAt :: Int -> Int -> Gen Locator
genMatchNoneAt depth remainingNodes = do
  locs <- genNonEmptyLocators depth remainingNodes
  pure $ MatchNone locs

-- | Generate a non-empty list of locators, distributing the node budget
genNonEmptyLocators :: Int -> Int -> Gen (NonEmpty Locator)
genNonEmptyLocators depth remainingNodes = do
  -- Generate 1 to min(5, remainingNodes) locators
  let maxCount = min 5 (max 1 remainingNodes)
  count <- G.integral $ R.between (1, maxCount)
  let nodesPerLoc = max 1 (remainingNodes `div` count)
  case count of
    1 -> do
      loc <- genLocatorWithLimits depth remainingNodes
      pure $ loc :| []
    n -> do
      first <- genLocatorWithLimits depth nodesPerLoc
      rest <- sequence [genLocatorWithLimits depth nodesPerLoc | _ <- [2 .. n]]
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
  let loc = MatchNone (MatchNone (MatchNone (falseLoc :| [trueLoc]) :| []) :| [])
  -- logPretty loc
  -- logPretty "--->"
  -- logPretty (flattenLoc loc)
  mockLocated loc @?= mockLocated (flattenLoc loc)
