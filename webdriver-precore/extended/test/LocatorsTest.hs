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
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext (..))
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locators.Internal (CaseSensitivity (..), Classification (..), JSPostFilter, Locator (..), Protocol (..), anyLoc, classify, flattenLoc, foldLoc, foldLocBottomUp, hasInvalidLoc, sortGroupChildLocs)
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
        [ prop_mock_logic_preserved_on_flattenning,
          prop_flatenning_simplification,
          test_nested_none_match,
          test_infix_precedence_i,
          test_infix_precedence_ii,
          test_parent_infix_precedence,
          prop_classify_xpath_only_is_xpath,
          prop_classify_invalid_iff_any_invalid_node
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
    "flattens nested All"
    MkFlattenCase
      { unflattened = All (All (CSS "a" :| [CSS "b"]) :| [CSS "c"]),
        flattenned = All (CSS "a" :| [CSS "b", CSS "c"])
      }

flattenNestedOr :: TestTree
flattenNestedOr =
  chkFlatten
    "flattens nested Any"
    MkFlattenCase
      { unflattened = Any (Any (CSS "a" :| [CSS "b"]) :| [CSS "c"]),
        flattenned = Any (CSS "a" :| [CSS "b", CSS "c"])
      }

reduceSingleAnd :: TestTree
reduceSingleAnd =
  chkFlatten
    "reduces single element All to the element"
    MkFlattenCase
      { unflattened = All (CSS "button" :| []),
        flattenned = CSS "button"
      }

-- >>> _eval reduceSingleOr

-- *** Exception: ExitSuccess

reduceSingleOr :: TestTree
reduceSingleOr =
  chkFlatten
    "reduces single element Any to the element"
    MkFlattenCase
      { unflattened = Any (CSS "button" :| []),
        flattenned = CSS "button"
      }

applyDoubleNegation :: TestTree
applyDoubleNegation =
  chkFlatten
    "applies double negation: None [None [x]] -> x"
    MkFlattenCase
      { unflattened = None (None (CSS "button" :| []) :| []),
        flattenned = CSS "button"
      }

-- >>> _eval applyDeMorganAnd

-- *** Exception: ExitSuccess

applyDeMorganAnd :: TestTree
applyDeMorganAnd =
  chkFlatten
    "applies De Morgan: None [All [x, y]] -> Any [None [x], None [y]]"
    MkFlattenCase
      { unflattened = None (All (CSS "a" :| [CSS "b"]) :| []),
        flattenned = Any (None (CSS "a" :| []) :| [None (CSS "b" :| [])])
      }

applyDeMorganOr :: TestTree
applyDeMorganOr =
  chkFlatten
    "applies De Morgan: None [Any [x, y]] -> All [None [x], None [y]]"
    MkFlattenCase
      { unflattened = None (Any (CSS "a" :| [CSS "b"]) :| []),
        flattenned = All (None (CSS "a" :| []) :| [None (CSS "b" :| [])])
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
      { unflattened = Parent (All (CSS "a" :| [])) (Any (CSS "b" :| [])),
        flattenned = Parent (CSS "a") (CSS "b")
      }

complexNestedFlattening :: TestTree
complexNestedFlattening =
  chkFlatten
    "complex nested flattening"
    MkFlattenCase
      { unflattened = All (All (CSS "a" :| [All (CSS "b" :| [CSS "c"])]) :| [CSS "d"]),
        flattenned = All (CSS "a" :| [CSS "b", CSS "c", CSS "d"])
      }

-- | Shared nested locator used by fold traversal tests.
-- Tree shape (4 different constructors, 3 levels deep):
--
--   Parent
--   ├── All
--   │   ├── CSS "a"
--   │   └── XPath "//b"
--   └── None
--       └── Tag "div"
nestedLoc :: Locator
nestedLoc = Parent (All (CSS "a" :| [XPath "//b"])) (None (Tag "div" :| []))

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
            All (CSS "a" :| [XPath "//b"]),
            CSS "a",
            XPath "//b",
            None (Tag "div" :| []),
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
            All (CSS "a" :| [XPath "//b"]),
            Tag "div",
            None (Tag "div" :| []),
            nestedLoc
          ]

mockLocated :: Locator -> Bool
mockLocated = \case
  CSS "True" -> True
  Role (Just Button) (Just "False") -> False
  All locs -> all mockLocated locs
  Any locs -> any mockLocated locs
  None locs -> not (any mockLocated locs)
  Parent parent child -> mockLocated parent && mockLocated child
  _ -> error "Locator not Mocked"

-- | Falsify generator for Locator with depth and node count limits.
-- Only generates Parent, All, Any, None, and singletons (trueLoc, falseLoc).
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
        (perConstructorProb, genAllAt genSingleton (depth + 1) (remainingNodes - 1)),
        (perConstructorProb, genAnyAt genSingleton (depth + 1) (remainingNodes - 1)),
        (perConstructorProb + remainder, genNoneAt genSingleton (depth + 1) (remainingNodes - 1))
      ]

trueLoc :: Locator
trueLoc = css "True"

falseLoc :: Locator
falseLoc = button "False"

-- | Generate a singleton locator (80% trueLoc, 20% falseLoc)
genTrueFalseLoc :: Gen Locator
genTrueFalseLoc =
  frequency
    [ (80, pure trueLoc),
      (20, pure falseLoc)
    ]

allLeavesBool :: Bool -> [Locator]
allLeavesBool val =
  let b = pack $ show val
   in [ CSS b,
        XPath b,
        AllElms,
        ID b,
        Class
          { value = b,
            matchType = Full,
            caseSensitivity = CaseSensitive
          },
        Attribute
          { value = b,
            matchType = Full,
            caseSensitivity = CaseSensitive
          },
        Tag {value = b},
        Default {value = b},
        -- double shot / difficult
        Role {role = Just Button, name = Just b},
        InnerText
          { value = b,
            matchType = Full,
            caseSesnsitivity = CaseSensitive,
            maxDepth = Nothing
          },
        -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
        BiDiContext {context = MkBrowsingContext b},
        PostFilter $
          JSPostFilter
            { description = b,
              js = b
            }
      ]

genLocatorXPathOrInvalidHttp :: Gen Locator
genLocatorXPathOrInvalidHttp = genLocatorWithLimits genXPathOrInvalidHTTP 0 1000

genXPathOrInvalidHTTP :: Gen Locator
genXPathOrInvalidHTTP = uniformFrequency (xPathOnlyLocs <> invalidHTTPLocs)

uniformFrequency :: [a] -> Gen a
uniformFrequency = frequency . fmap (1,) . fmap pure

xPathOnlyLocs :: [Locator]
xPathOnlyLocs =
  [ XPath "//div",
    AllElms,
    ID "my-id",
    Class "my-class" Partial CaseSensitive,
    Attribute "data-test" Partial CaseInsensitive,
    Tag "button"
  ]

invalidHTTPLocs :: [Locator]
invalidHTTPLocs =
  [ Default "nested",
    BiDiContext (MkBrowsingContext "context-id")
  ]

-- | Generate a Parent locator at a given depth
genParentAt :: Gen Locator -> Int -> Int -> Gen Locator
genParentAt genSingleton depth remainingNodes = do
  -- Split remaining nodes between parent and child
  let halfNodes = remainingNodes `div` 2
  parent <- genLocatorWithLimits genSingleton depth halfNodes
  child <- genLocatorWithLimits genSingleton depth (remainingNodes - halfNodes - 1)
  pure $ Parent parent child

-- | Generate an All locator at a given depth
genAllAt :: Gen Locator -> Int -> Int -> Gen Locator
genAllAt genSingleton depth remainingNodes = do
  locs <- genNonEmptyLocators genSingleton depth remainingNodes
  pure $ All locs

-- | Generate an Any locator at a given depth
genAnyAt :: Gen Locator -> Int -> Int -> Gen Locator
genAnyAt genSingleton depth remainingNodes = do
  locs <- genNonEmptyLocators genSingleton depth remainingNodes
  pure $ Any locs

-- | Generate a None locator at a given depth
genNoneAt :: Gen Locator -> Int -> Int -> Gen Locator
genNoneAt genSingleton depth remainingNodes = do
  locs <- genNonEmptyLocators genSingleton depth remainingNodes
  pure $ None locs

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
      -- overrideVerbose = Just Verbose,
      overrideVerbose = Just NotVerbose,
      overrideMaxShrinks = Nothing,
      overrideNumTests = Just 1000,
      overrideMaxRatio = Nothing
    }

-- >>> _eval prop_flatenning_simplification

-- *** Exception: ExitSuccess

prop_flatenning_simplification :: TestTree
prop_flatenning_simplification = testPropertyWith genLocatorOptions "Flattening simplification" $ do
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
      None (x :| []) -> complexity x --- Singleton None leaf - complexity of child
      None locs -> plus2Map locs
      All locs -> plus2Map locs
      All (_ :| []) -> error "Singleton All should be flattenned"
      Any locs -> plus2Map locs
      Any (_ :| []) -> error "Singleton Any should be flattenned"
      Parent parent child -> 2 + complexity parent + complexity child
      _ -> error "Locator not Mocked"

    plus2Map :: NonEmpty Locator -> Int
    plus2Map locs = 2 + (sum $ complexity <$> locs)

-- Mock property test that generates locators and logs them

-- >>> _eval prop_mock_logic_preserved_on_flattenning

-- *** Exception: ExitSuccess

prop_mock_logic_preserved_on_flattenning :: TestTree
prop_mock_logic_preserved_on_flattenning = testPropertyWith genLocatorOptions "Generate and log locators" $ do
  loc <- gen genLocator
  F.assert $ expect True `dot` fn ("flattenLoc preserves mockLocated", \l -> mockLocated l == mockLocated (flattenLoc l)) .$ ("loc", loc)

-- >>> _eval prop_mock_logic_preserved_on_sort_and_grouping

-- *** Exception: ExitSuccess

prop_mock_logic_preserved_on_sort_and_grouping :: TestTree
prop_mock_logic_preserved_on_sort_and_grouping = testPropertyWith genLocatorOptions "Generate and log locators" $ do
  loc <- gen genLocator
  F.assert $ expect True `dot` fn ("flattenLoc preserves mockLocated", \l -> mockLocated l == mockLocated (sortGroupChildLocs l)) .$ ("loc", loc)

-- >>> _eval test_fail

-- *** Exception: ExitSuccess

test_nested_none_match :: TestTree
test_nested_none_match = testCase "This test fails" $ do
  let loc = None (None (None (falseLoc :| [trueLoc]) :| []) :| [])
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

-- >>> _eval prop_classify_xpath_only_is_xpath

-- *** Exception: ExitSuccess

-- | Property: a tree built exclusively from xPathOnlyLocs is always classified
-- as IsXPath under the HTTP protocol.
prop_classify_xpath_only_is_xpath :: TestTree
prop_classify_xpath_only_is_xpath =
  testPropertyWith genLocatorOptions "classify HTTP: xpath-only tree is always IsXPath" $ do
    loc <- gen $ genLocatorWithLimits (uniformFrequency xPathOnlyLocs) 0 1000
    let classification = classify (\t -> Default t) HTTP loc
    info $ "Locator:\n" <> unpack (txt loc)
    info $ "Classification: " <> show classification
    F.assert $ expect True .$ ("classification == IsXPath", classification == IsXPath)

-- >>> _eval prop_classify_invalid_iff_any_invalid_node

-- *** Exception: ExitSuccess

-- | Property: classify with HTTP protocol and (\t -> Default t) as the default
-- function returns Invalid if and only if the locator tree contains any
-- Default or BiDiContext node.
--
-- Rationale: with defLoc = \t -> Default t, every Default node resolves to
-- another Default (nested) which classify rejects as invalid. BiDiContext is
-- unconditionally invalid under HTTP. mergeClassification propagates Invalid
-- upward through any combinator, so the top-level result is Invalid iff any
-- leaf is invalid.
prop_classify_invalid_iff_any_invalid_node :: TestTree
prop_classify_invalid_iff_any_invalid_node =
  testPropertyWith genLocatorOptions "classify HTTP (\\t->Default t): Invalid iff tree has an invalid node" $ do
    loc <- gen genLocatorXPathOrInvalidHttp
    let hasInvalid = hasInvalidLoc (\t -> Default t) HTTP loc
        classification = classify (\t -> Default t) HTTP loc

    info $ "Locator:\n" <> unpack (txt loc)
    info $ "Has invalid node: " <> show hasInvalid
    info $ "Classification: " <> show classification
    F.assert $ expect True .$ ("hasInvalid == classedasInvalid classification", hasInvalid == classedasInvalid classification)
  where
    classedasInvalid :: Classification -> Bool
    classedasInvalid = \case
      Invalid _ -> True
      _ -> False
