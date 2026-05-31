module Internal.LocatorsTest (tests) where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty (..), filter)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn)
import System.Environment (withArgs)
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Predicate (expect, satisfies, (.$))
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), Verbose (..), gen, info, testPropertyWith)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext (..))
import WebDriverPreCore.Extended.Locators hiding (Locator)
import WebDriverPreCore.Extended.Locators.Internal
  ( CaseSensitivity (..),
    Classification (..),
    Locator (..),
    Protocol (..),
    RoleLocator (..),
    classify,
    flattenLoc,
    foldLoc,
    foldLocBottomUp,
    hasInvalidLoc,
    prepare,
    sortGroupChildLocs, InvalidLocator,
  )
import WebDriverPreCore.Extended.ReducedLocator.Internal qualified as RL
import Prelude hiding (filter, head, putStrLn)

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
          preserveNonMatchLocators,
          recursiveReduceParent,
          complexNestedFlattening
        ],
      testGroup
        "foldLoc traversal order"
        [ foldLocTopDown,
          foldLocBottomUpTest
        ],
      prepareSimplifyXPathTests,
      testGroup
        "Property Tests"
        [ test_infix_precedence_i,
          test_infix_precedence_ii,
          test_parent_infix_precedence,
          prop_flatenning_simplification,
          prop_classify_xpath_only_is_xpath,
          prop_classify_invalid_iff_any_invalid_node,
          prop_mock_logic_preserved_on_flattenning,
          prop_mock_logic_preserved_on_sort_and_grouping,
          prop_prepare_logic_preserved,
          prop_simplification_merges_xpaths
        ]
    ]

logPretty :: (Show a) => a -> IO ()
logPretty = putStrLn . txt

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
      { unflattened = Contains (All (CSS "a" :| [])) (Any (CSS "b" :| [])),
        flattenned = Contains (CSS "a") (CSS "b")
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
-- Tree shape (3 different constructors, 3 levels deep):
--
--   Parent
--   ├── All
--   │   ├── CSS "a"
--   │   └── XPath "//b"
--   └── CSS "div"
nestedLoc :: Locator
nestedLoc = Contains (All (CSS "a" :| [XPath "//b"])) (CSS "div")

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
            CSS "div"
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
            CSS "div",
            nestedLoc
          ]

-- | Interpret a Locator as a Bool for property testing.
-- Leaf locators encode their boolean value as the text \"True\" or \"False\" in their
-- primary text field. 'AllElms' and 'Role' with no name use @allElmsDefault@.
-- Combinators recurse with standard boolean semantics.
mockLocated :: Bool -> Locator -> Bool
mockLocated allElmsDefault = go
  where
    go = \case
      CSS v -> readBool v
      XPath v -> readBool v
      AllElms -> allElmsDefault
      ID v -> readBool v
      Class {value = v} -> readBool v
      Attribute {value = v} -> readBool v
      Tag {value = v} -> readBool v
      Default {value = v} -> readBool v
      Role (RoleName v) -> readBool v
      Role _ -> allElmsDefault
      InnerText {value = v} -> readBool v
      BiDiContext {context = MkBrowsingContext v} -> readBool v
      All locs -> all go locs
      Any locs -> any go locs
      Contains p c -> go p && go c
      PostFilter _ _ -> error "Locator not supported by mockLocated"
    readBool "True" = True
    readBool "False" = False
    readBool v = error $ "mockLocated: unexpected value: " <> unpack v

-- | Falsify generator for Locator with depth and node count limits.
-- Only generates Parent, All, Any, and singletons (trueLoc, falseLoc).
-- Layers 0-1: Equal probability for all constructors (20% each)
-- Singleton selection: 80% trueLoc, 20% falseLoc
-- After layer 1: Increase singleton probability by 5% per layer
-- Terminates at max 10 layers or approximately 1000 nodes
genLocator :: Protocol -> Gen Locator
genLocator proto = genLocatorWithLimits (leafLocBool proto 80) 0 1000

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

    -- Remaining probability distributed evenly among 3 constructors
    nonSingletonProb = 100 - singletonProb
    perConstructorProb = nonSingletonProb `div` 3

    -- Small adjustment for rounding
    remainder = nonSingletonProb `mod` 3

    weights =
      [ (singletonProb, genSingleton),
        (perConstructorProb, genParentAt genSingleton (depth + 1) (remainingNodes - 1)),
        (perConstructorProb, genAllAt genSingleton (depth + 1) (remainingNodes - 1)),
        (perConstructorProb + remainder, genAnyAt genSingleton (depth + 1) (remainingNodes - 1))
      ]

trueLoc :: Locator
trueLoc = css "True"

falseLoc :: Locator
falseLoc = button "False"

httpLeavesBool :: Bool -> [Locator]
httpLeavesBool val =
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
          { name = "data-attr",
            value = b,
            matchType = Full,
            caseSensitivity = CaseSensitive
          },
        Tag {value = b},
        Default {value = b},
        -- double shot / difficult
        Role $ RoleName b,
        InnerText
          { value = b,
            matchType = Full,
            caseSesnsitivity = CaseSensitive,
            maxDepth = Nothing
          }
          -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
          -- PostFilter
      ]

bidiOnlyLeavesBool :: Bool -> [Locator]
bidiOnlyLeavesBool val =
  let b = pack $ show val
   in [ -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
        BiDiContext {context = MkBrowsingContext b}
      ]

allLeavesBool :: Bool -> [Locator]
allLeavesBool val = httpLeavesBool val <> bidiOnlyLeavesBool val

-- | Generate a leaf locator where the text value encodes the boolean result.
-- With probability @percentTrue/100@, picks uniformly from all True-valued
-- leaves (from 'allLeavesBool'); otherwise picks from all False-valued leaves.
leafLocBool :: Protocol -> Word -> Gen Locator
leafLocBool proto percentTrue =
  frequency
    [ (percentTrue, dist True),
      (100 - percentTrue, dist False)
    ]
  where
    dist b = case proto of
      HTTP -> frequency (uniformFrequencyTuples 10 (httpLeavesBool b) <> uniformFrequencyTuples 1 (bidiOnlyLeavesBool b))
      BiDi -> uniformFrequency 1 $ allLeavesBool b

genLocatorXPathOrInvalidHttp :: Gen Locator
genLocatorXPathOrInvalidHttp = genLocatorWithLimits genXPathOrInvalidHTTP 0 1000

genXPathOrInvalidHTTP :: Gen Locator
genXPathOrInvalidHTTP = uniformFrequency 1 (xPathOnlyLocs <> invalidHTTPLocs)

uniformFrequency :: Word -> [a] -> Gen a
uniformFrequency weight = frequency . uniformFrequencyTuples weight

uniformFrequencyTuples :: Word -> [a] -> [(Word, Gen a)]
uniformFrequencyTuples weight vals = (weight,) . pure <$> vals

xPathOnlyLocs :: [Locator]
xPathOnlyLocs =
  [ XPath "//div",
    AllElms,
    ID "my-id",
    Class "my-class" Partial CaseSensitive,
    Attribute "data-test" "my-value" Partial CaseInsensitive,
    Tag "button"
  ]

invalidHTTPLocs :: [Locator]
invalidHTTPLocs =
  [ Default "nested",
    BiDiContext (MkBrowsingContext "context-id")
  ]

-- | Generate a Contains locator at a given depth
genParentAt :: Gen Locator -> Int -> Int -> Gen Locator
genParentAt genSingleton depth remainingNodes = do
  -- Split remaining nodes between container and contained
  let halfNodes = remainingNodes `div` 2
  container <- genLocatorWithLimits genSingleton depth halfNodes
  contained <- genLocatorWithLimits genSingleton depth (remainingNodes - halfNodes - 1)
  pure $ Contains container contained

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
  p <- gen genProtocol
  loc <- gen $ genLocator p
  let unflattenedComplexity = complexity loc
      flatloc = flattenLoc loc
      flattenedComplexity = complexity flatloc
  info $ "Unflattened complexity: " <> show unflattenedComplexity
  info $ "Original locator:\n" <> unpack (txt loc)
  info $ "Flattened complexity: " <> show flattenedComplexity
  info $ "Flattened locator:\n" <> unpack (txt flatloc)
  F.assert $ satisfies ("flattenLoc simplifies or maintains complexity", \_l -> complexity flatloc <= complexity loc) .$ ("loc", loc)
  where
    -- Calculate complexity score: leaf = 1, combinator wrapper = 2 + children
    complexity :: Locator -> Int
    complexity = \case
      All locs -> plus2Map locs
      Any locs -> plus2Map locs
      Contains container contained -> 2 + complexity container + complexity contained
      _ -> 1 -- all leaf nodes have complexity 1
    plus2Map :: NonEmpty Locator -> Int
    plus2Map locs = 2 + (sum $ complexity <$> locs)

-- Mock property test that generates locators and logs them

-- >>> _eval prop_mock_logic_preserved_on_flattenning

-- *** Exception: ExitSuccess

-- *** Exception: ExitSuccess

prop_mock_logic_preserved_on_flattenning :: TestTree
prop_mock_logic_preserved_on_flattenning = testPropertyWith genLocatorOptions "Generate and log locators" $ do
  p <- gen genProtocol
  loc <- gen $ genLocator p
  F.assert $ satisfies ("flattenLoc preserves mockLocated", \l -> mockLocated False l == mockLocated False (flattenLoc l)) .$ ("loc", loc)

-- >>> _eval prop_mock_logic_preserved_on_sort_and_grouping

-- *** Exception: ExitSuccess

prop_mock_logic_preserved_on_sort_and_grouping :: TestTree
prop_mock_logic_preserved_on_sort_and_grouping = testPropertyWith genLocatorOptions "Generate and log locators" $ do
  p <- gen genProtocol
  loc <- gen $ genLocator p
  let grouped = sortGroupChildLocs (\t -> Default t) HTTP loc
  info $ "Original locator:\n" <> unpack (txt loc)
  info $ "Grouped locator:\n" <> unpack (txt grouped)
  F.assert $ satisfies ("group sort preserves mockLocated", \l -> mockLocated False l == mockLocated False grouped) .$ ("loc", loc)

-- >>> _eval prop_prepare_logic_preserved

-- *** Exception: ExitSuccess

prop_prepare_logic_preserved :: TestTree
prop_prepare_logic_preserved = testPropertyWith genLocatorOptions "prepare with ID default preserves mockLocated" $ do
  proto <- gen genProtocol
  loc <- gen $ genLocator proto
  let result = prepare ID proto loc
  info $ "Original locator:\n" <> unpack (txt loc)
  info $ "Protocol: " <> show proto
  info $ "Prepared: " <> either show (unpack . txt) result
  F.assert $ satisfies ("prepare preserves mockLocated when valid", \l -> either (const True) (\prepared -> mockLocated False l == mockLocated False prepared) (prepare ID proto l)) .$ ("loc", loc)

genProtocol :: Gen Protocol
genProtocol = uniformFrequency 1 [HTTP, BiDi]

-- >>> _eval test_infix_precedence

-- *** Exception: ExitSuccess

test_infix_precedence_i :: TestTree
test_infix_precedence_i =
  testCase "Test operator precedence i" $
    expected @?= actual
  where
    expected = True || False && False
    actual = mockLocated False $ trueLoc ||| falseLoc &&& falseLoc

-- >>> _eval test_infix_precedence_ii

-- *** Exception: ExitSuccess

test_infix_precedence_ii :: TestTree
test_infix_precedence_ii =
  testCase "Test operator precedence ii" $
    expected @?= actual
  where
    expected = False || True && False || True
    actual = mockLocated False $ falseLoc ||| trueLoc &&& falseLoc ||| trueLoc

-- >>> _eval test_parent_infix_precedence

-- *** Exception: ExitSuccess

test_parent_infix_precedence :: TestTree
test_parent_infix_precedence =
  testCase "Test Contains operator precedence" $
    expected @?= actual
  where
    expected = Contains (falseLoc ||| trueLoc) (trueLoc &&& falseLoc)
    actual = falseLoc ||| trueLoc >>> trueLoc &&& falseLoc

-- >>> _eval prop_classify_xpath_only_is_xpath

-- *** Exception: ExitSuccess

-- | Property: a tree built exclusively from xPathOnlyLocs is always classified
-- as IsXPath under the HTTP protocol.
prop_classify_xpath_only_is_xpath :: TestTree
prop_classify_xpath_only_is_xpath =
  testPropertyWith genLocatorOptions "classify HTTP: xpath-only tree is always IsXPath" $ do
    loc <- gen $ genLocatorWithLimits (uniformFrequency 1 xPathOnlyLocs) 0 1000
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



chkSimplifiedLoc :: Text -> Either InvalidLocator RL.ReducedLoc -> Locator -> TestTree
chkSimplifiedLoc message expected originalLoc =
  testCase (unpack message) $ RL.prepareSimplify ID HTTP originalLoc @?= expected

-- >>> _eval prepareSimplifyXPathTests

-- *** Exception: ExitSuccess

prepareSimplifyXPathTests :: TestTree
prepareSimplifyXPathTests =
  testGroup
    "prepareSimplify XPaths"
    [ chkSimplifiedLoc
        "CSS text is unchanged"
        (Right $ RL.Leaf $ RL.CSS "button")
        (CSS "button"),
      chkSimplifiedLoc
        "bare XPath is unchanged"
        (Right $ RL.Leaf $ RL.XPath "//footer")
        (XPath "//footer"),
      chkSimplifiedLoc
        "XPath already in //*[pred] form is unchanged"
        (Right $ RL.Leaf $ RL.XPath "//*[self::footer]")
        (XPath "//*[self::footer]"),
      chkSimplifiedLoc
        "ID converts to XPath //*[@id=...]"
        (Right $ RL.Leaf $ RL.XPath "//*[@id='my-id']")
        (ID "my-id"),
      chkSimplifiedLoc
        "Tag converts to XPath //tag"
        (Right $ RL.Leaf $ RL.XPath "//footer")
        (Tag "footer")
    ]

-- >>> _eval prop_simplification_merges_xpaths

-- *** Exception: ExitSuccess

prop_simplification_merges_xpaths :: TestTree
prop_simplification_merges_xpaths =
  testPropertyWith genLocatorOptions "simplified Locs should merge adjacent XPaths" $ do
    proto <- gen genProtocol
    loc <- gen $ genLocator proto
    let simpLoc = RL.prepareSimplify ID proto loc
    info $ "Original locator:\n" <> unpack (txt loc)
    info $ "prepared locator:\n" <> either show (unpack . txt) (prepare ID proto loc)
    info $ "Prepared simplified locator:\n" <> either show (unpack . txt) simpLoc
    F.assert $ satisfies ("prepareSimplify preserves mockLocated", either (const True) chkAllXPathsingleton) .$ ("loc", simpLoc)
  where
    chkAllXPathsingleton = \case
      RL.Combintor c -> case c of
        RL.Contains {} -> True
        RL.All {elms} -> chkSublocs elms
        RL.Any {elms} -> chkSublocs elms
      _ -> True

    chkSublocs l =
      chkListXPathSingleton l
        && all chkAllXPathsingleton l

    chkListXPathSingleton :: NonEmpty RL.ReducedLoc -> Bool
    chkListXPathSingleton l = not $ (length (filter RL.isXPath l)) > 1
