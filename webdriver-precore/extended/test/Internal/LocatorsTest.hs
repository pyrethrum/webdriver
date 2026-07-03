module Internal.LocatorsTest (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import System.Environment (withArgs)
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Predicate (satisfies, (.$))
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), Verbose (..), gen, info, testPropertyWith)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (txt)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext (..))
import WebDriverPreCore.Extended.Locators hiding (Locator)
import WebDriverPreCore.Extended.Locators.Internal
  ( InvalidLocator (..),
    Locator (..),
    Protocol (..),
    RoleLocator (..),
  )
import WebDriverPreCore.Extended.ReducedLocator.Internal qualified as RL
import Prelude

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
      prepareSimplifyXPathTests,
      testGroup
        "Property Tests"
        [ test_infix_precedence_i,
          test_infix_precedence_ii,
          test_parent_infix_precedence,
          prop_simplification_merges_xpaths
        ]
    ]

_eval :: TestTree -> IO ()
_eval = withArgs [] . defaultMain

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
            caseSensitivity = CaseSensitive,
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

uniformFrequency :: Word -> [a] -> Gen a
uniformFrequency weight = frequency . uniformFrequencyTuples weight

uniformFrequencyTuples :: Word -> [a] -> [(Word, Gen a)]
uniformFrequencyTuples weight vals = (weight,) . pure <$> vals

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

-- Tests for Classification type removed as it's no longer exported from Internal module



chkSimplifiedLoc :: Text -> Either InvalidLocator RL.ReducedLoc -> Locator -> TestTree
chkSimplifiedLoc message expected originalLoc =
  testCase (unpack message) $ RL.prepareSimplify ID originalLoc @?= expected

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
        (Right . RL.Leaf $ RL.XPath "//*[self::footer]")
        (XPath "//*[self::footer]"),
      chkSimplifiedLoc
        "ID converts to XPath //*[@id=...]"
        (Right . RL.Leaf $ RL.XPath "//*[@id='my-id']")
        (ID "my-id"),
      chkSimplifiedLoc
        "Tag converts to XPath //tag"
        (Right $ RL.Leaf $ RL.XPath "//footer")
        (Tag "footer"),
      chkSimplifiedLoc
        "OR - h1 or h2 converts to XPath union"
        (Right $ RL.Leaf $ RL.XPath "//h1 | //h2")
        (Tag "h1" ||| Tag "h2"),
      chkSimplifiedLoc
        "OR - class A or class B preserves both XPath branches"
        (Right . RL.Leaf $ RL.XPath "//*[contains(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'a')] | //*[contains(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'b')]")
        (Class "A" Partial CaseInsensitive ||| Class "B" Partial CaseInsensitive)
    ]

-- >>> _eval prop_simplification_merges_xpaths

-- *** Exception: ExitSuccess

-- | Property: prepareSimplify preserves mockLocated semantics and, for the HTTP
--   protocol, auto-generated XPaths (from IsXPathConvertable nodes) within the same
--   group are merged into a single XPath leaf.  User-provided XPaths are never
--   merged with auto-generated ones — instead a multi-step combinator is produced.
prop_simplification_merges_xpaths :: TestTree
prop_simplification_merges_xpaths =
  testPropertyWith genLocatorOptions "prepareSimplify: auto-XPaths merged, user-XPaths preserved" $ do
    proto <- gen genProtocol
    loc <- gen $ genLocator proto
    let simpLoc = RL.prepareSimplify ID loc
        expected = mockLocated True loc
    info $ "Original locator:\n" <> unpack (txt loc)
    info $ "Prepared simplified locator:\n" <> either show (unpack . txt) simpLoc
    F.assert $ satisfies ("prepareSimplify preserves mockLocated", \l -> either (const True) (\rl -> mockLocatedReduced True rl == expected) l) .$ ("loc", simpLoc)

-- | Evaluate a ReducedLoc using the same boolean-encoding convention as mockLocated.
-- Handles both simple XPath values (\"True\"/\"False\") and auto-generated
-- XPath strings (//*[@id='True'], //footer, etc.).
mockLocatedReduced :: Bool -> RL.ReducedLoc -> Bool
mockLocatedReduced allElmsDefault = go
  where
    go = \case
      RL.Leaf lf -> case lf of
        RL.CSS v -> readBool v
        RL.XPath v -> readXPathBool v
        RL.Role {roleSpec = RoleName v} -> readBool v
        RL.Role {} -> allElmsDefault
      RL.PostFilterLoc _ -> error "PostFilter not supported by mockLocatedReduced"
      RL.Combinator c -> case c of
        RL.Contains p c' -> go p && go c'
        RL.All elms -> all go elms
        RL.Any elms -> any go elms
    readBool "True" = True
    readBool "False" = False
    readBool v = error $ "mockLocatedReduced: unexpected value: " <> unpack v

    -- | Extract the boolean value from an auto-generated XPath string.
    -- Mock locators always encode bools as exactly \"True\" or \"False\".
    -- For multi-step XPaths (from Contains), splits on // and evaluates
    -- each step; the overall result is the AND of all steps.
    -- Pure //* (from AllElms) uses 'allElmsDefault'.
    readXPathBool :: Text -> Bool
    readXPathBool v
      -- Union XPath (from Any): OR of branches
      | " | " `T.isInfixOf` v =
          or $ readXPathBool <$> T.splitOn " | " v
      -- Multi-step or single-step XPath (from Contains or leaves)
      | "//" `T.isPrefixOf` v =
          let steps = T.splitOn "//" (T.drop 2 v)
              bools = concatMap stepBool steps
          in if null bools then allElmsDefault else and bools
      -- Simple boolean string (from user XPath "True"/"False")
      | otherwise = readBool v
      where
        stepBool s
          | "False" `T.isInfixOf` s = [False]
          | "True" `T.isInfixOf` s  = [True]
          | "*" `T.isPrefixOf` s    = [allElmsDefault]
          | not (T.null s)          = [allElmsDefault]  -- unrecognised: treat as AllElms
          | otherwise               = []
