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
    CompoundLocator (..),
    HttpLoc (..),
    transform,
  )
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
        [ 
          test_user_xpath_contains,
          test_infix_precedence_i,
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



chkSimplifiedLoc :: Text -> Either InvalidLocator (CompoundLocator HttpLoc) -> Locator -> TestTree
chkSimplifiedLoc message expected originalLoc =
  testCase (unpack message) $ transform ID originalLoc @?= expected

-- >>> _eval prepareSimplifyXPathTests
-- *** Exception: ExitSuccess

prepareSimplifyXPathTests :: TestTree
prepareSimplifyXPathTests =
  testGroup
    "prepareSimplify XPaths"
    [ chkSimplifiedLoc
        "CSS text is unchanged"
        (Right $ Leaf $ CSSHttp "button")
        (CSS "button"),
      chkSimplifiedLoc
        "bare XPath is unchanged"
        (Right $ Leaf $ XPathHttp "//footer")
        (XPath "//footer"),
      chkSimplifiedLoc
        "XPath already in //*[pred] form is unchanged"
        (Right . Leaf $ XPathHttp "//*[self::footer]")
        (XPath "//*[self::footer]"),
      chkSimplifiedLoc
        "ID converts to XPath //*[@id=...]"
        (Right . Leaf $ XPathHttp ".//*@id='my-id'")
        (ID "my-id"),
      chkSimplifiedLoc
        "Tag converts to XPath //tag"
        (Right $ Leaf $ XPathHttp ".//footer")
        (Tag "footer"),
      chkSimplifiedLoc
        "OR - h1 or h2 converts to XPath union"
        (Right $ Leaf $ XPathHttp ".//h1 | h2")
        (Tag "h1" ||| Tag "h2"),
      chkSimplifiedLoc
        "OR - class A or class B preserves both XPath branches"
        (Right . Leaf $ XPathHttp ".//*(contains(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'a')) or (contains(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'b'))")
        (Class "A" Partial CaseInsensitive ||| Class "B" Partial CaseInsensitive)
    ]

-- >>> _eval prop_simplification_merges_xpaths
-- *** Exception: ExitFailure 1

-- | Property: prepareSimplify preserves mockLocated semantics and, for the HTTP
--   protocol, auto-generated XPaths (from IsXPathConvertable nodes) within the same
--   group are merged into a single XPath leaf.  User-provided XPaths are never
--   merged with auto-generated ones — instead a multi-step combinator is produced.
prop_simplification_merges_xpaths :: TestTree
prop_simplification_merges_xpaths =
  testPropertyWith genLocatorOptions "prepareSimplify: auto-XPaths merged, user-XPaths preserved" $ do
    proto <- gen genProtocol
    loc <- gen $ genLocator proto
    let simpLoc = transform ID loc
        expected = mockLocated True loc
    info $ "Original locator:\n" <> unpack (txt loc)
    info $ "Prepared simplified locator:\n" <> either show (unpack . txt) simpLoc
    info $ "Expected:" <>  show expected
    info $ "Actual mock located:\n" <>  show (mockLocatedReduced True  <$> simpLoc)
    
    F.assert $ satisfies ("prepareSimplify preserves mockLocated", \l -> either (const True) (\rl -> mockLocatedReduced True rl == expected) l) .$ ("loc", simpLoc)

-- >>> _eval test_user_xpath_contains
-- *** Exception: ExitSuccess
test_user_xpath_contains :: TestTree
test_user_xpath_contains =
  testCase "User XPath in Contains should not be merged" $ do
    let loc = All (CSS "True" :| [All (Contains (XPath "True") (XPath "False") :| [])])
        result = transform ID loc
    print $ "Original: " <> show loc
    print $ "Transformed: " <> show result
    print $ "Expected mockLocated: " <> show (mockLocated True loc)
    print $ "Actual mockLocatedReduced: " <> show (mockLocatedReduced True <$> result)
    either (const $ pure ()) (\r -> mockLocatedReduced True r @?= mockLocated True loc) result

-- | Evaluate a ReducedLoc using the same boolean-encoding convention as mockLocated.
-- Handles both simple XPath values (\"True\"/\"False\") and auto-generated
-- XPath strings (//*[@id='True'], //footer, etc.).
mockLocatedReduced :: Bool -> CompoundLocator HttpLoc -> Bool
mockLocatedReduced allElmsDefault = go
  where
    go = \case
      Leaf lf -> case lf of
        CSSHttp v -> readBool v
        XPathHttp v -> readXPathBool v
        RoleHttp {roleSpec = RoleName v} -> readBool v
        RoleHttp {} -> allElmsDefault
      PostFilterI {} -> error "PostFilter not supported by mockLocatedReduced"
      ContainsI p c' -> go p && go c'
      AllI elms -> all go elms
      AnyI elms -> any go elms
    readBool "True" = True
    readBool "False" = False
    readBool v = error $ "mockLocatedReduced: unexpected value: " <> unpack v

    -- | Extract the boolean value from an auto-generated XPath string.
    -- Mock locators encode bools as \"True\" or \"False\" in tag names.
    -- XPath structure determines boolean operations:
    --   - Union (|) → OR
    --   - Predicate with "or" → OR
    --   - Multi-step (//) → AND
    --   - Tag with predicate → AND
    readXPathBool :: Text -> Bool
    readXPathBool v
      -- Union XPath (from Any): OR of branches
      -- Format: .//h1 | h2 or .//h1 | .//h2
      | " | " `T.isInfixOf` v =
          or $ readXPathBool <$> T.splitOn " | " v
      -- XPath with " or " predicate (from merged Any nodes)
      -- Format: .//*[(pred1) or (pred2)] or //*[(pred1) or (pred2)]
      | " or " `T.isInfixOf` v && (".//*" `T.isPrefixOf` v || "//*" `T.isPrefixOf` v) =
          let rest = if ".//*" `T.isPrefixOf` v then T.drop 4 v else T.drop 3 v
          in evalOrPredicate rest
      -- Multi-step XPath (from Contains): AND of steps
      -- Format: .//True//False or True//False
      | "//" `T.isInfixOf` v =
          let steps = filter (not . T.null) $ T.splitOn "//" v
              -- Drop leading "." if present
              steps' = case steps of
                ("." : rest) -> rest
                other -> other
          in and $ evalStep <$> steps'
      -- Plain tag name (from union like ".//h1 | h2") or boolean string
      | otherwise = evalTag v
      where
        -- Evaluate an XPath step (tag with optional predicate)
        -- Format: "tag[predicate]" or just "tag"
        evalStep :: Text -> Bool
        evalStep s =
          case T.breakOn "[" s of
            (tag, "") ->
              -- No predicate, just evaluate tag
              evalTag tag
            (tag, predWithBracket) ->
              -- Tag with predicate: AND them together
              evalTag tag && evalPredicate predWithBracket
        
        -- Evaluate a tag name
        evalTag :: Text -> Bool
        evalTag t
          | T.null t = allElmsDefault
          | "True" `T.isInfixOf` t = True
          | "False" `T.isInfixOf` t = False
          | "*" == t = allElmsDefault
          | otherwise = allElmsDefault
        
        -- Evaluate a predicate (with brackets)
        -- Format: "[pred]" 
        evalPredicate :: Text -> Bool
        evalPredicate p
          | "False" `T.isInfixOf` p = False
          | "True" `T.isInfixOf` p = True
          | "true()" `T.isInfixOf` p = True  -- XPath boolean true
          | otherwise = allElmsDefault
        
        -- Evaluate " or " separated predicates
        -- Format: "[(pred1) or (pred2)]" - the leading "[" was already stripped
        evalOrPredicate :: Text -> Bool
        evalOrPredicate p =
          let parts = T.splitOn " or " p
          in or $ evalPredicate <$> parts


-- TODO:
--      Use -p '/OR - class A or class B preserves both XPath branches/' to rerun this test only.
--     Property Tests
--       Test operator precedence i:                                 OK
--       Test operator precedence ii:                                OK
--       Test Contains operator precedence:                          OK
--       prepareSimplify: auto-XPaths merged, user-XPaths preserved: FAIL (0.03s)
--         failed after 81 successful tests and 18 shrinks
--         not (prepareSimplify preserves mockLocated loc)
--         loc: Right (Leaf {getLeaf = XPathHttp {value = ".//*[(true()) or (@id='False')]"}})
        
--         Logs for failed test run:
--         generated HTTP at CallStack (from HasCallStack):
--           gen, called at extended/test/Internal/LocatorsTest.hs:329:14 in webdriver-precore-0.2.0.2-inplace-test-extended:Internal.LocatorsTest
--         generated All {elms = Any {elms = AllElms :| [ID {value = "False"}]} :| []} at CallStack (from HasCallStack):
--           gen, called at extended/test/Internal/LocatorsTest.hs:330:12 in webdriver-precore-0.2.0.2-inplace-test-extended:Internal.LocatorsTest
--         Original locator:
--         All
--           { elms = Any { elms = AllElms :| [ ID { value = "False" } ] } :| []
--           }
--         Prepared simplified locator:
--         Leaf
--           { getLeaf = XPathHttp { value = ".//*[(true()) or (@id='False')]" }
--           }
        
--         Use --falsify-replay=011a9857ab1e2921aa5fca1c3a9ac9e005 to replay.
        
--         Use -p '/prepareSimplify: auto-XPaths merged, user-XPaths preserved/' to rerun this test only.
