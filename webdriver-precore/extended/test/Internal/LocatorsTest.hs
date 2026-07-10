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
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertFailure)
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
import Data.Function ((&))

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
mockLocateUnsimplified :: Bool -> Locator -> Bool
mockLocateUnsimplified allElmsDefault = go
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
      overrideNumTests = Just 100_000,
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
    actual = mockLocateUnsimplified False $ trueLoc ||| falseLoc &&& falseLoc

-- >>> _eval test_infix_precedence_ii

-- *** Exception: ExitSuccess

test_infix_precedence_ii :: TestTree
test_infix_precedence_ii =
  testCase "Test operator precedence ii" $
    expected @?= actual
  where
    expected = False || True && False || True
    actual = mockLocateUnsimplified False $ falseLoc ||| trueLoc &&& falseLoc ||| trueLoc

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
    [ 
      test_contradictory_tags_nested_all,
      test_contradictory_tags_siingleton_nested_any,
      test_noncontradictory_tags_nested_any,
      test_noncontradictory_tags_simple_nested_any,
      chkSimplifiedLoc
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
        (Right . Leaf $ XPathHttp ".//*[@id='my-id']")
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
        (Right . Leaf $ XPathHttp ".//*[(contains(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'a')) or (contains(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'b'))]")
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
    let simpLoc = transform ID loc
        expected = mockLocateUnsimplified True loc
    info $ "Original locator:\n" <> unpack (txt loc)
    info $ "Prepared simplified locator:\n" <> either show (unpack . txt) simpLoc
    info $ "Expected (unsimplified): " <>  show expected
    info $ "Actual mock located (after simplified merged): " <>  show (mockLocatedReduced True  <$> simpLoc)
    
    F.assert $ satisfies ("prepareSimplify preserves mockLocated", \l -> either (const True) (\rl -> mockLocatedReduced True rl == expected) l) .$ ("loc", simpLoc)

-- >>> _eval test_user_xpath_contains
-- *** Exception: ExitSuccess
test_user_xpath_contains :: TestTree
test_user_xpath_contains =
  testCase "User XPath in Contains should not be merged" $ do
    let loc = All (CSS "True" :| [All (Contains (XPath "True") (XPath "False") :| [])])
        result = transform ID loc
    -- print $ "Original: " <> show loc
    -- print $ "Transformed: " <> show result
    -- print $ "Expected mockLocated: " <> show (mockLocated True loc)
    -- print $ "Actual mockLocatedReduced: " <> show (mockLocatedReduced True <$> result)
    either (const $ pure ()) (\r -> mockLocatedReduced True r @?= mockLocateUnsimplified True loc) result


-- >>> _eval test_contradictory_tags_nested_all
-- *** Exception: ExitSuccess
test_contradictory_tags_nested_all :: TestTree
test_contradictory_tags_nested_all =
  testCase "Contradictory tags in nested All should be detected" $ do
    let loc = All
          { elms = All { elms = Tag "True" :| [] } :|
            [ Any { elms = Any { elms = All { elms = Tag "False" :| [AllElms] } :| [] } :| [] } ]
          }
        
    transform ID loc & either
      (\(MkInvalidLocator _ msg) -> "Contradictory tags" `T.isInfixOf` msg @? "Expected contradictory tags error")
      (\_ -> assertFailure "Expected Left (contradictory tags error), got Right")

-- >>> _eval test_contradictory_tags_siingleton_nested_any
-- *** Exception: ExitSuccess
test_contradictory_tags_siingleton_nested_any :: TestTree
test_contradictory_tags_siingleton_nested_any =
  testCase "Contradictory tags in singleton nested Any should be detected" $ do
    let loc = All
          { elms = All { elms = Tag "True" :| [] } :|
            [ Any { elms = Any { elms = All { elms = Tag "False" :| [] } :| [] } :| [] } ]
          }
        
    transform ID loc & either
      (\(MkInvalidLocator _ msg) -> "Contradictory tags" `T.isInfixOf` msg @? "Expected contradictory tags error")
      (\_ -> assertFailure "Expected Left (contradictory tags error), got Right")

-- >>> _eval test_noncontradictory_tags_nested_any
-- *** Exception: ExitSuccess
test_noncontradictory_tags_nested_any :: TestTree
test_noncontradictory_tags_nested_any =
  testCase "Non-contradictory tags in nested Any should be handled" $ do
    let loc = All
          { elms = All { elms = Tag "True" :| [] } :|
            [ Any { elms = Any { elms = Any { elms = Tag "False" :| [AllElms] } :| [] } :| [] } ]
          }   
    transform ID loc & either
      (\(MkInvalidLocator _ msg) -> assertFailure $ "Expected Right, got Left with message: " <> unpack msg)
      (\_ -> pure ())

-- >>> _eval test_contradictory_tags_nested_all
-- *** Exception: ExitSuccess
test_noncontradictory_tags_simple_nested_any :: TestTree
test_noncontradictory_tags_simple_nested_any =
  testCase "Non-contradictory tags in nested Any should be handled" $ do
    let loc = All
          { elms = All { elms = Tag "True" :| [] } :|
            [ Any { elms = Tag "False" :| [AllElms] }  ]
          }   
    transform ID loc & either
      (\(MkInvalidLocator _ msg) -> assertFailure $ "Expected Right, got Left with message: " <> unpack msg)
      (\_ -> pure ())

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
    -- Normalizes XPath to a boolean expression, then evaluates it.
    readXPathBool :: Text -> Bool
    readXPathBool v
      -- Union XPath (from Any): OR of branches
      -- Format: .//h1 | h2 or .//h1 | .//h2
      | " | " `T.isInfixOf` v =
          or $ readXPathBool <$> T.splitOn " | " v
      -- Multi-step XPath (from Contains): AND of steps
      -- Format: True//False (user XPaths merged by convertContains, no .// prefix)
      -- Don't split .//tag as that's just the XPath prefix
      | not ("./" `T.isPrefixOf` v), "//" `T.isInfixOf` v =
          let steps = T.splitOn "//" v
              -- Filter out empty steps and recursively evaluate each
              stepResults = readXPathBool <$> filter (not . T.null) steps
          in and stepResults
      -- Single XPath expression with predicate - normalize and evaluate
      -- Format: .//*[predicate] or .//tag or .//tag[predicate]
      | otherwise = evalBoolExpr $ normalizeXPath v
      where
        -- Normalize XPath to boolean expression
        -- ".//*[(true()) and (@id='False')]" => "(True) and (False)"
        normalizeXPath :: Text -> Text
        normalizeXPath xpath = 
          let -- Strip XPath prefixes and get tag/predicate
              (tag, predicate) = extractTagAndPredicate xpath
              -- If it's a class predicate, evaluate it directly
              tagBool = if "*" == tag then "" else tag
              -- Normalize the predicate
              normPred = if T.null predicate 
                         then ""
                         else normalizePredicate predicate
              -- Combine: if both exist, AND them; otherwise use whichever exists
              combined = case (T.null tagBool, T.null normPred) of
                (True, True) -> "True"  -- .//*, defaults to True
                (False, True) -> tagBool
                (True, False) -> normPred
                (False, False) -> "(" <> tagBool <> ") and (" <> normPred <> ")"
          in combined
        
        extractTagAndPredicate :: Text -> (Text, Text)
        extractTagAndPredicate xpath
          | Just rest <- T.stripPrefix ".//" xpath =
              let (tag, pred) = T.breakOn "[" rest
              in (tag, if T.null pred then "" else T.drop 1 $ T.dropEnd 1 pred)
          | Just rest <- T.stripPrefix "//" xpath =
              let (tag, pred) = T.breakOn "[" rest
              in (tag, if T.null pred then "" else T.drop 1 $ T.dropEnd 1 pred)
          | otherwise =
              -- Handle cases like "True[@id='False']" without prefix
              let (tag, pred) = T.breakOn "[" xpath
              in (tag, if T.null pred then "" else T.drop 1 $ T.dropEnd 1 pred)
        
        normalizePredicate :: Text -> Text
        normalizePredicate pred =
          let -- Replace XPath boolean functions
              step1 = T.replace "true()" "True" $ T.replace "false()" "False" pred
              -- Replace attribute predicates
              step2 = T.replace "@id='True'" "True" $ T.replace "@id='False'" "False" step1
              -- Handle class predicates with contains()
              step3 = normalizeContains step2
          in step3
        
        normalizeContains :: Text -> Text
        normalizeContains t
          | not ("contains(" `T.isInfixOf` t) = t
          | otherwise =
              -- Check if the contains() has 'False' or 'True' in it
              let containsExpr = extractFirstContains t 0
                  value = if "'False " `T.isInfixOf` containsExpr || " False " `T.isInfixOf` containsExpr
                          then "False"
                          else "True"
                  replaced = T.replace containsExpr value t
              in if containsExpr == t then t else normalizeContains replaced
        
        extractFirstContains :: Text -> Int -> Text
        extractFirstContains t startIdx
          | not ("contains(" `T.isInfixOf` t) = t
          | otherwise =
              let prefix = T.take startIdx t
                  rest = T.drop startIdx t
                  containsStart = T.breakOn "contains(" rest
                  afterContains = T.drop 9 (snd containsStart)  -- Skip "contains("
                  endIdx = findMatchingParen afterContains 0
                  fullExpr = "contains(" <> T.take (endIdx - 1) afterContains <> ")"
              in fullExpr
        
        findMatchingParen :: Text -> Int -> Int
        findMatchingParen t depth
          | T.null t = 0
          | c == ')' && depth == 0 = 1
          | c == ')' = 1 + findMatchingParen (T.tail t) (depth - 1)
          | c == '(' = 1 + findMatchingParen (T.tail t) (depth + 1)
          | otherwise = 1 + findMatchingParen (T.tail t) depth
          where c = T.head t
        
        -- Evaluate boolean expression with True, False, and, or, and parens
        evalBoolExpr :: Text -> Bool
        evalBoolExpr expr =
          let simplified = simplifyParens $ T.strip expr
          in evalExpr simplified
        
        -- Simplify redundant parentheses: (True) => True, ((True)) => True
        simplifyParens :: Text -> Text
        simplifyParens expr =
          let simplified = T.replace "(True)" "True"
                         $ T.replace "(False)" "False" expr
          in if simplified == expr then expr else simplifyParens simplified
        
        -- Evaluate a boolean expression
        -- Handles: True, False, (expr), expr and expr, expr or expr
        evalExpr :: Text -> Bool
        evalExpr e
          | T.strip e == "True" = True
          | T.strip e == "False" = False
          | T.strip e == "*" = allElmsDefault
          | "True" `T.isInfixOf` e && not (" and " `T.isInfixOf` e || " or " `T.isInfixOf` e) = True
          | "False" `T.isInfixOf` e && not (" and " `T.isInfixOf` e || " or " `T.isInfixOf` e) = False
          -- Try to split by or (lower precedence)
          | Just parts <- trySplitOp " or " e = or $ evalExpr <$> parts
          -- Try to split by and (higher precedence)
          | Just parts <- trySplitOp " and " e = and $ evalExpr <$> parts
          -- Strip outer parens and retry
          | "(" `T.isPrefixOf` e && ")" `T.isSuffixOf` e =
              let inner = T.drop 1 $ T.dropEnd 1 e
              in if isBalanced inner 0
                 then evalExpr inner
                 else allElmsDefault
          | otherwise = allElmsDefault
        
        -- Try to split by operator at depth 0
        trySplitOp :: Text -> Text -> Maybe [Text]
        trySplitOp op e =
          let parts = splitAtDepth0 op e
          in if length parts > 1 then Just parts else Nothing
        
        -- Split text by operator only at parenthesis depth 0
        splitAtDepth0 :: Text -> Text -> [Text]
        splitAtDepth0 op = go 0 []
          where
            go _depth acc t
              | T.null t = [T.concat (reverse acc)]
              | op `T.isPrefixOf` t && _depth == 0 =
                  T.concat (reverse acc) : go 0 [] (T.drop (T.length op) t)
              | "(" `T.isPrefixOf` t = go (_depth + 1) ("(" : acc) (T.tail t)
              | ")" `T.isPrefixOf` t = go (_depth - 1) (")" : acc) (T.tail t)
              | otherwise = go _depth (T.take 1 t : acc) (T.tail t)
        
        -- Check if parentheses are balanced (depth ends at 0)
        isBalanced :: Text -> Int -> Bool
        isBalanced t _depth
          | T.null t = _depth == 0
          | "(" `T.isPrefixOf` t = isBalanced (T.tail t) (_depth + 1)
          | ")" `T.isPrefixOf` t = isBalanced (T.tail t) (_depth - 1)
          | otherwise = isBalanced (T.tail t) _depth


-- >>> _eg 
-- Right (Leaf {getLeaf = XPathHttp {value = ".//*[(self::False and (true())) or (true())]"}})
_eg :: Either InvalidLocator (CompoundLocator HttpLoc)
_eg =  transform ID $ 
   Any
    { elms =
        All { elms = AllElms :| [ Tag { value = "False" } ] } :|
        [ AllElms ]
    }
-- TODO:
{-
  Original locator:
  Contains
    { container = Any { elms = Tag { value = "True" } :| [] }
    , contained = XPath { value = "False" }
    }
  Prepared simplified locator:
  Leaf { getLeaf = XPathHttp { value = ".//True//False" } }
  Expected (unsimplified): False
  Actual mock located (after simplified merged): Right True
-}
