module Internal.LocatorsTest (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack, unpack, replace)
import Data.Text qualified as T
import System.Environment (withArgs)
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Predicate (satisfies, (.$))
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), Verbose (..), gen, info, testPropertyWith)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertFailure)
import Utils (txt, db)
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
-- Terminates at max 10 layers or approx 1000 nodes
genBoolLocator :: Protocol -> Gen Locator
genBoolLocator proto = genLocatorWithLimits (leafLocBool proto 80) 0 1000

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
      overrideNumTests = Just 1_000,
      -- overrideNumTests = Just 100_000,
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

-- generates a mock Locator and checks that the transformed HTTPLoc produces the
-- same locate result as the as the inital Locator
prop_simplification_merges_xpaths :: TestTree
prop_simplification_merges_xpaths =
  testPropertyWith genLocatorOptions "prepareSimplify: auto-XPaths merged, user-XPaths preserved" $ do
    proto <- gen genProtocol
    loc <- gen $ genBoolLocator proto
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

-- | Evaluate a ReducedLoc assuming limited locators generated 
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
    readXPathBool xp = 
      tfToBool . reduceToTF {- . db "BEFORE REDUCE" . chkExpectedChrs -} $ initialSub xp
     where 
      initialSub = 
          replace "and" "&" 
          . replace "or" "|"
          . replace "True" "T" 
          . replace "False" "F" 
          . replace "true()" "T" 
          . replace "false()" "F"
          . replace "[" "("
          . replace "]" ")"
          . replace "//" "and"
          . deleteTxt " "
          . replace "True[" "True and["
          . replace "False[" "False and["
          . deleteAll [
            "'",
            ".//",
            ".//*",
            "@id=",
            "self::",
            "@data-attr=",
             -- role / text normalisationrelated guff
            "and normalize-space(.)=",
            "not(@hidden) and not(@aria-hidden='true') and not(contains(@style,'display:none')) and not(contains(@style,'visibility:hidden'))"
          ]
           . replace "contains(concat(' ', normalize-space(@class), ' '), ' True ')" "True"
           . replace "contains(concat(' ', normalize-space(@class), ' '), ' False ')" "False"
      
      reduceToTF xp' = 
        if replaced == xp' then
          replaced 
        else 
          reduceToTF replaced
       where 
        replaced =
          keepReplacing "T&T" "T" .
          keepReplacing "T&F" "F" .
          keepReplacing "F&T" "F" .
          keepReplacing "F&F" "F" .
          keepReplacing "T|T" "T" .
          keepReplacing "T|F" "T" .
          keepReplacing "F|T" "T" .
          keepReplacing "F|F" "F" .
          keepReplacing "(F)" "F" $
          keepReplacing "(T)" "T" xp'

      keepReplacing f t hs = 
          if replaced == hs then 
            replaced
          else 
            keepReplacing f t replaced
        where
          replaced = replace f t hs
      
      tfToBool = \case
        "T" -> True
        "F" -> False
        t -> 
          error $ 
            "Test error - readXPathBool: unexpected reduced XPath value (expected T or F): " 
            <> unpack t
            <> "\nInitial Reduced:\n"
            <> unpack (initialSub xp)
            <> "\nSource XPath:\n"
            <> unpack xp
      
      deleteTxt = flip replace ""

      deleteAll :: [Text] -> Text -> Text
      deleteAll dtxs xp' = foldr deleteTxt xp' dtxs 
      
      {-
      chkExpectedChrs t = 
        bool
          (error $ "Test error - readXPathBool: unexpected characters in reduced XPath value: " 
            <> unpack t
            <> "\nsimplified from original XPath: \n" 
            <> unpack xp)
          t
       $ T.all (`elem` ("TF()&|")) t
       -}

