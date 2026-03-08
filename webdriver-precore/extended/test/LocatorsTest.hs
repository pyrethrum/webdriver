module LocatorsTest (tests) where

import WebDriverPreCore.Extended.Locators
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text, unpack)
import Data.Text.IO (putStrLn)
import Prelude hiding (putStrLn)

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
        "logicalReduce"
        [ testCase "flattens nested MatchAll" $ do
            let input = MatchAll [MatchAll [CSS "a", CSS "b"], CSS "c"]
                expected = MatchAll [CSS "a", CSS "b", CSS "c"]
            flattenLoc input @?= expected,
          testCase "flattens nested MatchAny" $ do
            let input = MatchAny [MatchAny [CSS "a", CSS "b"], CSS "c"]
                expected = MatchAny [CSS "a", CSS "b", CSS "c"]
            flattenLoc input @?= expected,
          testCase "reduces single element MatchAll to the element" $ do
            let input = MatchAll [CSS "button"]
                expected = CSS "button"
            flattenLoc input @?= expected,
          testCase "reduces single element MatchAny to the element" $ do
            let input = MatchAny [CSS "button"]
                expected = CSS "button"
            flattenLoc input @?= expected,
          testCase "applies double negation: MatchNone [MatchNone [x]] -> x" $ do
            let input = MatchNone [MatchNone [CSS "button"]]
                expected = CSS "button"
            flattenLoc input @?= expected,
          testCase "applies De Morgan: MatchNone [MatchAll [x, y]] -> MatchAny [MatchNone [x], MatchNone [y]]" $ do
            let input = MatchNone [MatchAll [CSS "a", CSS "b"]]
                expected = MatchAny [MatchNone [CSS "a"], MatchNone [CSS "b"]]
            flattenLoc input @?= expected,
          testCase "applies De Morgan: MatchNone [MatchAny [x, y]] -> MatchAll [MatchNone [x], MatchNone [y]]" $ do
            let input = MatchNone [MatchAny [CSS "a", CSS "b"]]
                expected = MatchAll [MatchNone [CSS "a"], MatchNone [CSS "b"]]
            flattenLoc input @?= expected,
          testCase "preserves non-Match* locators" $ do
            let input = CSS "button"
                expected = CSS "button"
            flattenLoc input @?= expected,
          testCase "recursively reduces Parent locators" $ do
            let input = Parent (MatchAll [CSS "a"]) (MatchAny [CSS "b"])
                expected = Parent (CSS "a") (CSS "b")
            flattenLoc input @?= expected,
          testCase "complex nested flattening" $ do
            let input = MatchAll [MatchAll [CSS "a", MatchAll [CSS "b", CSS "c"]], CSS "d"]
                expected = MatchAll [CSS "a", CSS "b", CSS "c", CSS "d"]
            flattenLoc input @?= expected
        ]
    ]

logPretty :: Show a => a -> IO ()
logPretty = putStrLn . txt

http_login_navigation_demo :: IO ()
http_login_navigation_demo = do
  undefined
  where
    loginButton = button "Submit"
    navBar = navigation "Main Navigation"
    absurdLoc = notLoc (button "Submit" &&& navBar) ||| navBar



chkFlatten :: Text -> Locator -> Locator -> TestTree
chkFlatten description expected unflattened =
  testCase (unpack description) $ do
     
     expected @?= actual
  where 
     actual = flattenLoc unflattened


trueLoc :: Locator
trueLoc = css "NA"

falseLoc :: Locator
falseLoc = button "NA"

mockLocated :: Locator -> Bool
mockLocated = \case 
  CSS "NA" -> True
  Role (Just Button) (Just "NA") -> False
  MatchAll locs -> all mockLocated locs
  MatchAny locs -> any mockLocated locs 
  MatchNone locs -> not (any mockLocated locs)
  Parent parent child -> mockLocated parent && mockLocated child
  _ -> error "Locator not Mocked"
