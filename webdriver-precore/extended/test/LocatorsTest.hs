module LocatorsTest (tests) where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, unpack)
import Data.Text.IO (putStrLn)
import System.Environment (withArgs)
import Test.Tasty
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

chkFlatten :: Text -> Locator -> Locator -> TestTree
chkFlatten description expected unflattened =
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
    (MatchAll (CSS "a" :| [CSS "b", CSS "c"]))
    (MatchAll (MatchAll (CSS "a" :| [CSS "b"]) :| [CSS "c"]))

flattenNestedMatchAny :: TestTree
flattenNestedMatchAny =
  chkFlatten
    "flattens nested MatchAny"
    (MatchAny (CSS "a" :| [CSS "b", CSS "c"]))
    (MatchAny (MatchAny (CSS "a" :| [CSS "b"]) :| [CSS "c"]))

reduceSingleMatchAll :: TestTree
reduceSingleMatchAll =
  chkFlatten
    "reduces single element MatchAll to the element"
    (CSS "button")
    (MatchAll (CSS "button" :| []))

reduceSingleMatchAny :: TestTree
reduceSingleMatchAny =
  chkFlatten
    "reduces single element MatchAny to the element"
    (CSS "button")
    (MatchAny (CSS "button" :| []))

applyDoubleNegation :: TestTree
applyDoubleNegation =
  chkFlatten
    "applies double negation: MatchNone [MatchNone [x]] -> x"
    (CSS "button")
    (MatchNone (MatchNone (CSS "button" :| []) :| []))

applyDeMorganMatchAll :: TestTree
applyDeMorganMatchAll =
  chkFlatten
    "applies De Morgan: MatchNone [MatchAll [x, y]] -> MatchAny [MatchNone [x], MatchNone [y]]"
    (MatchAny (MatchNone (CSS "a" :| []) :| [MatchNone (CSS "b" :| [])]))
    (MatchNone (MatchAll (CSS "a" :| [CSS "b"]) :| []))

applyDeMorganMatchAny :: TestTree
applyDeMorganMatchAny =
  chkFlatten
    "applies De Morgan: MatchNone [MatchAny [x, y]] -> MatchAll [MatchNone [x], MatchNone [y]]"
    (MatchAll (MatchNone (CSS "a" :| []) :| [MatchNone (CSS "b" :| [])]))
    (MatchNone (MatchAny (CSS "a" :| [CSS "b"]) :| []))

preserveNonMatchLocators :: TestTree
preserveNonMatchLocators =
  chkFlatten
    "preserves non-Match* locators"
    (CSS "button")
    (CSS "button")

recursiveReduceParent :: TestTree
recursiveReduceParent =
  chkFlatten
    "recursively reduces Parent locators"
    (Parent (CSS "a") (CSS "b"))
    (Parent (MatchAll (CSS "a" :| [])) (MatchAny (CSS "b" :| [])))

complexNestedFlattening :: TestTree
complexNestedFlattening =
  chkFlatten
    "complex nested flattening"
    (MatchAll (CSS "a" :| [CSS "b", CSS "c", CSS "d"]))
    (MatchAll (MatchAll (CSS "a" :| [MatchAll (CSS "b" :| [CSS "c"])]) :| [CSS "d"]))

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



