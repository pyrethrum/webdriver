module HTTP.MatchTypeTest where

import Common.Runner (testUrl)
import Common.Utils
  ( chkCount,
    chkElms,
    chkElmsWithAutoId,
    chkSingleton,
    defOpts,
    locateAllHttp,
  )
import Data.Text (Text, unpack)
import Effectful
import HTTP.Runner (BaseHTTPEffs, WDSession, closeWDSession, getWDSession, runHttp, runHttpTest)
import Test.Tasty (TestTree, defaultMain, inOrderTestGroup, testGroup, withResource)
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions (navigateTo)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locators.Internal (CaseSensitivity (..))
import WebDriverPreCore.Test.TestData (fileUrl)
import Prelude
import System.Environment (withArgs)


tests :: TestTree
tests =
  withResource getWDSession closeWDSession runSessionTests
  where
    runSessionTests :: IO WDSession -> TestTree
    runSessionTests ses =
      inOrderTestGroup
        "MatchType and CaseSensitivity Tests"
        [ withResource (navToUrl ses matchTypeUrl) (\_ -> pure ()) $ \_ ->
            testGroup
              "Class Locator Tests"
              [ testGroup
                  "Exact Match - Full MatchType"
                  [ chkAutoId "Full CaseSensitive matches lowercase exactly" (elmClass' Full CaseSensitive "testclass") "cls-exact-lower",
                    chkAutoId "Full CaseSensitive matches uppercase exactly" (elmClass' Full CaseSensitive "TESTCLASS") "cls-exact-upper",
                    chkAutoId "Full CaseSensitive matches mixed case exactly" (elmClass' Full CaseSensitive "TestClass") "cls-exact-mixed",
                    test "Full CaseInsensitive finds all exact matches regardless of case" $ do
                      results <- locateAll $ elmClass' Full CaseInsensitive "testclass"
                      chkElms "Should find 3 exact matches (lower, upper, mixed)" (chkCount 3) results,
                    test "Full CaseSensitive lowercase does not match uppercase" $ do
                      results <- locateAll $ elmClass' Full CaseSensitive "testclass"
                      chkElms "Should only find lowercase match" chkSingleton results,
                    test "Full CaseSensitive uppercase does not match lowercase" $ do
                      results <- locateAll $ elmClass' Full CaseSensitive "TESTCLASS"
                      chkElms "Should only find uppercase match" chkSingleton results
                  ],
                testGroup
                  "Starts Match - Starts MatchType"
                  [ chkAutoId "Starts CaseSensitive matches lowercase prefix" (elmClass' Starts CaseSensitive "swtestclass") "cls-starts-lower",
                    chkAutoId "Starts CaseSensitive matches uppercase prefix" (elmClass' Starts CaseSensitive "SWTESTCLASS") "cls-starts-upper",
                    chkAutoId "Starts CaseSensitive matches mixed case prefix" (elmClass' Starts CaseSensitive "SwTestClass") "cls-starts-mixed",
                    test "Starts CaseInsensitive finds all starts matches regardless of case" $ do
                      results <- locateAll $ elmClass' Starts CaseInsensitive "swtestclass"
                      chkElms "Should find 3 starts matches (lower, upper, mixed)" (chkCount 3) results,
                    test "Starts CaseSensitive lowercase does not match uppercase prefix" $ do
                      results <- locateAll $ elmClass' Starts CaseSensitive "swtestclass"
                      chkElms "Should only find lowercase prefix match" chkSingleton results
                  ],
                testGroup
                  "Contains Match - Partial MatchType"
                  [ chkAutoId "Partial CaseSensitive matches lowercase substring" (elmClass' Partial CaseSensitive "ptestclass") "cls-contains-lower",
                    chkAutoId "Partial CaseSensitive matches uppercase substring" (elmClass' Partial CaseSensitive "PTESTCLASS") "cls-contains-upper",
                    chkAutoId "Partial CaseSensitive matches mixed case substring" (elmClass' Partial CaseSensitive "PTestClass") "cls-contains-mixed",
                    test "Partial CaseInsensitive finds all substring matches regardless of case" $ do
                      results <- locateAll $ elmClass' Partial CaseInsensitive "ptestclass"
                      chkElms "Should find 3 contains matches (lower, upper, mixed)" (chkCount 3) results,
                    test "Partial CaseSensitive lowercase does not match uppercase substring" $ do
                      results <- locateAll $ elmClass' Partial CaseSensitive "ptestclass"
                      chkElms "Should only find lowercase substring match" chkSingleton results
                  ],
                testGroup
                  "Multiple Classes - Full MatchType"
                  [ chkAutoId "Full CaseSensitive finds class in multi-class element - lowercase" (elmClass' Full CaseSensitive "mtestclass") "cls-multi-lower",
                    chkAutoId "Full CaseSensitive finds class in multi-class element - uppercase" (elmClass' Full CaseSensitive "MTESTCLASS") "cls-multi-upper",
                    chkAutoId "Full CaseSensitive finds class in multi-class element - mixed case" (elmClass' Full CaseSensitive "MTestClass") "cls-multi-mixed",
                    test "Full CaseInsensitive finds all multi-class elements" $ do
                      results <- locateAll $ elmClass' Full CaseInsensitive "mtestclass"
                      chkElms "Should find 3 multi-class elements regardless of case" (chkCount 3) results
                  ],
                testGroup
                  "Whitespace Normalization - Full MatchType"
                  [ chkAutoId "Full CaseSensitive matches class with leading spaces" (elmClass' Full CaseSensitive "wtestclass") "cls-whitespace-leading",
                    chkAutoId "Full CaseSensitive matches class with trailing spaces" (elmClass' Full CaseSensitive "wtestclass") "cls-whitespace-trailing",
                    chkAutoId "Full CaseSensitive matches class with multiple spaces between classes" (elmClass' Full CaseSensitive "wtestclass") "cls-whitespace-multiple",
                    chkAutoId "Full CaseSensitive matches class with tabs between classes" (elmClass' Full CaseSensitive "wtestclass") "cls-whitespace-tabs",
                    chkAutoId "Full CaseSensitive matches class with mixed whitespace" (elmClass' Full CaseSensitive "wtestclass") "cls-whitespace-mixed",
                    test "Full CaseSensitive finds all whitespace variants" $ do
                      results <- locateAll $ elmClass' Full CaseSensitive "wtestclass"
                      chkElms "Should find 5 whitespace variant elements" (chkCount 5) results
                  ]
              ],
          withResource (navToUrl ses matchTypeUrl) (\_ -> pure ()) $ \_ ->
            testGroup
              "Attribute Locator Tests"
              [ testGroup
                  "Data Attribute - Exact Match"
                  [ chkAutoId "Full CaseSensitive matches lowercase data attr value" (attribute' "data-testattr" Full CaseSensitive "value") "attr-exact-lower",
                    chkAutoId "Full CaseSensitive matches uppercase data attr value" (attribute' "data-testattr" Full CaseSensitive "VALUE") "attr-exact-upper",
                    chkAutoId "Full CaseSensitive matches mixed case data attr value" (attribute' "data-testattr" Full CaseSensitive "Value") "attr-exact-mixed",
                    test "Full CaseInsensitive finds all data attr matches regardless of case" $ do
                      results <- locateAll $ attribute' "data-testattr" Full CaseInsensitive "value"
                      chkElms "Should find 3 exact data attr matches" (chkCount 3) results,
                    test "Full CaseSensitive lowercase does not match uppercase data attr" $ do
                      results <- locateAll $ attribute' "data-testattr" Full CaseSensitive "value"
                      chkElms "Should only find lowercase data attr match" chkSingleton results
                  ],
                testGroup
                  "Data Attribute - Starts Match"
                  [ chkAutoId "Starts CaseSensitive matches lowercase data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "value") "attr-starts-lower",
                    chkAutoId "Starts CaseSensitive matches uppercase data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "VALUE") "attr-starts-upper",
                    chkAutoId "Starts CaseSensitive matches mixed case data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "Value") "attr-starts-mixed",
                    test "Starts CaseInsensitive finds all data attr prefix matches" $ do
                      results <- locateAll $ attribute' "data-testattr" Starts CaseInsensitive "value"
                      chkElms "Should find 3 starts data attr matches" (chkCount 3) results
                  ],
                testGroup
                  "Data Attribute - Contains Match"
                  [ chkAutoId "Partial CaseSensitive matches lowercase data attr substring" (attribute' "data-testattr" Partial CaseSensitive "value") "attr-contains-lower",
                    chkAutoId "Partial CaseSensitive matches uppercase data attr substring" (attribute' "data-testattr" Partial CaseSensitive "VALUE") "attr-contains-upper",
                    chkAutoId "Partial CaseSensitive matches mixed case data attr substring" (attribute' "data-testattr" Partial CaseSensitive "Value") "attr-contains-mixed",
                    test "Partial CaseInsensitive finds all data attr substring matches" $ do
                      results <- locateAll $ attribute' "data-testattr" Partial CaseInsensitive "value"
                      chkElms "Should find 3 contains data attr matches" (chkCount 3) results
                  ],
                testGroup
                  "Name Attribute - Exact Match"
                  [ chkAutoId "Full CaseSensitive matches lowercase name attr" (attribute' "name" Full CaseSensitive "username") "name-exact-lower",
                    chkAutoId "Full CaseSensitive matches uppercase name attr" (attribute' "name" Full CaseSensitive "USERNAME") "name-exact-upper",
                    chkAutoId "Full CaseSensitive matches mixed case name attr" (attribute' "name" Full CaseSensitive "UserName") "name-exact-mixed",
                    test "Full CaseInsensitive finds all name attr matches" $ do
                      results <- locateAll $ attribute' "name" Full CaseInsensitive "username"
                      chkElms "Should find 3 exact name attr matches" (chkCount 3) results
                  ],
                testGroup
                  "Name Attribute - Starts Match"
                  [ chkAutoId "Starts CaseSensitive matches lowercase name prefix" (attribute' "name" Starts CaseSensitive "username") "name-starts-lower",
                    chkAutoId "Starts CaseSensitive matches uppercase name prefix" (attribute' "name" Starts CaseSensitive "USERNAME") "name-starts-upper",
                    chkAutoId "Starts CaseSensitive matches mixed case name prefix" (attribute' "name" Starts CaseSensitive "UserName") "name-starts-mixed",
                    test "Starts CaseInsensitive finds all name prefix matches" $ do
                      results <- locateAll $ attribute' "name" Starts CaseInsensitive "username"
                      chkElms "Should find 3 starts name attr matches" (chkCount 3) results
                  ],
                testGroup
                  "Name Attribute - Contains Match"
                  [ chkAutoId "Partial CaseSensitive matches lowercase name substring" (attribute' "name" Partial CaseSensitive "username") "name-contains-lower",
                    chkAutoId "Partial CaseSensitive matches uppercase name substring" (attribute' "name" Partial CaseSensitive "USERNAME") "name-contains-upper",
                    chkAutoId "Partial CaseSensitive matches mixed case name substring" (attribute' "name" Partial CaseSensitive "UserName") "name-contains-mixed",
                    test "Partial CaseInsensitive finds all name substring matches" $ do
                      results <- locateAll $ attribute' "name" Partial CaseInsensitive "username"
                      chkElms "Should find 3 contains name attr matches" (chkCount 3) results
                  ],
                testGroup
                  "Title Attribute - Exact Match"
                  [ chkAutoId "Full CaseSensitive matches lowercase title" (attribute' "title" Full CaseSensitive "helpmessage") "title-exact-lower",
                    chkAutoId "Full CaseSensitive matches uppercase title" (attribute' "title" Full CaseSensitive "HELPMESSAGE") "title-exact-upper",
                    chkAutoId "Full CaseSensitive matches mixed case title" (attribute' "title" Full CaseSensitive "HelpMessage") "title-exact-mixed",
                    test "Full CaseInsensitive finds all title matches" $ do
                      results <- locateAll $ attribute' "title" Full CaseInsensitive "helpmessage"
                      chkElms "Should find 3 exact title matches" (chkCount 3) results
                  ],
                testGroup
                  "Title Attribute - Starts Match"
                  [ chkAutoId "Starts CaseSensitive matches lowercase title prefix" (attribute' "title" Starts CaseSensitive "helpmessage") "title-starts-lower",
                    chkAutoId "Starts CaseSensitive matches uppercase title prefix" (attribute' "title" Starts CaseSensitive "HELPMESSAGE") "title-starts-upper",
                    chkAutoId "Starts CaseSensitive matches mixed case title prefix" (attribute' "title" Starts CaseSensitive "HelpMessage") "title-starts-mixed",
                    test "Starts CaseInsensitive finds all title prefix matches" $ do
                      results <- locateAll $ attribute' "title" Starts CaseInsensitive "helpmessage"
                      chkElms "Should find 3 starts title matches" (chkCount 3) results
                  ],
                testGroup
                  "Title Attribute - Contains Match"
                  [ chkAutoId "Partial CaseSensitive matches lowercase title substring" (attribute' "title" Partial CaseSensitive "helpmessage") "title-contains-lower",
                    chkAutoId "Partial CaseSensitive matches uppercase title substring" (attribute' "title" Partial CaseSensitive "HELPMESSAGE") "title-contains-upper",
                    chkAutoId "Partial CaseSensitive matches mixed case title substring" (attribute' "title" Partial CaseSensitive "HelpMessage") "title-contains-mixed",
                    test "Partial CaseInsensitive finds all title substring matches" $ do
                      results <- locateAll $ attribute' "title" Partial CaseInsensitive "helpmessage"
                      chkElms "Should find 3 contains title matches" (chkCount 3) results
                  ]
              ]
        ]
      where
        test :: Text -> BaseHTTPEffs () -> TestTree
        test = runHttpTest ses

        locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
        locateAll = locateAllHttp defOpts

        chkAutoId :: Text -> Locator -> Text -> TestTree
        chkAutoId testName loc expctd =
          test testName $ do
            locRslt <- locateAll loc
            chkElmsWithAutoId (txt loc) expctd locRslt

matchTypeUrl :: IO URL
matchTypeUrl = fileUrl "locator-matchtype.html"

navToUrl :: IO WDSession -> IO URL -> IO WDSession
navToUrl getSes urlAction = do
  ses <- getSes
  runHttp ses $ testUrl urlAction >>= navigateTo
  pure ses

_eval :: Maybe Text -> TestTree -> IO ()
_eval mPattern = withArgs (maybe [] (\pat -> ["-p", (unpack pat)]) mPattern) . defaultMain

_pattern :: Maybe Text
_pattern = Just "Contains Match - Partial MatchType"
-- _pattern = Nothing

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess

