module HTTP.MatchTypeTest where

import Common.Runner (testUrl)
import Common.Utils qualified as U

import Data.Text (Text)
import Effectful
import HTTP.Runner (WDSession, closeWDSession, getWDSession, runHttp, runHttpTest)
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource)
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions (navigateTo, getElementAttribute, getElementProperty)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Test.TestData (fileUrl)
import Prelude
import WebDriver.Effectful.Logger
import Common.Utils (DriverActions(..), chkCount, chkSingleton)


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
                  [
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
                  [ chkAutoId "Starts CaseSensitive matches lowercase data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "swvalue") "attr-starts-lower",
                    chkAutoId "Starts CaseSensitive matches uppercase data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "SWVALUE") "attr-starts-upper",
                    chkAutoId "Starts CaseSensitive matches mixed case data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "SwValue") "attr-starts-mixed",
                    test "Starts CaseInsensitive finds all data attr prefix matches" $ do
                      results <- locateAll $ attribute' "data-testattr" Starts CaseInsensitive "swvalue"
                      chkElms "Should find 3 starts data attr matches" (chkCount 3) results
                  ],
                testGroup
                  "Data Attribute - Contains Match"
                  [ chkAutoId "Partial CaseSensitive matches lowercase data attr substring" (attribute' "data-testattr" Partial CaseSensitive "prefix-value") "attr-contains-lower",
                    chkAutoId "Partial CaseSensitive matches uppercase data attr substring" (attribute' "data-testattr" Partial CaseSensitive "PREFIX-VALUE") "attr-contains-upper",
                    chkAutoId "Partial CaseSensitive matches mixed case data attr substring" (attribute' "data-testattr" Partial CaseSensitive "Prefix-Value") "attr-contains-mixed",
                    test "Partial CaseInsensitive finds all data attr substring matches" $ do
                      results <- locateAll $ attribute' "data-testattr" Partial CaseInsensitive "prefix-value"
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
                  [ chkAutoId "Starts CaseSensitive matches lowercase name prefix" (attribute' "name" Starts CaseSensitive "swusername") "name-starts-lower",
                    chkAutoId "Starts CaseSensitive matches uppercase name prefix" (attribute' "name" Starts CaseSensitive "SWUSERNAME") "name-starts-upper",
                    chkAutoId "Starts CaseSensitive matches mixed case name prefix" (attribute' "name" Starts CaseSensitive "SwUserName") "name-starts-mixed",
                    test "Starts CaseInsensitive finds all name prefix matches" $ do
                      results <- locateAll $ attribute' "name" Starts CaseInsensitive "swusername"
                      chkElms "Should find 3 starts name attr matches" (chkCount 3) results
                  ],
                testGroup
                  "Name Attribute - Contains Match"
                  [ chkAutoId "Partial CaseSensitive matches lowercase name substring" (attribute' "name" Partial CaseSensitive "input-username") "name-contains-lower",
                    chkAutoId "Partial CaseSensitive matches uppercase name substring" (attribute' "name" Partial CaseSensitive "INPUT-USERNAME") "name-contains-upper",
                    chkAutoId "Partial CaseSensitive matches mixed case name substring" (attribute' "name" Partial CaseSensitive "Input-UserName") "name-contains-mixed",
                    test "Partial CaseInsensitive finds all name substring matches" $ do
                      results <- locateAll $ attribute' "name" Partial CaseInsensitive "input-username"
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
                  [ chkAutoId "Starts CaseSensitive matches lowercase title prefix" (attribute' "title" Starts CaseSensitive "swhelpmessage") "title-starts-lower",
                    chkAutoId "Starts CaseSensitive matches uppercase title prefix" (attribute' "title" Starts CaseSensitive "SWHELPMESSAGE") "title-starts-upper",
                    chkAutoId "Starts CaseSensitive matches mixed case title prefix" (attribute' "title" Starts CaseSensitive "SwHelpMessage") "title-starts-mixed",
                    test "Starts CaseInsensitive finds all title prefix matches" $ do
                      results <- locateAll $ attribute' "title" Starts CaseInsensitive "swhelpmessage"
                      chkElms "Should find 3 starts title matches" (chkCount 3) results
                  ],
                testGroup
                  "Title Attribute - Contains Match"
                  [ chkAutoId "Partial CaseSensitive matches lowercase title substring" (attribute' "title" Partial CaseSensitive "show-helpmessage") "title-contains-lower",
                    chkAutoId "Partial CaseSensitive matches uppercase title substring" (attribute' "title" Partial CaseSensitive "SHOW-HELPMESSAGE") "title-contains-upper",
                    chkAutoId "Partial CaseSensitive matches mixed case title substring" (attribute' "title" Partial CaseSensitive "Show-HelpMessage") "title-contains-mixed",
                    test "Partial CaseInsensitive finds all title substring matches" $ do
                      results <- locateAll $ attribute' "title" Partial CaseInsensitive "show-helpmessage"
                      chkElms "Should find 3 contains title matches" (chkCount 3) results
                  ]
              ]
        ]
      where
        testRunner = \name act -> runHttpTest ses name act
        getProperty = getElementProperty
        getAttribute = getElementAttribute
        locateFn = U.locateHttp U.defHttpOpts
        locateAllFn = U.locateAllHttp U.defHttpOpts
        
        da :: DriverActions (Eff '[WebDriverHttp, Logger, Pause, IOE])
        da = MkDriverActions { 
            testRunner,
            getProperty,
            getAttribute,
            locateFn,
            locateAllFn
        }

        test = runHttpTest ses
        chkElms = U.chkElms da

        locateAll = U.locateAllHttp U.defHttpOpts

        chkAutoId :: Text -> Locator -> Text -> TestTree
        chkAutoId testName loc expctd =
          test testName $ do
            locRslt <- locateAll loc
            U.chkElmsWithAutoId da (txt loc) expctd locRslt

matchTypeUrl :: IO URL
matchTypeUrl = fileUrl "locator-matchtype.html"

navToUrl :: IO WDSession -> IO URL -> IO WDSession
navToUrl getSes urlAction = do
  ses <- getSes
  runHttp ses $ testUrl urlAction >>= navigateTo
  pure ses

_eval :: Maybe Text -> TestTree -> IO ()
_eval = U.testPattern

_pattern :: Maybe Text
-- _pattern = Just "Contains Match - Partial MatchType"
_pattern = Nothing

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess

