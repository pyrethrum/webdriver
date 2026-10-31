module Common.MatchTypeTest
  ( tests,
  )
where

import Common.Utils (beforeAll_)
import Common.WebDriver.Effect (WebDriver, navigateTo)
import Common.Utils qualified as TU
import Data.Text (Text, unpack)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Prelude
import Test.Tasty (TestTree, inOrderTestGroup, testGroup)
import Test.Tasty.HUnit (testCase)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Test.TestData (fileUrl)

tests ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
tests run interp =
  inOrderTestGroup
    "MatchType and CaseSensitivity Tests"
    [ beforeAll_ (matchTypeUrl >>= \url -> run (interp (navigateTo @elm url))) $
        testGroup
          "Class Locator Tests"
          [ testGroup
              "Exact Match - Full MatchType"
              [ chkAutoId "Full CaseSensitive matches lowercase exactly" (elmClass' Full CaseSensitive "testclass") "cls-exact-lower",
                chkAutoId "Full CaseSensitive matches uppercase exactly" (elmClass' Full CaseSensitive "TESTCLASS") "cls-exact-upper",
                chkAutoId "Full CaseSensitive matches mixed case exactly" (elmClass' Full CaseSensitive "TestClass") "cls-exact-mixed",
                test "Full CaseInsensitive finds all exact matches regardless of case" $ do
                  results <- TU.locateAll @elm $ elmClass' Full CaseInsensitive "testclass"
                  TU.chkCount "Should find 3 exact matches (lower, upper, mixed)" 3 results,
                test "Full CaseSensitive lowercase does not match uppercase" $ do
                  results <- TU.locateAll @elm $ elmClass' Full CaseSensitive "testclass"
                  TU.chkSingleton results,
                test "Full CaseSensitive uppercase does not match lowercase" $ do
                  results <- TU.locateAll @elm $ elmClass' Full CaseSensitive "TESTCLASS"
                  TU.chkSingleton results
              ],
            testGroup
              "Starts Match - Starts MatchType"
              [ chkAutoId "Starts CaseSensitive matches lowercase prefix" (elmClass' Starts CaseSensitive "swtestclass") "cls-starts-lower",
                chkAutoId "Starts CaseSensitive matches uppercase prefix" (elmClass' Starts CaseSensitive "SWTESTCLASS") "cls-starts-upper",
                chkAutoId "Starts CaseSensitive matches mixed case prefix" (elmClass' Starts CaseSensitive "SwTestClass") "cls-starts-mixed",
                test "Starts CaseInsensitive finds all starts matches regardless of case" $ do
                  results <- TU.locateAll @elm $ elmClass' Starts CaseInsensitive "swtestclass"
                  TU.chkCount "Should find 3 starts matches (lower, upper, mixed)" 3 results,
                test "Starts CaseSensitive lowercase does not match uppercase prefix" $ do
                  results <- TU.locateAll @elm $ elmClass' Starts CaseSensitive "swtestclass"
                  TU.chkSingleton results
              ],
            testGroup
              "Contains Match - Partial MatchType"
              [ chkAutoId "Partial CaseSensitive matches lowercase substring" (elmClass' Partial CaseSensitive "ptestclass") "cls-contains-lower",
                chkAutoId "Partial CaseSensitive matches uppercase substring" (elmClass' Partial CaseSensitive "PTESTCLASS") "cls-contains-upper",
                chkAutoId "Partial CaseSensitive matches mixed case substring" (elmClass' Partial CaseSensitive "PTestClass") "cls-contains-mixed",
                test "Partial CaseInsensitive finds all substring matches regardless of case" $ do
                  results <- TU.locateAll @elm $ elmClass' Partial CaseInsensitive "ptestclass"
                  TU.chkCount "Should find 3 contains matches (lower, upper, mixed)" 3 results,
                test "Partial CaseSensitive lowercase does not match uppercase substring" $ do
                  results <- TU.locateAll @elm $ elmClass' Partial CaseSensitive "ptestclass"
                  TU.chkSingleton results
              ],
            testGroup
              "Multiple Classes - Full MatchType"
              [ chkAutoId "Full CaseSensitive finds class in multi-class element - lowercase" (elmClass' Full CaseSensitive "mtestclass") "cls-multi-lower",
                chkAutoId "Full CaseSensitive finds class in multi-class element - uppercase" (elmClass' Full CaseSensitive "MTESTCLASS") "cls-multi-upper",
                chkAutoId "Full CaseSensitive finds class in multi-class element - mixed case" (elmClass' Full CaseSensitive "MTestClass") "cls-multi-mixed",
                test "Full CaseInsensitive finds all multi-class elements" $ do
                  results <- TU.locateAll @elm $ elmClass' Full CaseInsensitive "mtestclass"
                  TU.chkCount "Should find 3 multi-class elements regardless of case" 3 results
              ],
            testGroup
              "Whitespace Normalization - Full MatchType"
              [
                test "Full CaseSensitive finds all whitespace variants" $ do
                  results <- TU.locateAll @elm $ elmClass' Full CaseSensitive "wtestclass"
                  TU.chkCount "Should find 5 whitespace variant elements" 5 results
              ]
          ],
      beforeAll_ (matchTypeUrl >>= \url -> run (interp (navigateTo @elm url))) $
        testGroup
          "Attribute Locator Tests"
          [ testGroup
              "Data Attribute - Exact Match"
              [ chkAutoId "Full CaseSensitive matches lowercase data attr value" (attribute' "data-testattr" Full CaseSensitive "value") "attr-exact-lower",
                chkAutoId "Full CaseSensitive matches uppercase data attr value" (attribute' "data-testattr" Full CaseSensitive "VALUE") "attr-exact-upper",
                chkAutoId "Full CaseSensitive matches mixed case data attr value" (attribute' "data-testattr" Full CaseSensitive "Value") "attr-exact-mixed",
                test "Full CaseInsensitive finds all data attr matches regardless of case" $ do
                  results <- TU.locateAll @elm $ attribute' "data-testattr" Full CaseInsensitive "value"
                  TU.chkCount "Should find 3 exact data attr matches" 3 results,
                test "Full CaseSensitive lowercase does not match uppercase data attr" $ do
                  results <- TU.locateAll @elm $ attribute' "data-testattr" Full CaseSensitive "value"
                  TU.chkSingleton results
              ],
            testGroup
              "Data Attribute - Starts Match"
              [ chkAutoId "Starts CaseSensitive matches lowercase data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "swvalue") "attr-starts-lower",
                chkAutoId "Starts CaseSensitive matches uppercase data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "SWVALUE") "attr-starts-upper",
                chkAutoId "Starts CaseSensitive matches mixed case data attr prefix" (attribute' "data-testattr" Starts CaseSensitive "SwValue") "attr-starts-mixed",
                test "Starts CaseInsensitive finds all data attr prefix matches" $ do
                  results <- TU.locateAll @elm $ attribute' "data-testattr" Starts CaseInsensitive "swvalue"
                  TU.chkCount "Should find 3 starts data attr matches" 3 results
              ],
            testGroup
              "Data Attribute - Contains Match"
              [ chkAutoId "Partial CaseSensitive matches lowercase data attr substring" (attribute' "data-testattr" Partial CaseSensitive "prefix-value") "attr-contains-lower",
                chkAutoId "Partial CaseSensitive matches uppercase data attr substring" (attribute' "data-testattr" Partial CaseSensitive "PREFIX-VALUE") "attr-contains-upper",
                chkAutoId "Partial CaseSensitive matches mixed case data attr substring" (attribute' "data-testattr" Partial CaseSensitive "Prefix-Value") "attr-contains-mixed",
                test "Partial CaseInsensitive finds all data attr substring matches" $ do
                  results <- TU.locateAll @elm $ attribute' "data-testattr" Partial CaseInsensitive "prefix-value"
                  TU.chkCount "Should find 3 contains data attr matches" 3 results
              ],
            testGroup
              "Name Attribute - Exact Match"
              [ chkAutoId "Full CaseSensitive matches lowercase name attr" (attribute' "name" Full CaseSensitive "username") "name-exact-lower",
                chkAutoId "Full CaseSensitive matches uppercase name attr" (attribute' "name" Full CaseSensitive "USERNAME") "name-exact-upper",
                chkAutoId "Full CaseSensitive matches mixed case name attr" (attribute' "name" Full CaseSensitive "UserName") "name-exact-mixed",
                test "Full CaseInsensitive finds all name attr matches" $ do
                  results <- TU.locateAll @elm $ attribute' "name" Full CaseInsensitive "username"
                  TU.chkCount "Should find 3 exact name attr matches" 3 results
              ],
            testGroup
              "Name Attribute - Starts Match"
              [ chkAutoId "Starts CaseSensitive matches lowercase name prefix" (attribute' "name" Starts CaseSensitive "swusername") "name-starts-lower",
                chkAutoId "Starts CaseSensitive matches uppercase name prefix" (attribute' "name" Starts CaseSensitive "SWUSERNAME") "name-starts-upper",
                chkAutoId "Starts CaseSensitive matches mixed case name prefix" (attribute' "name" Starts CaseSensitive "SwUserName") "name-starts-mixed",
                test "Starts CaseInsensitive finds all name prefix matches" $ do
                  results <- TU.locateAll @elm $ attribute' "name" Starts CaseInsensitive "swusername"
                  TU.chkCount "Should find 3 starts name attr matches" 3 results
              ],
            testGroup
              "Name Attribute - Contains Match"
              [ chkAutoId "Partial CaseSensitive matches lowercase name substring" (attribute' "name" Partial CaseSensitive "input-username") "name-contains-lower",
                chkAutoId "Partial CaseSensitive matches uppercase name substring" (attribute' "name" Partial CaseSensitive "INPUT-USERNAME") "name-contains-upper",
                chkAutoId "Partial CaseSensitive matches mixed case name substring" (attribute' "name" Partial CaseSensitive "Input-UserName") "name-contains-mixed",
                test "Partial CaseInsensitive finds all name substring matches" $ do
                  results <- TU.locateAll @elm $ attribute' "name" Partial CaseInsensitive "input-username"
                  TU.chkCount "Should find 3 contains name attr matches" 3 results
              ],
            testGroup
              "Title Attribute - Exact Match"
              [ chkAutoId "Full CaseSensitive matches lowercase title" (attribute' "title" Full CaseSensitive "helpmessage") "title-exact-lower",
                chkAutoId "Full CaseSensitive matches uppercase title" (attribute' "title" Full CaseSensitive "HELPMESSAGE") "title-exact-upper",
                chkAutoId "Full CaseSensitive matches mixed case title" (attribute' "title" Full CaseSensitive "HelpMessage") "title-exact-mixed",
                test "Full CaseInsensitive finds all title matches" $ do
                  results <- TU.locateAll @elm $ attribute' "title" Full CaseInsensitive "helpmessage"
                  TU.chkCount "Should find 3 exact title matches" 3 results
              ],
            testGroup
              "Title Attribute - Starts Match"
              [ chkAutoId "Starts CaseSensitive matches lowercase title prefix" (attribute' "title" Starts CaseSensitive "swhelpmessage") "title-starts-lower",
                chkAutoId "Starts CaseSensitive matches uppercase title prefix" (attribute' "title" Starts CaseSensitive "SWHELPMESSAGE") "title-starts-upper",
                chkAutoId "Starts CaseSensitive matches mixed case title prefix" (attribute' "title" Starts CaseSensitive "SwHelpMessage") "title-starts-mixed",
                test "Starts CaseInsensitive finds all title prefix matches" $ do
                  results <- TU.locateAll @elm $ attribute' "title" Starts CaseInsensitive "swhelpmessage"
                  TU.chkCount "Should find 3 starts title matches" 3 results
              ],
            testGroup
              "Title Attribute - Contains Match"
              [ chkAutoId "Partial CaseSensitive matches lowercase title substring" (attribute' "title" Partial CaseSensitive "show-helpmessage") "title-contains-lower",
                chkAutoId "Partial CaseSensitive matches uppercase title substring" (attribute' "title" Partial CaseSensitive "SHOW-HELPMESSAGE") "title-contains-upper",
                chkAutoId "Partial CaseSensitive matches mixed case title substring" (attribute' "title" Partial CaseSensitive "Show-HelpMessage") "title-contains-mixed",
                test "Partial CaseInsensitive finds all title substring matches" $ do
                  results <- TU.locateAll @elm $ attribute' "title" Partial CaseInsensitive "show-helpmessage"
                  TU.chkCount "Should find 3 contains title matches" 3 results
              ]
          ]
    ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId testName loc expctd =
      test testName $ TU.locatAllChkAutoId @elm loc expctd

matchTypeUrl :: IO URL
matchTypeUrl = fileUrl "locator-matchtype.html"
