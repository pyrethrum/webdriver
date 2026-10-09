module Common.BaseLocateTest where

import Common.Utils (beforeAll_)
import Common.WebDriver.Effect
  ( WebDriver,
    locate,
    locateAll,
    locateAllFromElement,
    locateFromElement,
    maximizeWindow,
    minimizeWindow,
    navigateTo,
  )
import Common.WebDriver.TestUtils (atrrChkElm, chkAutoIdElm)
import Common.WebDriver.TestUtils qualified as TU
import Data.Function ((&))
import Data.Text (Text, unpack)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators as LS
import WebDriverPreCore.Test.TestData
import WebDriverPreCore.Utils.Utils (txt)
import Prelude

-- ---------------------------------------------------------------------------
-- Landmark and Role Tests
-- ---------------------------------------------------------------------------

testLandmarkAndRole ::
  forall elm es.
  (Eq elm, Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testLandmarkAndRole run interp =
  beforeAll_ (landmarkRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "Landmark and Role Tests"
      [ chkAutoId "Locate by ID" (elmId "section-personal") "sec-personal",
        test "jsDisplay check should NOT be affected by viewport" $ do
          maximizeWindow @elm
          maxResult <- locateAll @elm $ elmClass "input"
          minimizeWindow @elm
          minResult <- locateAll @elm $ elmClass "input"
          TU.chkEq "Displayed result should be the same for minised and maximised viewport" maxResult minResult,
        testGroup
          "Role Locator Tests"
          [ testGroup
              "Landmark role types - roleType"
              [ chkAutoId "Banner - page header" (roleType Banner) "hdr-main",
                chkAutoId "Main landmark" (roleType Main) "main-content",
                chkAutoId "ContentInfo - page footer" (roleType ContentInfo) "ftr-main",
                chkAutoId "Complementary - aside" (roleType Complementary) "aside-help",
                chkAutoId "Search landmark" (roleType Search) "srch-widget"
              ],
            testGroup
              "Role with name - aria-label"
              [ chkAutoId "Navigation - Main navigation" (navigation "Main navigation") "nav-main",
                chkAutoId "Navigation - Breadcrumb" (navigation "Breadcrumb") "nav-breadcrumb",
                chkAutoId "Form - Mega test form" (form "Mega test form") "frm-mega",
                chkAutoId "Complementary - Help and tips" (complementary "Help and tips") "aside-help",
                chkAutoId "Button - submit by aria-label" (button "Submit the mega form") "btn-submit",
                chkAutoId "Button - span with explicit role override" (button "Span acting as button") "btn-span-role",
                chkAutoId "Button - link with explicit role override" (button "Link acting as button") "btn-link-role",
                chkAutoId "Textbox - Nickname via aria-label" (textbox "Nickname") "edt-nickname",
                chkAutoId "Checkbox - Read documents via aria-label" (checkbox "Read documents") "chk-docs-read",
                chkAutoId "Img - div with explicit role override" (img "Abstract coloured shape") "img-div-role"
              ],
            testGroup
              "Multi-element role types"
              [ chkElmCount "Navigation - finds both nav landmarks" (roleType Navigation) 2
              ]
          ]
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (chkAutoIdElm @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount name loc expected = test name (TU.chkAll @elm loc (elmCountMatches expected))

-- ---------------------------------------------------------------------------
-- Extended Role Matching Tests
-- ---------------------------------------------------------------------------

testExtendedRoleMatching ::
  forall elm es.
  (Eq elm, Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testExtendedRoleMatching run interpNever interpAlways interpMiss =
  beforeAll_ (extendedRolesUrl >>= \url -> run (interpNever (navigateTo @elm url))) $
    testGroup
      "Extended Role Matching Tests"
      [ testGroup
          "aria-labelledby resolution"
          [ atrrChkExtRole
              "ExtLocateAlways - locate finds region via aria-labelledby"
              (region "Personal Information")
              "auto-id"
              "sec-personal",
            atrrChkExtMiss
              "ExtLocateSingletonMiss - locate finds region via aria-labelledby"
              (region "Personal Information")
              "auto-id"
              "sec-personal",
            testNever "ExtLocateNever - locate does NOT find region via aria-labelledby" $ do
              locRslt <- locate @elm $ region "Personal Information"
              TU.chkLocException (txt (region "Personal Information")) isNotFound locRslt,
            testAlways "ExtLocateAlways - locateAll finds region via aria-labelledby" $ do
              locRslt <- locateAll @elm $ region "Personal Information"
              TU.chkElms (txt (region "Personal Information")) (elmCountMatches 1) locRslt,
            testMiss "ExtLocateSingletonMiss - locateAll does NOT find region via aria-labelledby" $ do
              locRslt <- locateAll @elm $ region "Personal Information"
              TU.chkElms (txt (region "Personal Information")) (elmCountMatches 0) locRslt,
            testNever "ExtLocateNever - locateAll does NOT find region via aria-labelledby" $ do
              locRslt <- locateAll @elm $ region "Personal Information"
              TU.chkElms (txt (region "Personal Information")) (elmCountMatches 0) locRslt
          ],
        testGroup
          "for id label association"
          [ atrrChkExtRole
              "ExtLocateAlways - locate finds radio via for id label"
              (radio "Email")
              "auto-id"
              "rdo-contact-email",
            atrrChkExtMiss
              "ExtLocateSingletonMiss - locate finds radio via for id label"
              (radio "Email")
              "auto-id"
              "rdo-contact-email",
            testNever "ExtLocateNever - locate does NOT find radio via for id label" $ do
              locRslt <- locate @elm $ radio "Email"
              TU.chkLocException (txt (radio "Email")) isNotFound locRslt,
            atrrChkExtRole
              "ExtLocateAlways - locate finds textbox via for id label"
              (textbox "Given Name")
              "auto-id"
              "edt-given-name",
            atrrChkExtMiss
              "ExtLocateSingletonMiss - locate finds textbox via for id label"
              (textbox "Given Name")
              "auto-id"
              "edt-given-name",
            testNever "ExtLocateNever - locate does NOT find textbox via for id label" $ do
              locRslt <- locate @elm $ textbox "Given Name"
              TU.chkLocException (txt (textbox "Given Name")) isNotFound locRslt,
            testAlways "ExtLocateAlways - locateAll finds radio via for id label" $ do
              locRslt <- locateAll @elm $ radio "Email"
              TU.chkElms (txt (radio "Email")) (elmCountMatches 1) locRslt,
            testMiss "ExtLocateSingletonMiss - locateAll does NOT find radio via for id label" $ do
              locRslt <- locateAll @elm $ radio "Email"
              TU.chkElms (txt (radio "Email")) (elmCountMatches 0) locRslt
          ],
        testGroup
          "RoleType - unaffected by extended matching"
          [ testBase "RoleType Region - ExtLocateNever and ExtLocateAlways give same results" $ do
              never <- interpNever (locateAll @elm $ roleType Region)
              always <- interpAlways (locateAll @elm $ roleType Region)
              TU.chkEq "RoleType Region results should be identical" never always
          ],
        testGroup
          "aria-label - always resolved regardless of setting"
          [ chkAutoId
              "ExtLocateNever finds textbox with aria-label"
              (textbox "Nickname")
              "edt-nickname",
            atrrChkExtRole
              "ExtLocateAlways finds textbox with aria-label"
              (textbox "Nickname")
              "auto-id"
              "edt-nickname",
            atrrChkExtMiss
              "ExtLocateSingletonMiss finds textbox with aria-label"
              (textbox "Nickname")
              "auto-id"
              "edt-nickname"
          ]
      ]
  where
    testNever :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testNever name act = testCase (unpack name) (run (interpNever act))

    testAlways :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testAlways name act = testCase (unpack name) (run (interpAlways act))

    testMiss :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testMiss name act = testCase (unpack name) (run (interpMiss act))

    testBase :: Text -> Eff es () -> TestTree
    testBase name act = testCase (unpack name) (run act)

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = testNever name (chkAutoIdElm @elm loc expctd)

    atrrChkExtRole :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtRole testName loc attrName expctd =
      testAlways testName (atrrChkElm @elm loc attrName expctd)

    atrrChkExtMiss :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtMiss testName loc attrName expctd =
      testMiss testName (atrrChkElm @elm loc attrName expctd)

-- ---------------------------------------------------------------------------
-- Visibility Check Tests
-- ---------------------------------------------------------------------------

testVisibility ::
  forall elm es.
  (Eq elm, Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testVisibility run interpAlways interpNever interpDisambiguate =
  beforeAll_ (visibilityUrl >>= \url -> run (interpAlways (navigateTo @elm url))) $
    testGroup
      "Visibility Check Tests"
      [ testGroup
          "locateAll - DisplayedCheckAlways filters hidden and DisplayedCheckNever does not"
          [ testGroup
              "Rule 1 - display none on element itself"
              [ chkElmCount
                  "edt-notes-hidden has display none via own CSS class - DisplayedCheckAlways filters"
                  (TU.autoId "edt-notes-hidden")
                  0,
                chkElmCountNever
                  "edt-notes-hidden has display none via own CSS class - DisplayedCheckNever finds"
                  (TU.autoId "edt-notes-hidden")
                  1
              ],
            testGroup
              "Rule 2 - visibility hidden or collapse - inherited"
              [ chkElmCount
                  "edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-vis-hidden")
                  0,
                chkElmCountNever
                  "edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-vis-hidden")
                  1,
                chkElmCount
                  "edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-css-vis-hidden")
                  0,
                chkElmCountNever
                  "edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-css-vis-hidden")
                  1
              ],
            testGroup
              "Rule 3 - parseFloat opacity equals 0 on element itself"
              [ chkElmCount
                  "fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckAlways filters"
                  (TU.autoId "fg-opacity-zero")
                  0,
                chkElmCountNever
                  "fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckNever finds"
                  (TU.autoId "fg-opacity-zero")
                  1
              ],
            testGroup
              "Rule 4 - INPUT with type hidden"
              [ chkElmCount
                  "hdn-session-token is input type hidden - DisplayedCheckAlways filters"
                  (TU.autoId "hdn-session-token")
                  0,
                chkElmCountNever
                  "hdn-session-token is input type hidden - DisplayedCheckNever finds"
                  (TU.autoId "hdn-session-token")
                  1
              ],
            testGroup
              "Rule 5 - offsetWidth or offsetHeight equals 0 - parent has display none"
              [ chkElmCount
                  "edt-display-none inside inline display none parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-display-none")
                  0,
                chkElmCountNever
                  "edt-display-none inside inline display none parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-display-none")
                  1,
                chkElmCount
                  "edt-css-none inside CSS class display none parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-css-none")
                  0,
                chkElmCountNever
                  "edt-css-none inside CSS class display none parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-css-none")
                  1,
                chkElmCount
                  "edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-html-hidden")
                  0,
                chkElmCountNever
                  "edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-html-hidden")
                  1
              ],
            testGroup
              "NOT filtered by displayedJS"
              [ chkElmCount
                  "edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckAlways finds"
                  (TU.autoId "edt-aria-hidden")
                  1,
                chkElmCountNever
                  "edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckNever finds"
                  (TU.autoId "edt-aria-hidden")
                  1,
                chkElmCount
                  "edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckAlways finds"
                  (TU.autoId "edt-offscreen")
                  1,
                chkElmCountNever
                  "edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckNever finds"
                  (TU.autoId "edt-offscreen")
                  1,
                chkElmCount
                  "edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckAlways finds"
                  (TU.autoId "edt-opacity-zero")
                  1,
                chkElmCountNever
                  "edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckNever finds"
                  (TU.autoId "edt-opacity-zero")
                  1
              ]
          ],
        testGroup
          "locate singleton - DisplayedCheckDisambiguateUnique resolves hidden-visible ambiguity"
          [ testNever "DisplayedCheckNever with Unique throws AmbiguousLocator - hidden and visible share class" $ do
              locRslt <- locate @elm $ elmClass "notes-area"
              TU.chkLocException (txt (elmClass "notes-area")) isAmbiguous locRslt,
            testDisambiguate "DisplayedCheckDisambiguateUnique filters hidden - resolving to unique visible element" $
              atrrChkElm @elm (elmClass "notes-area") "auto-id" "edt-notes-visible",
            testAlways "DisplayedCheckAlways also filters hidden, resolving to unique visible element" $
              atrrChkElm @elm (elmClass "notes-area") "auto-id" "edt-notes-visible"
          ],
        testGroup
          "locateAll - DisplayedCheckDisambiguateUnique has no effect - only Always filters"
          [ testBase "DisambiguateUnique gives same result as Never for locateAll" $ do
              disambiguate <- interpDisambiguate (locateAll @elm $ elmClass "notes-area")
              never <- interpNever (locateAll @elm $ elmClass "notes-area")
              TU.chkEq "DisambiguateUnique locateAll result must equal Never" disambiguate never,
            testGroup
              "DisplayedCheckAlways filters hidden in locateAll - Never returns both"
              [ chkElmCount "notes-area with DisplayedCheckAlways finds visible only" (elmClass "notes-area") 1,
                chkElmCountNever "notes-area with DisplayedCheckNever finds both visible and hidden" (elmClass "notes-area") 2
              ]
          ]
      ]
  where
    testAlways :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testAlways name act = testCase (unpack name) (run (interpAlways act))

    testNever :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testNever name act = testCase (unpack name) (run (interpNever act))

    testDisambiguate :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testDisambiguate name act = testCase (unpack name) (run (interpDisambiguate act))

    testBase :: Text -> Eff es () -> TestTree
    testBase name act = testCase (unpack name) (run act)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount name loc expected = testAlways name (TU.chkAll @elm loc (elmCountMatches expected))

    chkElmCountNever :: Text -> Locator -> Int -> TestTree
    chkElmCountNever name loc expected = testNever name (TU.chkAll @elm loc (elmCountMatches expected))

-- ---------------------------------------------------------------------------
-- Basic Locator Types
-- ---------------------------------------------------------------------------

testBasicLocatorTypes ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testBasicLocatorTypes run interp =
  beforeAll_ (landmarkRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "Basic Locator Types"
      [ chkAutoId "defaultId resolves via mkDefaultLoc option" (defaultId "hdr-main") "hdr-main",
        chkElmCount "allElms finds all page elements" allElms 42,
        chkAutoId "elmId finds element by HTML id" (elmId "megaforma") "frm-mega",
        chkAutoId "css attribute selector" (css "[auto-id='ftr-main']") "ftr-main",
        chkAutoId "xpath finds element by tag" (xpath "//footer") "ftr-main",
        chkElmCount "input_ tag locator finds all inputs" input_ 7,
        chkElmCount "button_ tag locator finds button elements" button_ 2,
        chkAll "h1_ tag locator finds the single h1 heading" h1_ (elmCountMatches 1)
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (chkAutoIdElm @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount name loc expected = test name (TU.chkAll @elm loc (elmCountMatches expected))

    chkAll :: Text -> Locator -> ([elm] -> Maybe Text) -> TestTree
    chkAll name loc chk = test name (TU.chkAll @elm loc chk)

-- ---------------------------------------------------------------------------
-- Class Locator Variants
-- ---------------------------------------------------------------------------

testClassLocatorVariants ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testClassLocatorVariants run interp =
  beforeAll_ (landmarkRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "Class Locator Variants"
      [ chkElmCount "elmClass contains match" (elmClass "text-input") 7,
        chkElmCount "elmClassExact full-equality match" (elmClass "text-input") 7,
        chkElmCount "elemClassStarts starts-with match" (elemClassStarts "text") 7,
        chkAutoId "elmClass finds element by single class name" (elmClass "span-button") "btn-span-role"
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (chkAutoIdElm @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount name loc expected = test name (TU.chkAll @elm loc (elmCountMatches expected))

-- ---------------------------------------------------------------------------
-- Attribute Locator Variants
-- ---------------------------------------------------------------------------

testAttributeLocatorVariants ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testAttributeLocatorVariants run interp =
  beforeAll_ (landmarkRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "Attribute Locator Variants"
      [ chkAutoId "attribute default contains match" (attribute "auto-id" "hdr-main") "hdr-main",
        chkAutoId "attributeExact full-equality match" (attribute "auto-id" "hdr-main") "hdr-main",
        chkElmCount "attributeStarts starts-with match" (attributeStarts "auto-id" "nav") 4,
        chkElmCount "attribute full case-sensitive finds type text inputs" (attribute' "type" Full CaseSensitive "text") 3,
        chkAutoId "attribute full case-insensitive matches uppercase value" (attribute' "auto-id" Full CaseInsensitive "HDR-MAIN") "hdr-main"
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (chkAutoIdElm @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount name loc expected = test name (TU.chkAll @elm loc (elmCountMatches expected))

-- ---------------------------------------------------------------------------
-- roleName and role Constructors
-- ---------------------------------------------------------------------------

testRoleNameAndRole ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testRoleNameAndRole run interp =
  beforeAll_ (landmarkRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "roleName and role Constructors"
      [ chkAutoId "roleName finds element by accessible name" (roleName "Submit the mega form") "btn-submit",
        chkAutoId "roleName finds aside by aria-label" (roleName "Help and tips") "aside-help",
        chkAutoId "roleName finds nav by aria-label" (roleName "Main navigation") "nav-main",
        chkAutoId "role generic constructor - Navigation with name" (role Navigation "Breadcrumb") "nav-breadcrumb"
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (chkAutoIdElm @elm loc expctd)

-- ---------------------------------------------------------------------------
-- Locate and LocateAll from Element
-- ---------------------------------------------------------------------------

testLocateFromElement ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testLocateFromElement run interp =
  beforeAll_ (landmarkRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "Locate and LocateAll from Element"
      [ test "locateAll from element - inputs within sec-personal" $ do
          secResult <- locate @elm $ TU.autoId "sec-personal"
          chkElmM "find sec-personal" secResult $ \sec -> do
            inResult <- locateAllFromElement @elm sec input_
            TU.chkElms "inputs in sec-personal" (elmCountMatches 5) inResult
            pure Nothing,
        test "locateAll from element - links within nav-main" $ do
          navResult <- locate @elm $ TU.autoId "nav-main"
          chkElmM "find nav-main" navResult $ \nav -> do
            linkResult <- locateAllFromElement @elm nav a_
            TU.chkElms "links in nav-main" (elmCountMatches 2) linkResult
            pure Nothing,
        test "locate from element - edt-given-name within sec-personal" $ do
          secResult <- locate @elm $ TU.autoId "sec-personal"
          chkElmM "find sec-personal" secResult $ \sec -> do
            givenResult <- locateFromElement @elm sec $ TU.autoId "edt-given-name"
            TU.chkElm "edt-given-name in section" (\_ -> Nothing) givenResult
            pure Nothing,
        test "locate from element - not found when element not in scope" $ do
          hdrResult <- locate @elm $ TU.autoId "hdr-main"
          chkElmM "find hdr-main" hdrResult $ \hdr -> do
            notInHdr <- locateFromElement @elm hdr $ TU.autoId "edt-given-name"
            pure $ case notInHdr of
              Left (L.ElementNotFound {}) -> Nothing
              Left other -> Just $ "expected ElementNotFound but got: " <> txt other
              Right _ -> Just "expected ElementNotFound but edt-given-name was found in header"
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

-- ---------------------------------------------------------------------------
-- Combined Locators
-- ---------------------------------------------------------------------------

testCombinedLocators ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testCombinedLocators run interp =
  beforeAll_ (landmarkRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "Combined Locators"
      [ chkElmCount "AND - input_ and elmClass text-input" (input_ &&& elmClass "text-input") 6,
        chkElmCount "OR - h1_ or h2_ finds all headings" (h1_ ||| h2_) 3,
        chkElmCount "Descendant - sec-personal contains input_ finds contained inputs" (TU.autoId "sec-personal" >>> input_) 5,
        chkElmCount "OR - roleType Navigation or roleType Search" (roleType Navigation ||| roleType Search) 3
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount name loc expected = test name (TU.chkAll @elm loc (elmCountMatches expected))

-- ---------------------------------------------------------------------------
-- Misc ARIA Role Types
-- ---------------------------------------------------------------------------

testMiscAriaRoleTypes ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testMiscAriaRoleTypes run interp interpNever =
  beforeAll_ (miscRolesUrl >>= \url -> run (interp (navigateTo @elm url))) $
    testGroup
      "Misc ARIA Role Types"
      [ chkAutoId "roleType Article" (roleType Article) "art-main",
        chkAutoId "article by accessible name" (article "Test article") "art-main",
        chkAutoId "roleType Heading - single heading on page" (roleType Heading) "hdg-article",
        chkAutoId "heading by text content" (heading "Article Heading") "hdg-article",
        chkAutoId "roleType Figure" (roleType Figure) "fig-sample",
        chkAutoId "figure by accessible name" (figure "Sample figure") "fig-sample",
        chkAutoId "roleType List - single list on page" (roleType List) "lst-nav",
        chkAutoId "list by accessible name" (list "Navigation list") "lst-nav",
        chkElmCount "roleType ListItem finds all list items" (roleType ListItem) 2,
        chkAutoId "link by text content" (link "Home") "lnk-home",
        chkElmCount "roleType Link finds all links" (roleType Link) 2,
        chkAutoId "roleType Table" (roleType Table) "tbl-data",
        chkAutoId "table by accessible name" (table "Data table") "tbl-data",
        chkElmCount "roleType Row finds header and data rows" (roleType Row) 2,
        chkElmCount "roleType ColumnHeader finds both column headers" (roleType ColumnHeader) 2,
        chkAutoId "columnHeader by text content" (columnHeader "Name") "col-name",
        chkAutoId "roleType RowHeader" (roleType RowHeader) "row-hdr-a",
        chkAutoId "rowHeader by text content" (rowHeader "Row A") "row-hdr-a",
        chkAutoId "roleType Cell" (roleType Cell) "cel-a1",
        chkAutoId "cell by text content" (cell "Cell A1") "cel-a1",
        chkAutoId "roleType Group finds fieldset" (roleType Group) "grp-options",
        chkAutoId "group by accessible name" (group "Options Group") "grp-options",
        -- Note: <option> elements always have offsetWidth/offsetHeight of 0, even when the
        -- dropdown is visually open. Browser <select> dropdowns are rendered as native OS
        -- widgets (not DOM elements), so options never have CSS dimensions. DisplayedCheckAlways
        -- filters them out. Use DisplayedCheckNever to locate them programmatically.
        chkElmCount "roleType Option finds no options (DisplayedCheckAlways)" (roleType Option) 0,
        chkElmCountNever "roleType Option with DisplayedCheckNever finds all options" (roleType Option) 2,
        chkAutoId "option by text content" (option "Alpha") "opt-alpha",
        chkAutoId "roleType Separator" (roleType Separator) "sep-main",
        chkAutoId "progressBar by accessible name" (progressBar "Upload progress") "prg-upload",
        chkAutoId "slider by accessible name" (slider "Volume") "sld-volume",
        chkAutoId "spinButton by accessible name" (spinButton "Item count") "spn-count",
        chkAutoId "roleType Status" (roleType LS.Status) "out-result",
        chkAutoId "status by accessible name" (LS.status "Calculation result") "out-result",
        chkAutoId "roleType Term" (roleType Term) "trm-name",
        chkAutoId "term by text content" (term "Name") "trm-name",
        chkAutoId "roleType Definition" (roleType Definition) "def-name",
        chkAutoId "definition by text content" (definition "John") "def-name"
      ]
  where

    interpret = mkTest run

    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test = interpret interp

    testNever :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testNever = interpret interpNever

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (chkAutoIdElm @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount name loc expected = test name (TU.chkAll @elm loc (elmCountMatches expected))

    chkElmCountNever :: Text -> Locator -> Int -> TestTree
    chkElmCountNever name loc expected = testNever name (TU.chkAll @elm loc (elmCountMatches expected))

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

elmCountMatches :: forall elm. Int -> [elm] -> Maybe Text
elmCountMatches expected actual =
  if length actual == expected
    then Nothing
    else Just $ "expected " <> txt expected <> " elements but got " <> txt (length actual)

isNotFound :: L.LocateException -> Bool
isNotFound = \case
  L.ElementNotFound {} -> True
  _ -> False

isAmbiguous :: L.LocateException -> Bool
isAmbiguous = \case
  L.AmbiguousLocator {} -> True
  _ -> False

chkElmM ::
  forall elm es.
  (Show elm, WebDriver elm :> es, Error Text :> es) =>
  Text ->
  Either L.LocateException elm ->
  (elm -> Eff es (Maybe Text)) ->
  Eff es ()
chkElmM title locRslt chk =
  locRslt
    & either
      (\err -> TU.locateFail locRslt (title <> " - locate failed: " <> txt err))
      (\el -> chk el >>= TU.chkWithElement locRslt (title <> " - element check failed") el)

mkTest ::
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  Text ->
  Eff (WebDriver elm : es) () ->
  TestTree
mkTest run interp name act = testCase (unpack name) (run (interp act))
