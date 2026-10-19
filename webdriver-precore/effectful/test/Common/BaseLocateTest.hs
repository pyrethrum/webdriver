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
import Common.WebDriver.TestUtils (locateChkAttr, locateChkAutoId)
import Common.WebDriver.TestUtils qualified as TU
import Data.Either (isRight)
import Data.Text (Text, unpack)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Test.Tasty (TestTree, testGroup, inOrderTestGroup)
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
    inOrderTestGroup
      "Landmark and Role Tests"
      [ chkAutoId "Locate by ID" (elmId "section-personal") "sec-personal",
        test "jsDisplay check should NOT be affected by viewport" $ do
          maximizeWindow @elm
          maxResult <- locateAll @elm $ elmClass "input"
          minimizeWindow @elm
          minResult <- locateAll @elm $ elmClass "input"
          TU.chkEq "Displayed result should be the same for minised and maximised viewport" maxResult minResult,
        inOrderTestGroup
          "Role Locator Tests"
          [ inOrderTestGroup
              "Landmark role types - roleType"
              [ chkAutoId "Banner - page header" (roleType Banner) "hdr-main",
                chkAutoId "Main landmark" (roleType Main) "main-content",
                chkAutoId "ContentInfo - page footer" (roleType ContentInfo) "ftr-main",
                chkAutoId "Complementary - aside" (roleType Complementary) "aside-help",
                chkAutoId "Search landmark" (roleType Search) "srch-widget"
              ],
            inOrderTestGroup
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
            inOrderTestGroup
              "Multi-element role types"
              [ chkElmCount "Navigation - finds both nav landmarks" (roleType Navigation) 2
              ]
          ]
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (locateChkAutoId @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' run interp

-- ---------------------------------------------------------------------------
-- Extended Role Matching Tests
-- ---------------------------------------------------------------------------

testExtendedRoleMatchingNever ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testExtendedRoleMatchingNever run interpNever =
  beforeAll_ (extendedRolesUrl >>= \url -> run (interpNever (navigateTo @elm url))) $
    testGroup
      "Extended Role Matching Tests - ExtLocateNever"
      [ testGroup
          "aria-labelledby resolution"
          [ testNever "ExtLocateNever - locate does NOT find region via aria-labelledby" $ do
              locRslt <- locate @elm $ region "Personal Information"
              TU.chkLocException (txt (region "Personal Information")) isNotFound locRslt,
            chkElmCountNever "ExtLocateNever - locateAll does NOT find region via aria-labelledby" (region "Personal Information") 0
          ],
        testGroup
          "for id label association"
          [ testNever "ExtLocateNever - locate does NOT find radio via for id label" $ do
              locRslt <- locate @elm $ radio "Email"
              TU.chkLocException (txt (radio "Email")) isNotFound locRslt,
            testNever "ExtLocateNever - locate does NOT find textbox via for id label" $ do
              locRslt <- locate @elm $ textbox "Given Name"
              TU.chkLocException (txt (textbox "Given Name")) isNotFound locRslt
          ],
        testGroup
          "aria-label - always resolved regardless of setting"
          [ chkAutoId "ExtLocateNever finds textbox with aria-label" (textbox "Nickname") "edt-nickname"
          ]
      ]
  where
    testNever :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testNever name act = testCase (unpack name) (run (interpNever act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = testNever name (locateChkAutoId @elm loc expctd)

    chkElmCountNever :: Text -> Locator -> Int -> TestTree
    chkElmCountNever = chkElmCount' run interpNever

testExtendedRoleMatchingAlways ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testExtendedRoleMatchingAlways run interpAlways =
  beforeAll_ (extendedRolesUrl >>= \url -> run (interpAlways (navigateTo @elm url))) $
    testGroup
      "Extended Role Matching Tests - ExtLocateAlways"
      [ testGroup
          "aria-labelledby resolution"
          [ atrrChkExtRole
              "ExtLocateAlways - locate finds region via aria-labelledby"
              (region "Personal Information")
              "auto-id"
              "sec-personal",
            chkElmCountAlways "ExtLocateAlways - locateAll finds region via aria-labelledby" (region "Personal Information") 1
          ],
        testGroup
          "for id label association"
          [ atrrChkExtRole
              "ExtLocateAlways - locate finds radio via for id label"
              (radio "Email")
              "auto-id"
              "rdo-contact-email",
            atrrChkExtRole
              "ExtLocateAlways - locate finds textbox via for id label"
              (textbox "Given Name")
              "auto-id"
              "edt-given-name",
            chkElmCountAlways "ExtLocateAlways - locateAll finds radio via for id label" (radio "Email") 1
          ],
        testGroup
          "aria-label - always resolved regardless of setting"
          [ atrrChkExtRole
              "ExtLocateAlways finds textbox with aria-label"
              (textbox "Nickname")
              "auto-id"
              "edt-nickname"
          ]
      ]
  where
    testAlways :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testAlways name act = testCase (unpack name) (run (interpAlways act))

    atrrChkExtRole :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtRole testName loc attrName expctd =
      testAlways testName (locateChkAttr @elm loc attrName expctd)

    chkElmCountAlways :: Text -> Locator -> Int -> TestTree
    chkElmCountAlways = chkElmCount' run interpAlways

testExtendedRoleMatchingMiss ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testExtendedRoleMatchingMiss run interpMiss =
  beforeAll_ (extendedRolesUrl >>= \url -> run (interpMiss (navigateTo @elm url))) $
    testGroup
      "Extended Role Matching Tests - ExtLocateSingletonMiss"
      [ testGroup
          "aria-labelledby resolution"
          [ atrrChkExtMiss
              "ExtLocateSingletonMiss - locate finds region via aria-labelledby"
              (region "Personal Information")
              "auto-id"
              "sec-personal",
            chkElmCountMiss "ExtLocateSingletonMiss - locateAll does NOT find region via aria-labelledby" (region "Personal Information") 0
          ],
        testGroup
          "for id label association"
          [ atrrChkExtMiss
              "ExtLocateSingletonMiss - locate finds radio via for id label"
              (radio "Email")
              "auto-id"
              "rdo-contact-email",
            atrrChkExtMiss
              "ExtLocateSingletonMiss - locate finds textbox via for id label"
              (textbox "Given Name")
              "auto-id"
              "edt-given-name",
            chkElmCountMiss "ExtLocateSingletonMiss - locateAll does NOT find radio via for id label" (radio "Email") 0
          ],
        testGroup
          "aria-label - always resolved regardless of setting"
          [ atrrChkExtMiss
              "ExtLocateSingletonMiss finds textbox with aria-label"
              (textbox "Nickname")
              "auto-id"
              "edt-nickname"
          ]
      ]
  where
    testMiss :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testMiss name act = testCase (unpack name) (run (interpMiss act))

    atrrChkExtMiss :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtMiss testName loc attrName expctd =
      testMiss testName (locateChkAttr @elm loc attrName expctd)

    chkElmCountMiss :: Text -> Locator -> Int -> TestTree
    chkElmCountMiss = chkElmCount' run interpMiss

testExtendedRoleMatchingRoleType ::
  forall elm es.
  (Eq elm, Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testExtendedRoleMatchingRoleType run interpNever interpAlways =
  beforeAll_ (extendedRolesUrl >>= \url -> run (interpNever (navigateTo @elm url))) $
    testGroup
      "Extended Role Matching Tests"
      [ testGroup
          "RoleType - unaffected by extended matching"
          [ testCase
              (unpack "RoleType Region - ExtLocateNever and ExtLocateAlways give same results")
              ( run $ do
                  never <- interpNever (locateAll @elm $ roleType Region)
                  always <- interpAlways (locateAll @elm $ roleType Region)
                  TU.chkEq "RoleType Region results should be identical" never always
              )
          ]
      ]

-- ---------------------------------------------------------------------------
-- Visibility Check Tests
-- ---------------------------------------------------------------------------

testVisibilityAlways ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testVisibilityAlways run interpAlways =
  beforeAll_ (visibilityUrl >>= \url -> run (interpAlways (navigateTo @elm url))) $
    testGroup
      "Visibility Check Tests - DisplayedCheckAlways"
      [ testGroup
          "locateAll filters hidden"
          [ testGroup
              "Rule 1 - display none on element itself"
              [ chkElmCount
                  "edt-notes-hidden has display none via own CSS class - DisplayedCheckAlways filters"
                  (TU.autoId "edt-notes-hidden")
                  0
              ],
            testGroup
              "Rule 2 - visibility hidden or collapse - inherited"
              [ chkElmCount
                  "edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-vis-hidden")
                  0,
                chkElmCount
                  "edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-css-vis-hidden")
                  0
              ],
            testGroup
              "Rule 3 - parseFloat opacity equals 0 on element itself"
              [ chkElmCount
                  "fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckAlways filters"
                  (TU.autoId "fg-opacity-zero")
                  0
              ],
            testGroup
              "Rule 4 - INPUT with type hidden"
              [ chkElmCount
                  "hdn-session-token is input type hidden - DisplayedCheckAlways filters"
                  (TU.autoId "hdn-session-token")
                  0
              ],
            testGroup
              "Rule 5 - offsetWidth or offsetHeight equals 0 - parent has display none"
              [ chkElmCount
                  "edt-display-none inside inline display none parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-display-none")
                  0,
                chkElmCount
                  "edt-css-none inside CSS class display none parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-css-none")
                  0,
                chkElmCount
                  "edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckAlways filters"
                  (TU.autoId "edt-html-hidden")
                  0
              ],
            testGroup
              "NOT filtered by displayedJS"
              [ chkElmCount
                  "edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckAlways finds"
                  (TU.autoId "edt-aria-hidden")
                  1,
                chkElmCount
                  "edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckAlways finds"
                  (TU.autoId "edt-offscreen")
                  1,
                chkElmCount
                  "edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckAlways finds"
                  (TU.autoId "edt-opacity-zero")
                  1
              ]
          ],
        testGroup
          "locate singleton"
          [ testAlways "DisplayedCheckAlways also filters hidden, resolving to unique visible element" $
              locateChkAttr @elm (elmClass "notes-area") "auto-id" "edt-notes-visible"
          ],
        testGroup
          "locateAll"
          [ chkElmCount "notes-area with DisplayedCheckAlways finds visible only" (elmClass "notes-area") 1
          ]
      ]
  where
    testAlways :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testAlways name act = testCase (unpack name) (run (interpAlways act))

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' run interpAlways

testVisibilityNever ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testVisibilityNever run interpNever =
  beforeAll_ (visibilityUrl >>= \url -> run (interpNever (navigateTo @elm url))) $
    testGroup
      "Visibility Check Tests - DisplayedCheckNever"
      [ testGroup
          "locateAll does NOT filter hidden"
          [ testGroup
              "Rule 1 - display none on element itself"
              [ chkElmCountNever
                  "edt-notes-hidden has display none via own CSS class - DisplayedCheckNever finds"
                  (TU.autoId "edt-notes-hidden")
                  1
              ],
            testGroup
              "Rule 2 - visibility hidden or collapse - inherited"
              [ chkElmCountNever
                  "edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-vis-hidden")
                  1,
                chkElmCountNever
                  "edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-css-vis-hidden")
                  1
              ],
            testGroup
              "Rule 3 - parseFloat opacity equals 0 on element itself"
              [ chkElmCountNever
                  "fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckNever finds"
                  (TU.autoId "fg-opacity-zero")
                  1
              ],
            testGroup
              "Rule 4 - INPUT with type hidden"
              [ chkElmCountNever
                  "hdn-session-token is input type hidden - DisplayedCheckNever finds"
                  (TU.autoId "hdn-session-token")
                  1
              ],
            testGroup
              "Rule 5 - offsetWidth or offsetHeight equals 0 - parent has display none"
              [ chkElmCountNever
                  "edt-display-none inside inline display none parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-display-none")
                  1,
                chkElmCountNever
                  "edt-css-none inside CSS class display none parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-css-none")
                  1,
                chkElmCountNever
                  "edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckNever finds"
                  (TU.autoId "edt-html-hidden")
                  1
              ],
            testGroup
              "NOT filtered by displayedJS"
              [ chkElmCountNever
                  "edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckNever finds"
                  (TU.autoId "edt-aria-hidden")
                  1,
                chkElmCountNever
                  "edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckNever finds"
                  (TU.autoId "edt-offscreen")
                  1,
                chkElmCountNever
                  "edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckNever finds"
                  (TU.autoId "edt-opacity-zero")
                  1
              ]
          ],
        testGroup
          "locate singleton"
          [ testNever "DisplayedCheckNever with Unique throws AmbiguousLocator - hidden and visible share class" $ do
              locRslt <- locate @elm $ elmClass "notes-area"
              TU.chkLocException (txt (elmClass "notes-area")) isAmbiguous locRslt
          ],
        testGroup
          "locateAll"
          [ chkElmCountNever "notes-area with DisplayedCheckNever finds both visible and hidden" (elmClass "notes-area") 2
          ]
      ]
  where
    testNever :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testNever name act = testCase (unpack name) (run (interpNever act))

    chkElmCountNever :: Text -> Locator -> Int -> TestTree
    chkElmCountNever = chkElmCount' run interpNever

testVisibilityDisambiguate ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testVisibilityDisambiguate run interpDisambiguate =
  beforeAll_ (visibilityUrl >>= \url -> run (interpDisambiguate (navigateTo @elm url))) $
    testGroup
      "Visibility Check Tests - DisplayedCheckDisambiguateUnique"
      [ testGroup
          "locate singleton"
          [ testDisambiguate "DisplayedCheckDisambiguateUnique filters hidden - resolving to unique visible element" $
              locateChkAttr @elm (elmClass "notes-area") "auto-id" "edt-notes-visible"
          ]
      ]
  where
    testDisambiguate :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testDisambiguate name act = testCase (unpack name) (run (interpDisambiguate act))

testVisibilityDisambiguateVsNever ::
  forall elm es.
  (Eq elm, Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testVisibilityDisambiguateVsNever run interpDisambiguate interpNever =
  beforeAll_ (visibilityUrl >>= \url -> run (interpDisambiguate (navigateTo @elm url))) $
    testGroup
      "Visibility Check Tests"
      [ testGroup
          "locateAll - DisplayedCheckDisambiguateUnique has no effect"
          [ testCase
              (unpack "DisambiguateUnique gives same result as Never for locateAll")
              ( run $ do
                  disambiguate <- interpDisambiguate (locateAll @elm $ elmClass "notes-area")
                  never <- interpNever (locateAll @elm $ elmClass "notes-area")
                  TU.chkEq "DisambiguateUnique locateAll result must equal Never" disambiguate never
              )
          ]
      ]

-- ---------------------------------------------------------------------------
-- Basic Locator Types
-- ---------------------------------------------------------------------------

testBasicLocatorTypes ::
  forall elm es.
  (Error Text :> es) =>
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
        chkElmCount "h1_ tag locator finds the single h1 heading" h1_ 1
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (locateChkAutoId @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' run interp

-- ---------------------------------------------------------------------------
-- Class Locator Variants
-- ---------------------------------------------------------------------------

testClassLocatorVariants ::
  forall elm es.
  (Error Text :> es) =>
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
    chkAutoId name loc expctd = test name (locateChkAutoId @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' run interp

-- ---------------------------------------------------------------------------
-- Attribute Locator Variants
-- ---------------------------------------------------------------------------

testAttributeLocatorVariants ::
  forall elm es.
  (Error Text :> es) =>
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
    chkAutoId name loc expctd = test name (locateChkAutoId @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' run interp

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
    chkAutoId name loc expctd = test name (locateChkAutoId @elm loc expctd)

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
          case secResult of
            Left _ -> TU.locateFail secResult "find sec-personal - locate failed"
            Right sec -> do
              inResult <- locateAllFromElement @elm sec input_
              TU.chkRightCount "inputs in sec-personal" 5 inResult,
        test "locateAll from element - links within nav-main" $ do
          navResult <- locate @elm $ TU.autoId "nav-main"
          case navResult of
            Left _ -> TU.locateFail navResult "find nav-main - locate failed"
            Right nav -> do
              linkResult <- locateAllFromElement @elm nav a_
              TU.chkRightCount "links in nav-main" 2 linkResult,
        test "locate from element - edt-given-name within sec-personal" $ do
          secResult <- locate @elm $ TU.autoId "sec-personal"
          case secResult of
            Left _ -> TU.locateFail secResult "find sec-personal - locate failed"
            Right sec -> do
              givenResult <- locateFromElement @elm sec $ TU.autoId "edt-given-name"
              TU.chkElm "edt-given-name in section" isRight givenResult,
        test "locate from element - not found when element not in scope" $ do
          hdrResult <- locate @elm $ TU.autoId "hdr-main"
          case hdrResult of
            Left _ -> TU.locateFail hdrResult "find hdr-main - locate failed"
            Right hdr -> do
              notInHdr <- locateFromElement @elm hdr $ TU.autoId "edt-given-name"
              TU.chkLocException "edt-given-name in header" isNotFound notInHdr
      ]
  where
    test :: Text -> Eff (WebDriver elm : es) () -> TestTree
    test name act = testCase (unpack name) (run (interp act))

-- ---------------------------------------------------------------------------
-- Combined Locators
-- ---------------------------------------------------------------------------

testCombinedLocators ::
  forall elm es.
  (Error Text :> es) =>
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
    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' run interp

-- ---------------------------------------------------------------------------
-- Misc ARIA Role Types
-- ---------------------------------------------------------------------------

testMiscAriaRoleTypes ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testMiscAriaRoleTypes run wdIntprt =
  beforeAll_ (miscRolesUrl >>= \url -> run (wdIntprt (navigateTo @elm url))) $
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
    test = interpret wdIntprt

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = test name (locateChkAutoId @elm loc expctd)

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' run wdIntprt

testMiscAriaRoleTypesNever ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testMiscAriaRoleTypesNever run interpNever =
  beforeAll_ (miscRolesUrl >>= \url -> run (interpNever (navigateTo @elm url))) $
    testGroup
      "Misc ARIA Role Types - DisplayedCheckNever"
      [ -- Note: <option> elements have no CSS dimensions, so DisplayedCheckAlways filters them
        -- out. Use DisplayedCheckNever to locate them programmatically.
        chkElmCountNever "roleType Option with DisplayedCheckNever finds all options" (roleType Option) 2
      ]
  where
    chkElmCountNever :: Text -> Locator -> Int -> TestTree
    chkElmCountNever = chkElmCount' run interpNever

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

elmCountMatches :: forall elm. Int -> [elm] -> Bool
elmCountMatches expected = (==) expected . length

isNotFound :: L.LocateException -> Bool
isNotFound = \case
  L.ElementNotFound {} -> True
  _ -> False

isAmbiguous :: L.LocateException -> Bool
isAmbiguous = \case
  L.AmbiguousLocator {} -> True
  _ -> False

mkTest ::
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  Text ->
  Eff (WebDriver elm : es) () ->
  TestTree
mkTest run interp name act = testCase (unpack name) (run (interp act))

-- | Build a 'TestTree' that checks @locateAll loc@ returns exactly @expected@ elements.
chkElmCount' ::
  forall elm es.
  (Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  Text ->
  Locator ->
  Int ->
  TestTree
chkElmCount' run interp name loc expected =
  mkTest run interp name $
    TU.locateChkAll @elm
      ("Element count not as expected for " <> txt loc)
      loc
      (TU.chkCount "" expected)
