module HTTP.BaseLocateTest where


import Common.Utils (beforeAll_, DriverActions (..),chkEq, chkLocException, chkSingleton, chkEmpty, autoId )
import Common.Utils qualified as U
import Data.Text (Text)
import Effectful
import HTTP.Runner (WDSession, runHttp, runHttpTest, testUrl)
import Prelude
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource)
import WebDriverPreCore.Utils.Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, URL)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators as LS
import WebDriverPreCore.Test.TestData
import WebDriver.Effectful.Logger (Logger)
import Common.SessionInit (getWDSession, closeWDSession)

-- >>> _eval tests
-- *** Exception: ExitSuccess
tests :: TestTree
tests =
  withResource (getWDSession False) closeWDSession runSessionTests
  where
  runSessionTests :: IO WDSession -> TestTree
  runSessionTests ses =
    inOrderTestGroup "Base Locate Tests"
      [ -- Landmark roles and basic element locators on locator-landmark-roles.html
        beforeAll_ (navToUrl landmarkRolesUrl) $
          testGroup "Landmark and Role Tests"
              [ chkAutoId "Locate by ID" (elmId "section-personal") "sec-personal"
              , test "jsDisplay check should NOT be affected by viewport" $ do
                  maximizeWindow
                  maxResult <- locateAll $ elmClass "input"
                  minimizeWindow
                  minResult <- locateAll $ elmClass "input"
                  chkEq "Displayed result should be the same for minised and maximised viewport" maxResult minResult
              , testGroup "Role Locator Tests"
                  [ testGroup "Landmark role types - roleType"
                      [ chkAutoId "Banner - page header" (roleType Banner) "hdr-main"
                      , chkAutoId "Main landmark" (roleType Main) "main-content"
                      , chkAutoId "ContentInfo - page footer" (roleType ContentInfo) "ftr-main"
                      , chkAutoId "Complementary - aside" (roleType Complementary) "aside-help"
                      , chkAutoId "Search landmark" (roleType Search) "srch-widget"
                      ]
                  , testGroup "Role with name - aria-label"
                      [ chkAutoId "Navigation - Main navigation" (navigation "Main navigation") "nav-main"
                      , chkAutoId "Navigation - Breadcrumb" (navigation "Breadcrumb") "nav-breadcrumb"
                      , chkAutoId "Form - Mega test form" (form "Mega test form") "frm-mega"
                      , chkAutoId "Complementary - Help and tips" (complementary "Help and tips") "aside-help"
                      , chkAutoId "Button - submit by aria-label" (button "Submit the mega form") "btn-submit"
                      , chkAutoId "Button - span with explicit role override" (button "Span acting as button") "btn-span-role"
                      , chkAutoId "Button - link with explicit role override" (button "Link acting as button") "btn-link-role"
                      , chkAutoId "Textbox - Nickname via aria-label" (textbox "Nickname") "edt-nickname"
                      , chkAutoId "Checkbox - Read documents via aria-label" (checkbox "Read documents") "chk-docs-read"
                      , chkAutoId "Img - div with explicit role override" (img "Abstract coloured shape") "img-div-role"
                      ]
                  , testGroup "Multi-element role types"
                      [ chkElmCount "Navigation - finds both nav landmarks" (roleType Navigation) 2
                      ]
                  ]
              ]

      , -- Extended role matching (aria-labelledby, for id label) on locator-extended-roles.html
        beforeAll_ (navToUrl extendedRolesUrl) $
          testGroup "Extended Role Matching Tests"
              [ testGroup "aria-labelledby resolution"
                  [ 
                    atrrChkExtRole "ExtLocateAlways - locate finds region via aria-labelledby"
                      (region "Personal Information") "auto-id" "sec-personal"
                  , atrrChkExtMiss "ExtLocateSingletonMiss - locate finds region via aria-labelledby"
                      (region "Personal Information") "auto-id" "sec-personal"
                  , test "ExtLocateNever - locate does NOT find region via aria-labelledby" $ do
                      locRslt <- locate $ region "Personal Information"
                      chkLocException (txt (region "Personal Information")) isNotFound locRslt, 
                    test "ExtLocateAlways - locateAll finds region via aria-labelledby" $ do
                      locRslt <- locateAllExt $ region "Personal Information"
                      chkElms (txt (region "Personal Information")) chkSingleton locRslt
                  , test "ExtLocateSingletonMiss - locateAll does NOT find region via aria-labelledby" $ do
                      locRslt <- locateAllExtMiss $ region "Personal Information"
                      chkElms (txt (region "Personal Information")) chkEmpty locRslt
                  , test "ExtLocateNever - locateAll does NOT find region via aria-labelledby" $ do
                      locRslt <- locateAll $ region "Personal Information"
                      chkElms (txt (region "Personal Information")) chkEmpty locRslt
                  ]
              , testGroup "for id label association"
                  [ atrrChkExtRole "ExtLocateAlways - locate finds radio via for id label"
                      (radio "Email") "auto-id" "rdo-contact-email"
                  , atrrChkExtMiss "ExtLocateSingletonMiss - locate finds radio via for id label"
                      (radio "Email") "auto-id" "rdo-contact-email"
                  , test "ExtLocateNever - locate does NOT find radio via for id label" $ do
                      locRslt <- locate $ radio "Email"
                      chkLocException (txt (radio "Email")) isNotFound locRslt
                  , atrrChkExtRole "ExtLocateAlways - locate finds textbox via for id label"
                      (textbox "Given Name") "auto-id" "edt-given-name"
                  , atrrChkExtMiss "ExtLocateSingletonMiss - locate finds textbox via for id label"
                      (textbox "Given Name") "auto-id" "edt-given-name"
                  , test "ExtLocateNever - locate does NOT find textbox via for id label" $ do
                      locRslt <- locate $ textbox "Given Name"
                      chkLocException (txt (textbox "Given Name")) isNotFound locRslt
                  , test "ExtLocateAlways - locateAll finds radio via for id label" $ do
                      locRslt <- locateAllExt $ radio "Email"
                      chkElms (txt (radio "Email")) chkSingleton locRslt
                  , test "ExtLocateSingletonMiss - locateAll does NOT find radio via for id label" $ do
                      locRslt <- locateAllExtMiss $ radio "Email"
                      chkElms (txt (radio "Email")) chkEmpty locRslt
                  ]
              , testGroup "RoleType - unaffected by extended matching"
                  [ test "RoleType Region - ExtLocateNever and ExtLocateAlways give same results" $ do
                      never <- locateAll $ roleType Region
                      always <- locateAllExt $ roleType Region
                      chkEq "RoleType Region results should be identical" never always
                  ]
              , testGroup "aria-label - always resolved regardless of setting"
                  [ chkAutoId "ExtLocateNever finds textbox with aria-label"
                      (textbox "Nickname") "edt-nickname"
                  , atrrChkExtRole "ExtLocateAlways finds textbox with aria-label"
                      (textbox "Nickname") "auto-id" "edt-nickname"
                  , atrrChkExtMiss "ExtLocateSingletonMiss finds textbox with aria-label"
                      (textbox "Nickname") "auto-id" "edt-nickname"
                  ]
              ]

      , -- Visibility checks on locator-visibility.html
        beforeAll_ (navToUrl visibilityUrl) $
          testGroup "Visibility Check Tests"
              [ testGroup "locateAll - DisplayedCheckAlways filters hidden and DisplayedCheckNever does not"
                  [ testGroup "Rule 1 - display none on element itself"
                      [ chkElmCount "edt-notes-hidden has display none via own CSS class - DisplayedCheckAlways filters" 
                          (autoId "edt-notes-hidden") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-notes-hidden has display none via own CSS class - DisplayedCheckNever finds" 
                          (autoId "edt-notes-hidden") 1
                      ]
                  , testGroup "Rule 2 - visibility hidden or collapse - inherited"
                      [ chkElmCount "edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckAlways filters" 
                          (autoId "edt-vis-hidden") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckNever finds" 
                          (autoId "edt-vis-hidden") 1
                      , chkElmCount "edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckAlways filters" 
                          (autoId "edt-css-vis-hidden") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckNever finds" 
                          (autoId "edt-css-vis-hidden") 1
                      ]
                  , testGroup "Rule 3 - parseFloat opacity equals 0 on element itself"
                      [ chkElmCount "fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckAlways filters" 
                          (autoId "fg-opacity-zero") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckNever finds" 
                          (autoId "fg-opacity-zero") 1
                      ]
                  , testGroup "Rule 4 - INPUT with type hidden"
                      [ chkElmCount "hdn-session-token is input type hidden - DisplayedCheckAlways filters" 
                          (autoId "hdn-session-token") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "hdn-session-token is input type hidden - DisplayedCheckNever finds" 
                          (autoId "hdn-session-token") 1
                      ]
                  , testGroup "Rule 5 - offsetWidth or offsetHeight equals 0 - parent has display none"
                      [ chkElmCount "edt-display-none inside inline display none parent - DisplayedCheckAlways filters" 
                          (autoId "edt-display-none") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-display-none inside inline display none parent - DisplayedCheckNever finds" 
                          (autoId "edt-display-none") 1
                      , chkElmCount "edt-css-none inside CSS class display none parent - DisplayedCheckAlways filters" 
                          (autoId "edt-css-none") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-css-none inside CSS class display none parent - DisplayedCheckNever finds" 
                          (autoId "edt-css-none") 1
                      , chkElmCount "edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckAlways filters" 
                          (autoId "edt-html-hidden") 0
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckNever finds" 
                          (autoId "edt-html-hidden") 1
                      ]
                  , testGroup "NOT filtered by displayedJS"
                      [ chkElmCount "edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckAlways finds" 
                          (autoId "edt-aria-hidden") 1
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckNever finds" 
                          (autoId "edt-aria-hidden") 1
                      , chkElmCount "edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckAlways finds" 
                          (autoId "edt-offscreen") 1
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckNever finds" 
                          (autoId "edt-offscreen") 1
                      , chkElmCount "edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckAlways finds" 
                          (autoId "edt-opacity-zero") 1
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckNever finds" 
                          (autoId "edt-opacity-zero") 1
                      ]
                  ]
              , testGroup "locate singleton - DisplayedCheckDisambiguateUnique resolves hidden-visible ambiguity"
                  [ test "DisplayedCheckNever with Unique throws AmbiguousLocator - hidden and visible share class" $ do
                      locRslt <- locateNever $ elmClass "notes-area"
                      chkLocException (txt (elmClass "notes-area")) isAmbiguous locRslt
                  , test "DisplayedCheckDisambiguateUnique filters hidden - resolving to unique visible element" $ do
                      locRslt <- locateDisambiguate $ elmClass "notes-area"
                      chkAttributeEqElm (txt (elmClass "notes-area")) "auto-id" "edt-notes-visible" locRslt
                  , test "DisplayedCheckAlways also filters hidden, resolving to unique visible element" $ do
                      locRslt <- locate $ elmClass "notes-area"
                      chkAttributeEqElm (txt (elmClass "notes-area")) "auto-id" "edt-notes-visible" locRslt
                  ]
              , testGroup "locateAll - DisplayedCheckDisambiguateUnique has no effect - only Always filters"
                  [ test "DisambiguateUnique gives same result as Never for locateAll" $ do
                      disambiguate <- locateAllDisambiguate $ elmClass "notes-area"
                      never        <- locateAllNever $ elmClass "notes-area"
                      chkEq "DisambiguateUnique locateAll result must equal Never" disambiguate never
                  , testGroup "DisplayedCheckAlways filters hidden in locateAll - Never returns both"
                      [ chkElmCount "notes-area with DisplayedCheckAlways finds visible only"  (elmClass "notes-area") 1
                      , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed}
                          "notes-area with DisplayedCheckNever finds both visible and hidden" 
                          (elmClass "notes-area") 2
                      ]
                  ]
              ]

      , beforeAll_ (navToUrl landmarkRolesUrl) $
          testGroup "Basic Locator Types"
              [ chkAutoId "defaultId resolves via mkDefaultLoc option" (defaultId "hdr-main") "hdr-main"
              , chkElmCount "allElms finds all page elements" allElms 42
              , chkAutoId "elmId finds element by HTML id" (elmId "megaforma") "frm-mega"
              , chkAutoId "css attribute selector" (css "[auto-id='ftr-main']") "ftr-main"
              , chkAutoId "xpath finds element by tag" (xpath "//footer") "ftr-main"
              , chkElmCount "input_ tag locator finds all inputs" input_ 7
              , chkElmCount "button_ tag locator finds button elements" button_ 2
              , chkAll "h1_ tag locator finds the single h1 heading" h1_ chkSingleton
              ]

      , beforeAll_ (navToUrl landmarkRolesUrl) $
          testGroup "Class Locator Variants"
              [ chkElmCount "elmClass contains match" (elmClass "text-input") 7
              , chkElmCount "elmClassExact full-equality match" (elmClass "text-input") 7
              , chkElmCount "elemClassStarts starts-with match" (elemClassStarts "text") 7
              , chkAutoId "elmClass finds element by single class name" (elmClass "span-button") "btn-span-role"
              ]

      , beforeAll_ (navToUrl landmarkRolesUrl) $
          testGroup "Attribute Locator Variants"
              [ chkAutoId "attribute default contains match" (attribute "auto-id" "hdr-main") "hdr-main"
              , chkAutoId "attributeExact full-equality match" (attribute "auto-id" "hdr-main") "hdr-main"
              , chkElmCount "attributeStarts starts-with match" (attributeStarts "auto-id" "nav") 4
              , chkElmCount "attribute full case-sensitive finds type text inputs" (attribute' "type" Full CaseSensitive "text") 3
              , chkAutoId "attribute full case-insensitive matches uppercase value" (attribute' "auto-id" Full CaseInsensitive "HDR-MAIN") "hdr-main"
              ]

      , beforeAll_ (navToUrl landmarkRolesUrl) $
          testGroup "roleName and role Constructors"
              [ chkAutoId "roleName finds element by accessible name" (roleName "Submit the mega form") "btn-submit"
              , chkAutoId "roleName finds aside by aria-label" (roleName "Help and tips") "aside-help"
              , chkAutoId "roleName finds nav by aria-label" (roleName "Main navigation") "nav-main"
              , chkAutoId "role generic constructor - Navigation with name" (role Navigation "Breadcrumb") "nav-breadcrumb"
              ]

      , beforeAll_ (navToUrl landmarkRolesUrl) $
          testGroup "Locate and LocateAll from Element"
              [ test "locateAll from element - inputs within sec-personal" $ do
                  secResult <- locate $ autoId "sec-personal"
                  chkElmM "find sec-personal" secResult $ \sec -> do
                    inResult <- locateAllFromElement sec input_
                    chkElms "inputs in sec-personal" (elmCountMatches 5) inResult
                    pure Nothing
              , test "locateAll from element - links within nav-main" $ do
                  navResult <- locate $ autoId "nav-main"
                  chkElmM "find nav-main" navResult $ \nav -> do
                    linkResult <- locateAllFromElement nav a_
                    chkElms "links in nav-main" (elmCountMatches 2) linkResult
                    pure Nothing
              , test "locate from element - edt-given-name within sec-personal" $ do
                  secResult <- locate $ autoId "sec-personal"
                  chkElmM "find sec-personal" secResult $ \sec -> do
                    givenResult <- locateFromElement sec $ autoId "edt-given-name"
                    chkElm "edt-given-name in section" (\_ -> Nothing) givenResult
                    pure Nothing
              , test "locate from element - not found when element not in scope" $ do
                  hdrResult <- locate $ autoId "hdr-main"
                  chkElmM "find hdr-main" hdrResult $ \hdr -> do
                    notInHdr <- locateFromElement hdr $ autoId "edt-given-name"
                    pure $ case notInHdr of
                      Left (L.ElementNotFound {}) -> Nothing
                      Left other -> Just $ "expected ElementNotFound but got: " <> txt other
                      Right _ -> Just "expected ElementNotFound but edt-given-name was found in header"
              ]

      , beforeAll_ (navToUrl landmarkRolesUrl) $
          testGroup "Combined Locators"
              [ chkElmCount "AND - input_ and elmClass text-input" (input_ &&& elmClass "text-input") 6
              , chkElmCount "OR - h1_ or h2_ finds all headings" (h1_ ||| h2_) 3
              , chkElmCount "Descendant - sec-personal contains input_ finds contained inputs" (autoId "sec-personal" >>> input_) 5
              , chkElmCount "OR - roleType Navigation or roleType Search" (roleType Navigation ||| roleType Search) 3
              ]

      , beforeAll_ (navToUrl miscRolesUrl) $
          testGroup "Misc ARIA Role Types"
              [ chkAutoId "roleType Article" (roleType Article) "art-main"
              , chkAutoId "article by accessible name" (article "Test article") "art-main"
              , chkAutoId "roleType Heading - single heading on page" (roleType Heading) "hdg-article"
              , chkAutoId "heading by text content" (heading "Article Heading") "hdg-article"
              , chkAutoId "roleType Figure" (roleType Figure) "fig-sample"
              , chkAutoId "figure by accessible name" (figure "Sample figure") "fig-sample"
              , chkAutoId "roleType List - single list on page" (roleType List) "lst-nav"
              , chkAutoId "list by accessible name" (list "Navigation list") "lst-nav"
              , chkElmCount "roleType ListItem finds all list items" (roleType ListItem) 2
              , chkAutoId "link by text content" (link "Home") "lnk-home"
              , chkElmCount "roleType Link finds all links" (roleType Link) 2
              , chkAutoId "roleType Table" (roleType Table) "tbl-data"
              , chkAutoId "table by accessible name" (table "Data table") "tbl-data"
              , chkElmCount "roleType Row finds header and data rows" (roleType Row) 2
              , chkElmCount "roleType ColumnHeader finds both column headers" (roleType ColumnHeader) 2
              , chkAutoId "columnHeader by text content" (columnHeader "Name") "col-name"
              , chkAutoId "roleType RowHeader" (roleType RowHeader) "row-hdr-a"
              , chkAutoId "rowHeader by text content" (rowHeader "Row A") "row-hdr-a"
              , chkAutoId "roleType Cell" (roleType Cell) "cel-a1"
              , chkAutoId "cell by text content" (cell "Cell A1") "cel-a1"
              , chkAutoId "roleType Group finds fieldset" (roleType Group) "grp-options"
              , chkAutoId "group by accessible name" (group "Options Group") "grp-options"
              -- Note: <option> elements always have offsetWidth/offsetHeight of 0, even when the
              -- dropdown is visually open. Browser <select> dropdowns are rendered as native OS
              -- widgets (not DOM elements), so options never have CSS dimensions. DisplayedCheckAlways
              -- filters them out. Use DisplayedCheckNever to locate them programmatically.
              , chkElmCount "roleType Option finds no options (DisplayedCheckAlways)" (roleType Option) 0
              , chkElmCount' da {locateAllFn = locateAllNeverCheckDisplayed} 
                            "roleType Option with DisplayedCheckNever finds all options" 
                            (roleType Option) 2
              , chkAutoId "option by text content" (option "Alpha") "opt-alpha"
              , chkAutoId "roleType Separator" (roleType Separator) "sep-main"
              , chkAutoId "progressBar by accessible name" (progressBar "Upload progress") "prg-upload"
              , chkAutoId "slider by accessible name" (slider "Volume") "sld-volume"
              , chkAutoId "spinButton by accessible name" (spinButton "Item count") "spn-count"
              , chkAutoId "roleType Status" (roleType LS.Status) "out-result"
              , chkAutoId "status by accessible name" (LS.status "Calculation result") "out-result"
              , chkAutoId "roleType Term" (roleType Term) "trm-name"
              , chkAutoId "term by text content" (term "Name") "trm-name"
              , chkAutoId "roleType Definition" (roleType Definition) "def-name"
              , chkAutoId "definition by text content" (definition "John") "def-name"
              ]
      ]
    where
     
    testRunner = \name act -> runHttpTest ses name act
    getProperty = getElementProperty
    getAttribute = getElementAttribute
    locateFn = U.locateHttp U.defHttpOpts
    locateAllFn = U.locateAllHttp U.defHttpOpts
    locateAllNeverCheckDisplayed = U.locateAllHttp U.defHttpOpts { L.jsRecheckDisplayed = L.DisplayedCheckNever }
    
    da :: DriverActions (Eff '[WebDriverHttp, Logger, WaitPrimative, IOE])
    da = MkDriverActions { 
        testRunner,
        getProperty,
        getAttribute,
        locateFn,
        locateAllFn
    }

    test = runHttpTest ses

    chkElm = U.chkElm da

    chkElms = U.chkElms da

    -- Partially applied test helpers using shared functions from Common.Utils
    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId = U.chkAutoIdElm da

    chkElmCount :: Text -> Locator -> Int -> TestTree
    chkElmCount = chkElmCount' da

    chkElmCount' :: forall m. MonadIO m => DriverActions m -> Text -> Locator -> Int -> TestTree
    chkElmCount' dact header loc expected = U.chkAll dact header loc (elmCountMatches expected)
          
    elmCountMatches :: Int -> [ElementId] -> Maybe Text
    elmCountMatches expected actual =
        if length actual == expected
        then Nothing
        else Just $ "expected " <> txt expected <> " elements but got " <> txt (length actual)

    chkAll :: Text -> Locator -> ([ElementId] -> Maybe Text) -> TestTree
    chkAll = U.chkAll da

    chkAttributeEqElm = U.chkAttributeEqElm da

    chkElmM = U.chkElmM da

    atrrChkExtRole :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtRole testName loc attrName expctd =
      test testName $ locateExt loc >>= chkAttributeEqElm (txt loc) attrName expctd

    atrrChkExtMiss :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtMiss testName loc attrName expctd =
      test testName $ do
        locRslt <- locateExtMiss loc
        chkAttributeEqElm (txt loc) attrName expctd locRslt

    navToUrl :: IO URL -> IO WDSession
    navToUrl urlAction = do
        s <-ses
        runHttp s $ testUrl urlAction >>= navigateTo
        pure s

    locate = da.locateFn 

    locateAll = da.locateAllFn 

    locateFromElement = U.locateFromElementHttp U.defHttpOpts

    locateAllFromElement = U.locateAllFromElementHttp U.defHttpOpts

    withExtendedRoleLocation er = U.defHttpOpts { L.extendedRoleLocation = er }

    locateExt = U.locateHttp (withExtendedRoleLocation L.ExtLocateAlways)

    locateExtMiss = U.locateHttp (withExtendedRoleLocation L.ExtLocateSingletonMiss)

    locateAllExt = U.locateAllHttp (withExtendedRoleLocation L.ExtLocateAlways)

    locateAllExtMiss = U.locateAllHttp (withExtendedRoleLocation L.ExtLocateSingletonMiss)

    isNotFound :: L.LocateException -> Maybe Text
    isNotFound = \case
            L.ElementNotFound {} -> Nothing
            other -> Just $ "expected ElementNotFound but got: " <> txt other

    withDisplayCheck dc = U.defHttpOpts { L.jsRecheckDisplayed = dc }

    locateAllDisambiguate = U.locateAllHttp (withDisplayCheck L.DisplayedCheckDisambiguateUnique)
    locateAllNever = U.locateAllHttp (withDisplayCheck L.DisplayedCheckNever)

    locateNever = U.locateHttp (withDisplayCheck L.DisplayedCheckNever)
    locateDisambiguate = U.locateHttp (withDisplayCheck L.DisplayedCheckDisambiguateUnique)

    isAmbiguous :: L.LocateException -> Maybe Text
    isAmbiguous (L.AmbiguousLocator {}) = Nothing
    isAmbiguous other = Just $ "expected AmbiguousLocator but got: " <> txt other


_eval :: Maybe Text -> TestTree -> IO Bool
_eval = U.testPattern

_pattern :: Maybe Text
_pattern = Just "ExtLocateAlways finds textbox with aria-label"

-- Specific test
--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess

-- All tests
--- >>> _eval Nothing tests
-- Using debug local config
-- [2026-09-28 10:36:16][Debug] HTTP POST Url Http ("session" :| ["127.0.0.1"])
-- Base Locate Tests
--   Landmark and Role Tests
--     Locate by ID:                                                                                                 [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] HTTP POST Url Http ("maximize" :| ["window","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","9a2cd7c2-7091-4bd5-9dad-895d0d1fef79","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","fc098ddd-2287-4e15-8e47-61ed78d09fc7","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","82fc40bd-828d-4931-ad61-85053812c0a6","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","88230fb1-4cdc-4494-ae8c-4fafdbf9db41","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","24821ec3-7d9e-43be-8376-cab4369b495d","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:17][Debug] Response: 200
-- [2026-09-28 10:36:17][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","11d16197-cf7c-4ae9-aaf0-b42481ddaff9","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","fddd7f8f-2972-43fb-8486-3af5f19cd3c4","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","5de65dee-631f-46ef-8d5c-8c3a4bd848c8","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","fd7e984e-37b7-4dc0-85b1-ac911e896e01","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","f31ca356-ee33-4ddb-8708-11287618c16a","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","234a254b-c73a-4fd5-b989-dbb0ba431009","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","53e95188-ddd2-4f13-9692-80c9fd80b45d","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e153b161-6b46-4f68-bd31-120d68cfe879","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","42fd2c58-54a0-484b-bb17-6db5f130262a","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","88230fb1-4cdc-4494-ae8c-4fafdbf9db41","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","a2a2c5e7-756a-4412-a816-7e4b90a2cfd6","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- OK (0.28s)
--     jsDisplay check should NOT be affected by viewport:                                                           [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:18][Debug] Response: 200
-- [2026-09-28 10:36:18][Debug] HTTP POST Url Http ("minimize" :| ["window","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (1.69s)
--     Role Locator Tests
--       Landmark role types - roleType
--         Banner - page header:                                                                                     OK (0.28s)
--         Main landmark:                                                                                            OK (0.28s)
--         ContentInfo - page footer:                                                                                OK (0.28s)
--         Complementary - aside:                                                                                    OK (0.28s)
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
--         Search landmark:                                                                                          OK (0.28s)
--       Role with name - aria-label
--         Navigation - Main navigation:                                                                             OK (0.28s)
--         Navigation - Breadcrumb:                                                                                  OK (0.28s)
--         Form - Mega test form:                                                                                    OK (0.28s)
--         Complementary - Help and tips:                                                                            OK (0.28s)
--         Button - submit by aria-label:                                                                            OK (0.28s)
--         Button - span with explicit role override:                                                                OK (0.28s)
--         Button - link with explicit role override:                                                                OK (0.28s)
--         Textbox - Nickname via aria-label:                                                                        OK (0.28s)
--         Checkbox - Read documents via aria-label:                                                                 OK (0.28s)
--         Img - div with explicit role override:                                                                    OK (0.28s)
--       Multi-element role types
--         Navigation - finds both nav landmarks:                                                                    OK (0.28s)
--   Extended Role Matching Tests
--     aria-labelledby resolution
--       ExtLocateAlways - locate finds region via aria-labelledby:                                                  [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","2112d5a2-76f0-448a-9944-b126694c1c8c","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","2112d5a2-76f0-448a-9944-b126694c1c8c","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("aria-labelledby" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("aria-labelledby" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","dcd13f00-5bb8-4e11-aaf0-eecc7fc0cf47","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","9a4e4884-8ee7-40a1-ac6a-8504fa56a410","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","dcd13f00-5bb8-4e11-aaf0-eecc7fc0cf47","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","9a4e4884-8ee7-40a1-ac6a-8504fa56a410","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("aria-labelledby" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["305dcf76-fe04-4862-8f9f-4ebf94dd5145","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","dcd13f00-5bb8-4e11-aaf0-eecc7fc0cf47","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["305dcf76-fe04-4862-8f9f-4ebf94dd5145","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","9a4e4884-8ee7-40a1-ac6a-8504fa56a410","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["06f43ba9-3483-45ca-9b7f-c48dab1fde10","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["4315265f-d1a6-4241-a85a-93ffda01c180","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["06f43ba9-3483-45ca-9b7f-c48dab1fde10","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["4315265f-d1a6-4241-a85a-93ffda01c180","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["305dcf76-fe04-4862-8f9f-4ebf94dd5145","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","be467809-f0c2-4d32-82f7-5f97b53c14f4","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","2112d5a2-76f0-448a-9944-b126694c1c8c","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["06f43ba9-3483-45ca-9b7f-c48dab1fde10","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","be467809-f0c2-4d32-82f7-5f97b53c14f4","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","9a4e4884-8ee7-40a1-ac6a-8504fa56a410","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["4315265f-d1a6-4241-a85a-93ffda01c180","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","be467809-f0c2-4d32-82f7-5f97b53c14f4","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","9a4e4884-8ee7-40a1-ac6a-8504fa56a410","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("id" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["b7348ccd-d283-4077-a9d9-97d32f717bdd","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["b7348ccd-d283-4077-a9d9-97d32f717bdd","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.05s)
--       ExtLocateSingletonMiss - locate finds region via aria-labelledby:                                           [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("text" :| ["b7348ccd-d283-4077-a9d9-97d32f717bdd","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","dcd13f00-5bb8-4e11-aaf0-eecc7fc0cf47","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e1514783-7629-4882-98be-4024681d24dc","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","dcd13f00-5bb8-4e11-aaf0-eecc7fc0cf47","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.06s)
--       ExtLocateNever - locate does NOT find region via aria-labelledby:                                           OK
--       ExtLocateAlways - locateAll finds region via aria-labelledby:                                               OK (0.05s)
--       ExtLocateSingletonMiss - locateAll does NOT find region via aria-labelledby:                                OK
--       ExtLocateNever - locateAll does NOT find region via aria-labelledby:                                        OK (0.01s)
--     for id label association
--       ExtLocateAlways - locate finds radio via for id label:                                                      OK (0.06s)
--       ExtLocateSingletonMiss - locate finds radio via for id label:                                               [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.06s)
--       ExtLocateNever - locate does NOT find radio via for id label:                                               OK
--       ExtLocateAlways - locate finds textbox via for id label:                                                    OK (0.04s)
--       ExtLocateSingletonMiss - locate finds textbox via for id label:                                             OK (0.05s)
--       ExtLocateNever - locate does NOT find textbox via for id label:                                             OK
--       ExtLocateAlways - locateAll finds radio via for id label:                                                   OK (0.05s)
--       ExtLocateSingletonMiss - locateAll does NOT find radio via for id label:                                    OK (0.01s)
--     RoleType - unaffected by extended matching
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
--       RoleType Region - ExtLocateNever and ExtLocateAlways give same results:                                     OK (0.02s)
--     aria-label - always resolved regardless of setting
--       ExtLocateNever finds textbox with aria-label:                                                               OK (0.01s)
--       ExtLocateAlways finds textbox with aria-label:                                                              OK (0.04s)
--       ExtLocateSingletonMiss finds textbox with aria-label:                                                       OK (0.01s)
--   Visibility Check Tests
--     locateAll - DisplayedCheckAlways filters hidden and DisplayedCheckNever does not
--       Rule 1 - display none on element itself
--         edt-notes-hidden has display none via own CSS class - DisplayedCheckAlways filters:                       [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e0fa2317-43e6-4c40-b564-9258085b6f57","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.02s)
--         edt-notes-hidden has display none via own CSS class - DisplayedCheckNever finds:                          OK
--       Rule 2 - visibility hidden or collapse - inherited
--         edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckAlways filters:                     [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.02s)
--         edt-vis-hidden inside inline visibility hidden parent - DisplayedCheckNever finds:                        OK
--         edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckAlways filters:              OK (0.01s)
--         edt-css-vis-hidden inside CSS class visibility hidden parent - DisplayedCheckNever finds:                 OK
--       Rule 3 - parseFloat opacity equals 0 on element itself
--         fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckAlways filters:                        [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.02s)
--         fg-opacity-zero div has opacity 0 applied directly - DisplayedCheckNever finds:                           OK
--       Rule 4 - INPUT with type hidden
--         hdn-session-token is input type hidden - DisplayedCheckAlways filters:                                    OK
--         hdn-session-token is input type hidden - DisplayedCheckNever finds:                                       OK
--       Rule 5 - offsetWidth or offsetHeight equals 0 - parent has display none
--         edt-display-none inside inline display none parent - DisplayedCheckAlways filters:                        OK (0.02s)
--         edt-display-none inside inline display none parent - DisplayedCheckNever finds:                           OK
--         edt-css-none inside CSS class display none parent - DisplayedCheckAlways filters:                         OK (0.01s)
--         edt-css-none inside CSS class display none parent - DisplayedCheckNever finds:                            OK
--         edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckAlways filters:                       OK (0.01s)
--         edt-html-hidden inside HTML hidden attribute parent - DisplayedCheckNever finds:                          OK
--       NOT filtered by displayedJS
--         edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckAlways finds:                       [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.02s)
--         edt-aria-hidden - aria-hidden does not affect display - DisplayedCheckNever finds:                        OK
--         edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckAlways finds:           OK (0.02s)
--         edt-offscreen positioned off-viewport but has non-zero dimensions - DisplayedCheckNever finds:            OK
--         edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckAlways finds: OK (0.02s)
--         edt-opacity-zero input child of opacity 0 container - opacity not inherited - DisplayedCheckNever finds:  OK
--     locate singleton - DisplayedCheckDisambiguateUnique resolves hidden-visible ambiguity
--       DisplayedCheckNever with Unique throws AmbiguousLocator - hidden and visible share class:                   OK
--       DisplayedCheckDisambiguateUnique filters hidden - resolving to unique visible element:                      [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e0fa2317-43e6-4c40-b564-9258085b6f57","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.02s)
--       DisplayedCheckAlways also filters hidden, resolving to unique visible element:                              OK (0.02s)
--     locateAll - DisplayedCheckDisambiguateUnique has no effect - only Always filters
--       DisambiguateUnique gives same result as Never for locateAll:                                                OK (0.01s)
--       DisplayedCheckAlways filters hidden in locateAll - Never returns both
--         notes-area with DisplayedCheckAlways finds visible only:                                                  OK (0.02s)
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
--         notes-area with DisplayedCheckNever finds both visible and hidden:                                        OK
--   Basic Locator Types
--     defaultId resolves via mkDefaultLoc option:                                                                   [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","685a595b-9b8b-4bf4-b972-e6cf2c44d457","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","685a595b-9b8b-4bf4-b972-e6cf2c44d457","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e54293f0-cacb-4af8-b61a-58ca02d374d6","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","ee97fe1b-d6c2-4263-8044-6dcd03e51ebe","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     allElms finds all page elements:                                                                              [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK (0.01s)
--     elmId finds element by HTML id:                                                                               OK
--     css attribute selector:                                                                                       OK
--     xpath finds element by tag:                                                                                   OK
--     input_ tag locator finds all inputs:                                                                          OK
--     button_ tag locator finds button elements:                                                                    OK
--     h1_ tag locator finds the single h1 heading:                                                                  OK
--   Class Locator Variants
--     elmClass contains match:                                                                                      [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","871a0af1-6b8a-40fd-afec-cd38defe365e","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     elmClassExact full-equality match:                                                                            [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     elemClassStarts starts-with match:                                                                            [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     elmClass finds element by single class name:                                                                  OK
--   Attribute Locator Variants
--     attribute default contains match:                                                                             [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","b80bcb76-0de8-4c4e-b3fd-eb03b5603334","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","b80bcb76-0de8-4c4e-b3fd-eb03b5603334","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","b80bcb76-0de8-4c4e-b3fd-eb03b5603334","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     attributeExact full-equality match:                                                                           OK
--     attributeStarts starts-with match:                                                                            OK
--     attribute full case-sensitive finds type text inputs:                                                         OK
--     attribute full case-insensitive matches uppercase value:                                                      OK
--   roleName and role Constructors
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
--     roleName finds element by accessible name:                                                                    [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","290300f2-f068-4966-abc9-2c1ac7aded01","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","7bff557e-8a0d-4853-912d-e89ba3b9dcc6","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","65ddb51c-47d9-4fcd-9a72-7fa8a6c74e60","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","c31617d4-694d-478d-ac57-3cbeb136b105","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     roleName finds aside by aria-label:                                                                           [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     roleName finds nav by aria-label:                                                                             OK
--     role generic constructor - Navigation with name:                                                              OK
--   Locate and LocateAll from Element
--     locateAll from element - inputs within sec-personal:                                                          [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["f60e3801-67fd-4966-b9da-f0c9cdd4a13d","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["f38e7401-ab00-49ff-9ba1-c5cc0e42f61e","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["1c7225db-2707-40a9-89ba-9e7c8e6f54d7","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["1c7225db-2707-40a9-89ba-9e7c8e6f54d7","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     locateAll from element - links within nav-main:                                                               OK
--     locate from element - edt-given-name within sec-personal:                                                     OK
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
--     locate from element - not found when element not in scope:                                                    OK
--   Combined Locators
--     AND - input_ and elmClass text-input:                                                                         [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("elements" :| ["defbc317-0840-4170-86be-ad379d476cc6","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     OR - h1_ or h2_ finds all headings:                                                                           [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
--     Descendant - sec-personal contains input_ finds contained inputs:                                             [2026-09-28 10:36:19][Debug] Response: 200
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:19][Debug] Response: 200
-- OK
-- [2026-09-28 10:36:19][Debug] Response: 200
--     OR - roleType Navigation or roleType Search:                                                                  OK
-- [2026-09-28 10:36:19][Debug] HTTP POST Url Http ("url" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
--   Misc ARIA Role Types
--     roleType Article:                                                                                             [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","f16d1650-b9a0-463f-a8b3-16f267fe276a","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","ade539b3-a5eb-4cb4-b6ac-95783f62d8b9","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","b77b97c7-df90-49c0-9798-1067ea73bc8c","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","aab8badf-cc8c-4cbd-8a52-03d7be55f0bd","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","ce550ca6-c687-4d5f-8a1a-f13bdc8e6ff5","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","ce550ca6-c687-4d5f-8a1a-f13bdc8e6ff5","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","ade539b3-a5eb-4cb4-b6ac-95783f62d8b9","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.01s)
--     article by accessible name:                                                                                   [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","2422663e-8917-4d3e-ae2a-d8aa21b922a5","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","11900d8c-bdb3-4bc3-a23d-6fbd5f69ff0d","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","11900d8c-bdb3-4bc3-a23d-6fbd5f69ff0d","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e6292da5-968e-40ab-989e-6da56050c40c","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","d685a46a-4a76-4be8-b56a-627340c6ca28","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","8f823278-db09-4277-a75e-6eb65fe15a50","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","2422663e-8917-4d3e-ae2a-d8aa21b922a5","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","aab8badf-cc8c-4cbd-8a52-03d7be55f0bd","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("sync" :| ["execute","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","d685a46a-4a76-4be8-b56a-627340c6ca28","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","f16d1650-b9a0-463f-a8b3-16f267fe276a","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e6292da5-968e-40ab-989e-6da56050c40c","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP POST Url Http ("elements" :| ["2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","d3360b1d-dc66-4487-97ba-df642a073b58","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","42257446-3134-4a5b-b203-354391fa77e9","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","8ce7c435-e29f-469b-b978-07bc7f4779f1","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","a98544f3-8189-4bcc-a8be-ac9c0bc6ffe4","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","151848e2-0464-4d95-a924-d4913dfd648a","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","673f19f5-9342-4c84-a3d4-fd7b1df0b104","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","673f19f5-9342-4c84-a3d4-fd7b1df0b104","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.03s)
--     roleType Heading - single heading on page:                                                                    OK (0.03s)
--     heading by text content:                                                                                      OK (0.02s)
--     roleType Figure:                                                                                              OK (0.02s)
--     figure by accessible name:                                                                                    OK (0.02s)
--     roleType List - single list on page:                                                                          OK (0.03s)
--     list by accessible name:                                                                                      OK (0.02s)
--     roleType ListItem finds all list items:                                                                       OK (0.02s)
--     link by text content:                                                                                         OK (0.02s)
--     roleType Link finds all links:                                                                                OK (0.02s)
--     roleType Table:                                                                                               OK (0.02s)
--     table by accessible name:                                                                                     [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.03s)
--     roleType Row finds header and data rows:                                                                      OK (0.03s)
--     roleType ColumnHeader finds both column headers:                                                              OK (0.02s)
--     columnHeader by text content:                                                                                 OK (0.02s)
--     roleType RowHeader:                                                                                           OK (0.02s)
--     rowHeader by text content:                                                                                    OK (0.03s)
--     roleType Cell:                                                                                                OK (0.02s)
--     cell by text content:                                                                                         OK (0.02s)
--     roleType Group finds fieldset:                                                                                OK (0.02s)
--     group by accessible name:                                                                                     OK (0.02s)
--     roleType Option finds no options (DisplayedCheckAlways):                                                      OK (0.03s)
--     roleType Option with DisplayedCheckNever finds all options:                                                   OK (0.02s)
--     option by text content:                                                                                       [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e92e1d93-d4fd-43f6-9ec1-591ef86d7831","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","e92e1d93-d4fd-43f6-9ec1-591ef86d7831","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","07fd2cd1-c6d3-4cd3-ba46-8152c8f751f8","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP GET Url Http ("auto-id" :| ["attribute","07fd2cd1-c6d3-4cd3-ba46-8152c8f751f8","element","2c091347-0e41-488b-a69f-dd6fd06f0e52","session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.01s)
--     roleType Separator:                                                                                           [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.01s)
--     progressBar by accessible name:                                                                               [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.01s)
--     slider by accessible name:                                                                                    [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.01s)
--     spinButton by accessible name:                                                                                [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.01s)
--     roleType Status:                                                                                              [2026-09-28 10:36:20][Debug] Response: 200
-- OK (0.01s)
--     status by accessible name:                                                                                    [2026-09-28 10:36:20][Debug] Response: 200
-- OK
--     roleType Term:                                                                                                [2026-09-28 10:36:20][Debug] Response: 200
-- OK
--     term by text content:                                                                                         [2026-09-28 10:36:20][Debug] Response: 200
-- OK
--     roleType Definition:                                                                                          [2026-09-28 10:36:20][Debug] Response: 200
-- OK
--     definition by text content:                                                                                   [2026-09-28 10:36:20][Debug] Response: 200
-- [2026-09-28 10:36:20][Debug] HTTP DELETE Url Http ("2c091347-0e41-488b-a69f-dd6fd06f0e52" :| ["session","127.0.0.1"])
-- [2026-09-28 10:36:20][Debug] Response: 200
-- OK
-- 
-- All 128 tests passed (3.63s)
-- True
-- 
-- All 128 tests passed (3.63s)
-- True
-- 
-- All 128 tests passed (3.69s)
-- True

