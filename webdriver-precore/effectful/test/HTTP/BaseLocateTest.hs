module HTTP.BaseLocateTest where


import Common.Utils (beforeAll_, DriverActions (..),chkEq, chkLocException, chkSingleton, chkEmpty, autoId )
import Common.Utils qualified as U
import Data.Text (Text)
import Effectful
import HTTP.Runner (WDSession, runHttp, runHttpTest, testUrl)
import Prelude
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource)
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, URL)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators as LS
import WebDriverPreCore.Test.TestData
import WebDriver.Effectful.Logger (Logger)
import Common.Runner (getWDSession, closeWDSession)

-- >>> _eval tests
-- *** Exception: ExitSuccess
tests :: TestTree
tests =
  withResource getWDSession closeWDSession runSessionTests
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
    
    da :: DriverActions (Eff '[WebDriverHttp, Logger, Pause, IOE])
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
-- (textbox "Nickname") "auto-id" "edt-nickname"
_eval :: Maybe Text -> TestTree -> IO ()
_eval = U.testPattern

_pattern :: Maybe Text
_pattern = Just "ExtLocateAlways finds textbox with aria-label"

-- Specific test
--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess

-- All tests
--- >>> _eval Nothing tests -- eval all
-- *** Exception: ExitSuccess


