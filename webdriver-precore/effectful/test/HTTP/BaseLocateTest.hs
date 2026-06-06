module HTTP.BaseLocateTest where

import Common.Utils
  ( autoId,
    chkAttributeEq,
    chkElms,
    chkElmsM,
    chkEmpty,
    chkEq,
    chkLocException,
    chkSingleton,
    defOpts,
    locateAllFromElementHttp,
    locateAllHttp,
    locateFromElementHttp,
    locateHttp,
  )
import Common.Utils qualified as CU
import Data.Text (Text, unpack)
import Effectful
import HTTP.Runner (BaseHTTPEffs, WDSession, closeWDSession, getWDSession, runHttp, runHttpTest, testUrl)
import Prelude
import System.Environment (withArgs)
import Test.Tasty (TestTree, defaultMain, inOrderTestGroup, testGroup, withResource)
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, URL)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locators.Internal (CaseSensitivity (..))
import WebDriverPreCore.Test.TestData

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
        withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Landmark and Role Tests"
              [ chkAutoId "Locate by ID" (elmId "section-personal") "sec-personal"
              , test "jsDisplay check should NOT be affected by viewport" $ do
                  maximizeWindow
                  maxResult <- locateAll $ elmClass "input"
                  minimizeWindow
                  minResult <- locateAll $ elmClass "input"
                  chkEq "Displayed result should be the same for minised and maximised viewport" maxResult.result minResult.result
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
                      [ chkAll "Navigation - finds both nav landmarks" (roleType Navigation)
                          (\elms -> if length elms == 2 then Nothing
                                    else Just $ "expected 2 navigation landmarks but got " <> txt (length elms))
                      ]
                  ]
              ]

      , -- Extended role matching (aria-labelledby, for id label) on locator-extended-roles.html
        withResource (navToUrl ses extendedRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Extended Role Matching Tests"
              [ testGroup "aria-labelledby resolution"
                  [ atrrChkExtRole "ExtLocateAlways - locate finds region via aria-labelledby"
                      (region "Personal Information") "auto-id" "sec-personal"
                  , atrrChkExtMiss "ExtLocateSingletonMiss - locate finds region via aria-labelledby"
                      (region "Personal Information") "auto-id" "sec-personal"
                  , test "ExtLocateNever - locate does NOT find region via aria-labelledby" $ do
                      locRslt <- locate $ region "Personal Information"
                      chkLocException (txt (region "Personal Information")) isNotFound locRslt
                  , test "ExtLocateAlways - locateAll finds region via aria-labelledby" $ do
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
                      chkEq "RoleType Region results should be identical" never.result always.result
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
        withResource (navToUrl ses visibilityUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Visibility Check Tests"
              [ testGroup "locateAll - DisplayedCheckAlways filters hidden and DisplayedCheckNever does not"
                  [ testGroup "Rule 1 - display none on element itself"
                      [ test "edt-notes-hidden has display none via own CSS class" $ do
                          always <- locateAll      $ autoId "edt-notes-hidden"
                          never  <- locateAllNever $ autoId "edt-notes-hidden"
                          chkElms "DisplayedCheckAlways must filter display:none element"    chkEmpty    always
                          chkElms "DisplayedCheckNever must find display:none element"       chkSingleton never
                      ]
                  , testGroup "Rule 2 - visibility hidden or collapse - inherited"
                      [ test "edt-vis-hidden is inside inline visibility hidden parent" $ do
                          always <- locateAll      $ autoId "edt-vis-hidden"
                          never  <- locateAllNever $ autoId "edt-vis-hidden"
                          chkElms "DisplayedCheckAlways must filter visibility:hidden element" chkEmpty    always
                          chkElms "DisplayedCheckNever must find visibility:hidden element"    chkSingleton never
                      , test "edt-css-vis-hidden is inside CSS class visibility hidden parent" $ do
                          always <- locateAll      $ autoId "edt-css-vis-hidden"
                          never  <- locateAllNever $ autoId "edt-css-vis-hidden"
                          chkElms "DisplayedCheckAlways must filter CSS visibility:hidden element" chkEmpty    always
                          chkElms "DisplayedCheckNever must find CSS visibility:hidden element"    chkSingleton never
                      ]
                  , testGroup "Rule 3 - parseFloat opacity equals 0 on element itself"
                      [ test "fg-opacity-zero div has opacity 0 applied directly" $ do
                          always <- locateAll      $ autoId "fg-opacity-zero"
                          never  <- locateAllNever $ autoId "fg-opacity-zero"
                          chkElms "DisplayedCheckAlways must filter opacity:0 element" chkEmpty    always
                          chkElms "DisplayedCheckNever must find opacity:0 element"    chkSingleton never
                      ]
                  , testGroup "Rule 4 - INPUT with type hidden"
                      [ test "hdn-session-token is input type hidden" $ do
                          always <- locateAll      $ autoId "hdn-session-token"
                          never  <- locateAllNever $ autoId "hdn-session-token"
                          chkElms "DisplayedCheckAlways must filter input type=hidden" chkEmpty    always
                          chkElms "DisplayedCheckNever must find input type=hidden"    chkSingleton never
                      ]
                  , testGroup "Rule 5 - offsetWidth or offsetHeight equals 0 - parent has display none"
                      [ test "edt-display-none is inside inline display none parent" $ do
                          always <- locateAll      $ autoId "edt-display-none"
                          never  <- locateAllNever $ autoId "edt-display-none"
                          chkElms "DisplayedCheckAlways must filter zero-size element (inline display:none parent)" chkEmpty    always
                          chkElms "DisplayedCheckNever must find zero-size element (inline display:none parent)"    chkSingleton never
                      , test "edt-css-none is inside CSS class display none parent" $ do
                          always <- locateAll      $ autoId "edt-css-none"
                          never  <- locateAllNever $ autoId "edt-css-none"
                          chkElms "DisplayedCheckAlways must filter zero-size element (CSS display:none parent)" chkEmpty    always
                          chkElms "DisplayedCheckNever must find zero-size element (CSS display:none parent)"    chkSingleton never
                      , test "edt-html-hidden is inside HTML hidden attribute parent" $ do
                          always <- locateAll      $ autoId "edt-html-hidden"
                          never  <- locateAllNever $ autoId "edt-html-hidden"
                          chkElms "DisplayedCheckAlways must filter zero-size element (HTML hidden parent)" chkEmpty    always
                          chkElms "DisplayedCheckNever must find zero-size element (HTML hidden parent)"    chkSingleton never
                      ]
                  , testGroup "NOT filtered by displayedJS"
                      [ test "edt-aria-hidden - aria-hidden does not affect display opacity or dimensions" $ do
                          always <- locateAll      $ autoId "edt-aria-hidden"
                          never  <- locateAllNever $ autoId "edt-aria-hidden"
                          chkElms "DisplayedCheckAlways must NOT filter aria-hidden element" chkSingleton always
                          chkElms "DisplayedCheckNever must find aria-hidden element"        chkSingleton never
                      , test "edt-offscreen - positioned off-viewport but has non-zero dimensions" $ do
                          always <- locateAll      $ autoId "edt-offscreen"
                          never  <- locateAllNever $ autoId "edt-offscreen"
                          chkElms "DisplayedCheckAlways must NOT filter off-screen element" chkSingleton always
                          chkElms "DisplayedCheckNever must find off-screen element"        chkSingleton never
                      , test "edt-opacity-zero - input child of opacity 0 container - opacity not inherited" $ do
                          always <- locateAll      $ autoId "edt-opacity-zero"
                          never  <- locateAllNever $ autoId "edt-opacity-zero"
                          chkElms "DisplayedCheckAlways must NOT filter opacity:0 child element" chkSingleton always
                          chkElms "DisplayedCheckNever must find opacity:0 child element"        chkSingleton never
                      ]
                  ]
              , testGroup "locate singleton - DisplayedCheckDisambiguateUnique resolves hidden-visible ambiguity"
                  [ test "DisplayedCheckNever with Unique throws AmbiguousLocator - hidden and visible share class" $ do
                      locRslt <- locateNever $ elmClass "notes-area"
                      chkLocException (txt (elmClass "notes-area")) isAmbiguous locRslt
                  , test "DisplayedCheckDisambiguateUnique filters hidden - resolving to unique visible element" $ do
                      locRslt <- locateDisambiguate $ elmClass "notes-area"
                      chkAttributeEq (txt (elmClass "notes-area")) "auto-id" "edt-notes-visible" locRslt
                  , test "DisplayedCheckAlways also filters hidden, resolving to unique visible element" $ do
                      locRslt <- locate $ elmClass "notes-area"
                      chkAttributeEq (txt (elmClass "notes-area")) "auto-id" "edt-notes-visible" locRslt
                  ]
              , testGroup "locateAll - DisplayedCheckDisambiguateUnique has no effect - only Always filters"
                  [ test "DisambiguateUnique gives same result as Never for locateAll" $ do
                      disambiguate <- locateAllDisambiguate $ elmClass "notes-area"
                      never        <- locateAllNever $ elmClass "notes-area"
                      chkEq "DisambiguateUnique locateAll result must equal Never" disambiguate.result never.result
                  , test "DisplayedCheckAlways filters hidden in locateAll - Never returns both" $ do
                      always <- locateAll      $ elmClass "notes-area"
                      never  <- locateAllNever $ elmClass "notes-area"
                      chkElms (txt (elmClass "notes-area")) chkSingleton always
                      chkElms (txt (elmClass "notes-area"))
                        (\elms -> if length elms == 2 then Nothing
                                  else Just $ "expected 2 elements (visible + hidden) but got " <> txt (length elms))
                        never
                  ]
              ]

      , withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Basic Locator Types"
              [ chkAutoId "defaultId resolves via mkDefaultLoc option" (defaultId "hdr-main") "hdr-main"
              , chkAll "allElms finds all page elements" allElms
                  (\elms -> if length elms > 20 then Nothing else Just $ "expected >20 elements but got " <> txt (length elms))
              , chkAutoId "elmId finds element by HTML id" (elmId "megaforma") "frm-mega"
              , chkAutoId "css attribute selector" (css "[auto-id='ftr-main']") "ftr-main"
              , chkAutoId "xpath finds element by tag" (xpath "//footer") "ftr-main"
              , chkAll "input_ tag locator finds all inputs" input_
                  (\elms -> if length elms >= 5 then Nothing else Just $ "expected >=5 inputs but got " <> txt (length elms))
              , chkAll "button_ tag locator finds button elements" button_
                  (\elms -> if null elms then Just "expected at least one button" else Nothing)
              , chkAll "h1_ tag locator finds the single h1 heading" h1_ chkSingleton
              ]

      , withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Class Locator Variants"
              [ chkAll "elmClass contains match" (elmClass "text-input")
                  (\elms -> if length elms >= 6 then Nothing else Just $ "expected >=6 elements with class text-input but got " <> txt (length elms))
              , chkAll "elmClassExact full-equality match" (elmClassExact "text-input")
                  (\elms -> if length elms >= 6 then Nothing else Just $ "expected >=6 exact text-input class elements but got " <> txt (length elms))
              , chkAll "elemClassStarts starts-with match" (elemClassStarts "text")
                  (\elms -> if length elms >= 6 then Nothing else Just $ "expected >=6 class starts-with-text elements but got " <> txt (length elms))
              , chkAutoId "elmClass finds element by single class name" (elmClass "span-button") "btn-span-role"
              ]

      , withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Attribute Locator Variants"
              [ chkAutoId "attribute default contains match" (attribute "auto-id" "hdr-main") "hdr-main"
              , chkAutoId "attributeExact full-equality match" (attributeExact "auto-id" "hdr-main") "hdr-main"
              , chkAll "attributeStarts starts-with match" (attributeStarts "auto-id" "nav")
                  (\elms -> if length elms >= 2 then Nothing else Just $ "expected >=2 nav* auto-id elements but got " <> txt (length elms))
              , chkAll "attribute full case-sensitive finds type text inputs" (attribute' "type" Full CaseSensitive "text")
                  (\elms -> if length elms == 3 then Nothing else Just $ "expected 3 type=text inputs but got " <> txt (length elms))
              , chkAutoId "attribute full case-insensitive matches uppercase value" (attribute' "auto-id" Full CaseInsensitive "HDR-MAIN") "hdr-main"
              ]

      , withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "roleName and role Constructors"
              [ chkAutoId "roleName finds element by accessible name" (roleName "Submit the mega form") "btn-submit"
              , chkAutoId "roleName finds aside by aria-label" (roleName "Help and tips") "aside-help"
              , chkAutoId "roleName finds nav by aria-label" (roleName "Main navigation") "nav-main"
              , chkAutoId "role generic constructor - Navigation with name" (role Navigation "Breadcrumb") "nav-breadcrumb"
              ]

      , withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Locate and LocateAll from Element"
              [ test "locateAll from element - inputs within sec-personal" $ do
                  secResult <- locate $ autoId "sec-personal"
                  chkElmsM "find sec-personal" secResult $ \elms ->
                    case elms of
                      [sec] -> do
                        inResult <- locateAllFromElement sec input_
                        chkElms "inputs in sec-personal"
                          (\is -> if length is == 5 then Nothing else Just $ "expected 5 inputs but got " <> txt (length is))
                          inResult
                        pure Nothing
                      _ -> pure $ Just $ "expected singleton section but got " <> txt (length elms)
              , test "locateAll from element - links within nav-main" $ do
                  navResult <- locate $ autoId "nav-main"
                  chkElmsM "find nav-main" navResult $ \elms ->
                    case elms of
                      [nav] -> do
                        linkResult <- locateAllFromElement nav a_
                        chkElms "links in nav-main"
                          (\ls -> if length ls >= 2 then Nothing else Just $ "expected >=2 links but got " <> txt (length ls))
                          linkResult
                        pure Nothing
                      _ -> pure $ Just $ "expected singleton nav but got " <> txt (length elms)
              , test "locate from element - edt-given-name within sec-personal" $ do
                  secResult <- locate $ autoId "sec-personal"
                  chkElmsM "find sec-personal" secResult $ \elms ->
                    case elms of
                      [sec] -> do
                        givenResult <- locateFromElement sec $ autoId "edt-given-name"
                        chkElms "edt-given-name in section" chkSingleton givenResult
                        pure Nothing
                      _ -> pure $ Just $ "expected singleton section but got " <> txt (length elms)
              , test "locate from element - not found when element not in scope" $ do
                  hdrResult <- locate $ autoId "hdr-main"
                  chkElmsM "find hdr-main" hdrResult $ \elms ->
                    case elms of
                      [hdr] -> do
                        notInHdr <- locateFromElement hdr $ autoId "edt-given-name"
                        pure $ case notInHdr.result of
                          Left (L.ElementNotFound {}) -> Nothing
                          Left other -> Just $ "expected ElementNotFound but got: " <> txt other
                          Right _ -> Just "expected ElementNotFound but edt-given-name was found in header"
                      _ -> pure $ Just $ "expected singleton header but got " <> txt (length elms)
              ]

      , withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Combined Locators"
              [ chkAll "AND - input_ and elmClass text-input" (input_ &&& elmClass "text-input")
                  (\elms -> if length elms == 6 then Nothing else Just $ "expected 6 input+text-input elements but got " <> txt (length elms))
              , chkAll "OR - h1_ or h2_ finds all headings" (h1_ ||| h2_)
                  (\elms -> if length elms == 3 then Nothing else Just $ "expected 3 headings (1×h1 + 2×h2) but got " <> txt (length elms))
              , chkAll "Descendant - sec-personal contains input_ finds contained inputs" (autoId "sec-personal" >>> input_)
                  (\elms -> if length elms == 5 then Nothing else Just $ "expected 5 inputs in sec-personal but got " <> txt (length elms))
              , chkAll "OR - roleType Navigation or roleType Search" (roleType Navigation ||| roleType Search)
                  (\elms -> if length elms == 3 then Nothing else Just $ "expected 3 nav+search landmarks but got " <> txt (length elms))
              ]

      , withResource (navToUrl ses miscRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Misc ARIA Role Types"
              [ chkAutoId "roleType Article" (roleType Article) "art-main"
              , chkAutoId "article by accessible name" (article "Test article") "art-main"
              , chkAutoId "roleType Heading - single heading on page" (roleType Heading) "hdg-article"
              , chkAutoId "heading by text content" (heading "Article Heading") "hdg-article"
              , chkAutoId "roleType Figure" (roleType Figure) "fig-sample"
              , chkAutoId "figure by accessible name" (figure "Sample figure") "fig-sample"
              , chkAutoId "roleType List - single list on page" (roleType List) "lst-nav"
              , chkAutoId "list by accessible name" (list "Navigation list") "lst-nav"
              , chkAll "roleType ListItem finds all list items" (roleType ListItem)
                  (\elms -> if length elms == 2 then Nothing else Just $ "expected 2 list items but got " <> txt (length elms))
              , chkAutoId "link by text content" (link "Home") "lnk-home"
              , chkAll "roleType Link finds all links" (roleType Link)
                  (\elms -> if length elms == 2 then Nothing else Just $ "expected 2 links but got " <> txt (length elms))
              , chkAutoId "roleType Table" (roleType Table) "tbl-data"
              , chkAutoId "table by accessible name" (table "Data table") "tbl-data"
              , chkAll "roleType Row finds header and data rows" (roleType Row)
                  (\elms -> if length elms == 2 then Nothing else Just $ "expected 2 rows but got " <> txt (length elms))
              , chkAll "roleType ColumnHeader finds both column headers" (roleType ColumnHeader)
                  (\elms -> if length elms == 2 then Nothing else Just $ "expected 2 column headers but got " <> txt (length elms))
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
              , chkAll "roleType Option finds no options (DisplayedCheckAlways)" (roleType Option)
                  (\elms -> if length elms == 0 then Nothing else Just $ "expected 0 options (native widget has no CSS dimensions) but got " <> txt (length elms))
              , chkAllNever "roleType Option with DisplayedCheckNever finds all options" (roleType Option)
                  (\elms -> if length elms == 2 then Nothing else Just $ "expected 2 options (no display check) but got " <> txt (length elms))
              , chkAutoId "option by text content" (option "Alpha") "opt-alpha"
              , chkAutoId "roleType Separator" (roleType Separator) "sep-main"
              , chkAutoId "progressBar by accessible name" (progressBar "Upload progress") "prg-upload"
              , chkAutoId "slider by accessible name" (slider "Volume") "sld-volume"
              , chkAutoId "spinButton by accessible name" (spinButton "Item count") "spn-count"
              , chkAutoId "roleType Status" (roleType Status) "out-result"
              , chkAutoId "status by accessible name" (status "Calculation result") "out-result"
              , chkAutoId "roleType Term" (roleType Term) "trm-name"
              , chkAutoId "term by text content" (term "Name") "trm-name"
              , chkAutoId "roleType Definition" (roleType Definition) "def-name"
              , chkAutoId "definition by text content" (definition "John") "def-name"
              ]

      {-
      , withResource (navToUrl ses landmarkRolesUrl) (\_ -> pure ()) $ \_ ->
          testGroup "Value PostFilter - not yet implemented in HTTP"
              -- These document expected behaviour once PostFilter is implemented.
              -- Currently fail with: "PostFilter locators are not yet implemented in HTTP WebDriver"
              [ test "value - find input with matching current value - partial match" $ do
                  locRslt <- locateAll $ value "Jane" input_
                  chkElms (txt (value "Jane" input_)) chkSingleton locRslt
              , test "valueExact - find input with exact current value" $ do
                  locRslt <- locateAll $ valueExact "Jay" input_
                  chkElms (txt (valueExact "Jay" input_)) chkSingleton locRslt
              , test "valueStarts - find input whose value starts with prefix" $ do
                  locRslt <- locateAll $ valueStarts "Jane" input_
                  chkElms (txt (valueStarts "Jane" input_)) chkSingleton locRslt
              ]
              -}
      ]
    where
    test :: Text -> BaseHTTPEffs () -> TestTree
    test = runHttpTest ses

    -- Partially applied test helpers using shared functions from Common.Utils
    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId = CU.chkAutoId test locate

    chkAll :: Text -> Locator -> ([ElementId] -> Maybe Text) -> TestTree
    chkAll = CU.chkAll test locateAll

    chkAllNever :: Text -> Locator -> ([ElementId] -> Maybe Text) -> TestTree
    chkAllNever = CU.chkAllNever test locateAllNever

    atrrChkExtRole :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtRole testName loc attrName expctd =
      test testName $ locateExt loc >>= chkAttributeEq (txt loc) attrName expctd

    atrrChkExtMiss :: Text -> Locator -> Text -> Text -> TestTree
    atrrChkExtMiss testName loc attrName expctd =
      test testName $ do
        locRslt <- locateExtMiss loc
        chkAttributeEq (txt loc) attrName expctd locRslt

  navToUrl :: IO WDSession -> IO URL -> IO WDSession
  navToUrl getSes urlAction = do
    ses <- getSes
    runHttp ses $ testUrl urlAction >>= navigateTo
    pure ses

  locate :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locate = locateHttp defOpts

  locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateAll = locateAllHttp defOpts

  locateFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es L.LocateResult
  locateFromElement = locateFromElementHttp defOpts

  locateAllFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es L.LocateResult
  locateAllFromElement = locateAllFromElementHttp defOpts

  extAlwaysOpts :: L.HttpLocateOpts
  extAlwaysOpts = defOpts { L.extendedRoleLocation = L.ExtLocateAlways }

  extMissOpts :: L.HttpLocateOpts
  extMissOpts = defOpts { L.extendedRoleLocation = L.ExtLocateSingletonMiss }

  locateExt :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateExt = locateHttp extAlwaysOpts

  locateAllExt :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateAllExt = locateAllHttp extAlwaysOpts

  locateExtMiss :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateExtMiss = locateHttp extMissOpts

  locateAllExtMiss :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateAllExtMiss = locateAllHttp extMissOpts

  isNotFound :: L.LocateException -> Maybe Text
  isNotFound (L.ElementNotFound {}) = Nothing
  isNotFound other = Just $ "expected ElementNotFound but got: " <> txt other

  neverOpts :: L.HttpLocateOpts
  neverOpts = defOpts { L.jsRecheckDisplayed = L.DisplayedCheckNever }

  disambiguateOpts :: L.HttpLocateOpts
  disambiguateOpts = defOpts { L.jsRecheckDisplayed = L.DisplayedCheckDisambiguateUnique }

  locateAllNever :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateAllNever = locateAllHttp neverOpts

  locateAllDisambiguate :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateAllDisambiguate = locateAllHttp disambiguateOpts

  locateNever :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateNever = locateHttp neverOpts

  locateDisambiguate :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
  locateDisambiguate = locateHttp disambiguateOpts

  isAmbiguous :: L.LocateException -> Maybe Text
  isAmbiguous (L.AmbiguousLocator {}) = Nothing
  isAmbiguous other = Just $ "expected AmbiguousLocator but got: " <> txt other

_pattern :: Maybe Text
-- _pattern = Just "roleType Option"
_pattern = Nothing

_eval :: Maybe Text -> TestTree -> IO ()
_eval mPattern = withArgs (maybe [] (\pat -> ["-p", (unpack pat)]) mPattern) . defaultMain

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess


