module HTTP.BaseLocateTest where

import HTTP.Runner (getWDSession, closeWDSession, runHttpTest, WDSession, testUrl, runHttp, BaseHTTPEffs)
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import System.Environment (withArgs)
import Test.Tasty.HUnit (assertBool, assertFailure, assertEqual)
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriver.Effectful.HTTP.Base.Actions 
import WebDriverPreCore.Test.TestData
import Effectful
import Effectful.Exception (catch)
import UnliftIO (throwIO)
import WebDriver.Effectful
import WebDriver.Effectful.Logger
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as TIO
import Utils (txt)
import Control.Monad (when)
import Data.Function ((&))
import Data.Functor ((<&>))

-- >>> _eval baseLocateTests
-- *** Exception: ExitSuccess
baseLocateTests :: TestTree
baseLocateTests =
    --  these tests run against megaforma.html
  withResource navToMegaForm closeWDSession $ \ses ->
    do 
     let
      test :: Text -> BaseHTTPEffs () -> TestTree
      test = runHttpTest ses

      atrrChk :: Text -> Locator -> Text -> Text -> TestTree
      atrrChk testName loc attrName expctd = 
        test testName $ do
          locRslt <- locate loc
          chkAttributeEq (txt loc) attrName expctd locRslt

      chkAll :: Text -> Locator -> ([ElementId] -> Maybe Text) -> TestTree
      chkAll testName loc chk =
        test testName $ do
          locRslt <- locateAll loc
          -- liftIO $ TIO.putStrLn $ txt locRslt
          chkElms (txt loc) chk locRslt

      atrrChkExt :: Text -> Locator -> Text -> Text -> TestTree
      atrrChkExt testName loc attrName expctd =
        test testName $ do
          locRslt <- locateExt loc
          chkAttributeEq (txt loc) attrName expctd locRslt

      atrrChkExtMiss :: Text -> Locator -> Text -> Text -> TestTree
      atrrChkExtMiss testName loc attrName expctd =
        test testName $ do
          locRslt <- locateExtMiss loc
          chkAttributeEq (txt loc) attrName expctd locRslt

     testGroup "Base Locate Tests"
      [ atrrChk "Locate by ID" (elmId "section-personal") "auto-id" "sec-personal"
      , test "jsDisplay check should NOT be affected by viewport" $ do
          maximizeWindow
          maxResult <- locateAll (elmClass "input")
          minimizeWindow
          minResult <- locateAll (elmClass "input")
          chkEq "Displayed result should be the same for minised and maximised viewport" maxResult.result minResult.result
      , testGroup "Role Locator Tests"
          [ testGroup "Landmark role types (roleType)"
              [ atrrChk "Banner - page header" (roleType Banner) "auto-id" "hdr-main"
              , atrrChk "Main landmark" (roleType Main) "auto-id" "main-content"
              , atrrChk "ContentInfo - page footer" (roleType ContentInfo) "auto-id" "ftr-main"
              , atrrChk "Complementary - aside" (roleType Complementary) "auto-id" "aside-help"
              , atrrChk "Search landmark" (roleType Search) "auto-id" "srch-widget"
              ]
          , testGroup "Role with name (aria-label)"
              [ atrrChk "Navigation - Main navigation" (navigation "Main navigation") "auto-id" "nav-main"
              , atrrChk "Navigation - Breadcrumb" (navigation "Breadcrumb") "auto-id" "nav-breadcrumb"
              , atrrChk "Form - Mega test form" (form "Mega test form") "auto-id" "frm-mega"
              , atrrChk "Complementary - Help and tips" (complementary "Help and tips") "auto-id" "aside-help"
              , atrrChk "Button - submit by aria-label" (button "Submit the mega form") "auto-id" "btn-submit"
              , atrrChk "Button - span with explicit role override" (button "Span acting as button") "auto-id" "btn-span-role"
              , atrrChk "Button - link with explicit role override" (button "Link acting as button") "auto-id" "btn-link-role"
              , atrrChk "Textbox - Nickname (aria-label)" (textbox "Nickname") "auto-id" "edt-nickname"
              , atrrChk "Checkbox - Read documents (aria-label)" (checkbox "Read documents") "auto-id" "chk-docs-read"
              , atrrChk "Img - div with explicit role override" (img "Abstract coloured shape") "auto-id" "img-div-role"
              ]
          , testGroup "Multi-element role types"
              [ chkAll "Navigation - finds both nav landmarks" (roleType Navigation)
                  (\elms -> if length elms == 2 then Nothing
                            else Just $ "expected 2 navigation landmarks but got " <> txt (length elms))
              ]
          ]
      , testGroup "Extended Role Matching Tests"
          [ testGroup "aria-labelledby resolution"
              [ atrrChkExt "ExtLocateAlways - locate finds region via aria-labelledby"
                  (region "Personal Information") "auto-id" "sec-personal"
              , atrrChkExtMiss "ExtLocateSingletonMiss - locate finds region via aria-labelledby"
                  (region "Personal Information") "auto-id" "sec-personal"
              , test "ExtLocateNever - locate does NOT find region via aria-labelledby" $ do
                  locRslt <- locate (region "Personal Information")
                  chkLocException (txt (region "Personal Information")) isNotFound locRslt
              , test "ExtLocateAlways - locateAll finds region via aria-labelledby" $ do
                  locRslt <- locateAllExt (region "Personal Information")
                  chkElms (txt (region "Personal Information")) chkSingleton locRslt
              , test "ExtLocateSingletonMiss - locateAll does NOT find region via aria-labelledby" $ do
                  locRslt <- locateAllExtMiss (region "Personal Information")
                  chkElms (txt (region "Personal Information")) chkEmpty locRslt
              , test "ExtLocateNever - locateAll does NOT find region via aria-labelledby" $ do
                  locRslt <- locateAll (region "Personal Information")
                  chkElms (txt (region "Personal Information")) chkEmpty locRslt
              ]
          , testGroup "for/id label association"
              [ atrrChkExt "ExtLocateAlways - locate finds radio via for/id label"
                  (radio "Email") "auto-id" "rdo-contact-email"
              , atrrChkExtMiss "ExtLocateSingletonMiss - locate finds radio via for/id label"
                  (radio "Email") "auto-id" "rdo-contact-email"
              , test "ExtLocateNever - locate does NOT find radio via for/id label" $ do
                  locRslt <- locate (radio "Email")
                  chkLocException (txt (radio "Email")) isNotFound locRslt
              , atrrChkExt "ExtLocateAlways - locate finds textbox via for/id label"
                  (textbox "Given Name") "auto-id" "edt-given-name"
              , atrrChkExtMiss "ExtLocateSingletonMiss - locate finds textbox via for/id label"
                  (textbox "Given Name") "auto-id" "edt-given-name"
              , test "ExtLocateNever - locate does NOT find textbox via for/id label" $ do
                  locRslt <- locate (textbox "Given Name")
                  chkLocException (txt (textbox "Given Name")) isNotFound locRslt
              , test "ExtLocateAlways - locateAll finds radio via for/id label" $ do
                  locRslt <- locateAllExt (radio "Email")
                  chkElms (txt (radio "Email")) chkSingleton locRslt
              , test "ExtLocateSingletonMiss - locateAll does NOT find radio via for/id label" $ do
                  locRslt <- locateAllExtMiss (radio "Email")
                  chkElms (txt (radio "Email")) chkEmpty locRslt
              ]
          , testGroup "RoleType - unaffected by extended matching"
              [ test "RoleType Region: ExtLocateNever and ExtLocateAlways give same results" $ do
                  never <- locateAll (roleType Region)
                  always <- locateAllExt (roleType Region)
                  chkEq "RoleType Region results should be identical" never.result always.result
              ]
          , testGroup "aria-label - always resolved regardless of setting"
              [ atrrChk "ExtLocateNever finds textbox with aria-label"
                  (textbox "Nickname") "auto-id" "edt-nickname"
              , atrrChkExt "ExtLocateAlways finds textbox with aria-label"
                  (textbox "Nickname") "auto-id" "edt-nickname"
              , atrrChkExtMiss "ExtLocateSingletonMiss finds textbox with aria-label"
                  (textbox "Nickname") "auto-id" "edt-nickname"
              ]
          ]
      , testGroup "Visibility Check Tests"
          [ testGroup "locateAll: DisplayedCheckAlways filters hidden, DisplayedCheckNever does not"
              [ testGroup "Rule 1 (display=none on element itself)"
                  [ test "edt-notes-hidden has display:none via own CSS class" $ do
                      always <- locateAll      (attribute "auto-id" "edt-notes-hidden")
                      never  <- locateAllNever (attribute "auto-id" "edt-notes-hidden")
                      chkElms "DisplayedCheckAlways must filter display:none element"    chkEmpty    always
                      chkElms "DisplayedCheckNever must find display:none element"       chkSingleton never
                  ]
              , testGroup "Rule 2 (visibility=hidden or collapse, inherited)"
                  [ test "edt-vis-hidden is inside inline visibility:hidden parent" $ do
                      always <- locateAll      (attribute "auto-id" "edt-vis-hidden")
                      never  <- locateAllNever (attribute "auto-id" "edt-vis-hidden")
                      chkElms "DisplayedCheckAlways must filter visibility:hidden element" chkEmpty    always
                      chkElms "DisplayedCheckNever must find visibility:hidden element"    chkSingleton never
                  , test "edt-css-vis-hidden is inside CSS class visibility:hidden parent" $ do
                      always <- locateAll      (attribute "auto-id" "edt-css-vis-hidden")
                      never  <- locateAllNever (attribute "auto-id" "edt-css-vis-hidden")
                      chkElms "DisplayedCheckAlways must filter CSS visibility:hidden element" chkEmpty    always
                      chkElms "DisplayedCheckNever must find CSS visibility:hidden element"    chkSingleton never
                  ]
              , testGroup "Rule 3 (parseFloat(opacity) === 0 on element itself)"
                  [ test "fg-opacity-zero div has opacity:0 applied directly" $ do
                      always <- locateAll      (attribute "auto-id" "fg-opacity-zero")
                      never  <- locateAllNever (attribute "auto-id" "fg-opacity-zero")
                      chkElms "DisplayedCheckAlways must filter opacity:0 element" chkEmpty    always
                      chkElms "DisplayedCheckNever must find opacity:0 element"    chkSingleton never
                  ]
              , testGroup "Rule 4 (INPUT with type=hidden)"
                  [ test "hdn-session-token is input[type=hidden]" $ do
                      always <- locateAll      (attribute "auto-id" "hdn-session-token")
                      never  <- locateAllNever (attribute "auto-id" "hdn-session-token")
                      chkElms "DisplayedCheckAlways must filter input type=hidden" chkEmpty    always
                      chkElms "DisplayedCheckNever must find input type=hidden"    chkSingleton never
                  ]
              , testGroup "Rule 5 (offsetWidth===0 or offsetHeight===0, parent has display:none)"
                  [ test "edt-display-none is inside inline display:none parent" $ do
                      always <- locateAll      (attribute "auto-id" "edt-display-none")
                      never  <- locateAllNever (attribute "auto-id" "edt-display-none")
                      chkElms "DisplayedCheckAlways must filter zero-size element (inline display:none parent)" chkEmpty    always
                      chkElms "DisplayedCheckNever must find zero-size element (inline display:none parent)"    chkSingleton never
                  , test "edt-css-none is inside CSS class display:none parent" $ do
                      always <- locateAll      (attribute "auto-id" "edt-css-none")
                      never  <- locateAllNever (attribute "auto-id" "edt-css-none")
                      chkElms "DisplayedCheckAlways must filter zero-size element (CSS display:none parent)" chkEmpty    always
                      chkElms "DisplayedCheckNever must find zero-size element (CSS display:none parent)"    chkSingleton never
                  , test "edt-html-hidden is inside HTML hidden-attribute parent" $ do
                      always <- locateAll      (attribute "auto-id" "edt-html-hidden")
                      never  <- locateAllNever (attribute "auto-id" "edt-html-hidden")
                      chkElms "DisplayedCheckAlways must filter zero-size element (HTML hidden parent)" chkEmpty    always
                      chkElms "DisplayedCheckNever must find zero-size element (HTML hidden parent)"    chkSingleton never
                  ]
              , testGroup "NOT filtered by displayedJS"
                  [ test "edt-aria-hidden: aria-hidden does not affect display, opacity or dimensions" $ do
                      always <- locateAll      (attribute "auto-id" "edt-aria-hidden")
                      never  <- locateAllNever (attribute "auto-id" "edt-aria-hidden")
                      chkElms "DisplayedCheckAlways must NOT filter aria-hidden element" chkSingleton always
                      chkElms "DisplayedCheckNever must find aria-hidden element"        chkSingleton never
                  , test "edt-offscreen: positioned off-viewport but has non-zero dimensions" $ do
                      always <- locateAll      (attribute "auto-id" "edt-offscreen")
                      never  <- locateAllNever (attribute "auto-id" "edt-offscreen")
                      chkElms "DisplayedCheckAlways must NOT filter off-screen element" chkSingleton always
                      chkElms "DisplayedCheckNever must find off-screen element"        chkSingleton never
                  , test "edt-opacity-zero: input child of opacity:0 container; opacity not inherited" $ do
                      always <- locateAll      (attribute "auto-id" "edt-opacity-zero")
                      never  <- locateAllNever (attribute "auto-id" "edt-opacity-zero")
                      chkElms "DisplayedCheckAlways must NOT filter opacity:0 child element" chkSingleton always
                      chkElms "DisplayedCheckNever must find opacity:0 child element"        chkSingleton never
                  ]
              ]
          , testGroup "locate (singleton): DisplayedCheckDisambiguateUnique resolves hidden/visible ambiguity"
              [ test "DisplayedCheckNever with Unique throws AmbiguousLocator (hidden+visible share class)" $ do
                  locRslt <- locateNever (elmClass "notes-area")
                  chkLocException (txt (elmClass "notes-area")) isAmbiguous locRslt
              , test "DisplayedCheckDisambiguateUnique filters hidden, resolving to unique visible element" $ do
                  locRslt <- locateDisambiguate (elmClass "notes-area")
                  chkAttributeEq (txt (elmClass "notes-area")) "auto-id" "edt-notes-visible" locRslt
              , test "DisplayedCheckAlways also filters hidden, resolving to unique visible element" $ do
                  locRslt <- locate (elmClass "notes-area")
                  chkAttributeEq (txt (elmClass "notes-area")) "auto-id" "edt-notes-visible" locRslt
              ]
          , testGroup "locateAll: DisplayedCheckDisambiguateUnique has no effect (only Always filters)"
              [ test "DisambiguateUnique gives same result as Never for locateAll" $ do
                  disambiguate <- locateAllDisambiguate (elmClass "notes-area")
                  never        <- locateAllNever (elmClass "notes-area")
                  chkEq "DisambiguateUnique locateAll result must equal Never" disambiguate.result never.result
              , test "DisplayedCheckAlways filters hidden in locateAll; Never returns both" $ do
                  always <- locateAll      (elmClass "notes-area")
                  never  <- locateAllNever (elmClass "notes-area")
                  chkElms (txt (elmClass "notes-area")) chkSingleton always
                  chkElms (txt (elmClass "notes-area"))
                    (\elms -> if length elms == 2 then Nothing
                              else Just $ "expected 2 elements (visible + hidden) but got " <> txt (length elms))
                    never
              ]
          ]
      ]
     where

      defOpts :: L.HttpLocateOpts
      defOpts = L.MkHttpLocateOpts { extendedRoleLocation = L.ExtLocateNever
                                 , jsRecheckDisplayed = L.DisplayedCheckAlways
                                 , singletonCardinality = L.Unique
                                 , mkDefaultLoc = attribute "auto-id"
                                 , locateTracing = L.LocateTracing
                                 }
    
      locate :: forall es. (IOE :> es, WebDriverHttp :> es)  => Locator -> Eff es L.LocateResult
      locate = locateHttp defOpts
      
      locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
      locateAll = locateAllHttp defOpts


      locateFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es L.LocateResult
      locateFromElement = locateFromElementHttp defOpts

      locateAllFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es L.LocateResult
      locateAllFromElement = locateAllFromElementHttp defOpts

      chkAttrEq :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Text -> Text -> Text -> Eff es ()
      chkAttrEq loc msg attr expected = 
        locate loc >>= chkAttributeEq msg attr expected

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

      chkEmpty :: [ElementId] -> Maybe Text
      chkEmpty [] = Nothing
      chkEmpty elms = Just $ "expected 0 results but got " <> txt (length elms)

      chkSingleton :: [ElementId] -> Maybe Text
      chkSingleton [_] = Nothing
      chkSingleton elms = Just $ "expected exactly 1 result but got " <> txt (length elms)

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


_eval :: TestTree -> IO ()
_eval = withArgs [] . defaultMain

navToMegaForm :: IO WDSession
navToMegaForm = do
  ses <- getWDSession
  runHttp ses $ do 
    url <- testUrl megaformaUrl 
    navigateTo url
    maximizeWindow
  pure ses

-- actions :: forall es. (IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) => Eff es (L.LocateActions (Eff es))
actions :: forall es. (IOE :> es, WebDriverHttp :> es) => Eff es (L.LocateActions (Eff es))
actions = pure $ L.MkLocateActions { 
                                   throw = throwIO,
                                   catch,
                                   findElement,
                                   findElementFromElement,
                                   findElements,
                                   findElementsFromElement,
                                   executeScript,
                                   getElementAttribute,  
                                   getElementText
                                }
         
-- ################ Base Eff Actions ################

locateHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es L.LocateResult
locateHttp opts loc =  (actions >>= \a -> L.locateHttp a opts loc)

locateAllHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator ->  Eff es L.LocateResult
locateAllHttp opts loc =  (actions >>= \a -> L.locateAllHttp a opts loc)

locateFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator ->  Eff es L.LocateResult
locateFromElementHttp ops loc elmId' =  (actions >>= \a -> L.locateFromElementHttp a ops loc elmId')

locateAllFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator ->  Eff es L.LocateResult
locateAllFromElementHttp ops loc elmId' =  (actions >>= \a -> L.locateAllFromElementHttp a ops loc elmId')

-- ################ Checks ################

chkLocException :: (IOE :> es) => Text -> (L.LocateException -> Maybe Text) -> L.LocateResult -> Eff es ()
chkLocException errMsg p locRslt =
  either
    (\ex -> liftChk (errMsg <> ": LocateException check failed: " <> txt ex) $ p ex)
    (const . liftFail $ errMsg <> ": expected Left LocateException but got Right")
    (locRslt.result)

chkElms :: (IOE :> es) => Text -> ([ElementId] -> Maybe Text) -> L.LocateResult -> Eff es ()
chkElms errMsg p locRslt =
  either
    (liftFail . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (liftChk (errMsg <> ": element list check failed") . p)
    (locRslt.result)


chkElmsM :: (IOE :> es) => Text -> L.LocateResult -> ([ElementId] -> Eff es (Maybe Text)) -> Eff es ()
chkElmsM testTitle locRslt chkM =
  locRslt.result & either
    (\err -> liftFail $ " - locate failed:\n" <> testTitle <> "\n" <> txt err <> "\n" <> txt locRslt)
    (\elms -> chkM elms >>= liftChk (testTitle <> " - element list check failed"))

chkAttribute :: forall es. (IOE :> es, WebDriverHttp :> es)=> Text -> L.LocateResult -> Text -> (Text -> Maybe Text) -> Eff es ()
chkAttribute testTitle locRslt attrName attrValChkM = 
    chkElmsM testTitle locRslt elmChk 
    where 
      elmChk :: [ElementId] -> Eff es (Maybe Text)
      elmChk = \case 
        [el] ->  do
          attr <- getElementAttribute el attrName 
          pure $ maybe (Just $ testTitle <> " - attribute not found: " <> txt attrName) attrValChkM attr
        elms -> pure $ Just $ testTitle <> " - expected singlet locate resultlist but got " <> txt (length elms) <> " elms"
   
--   -- do 
--   --  attrs <- getElementAttribute el attrName
--   --  _
--   -- chkElmsM errMsg $ 
--   --   case attrs of
--   --   [el] ->  getElementAttribute el attrName >>= _
--   --       -- maybe
--   --       --   (liftFail $ errMsg <> ": attribute not found: " <> txt attrName)
--   --       --    chkM
--   --   elms -> Just $ errMsg <> ": expected singleton element list but got " <> txt (length elms) <> " elms"

chkAttributeEq :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> Text -> L.LocateResult -> Eff es ()
chkAttributeEq testTitle attrName expctd locrslt = 
  chkAttribute testTitle locrslt attrName (\actual -> if actual == expctd 
                                                      then Nothing 
                                                      else Just $ 
                                                       testTitle <> " - expected attribute value: " <> txt expctd <> " but got: " <> txt actual)

liftFail :: (IOE :> es) => Text -> Eff es a
liftFail = liftIO . assertFailure . unpack

liftChk :: (IOE :> es) => Text -> Maybe Text -> Eff es ()
liftChk testTitle mErr = mErr & maybe (pure ()) (\erMsg -> liftFail $ testTitle <> " - " <> erMsg)

chkEq :: (IOE :> es, Show a, Eq a) => Text -> a -> a -> Eff es ()
chkEq msg a b = liftIO $ assertEqual (unpack msg) a b


