module BiDi.BaseLocateTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    BiDiResources,
    interpBiDiLocate,
    runBiDiLocate,
    withBiDiResources,
  )
import Common.BaseLocateTest qualified as Common
import Common.TastyEval qualified as TE
import Common.WebDriver.Effect (WebDriver)
import Data.Text (Text)
import Effectful (Eff)
import Test.Tasty (TestTree, inOrderTestGroup)
import WebDriverPreCore.BiDi.Protocol (NodeRemoteValue)
import Prelude

-- >>> _eval tests
-- *** Exception: ExitSuccess
tests :: TestTree
tests =
  withBiDiResources runSessionTests
  where
    runSessionTests :: IO BiDiResources -> TestTree
    runSessionTests res =
      inOrderTestGroup "Base Locate Tests"
        [ Common.testLandmarkAndRole run interp,
          Common.testExtendedRoleMatchingNever run interp,
          Common.testExtendedRoleMatchingAlways run interp,
          Common.testExtendedRoleMatchingMiss run interp,
          Common.testExtendedRoleMatchingRoleType run interp interp,
          Common.testVisibilityAlways run interp,
          Common.testVisibilityNever run interp,
          Common.testVisibilityDisambiguate run interp,
          Common.testVisibilityDisambiguateVsNever run interp interp,
          Common.testBasicLocatorTypes run interp,
          Common.testClassLocatorVariants run interp,
          Common.testAttributeLocatorVariants run interp,
          Common.testRoleNameAndRole run interp,
          Common.testLocateFromElement run interp,
          Common.testCombinedLocators run interp,
          Common.testMiscAriaRoleTypes run interp,
          Common.testMiscAriaRoleTypesNever run interp
        ]
      where
        run :: forall a. Eff BiDiLocateEffs a -> IO a
        run = runBiDiLocate res

        interp :: forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
        interp = interpBiDiLocate

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOut

_pattern :: Maybe Text
_pattern = Just "Landmark and Role Tests"


-- Run it all
--- >>> _eval Nothing tests
-- Using debug local config

-- Specific test
--- >>> _eval _pattern tests
-- Using debug local config
-- [2026-10-18 09:39:08][Debug] HTTP POST Url Http ("session" :| ["127.0.0.1"])
-- [2026-10-18 09:39:08][Debug] Response: 500
-- Base Locate Tests
--   Landmark and Role Tests
--     Locate by ID:                                       FAIL
--       Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--       
--       MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--       
--       While handling MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--       
--       IPE backtrace:
--       HasCallStack backtrace:
--         throwIO, called at ./Test/Tasty/Run.hs:169:19 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--       
--       Use -p '/Landmark and Role Tests/&&/Locate by ID/' to rerun this test only.
--     jsDisplay check should NOT be affected by viewport: FAIL
--       Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--       
--       MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--       
--       IPE backtrace:
--       HasCallStack backtrace:
--         throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--       
--       Use -p '/Landmark and Role Tests/&&/jsDisplay check should NOT be affected by viewport/' to rerun this test only.
--     Role Locator Tests
--       Landmark role types - roleType
--         Banner - page header:                           FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Banner - page header/' to rerun this test only.
--         Main landmark:                                  FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Main landmark/' to rerun this test only.
--         ContentInfo - page footer:                      FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/ContentInfo - page footer/' to rerun this test only.
--         Complementary - aside:                          FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Complementary - aside/' to rerun this test only.
--         Search landmark:                                FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Search landmark/' to rerun this test only.
--       Role with name - aria-label
--         Navigation - Main navigation:                   FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Navigation - Main navigation/' to rerun this test only.
--         Navigation - Breadcrumb:                        FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Navigation - Breadcrumb/' to rerun this test only.
--         Form - Mega test form:                          FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Form - Mega test form/' to rerun this test only.
--         Complementary - Help and tips:                  FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Complementary - Help and tips/' to rerun this test only.
--         Button - submit by aria-label:                  FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Button - submit by aria-label/' to rerun this test only.
--         Button - span with explicit role override:      FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Button - span with explicit role override/' to rerun this test only.
--         Button - link with explicit role override:      FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Button - link with explicit role override/' to rerun this test only.
--         Textbox - Nickname via aria-label:              FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Textbox - Nickname via aria-label/' to rerun this test only.
--         Checkbox - Read documents via aria-label:       FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Checkbox - Read documents via aria-label/' to rerun this test only.
--         Img - div with explicit role override:          FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Img - div with explicit role override/' to rerun this test only.
--       Multi-element role types
--         Navigation - finds both nav landmarks:          FAIL
--           Exception: webdriver-precore-0.3.0.4-inplace-utils-internal:WebDriverPreCore.Types.ParseFailure.ParseFailure:
--           
--           MkParseFailure {info = "Error in $: key \"sessionId\" not found", response = Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")])}
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at ./Test/Tasty/Run.hs:172:44 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
--           
--           Use -p '/Landmark and Role Tests/&&/Navigation - finds both nav landmarks/' to rerun this test only.
-- 
-- 18 out of 18 tests failed (0.00s)
