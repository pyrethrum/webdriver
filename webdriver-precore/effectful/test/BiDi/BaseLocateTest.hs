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
-- [2026-10-19 08:06:10][Debug] HTTP POST Url Http ("session" :| ["127.0.0.1"])
-- Base Locate Tests
--   Landmark and Role Tests
--     Locate by ID:                                       [2026-10-19 08:06:11][Debug] Response: 200
-- [2026-10-19 08:06:11][Debug] Connecting to WebDriver at MkBiDiUrl {host = "127.0.0.1", port = 44937, path = "/session/9029fd90-407f-48b4-9e38-cbe109d20cc5"}
-- [2026-10-19 08:06:11][Debug] WebSocket connection established
-- [2026-10-19 08:06:11][Debug] Starting Sender thread
-- [2026-10-19 08:06:11][Debug] Starting EventHandler thread
-- [2026-10-19 08:06:11][Debug] Starting Receiver thread
-- [2026-10-19 08:06:11][Debug] Sending Message: {
--     "id": 1,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-19 08:06:11][Debug] Received raw data: {"type":"success","id":1,"result":{"contexts":[{"children":[],"context":"7717ecb6-287e-4890-a97b-5f7...
-- [2026-10-19 08:06:11][Debug] Sending Message: {
--     "id": 2,
--     "method": "browsingContext.navigate",
--     "params": {
--         "context": "7717ecb6-287e-4890-a97b-5f7ba9cb5106",
--         "url": "file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"
--     }
-- }
-- [2026-10-19 08:06:11][Debug] Received raw data: {"type":"success","id":2,"result":{"navigation":"a1a058a7-d301-4f03-a822-f102b75aa137","url":"file:/...
-- [2026-10-19 08:06:11][Debug] Exception thrown in Receiver thread: ConnectionClosed
-- [2026-10-19 08:06:11][Debug] HTTP DELETE Url Http ("9029fd90-407f-48b4-9e38-cbe109d20cc5" :| ["session","127.0.0.1"])
-- FAIL
--       Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--       
--       user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283584 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--       
--       While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283584 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--       
--       IPE backtrace:
--       HasCallStack backtrace:
--         throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--       
--       Use -p '/Landmark and Role Tests/&&/Locate by ID/' to rerun this test only.
--     jsDisplay check should NOT be affected by viewport: FAIL
--       Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--       
--       user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283591 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--       
--       While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283591 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--       
--       IPE backtrace:
--       HasCallStack backtrace:
--         throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--       
--       Use -p '/Landmark and Role Tests/&&/jsDisplay check should NOT be affected by viewport/' to rerun this test only.
--     Role Locator Tests
--       Landmark role types - roleType
--         Banner - page header:                           FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283586 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283586 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Banner - page header/' to rerun this test only.
--         Main landmark:                                  FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283585 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283585 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Main landmark/' to rerun this test only.
--         ContentInfo - page footer:                      FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283593 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283593 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/ContentInfo - page footer/' to rerun this test only.
--         Complementary - aside:                          FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283592 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283592 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Complementary - aside/' to rerun this test only.
--         Search landmark:                                FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283596 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283596 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Search landmark/' to rerun this test only.
--       Role with name - aria-label
--         Navigation - Main navigation:                   FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283595 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283595 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Navigation - Main navigation/' to rerun this test only.
--         Navigation - Breadcrumb:                        FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283594 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283594 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Navigation - Breadcrumb/' to rerun this test only.
--         Form - Mega test form:                          FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283588 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283588 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Form - Mega test form/' to rerun this test only.
--         Complementary - Help and tips:                  FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283601 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283601 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Complementary - Help and tips/' to rerun this test only.
--         Button - submit by aria-label:                  FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283598 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283598 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Button - submit by aria-label/' to rerun this test only.
--         Button - span with explicit role override:      FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283597 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283597 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Button - span with explicit role override/' to rerun this test only.
--         Button - link with explicit role override:      [2026-10-19 08:06:11][Debug] Response: 200
-- FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283600 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283600 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Button - link with explicit role override/' to rerun this test only.
--         Textbox - Nickname via aria-label:              FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283590 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283590 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Textbox - Nickname via aria-label/' to rerun this test only.
--         Checkbox - Read documents via aria-label:       FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283587 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283587 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Checkbox - Read documents via aria-label/' to rerun this test only.
--         Img - div with explicit role override:          FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283589 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283589 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Img - div with explicit role override/' to rerun this test only.
--       Multi-element role types
--         Navigation - finds both nav landmarks:          FAIL
--           Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--           
--           user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283599 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           While handling user error (BiDi runner multiplexing detected: command issued from thread ThreadId 283599 but the session was opened on thread ThreadId 283554. Use one BiDi runner per thread (e.g. 'withBiDi' within the thread).)
--           
--           IPE backtrace:
--           HasCallStack backtrace:
--             throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
--           
--           Use -p '/Landmark and Role Tests/&&/Navigation - finds both nav landmarks/' to rerun this test only.
-- 
-- 18 out of 18 tests failed (1.58s)
