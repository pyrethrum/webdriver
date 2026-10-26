module BiDi.BaseLocateTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    BiDiResources,
    interpBiDiLocate,
    runBiDiLocate,
    withBiDiResources,
  )
import Common.BaseLocateTest
  ( testAttributeLocatorVariants,
    testBasicLocatorTypes,
    testClassLocatorVariants,
    testCombinedLocators,
    testExtendedRoleMatchingAlways,
    testExtendedRoleMatchingMiss,
    testExtendedRoleMatchingNever,
    testExtendedRoleMatchingRoleType,
    testLandmarkAndRole,
    testLocateFromElement,
    testMiscAriaRoleTypes,
    testMiscAriaRoleTypesNever,
    testRoleNameAndRole,
    testViewportDisplayCheck,
    testVisibilityAlways,
    testVisibilityDisambiguate,
    testVisibilityDisambiguateVsNever,
    testVisibilityNever,
  )
import Common.TastyEval qualified as TE
import Common.WebDriver.Effect (WebDriver)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Test.Tasty (TestTree, inOrderTestGroup)
import WebDriverPreCore.BiDi.Protocol (NodeRemoteValue)
import Prelude

-- >>> _eval tests

-- *** Exception: ExitSuccess

tests :: TestTree
tests =
  inOrderTestGroup
    "Base Locate Test Suite"
    [ withBiDiResources viewportTests,
      withBiDiResources runSessionTests
    ]
  where
    viewportTests :: IO BiDiResources -> TestTree
    viewportTests res =
      testViewportDisplayCheck (runBiDiLocate res) interpBiDiLocate

    runSessionTests :: IO BiDiResources -> TestTree
    runSessionTests res =
      inOrderTestGroup
        "Base Locate Tests"
        [ runInprt testLandmarkAndRole,
          runInprt testExtendedRoleMatchingNever,
          runInprt testExtendedRoleMatchingAlways,
          runInprt testExtendedRoleMatchingMiss,
          runInprt testVisibilityAlways,
          runInprt testVisibilityNever,
          runInprt testVisibilityDisambiguate,
          runInprt testBasicLocatorTypes,
          runInprt testClassLocatorVariants,
          runInprt testAttributeLocatorVariants,
          runInprt testRoleNameAndRole,
          runInprt testLocateFromElement,
          runInprt testCombinedLocators,
          runInprt testMiscAriaRoleTypes,
          runInprt testMiscAriaRoleTypesNever,
          testExtendedRoleMatchingRoleType run interp interp,
          testVisibilityDisambiguateVsNever run interp interp
        ]
      where
        runInprt ::
          ( forall elm es.
            (Eq elm, Show elm, Error Text :> es) =>
            (forall a. Eff es a -> IO a) ->
            (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
            TestTree
          ) ->
          TestTree
        runInprt test = test run interp

        run :: forall a. Eff BiDiLocateEffs a -> IO a
        run = runBiDiLocate res

        interp :: forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
        interp = interpBiDiLocate



arggh :: IO TE.TastyResult
arggh = TE.tastyEvalStdOut (Just "/Landmark and Role Tests/&&/Img - div with explicit role override/") tests

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOut

_pattern :: Maybe Text
_pattern = Just "/Landmark and Role Tests/"


--- >>> arggh
-- Using debug local config
-- [2026-10-26 20:03:34][Debug] HTTP POST Url Http ("session" :| ["127.0.0.1"])
-- Base Locate Test Suite
--   Base Locate Tests
--     Landmark and Role Tests
--       Role Locator Tests
--         Role with name - aria-label
--           Img - div with explicit role override: [2026-10-26 20:03:35][Debug] Response: 200
-- [2026-10-26 20:03:35][Debug] Connecting to WebDriver at MkBiDiUrl {host = "127.0.0.1", port = 43597, path = "/session/35630206-74c7-49c5-b3f0-e5a016aca8cd"}
-- [2026-10-26 20:03:35][Debug] WebSocket connection established
-- [2026-10-26 20:03:35][Debug] Starting Sender thread
-- [2026-10-26 20:03:35][Debug] Starting Receiver thread
-- [2026-10-26 20:03:35][Debug] Starting EventHandler thread
-- [2026-10-26 20:03:35][Debug] Sending Message: {
--     "id": 1,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:03:35][Debug] Received raw data: {"type":"success","id":1,"result":{"contexts":[{"children":[],"context":"7e4a375c-ee67-4ca3-ae50-aefba4cf8681","originalOpener":null,"url":"about:blank","userContext":"default","clientWindow":"0e03c52e-dde1-46ef-ae60-1117ac58790d","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:03:35][Debug] Sending Message: {
--     "id": 2,
--     "method": "browsingContext.navigate",
--     "params": {
--         "context": "7e4a375c-ee67-4ca3-ae50-aefba4cf8681",
--         "url": "file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"
--     }
-- }
-- [2026-10-26 20:03:35][Debug] Received raw data: {"type":"success","id":2,"result":{"navigation":"09929683-3745-412a-afa7-5c9079bbed87","url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"}}
-- [2026-10-26 20:03:35][Debug] Sending Message: {
--     "id": 3,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:03:35][Debug] Received raw data: {"type":"success","id":3,"result":{"contexts":[{"children":[],"context":"7e4a375c-ee67-4ca3-ae50-aefba4cf8681","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"0e03c52e-dde1-46ef-ae60-1117ac58790d","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:03:35][Debug] Sending Message: {
--     "id": 4,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "7e4a375c-ee67-4ca3-ae50-aefba4cf8681",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Abstract coloured shape",
--                 "role": "img"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:03:36][Debug] Received raw data: {"type":"success","id":4,"result":{"nodes":[]}}
-- [2026-10-26 20:03:36][Error] ElementNotFound
--   { description = "No element found matching locator."
--   , locator =
--       Role
--         { role = RoleFull { role = Img , name = "Abstract coloured shape" }
--         }
--   }
-- [2026-10-26 20:03:36][Debug] Exception thrown in Receiver thread: ConnectionClosed
-- [2026-10-26 20:03:36][Debug] HTTP DELETE Url Http ("35630206-74c7-49c5-b3f0-e5a016aca8cd" :| ["session","127.0.0.1"])
-- [2026-10-26 20:03:36][Debug] Response: 200
-- FAIL
--             Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--             
--             user error (ElementNotFound
--               { description = "No element found matching locator."
--               , locator =
--                   Role
--                     { role = RoleFull { role = Img , name = "Abstract coloured shape" }
--                     }
--               })
--             
--             IPE backtrace:
--             HasCallStack backtrace:
--               throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
-- 
-- 1 out of 1 tests failed (1.46s)
