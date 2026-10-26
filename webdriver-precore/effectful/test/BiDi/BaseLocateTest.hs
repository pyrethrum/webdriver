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
  withBiDiResources runSessionTests
  where
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
arggh = TE.tastyEvalStdOut (Just "/Landmark and Role Tests/&&/Locate by ID/") tests

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOut

_pattern :: Maybe Text
_pattern = Just "Landmark and Role Tests"

-- Run it all
--- >>> _eval Nothing tests

--- >>> arggh
-- Using debug local config
-- [2026-10-26 16:56:23][Debug] HTTP POST Url Http ("session" :| ["127.0.0.1"])
-- Base Locate Tests
--   Landmark and Role Tests
--     Locate by ID: [2026-10-26 16:56:24][Debug] Response: 200
-- [2026-10-26 16:56:24][Debug] Connecting to WebDriver at MkBiDiUrl {host = "127.0.0.1", port = 33169, path = "/session/18dce55b-6069-45ab-83a7-11dd56a26d20"}
-- [2026-10-26 16:56:24][Debug] WebSocket connection established
-- [2026-10-26 16:56:24][Debug] Starting Receiver thread
-- [2026-10-26 16:56:24][Debug] Starting EventHandler thread
-- [2026-10-26 16:56:24][Debug] Starting Sender thread
-- [2026-10-26 16:56:24][Debug] Sending Message: {
--     "id": 1,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 16:56:24][Debug] Received raw data: {"type":"success","id":1,"result":{"contexts":[{"children":[],"context":"b1b7c0b1-b37d-404a-9dcf-0b8de2fe4c68","originalOpener":null,"url":"about:blank","userContext":"default","clientWindow":"424b722c-f2e4-404f-a92f-161335f409f6","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 16:56:24][Debug] Sending Message: {
--     "id": 2,
--     "method": "browsingContext.navigate",
--     "params": {
--         "context": "b1b7c0b1-b37d-404a-9dcf-0b8de2fe4c68",
--         "url": "file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"
--     }
-- }
-- [2026-10-26 16:56:24][Debug] Received raw data: {"type":"success","id":2,"result":{"navigation":"d6f3b058-1aa3-480c-9e4b-c09053c660f3","url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"}}
-- [2026-10-26 16:56:24][Debug] Sending Message: {
--     "id": 3,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 16:56:24][Debug] Received raw data: {"type":"success","id":3,"result":{"contexts":[{"children":[],"context":"b1b7c0b1-b37d-404a-9dcf-0b8de2fe4c68","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"424b722c-f2e4-404f-a92f-161335f409f6","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 16:56:24][Debug] Sending Message: {
--     "id": 4,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "b1b7c0b1-b37d-404a-9dcf-0b8de2fe4c68",
--         "locator": {
--             "type": "xpath",
--             "value": ".//*[@id='section-personal']"
--         }
--     }
-- }
-- [2026-10-26 16:56:24][Debug] Received raw data: {"type":"success","id":4,"result":{"nodes":[{"type":"node","sharedId":"d533d8aa-bc29-4132-b9b6-24a7a4f10419","value":{"nodeType":1,"localName":"section","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":17,"attributes":{"id":"section-personal","auto-id":"sec-personal"},"shadowRoot":null}}]}}
-- [2026-10-26 16:56:24][Debug] Exception thrown in Receiver thread: ConnectionClosed
-- [2026-10-26 16:56:24][Debug] HTTP DELETE Url Http ("18dce55b-6069-45ab-83a7-11dd56a26d20" :| ["session","127.0.0.1"])
-- [2026-10-26 16:56:24][Debug] Response: 200
-- FAIL
--       Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--       
--       user error (Attribute not found: auto-id
--       Failed to get outer HTMLs:
--       ( [ ( "throwError"
--           , SrcLoc
--               { srcLocPackage =
--                   "webdriver-precore-0.3.0.4-inplace-test-effectful"
--               , srcLocModule = "Common.WebDriver.TestUtils"
--               , srcLocFile =
--                   "/home/john-walker/repos/webdriver/webdriver-precore/effectful/test/Common/WebDriver/TestUtils.hs"
--               , srcLocStartLine = 77
--               , srcLocStartCol = 8
--               , srcLocEndLine = 77
--               , srcLocEndCol = 18
--               }
--           )
--         ]
--       , "Property not found: outerHTML"
--       ))
--       
--       IPE backtrace:
--       HasCallStack backtrace:
--         throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
-- 
-- 1 out of 1 tests failed (1.44s)
