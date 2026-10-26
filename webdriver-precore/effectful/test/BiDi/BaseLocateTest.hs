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
arggh = TE.tastyEvalStdOut (Just "/Landmark and Role Tests/&&/Banner - page header/") tests

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOut

_pattern :: Maybe Text
_pattern = Just "/Landmark and Role Tests/"


--- >>> arggh
-- Using debug local config
-- [2026-10-26 19:31:01][Debug] HTTP POST Url Http ("session" :| ["127.0.0.1"])
-- Base Locate Tests
--   Landmark and Role Tests
--     Role Locator Tests
--       Landmark role types - roleType
--         Banner - page header: [2026-10-26 19:31:02][Debug] Response: 200
-- [2026-10-26 19:31:02][Debug] Connecting to WebDriver at MkBiDiUrl {host = "127.0.0.1", port = 35463, path = "/session/a130e43d-f347-47d9-a986-cd8189283c80"}
-- [2026-10-26 19:31:02][Debug] WebSocket connection established
-- [2026-10-26 19:31:02][Debug] Starting Sender thread
-- [2026-10-26 19:31:02][Debug] Starting Receiver thread
-- [2026-10-26 19:31:02][Debug] Starting EventHandler thread
-- [2026-10-26 19:31:02][Debug] Sending Message: {
--     "id": 1,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 19:31:02][Debug] Received raw data: {"type":"success","id":1,"result":{"contexts":[{"children":[],"context":"99a92183-a387-4012-9f89-a2b271659ae1","originalOpener":null,"url":"about:blank","userContext":"default","clientWindow":"feefb45c-4a88-45ab-b55a-d626a7fc6308","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 19:31:02][Debug] Sending Message: {
--     "id": 2,
--     "method": "browsingContext.navigate",
--     "params": {
--         "context": "99a92183-a387-4012-9f89-a2b271659ae1",
--         "url": "file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"
--     }
-- }
-- [2026-10-26 19:31:02][Debug] Received raw data: {"type":"success","id":2,"result":{"navigation":"7590ba7c-dbb1-4128-8946-74d8e118c89d","url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"}}
-- [2026-10-26 19:31:02][Debug] Sending Message: {
--     "id": 3,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 19:31:02][Debug] Received raw data: {"type":"success","id":3,"result":{"contexts":[{"children":[],"context":"99a92183-a387-4012-9f89-a2b271659ae1","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"feefb45c-4a88-45ab-b55a-d626a7fc6308","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 19:31:02][Debug] Sending Message: {
--     "id": 4,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "99a92183-a387-4012-9f89-a2b271659ae1",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "role": "banner"
--             }
--         }
--     }
-- }
-- [2026-10-26 19:31:02][Debug] Received raw data: {"type":"success","id":4,"result":{"nodes":[{"type":"node","sharedId":"ea11daa6-1fe5-4fdf-8a02-ad84b367625a","value":{"nodeType":1,"localName":"header","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":7,"attributes":{"auto-id":"hdr-main"},"shadowRoot":null}}]}}
-- [2026-10-26 19:31:02][Debug] Sending Message: {
--     "id": 5,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "ea11daa6-1fe5-4fdf-8a02-ad84b367625a"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "99a92183-a387-4012-9f89-a2b271659ae1"
--         }
--     }
-- }
-- [2026-10-26 19:31:02][Debug] Received raw data: {"type":"success","id":5,"result":{"realm":"084babad-737f-4cb8-908f-c2604e3b7a6b","type":"success","result":{"type":"string","value":"hdr-main"}}}
-- [2026-10-26 19:31:02][Debug] HTTP DELETE Url Http ("a130e43d-f347-47d9-a986-cd8189283c80" :| ["session","127.0.0.1"])
-- [2026-10-26 19:31:03][Debug] Response: 200
-- OK (0.05s)
-- 
-- All 1 tests passed (1.55s)

--- >>> _eval _pattern tests
-- Using debug local config
-- [2026-10-26 20:01:04][Debug] HTTP POST Url Http ("session" :| ["127.0.0.1"])
-- Base Locate Test Suite
--   Base Locate Tests
--     Landmark and Role Tests
--       Locate by ID:                                  [2026-10-26 20:01:05][Debug] Response: 200
-- [2026-10-26 20:01:05][Debug] Connecting to WebDriver at MkBiDiUrl {host = "127.0.0.1", port = 34567, path = "/session/d6bbef55-4a84-4b0b-91ae-4df708adcdfd"}
-- [2026-10-26 20:01:05][Debug] WebSocket connection established
-- [2026-10-26 20:01:05][Debug] Starting Sender thread
-- [2026-10-26 20:01:05][Debug] Starting Receiver thread
-- [2026-10-26 20:01:05][Debug] Starting EventHandler thread
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 1,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":1,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"about:blank","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 2,
--     "method": "browsingContext.navigate",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "url": "file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"
--     }
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":2,"result":{"navigation":"b78f7537-078f-4f49-a628-7ede7aadd2c1","url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html"}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 3,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":3,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 4,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "xpath",
--             "value": ".//*[@id='section-personal']"
--         }
--     }
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":4,"result":{"nodes":[{"type":"node","sharedId":"efae77e1-e930-493a-b690-14003284eab1","value":{"nodeType":1,"localName":"section","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":17,"attributes":{"id":"section-personal","auto-id":"sec-personal"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 5,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "efae77e1-e930-493a-b690-14003284eab1"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":5,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"sec-personal"}}}
-- OK (0.10s)
--       Role Locator Tests
--         Landmark role types - roleType
--           Banner - page header:                      [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 6,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":6,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 7,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "role": "banner"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":7,"result":{"nodes":[{"type":"node","sharedId":"a1462445-25a0-4c89-aee7-c7efdbcc329a","value":{"nodeType":1,"localName":"header","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":7,"attributes":{"auto-id":"hdr-main"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 8,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "a1462445-25a0-4c89-aee7-c7efdbcc329a"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":8,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"hdr-main"}}}
-- OK (0.02s)
--           Main landmark:                             [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 9,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":9,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 10,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "role": "main"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":10,"result":{"nodes":[{"type":"node","sharedId":"bebca281-c9f8-4815-930c-95541de54a2b","value":{"nodeType":1,"localName":"main","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":9,"attributes":{"auto-id":"main-content"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 11,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "bebca281-c9f8-4815-930c-95541de54a2b"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":11,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"main-content"}}}
-- OK (0.01s)
--           ContentInfo - page footer:                 [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 12,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:05][Debug] Received raw data: {"type":"success","id":12,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:05][Debug] Sending Message: {
--     "id": 13,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "role": "contentinfo"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":13,"result":{"nodes":[{"type":"node","sharedId":"7dc7e852-4713-4dbe-b385-8acf4da3720c","value":{"nodeType":1,"localName":"footer","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":3,"attributes":{"auto-id":"ftr-main"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 14,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "7dc7e852-4713-4dbe-b385-8acf4da3720c"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":14,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"ftr-main"}}}
-- OK (0.01s)
--           Complementary - aside:                     [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 15,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":15,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 16,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "role": "complementary"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":16,"result":{"nodes":[{"type":"node","sharedId":"e3dc37cc-dc16-4819-a041-b9e544d1ab27","value":{"nodeType":1,"localName":"aside","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":5,"attributes":{"aria-label":"Help and tips","auto-id":"aside-help"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 17,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "e3dc37cc-dc16-4819-a041-b9e544d1ab27"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":17,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"aside-help"}}}
-- OK (0.02s)
--           Search landmark:                           [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 18,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":18,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 19,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "role": "search"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":19,"result":{"nodes":[{"type":"node","sharedId":"6aca5a21-f845-4d0f-9a35-56cc8768fe67","value":{"nodeType":1,"localName":"search","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":7,"attributes":{"auto-id":"srch-widget"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 20,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "6aca5a21-f845-4d0f-9a35-56cc8768fe67"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":20,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"srch-widget"}}}
-- OK (0.01s)
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 21,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
--         Role with name - aria-label
--           Navigation - Main navigation:              [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":21,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 22,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Main navigation",
--                 "role": "navigation"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":22,"result":{"nodes":[{"type":"node","sharedId":"58320967-69ea-4778-9520-a9bc4d5696f8","value":{"nodeType":1,"localName":"nav","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":3,"attributes":{"aria-label":"Main navigation","auto-id":"nav-main"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 23,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "58320967-69ea-4778-9520-a9bc4d5696f8"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":23,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"nav-main"}}}
-- OK (0.02s)
--           Navigation - Breadcrumb:                   [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 24,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":24,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 25,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Breadcrumb",
--                 "role": "navigation"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":25,"result":{"nodes":[{"type":"node","sharedId":"1ebb3e95-2077-4105-9901-b5e7beaeb421","value":{"nodeType":1,"localName":"nav","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":3,"attributes":{"aria-label":"Breadcrumb","auto-id":"nav-breadcrumb"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 26,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "1ebb3e95-2077-4105-9901-b5e7beaeb421"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":26,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"nav-breadcrumb"}}}
-- OK (0.02s)
--           Form - Mega test form:                     [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 27,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":27,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 28,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Mega test form",
--                 "role": "form"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":28,"result":{"nodes":[{"type":"node","sharedId":"c0ff89af-db22-481b-be9c-aa380d845a2a","value":{"nodeType":1,"localName":"form","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":21,"attributes":{"id":"megaforma","method":"post","action":"#","novalidate":"","aria-label":"Mega test form","auto-id":"frm-mega"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 29,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "c0ff89af-db22-481b-be9c-aa380d845a2a"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":29,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"frm-mega"}}}
-- OK (0.02s)
--           Complementary - Help and tips:             [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 30,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":30,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 31,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Help and tips",
--                 "role": "complementary"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":31,"result":{"nodes":[{"type":"node","sharedId":"e3dc37cc-dc16-4819-a041-b9e544d1ab27","value":{"nodeType":1,"localName":"aside","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":5,"attributes":{"aria-label":"Help and tips","auto-id":"aside-help"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 32,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "e3dc37cc-dc16-4819-a041-b9e544d1ab27"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":32,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"aside-help"}}}
-- OK[2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 33,
--  (0.02s)    "method": "browsingContext.getTree",
--     "params": {}
-- 
-- }
--           Button - submit by aria-label:             [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":33,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 34,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Submit the mega form",
--                 "role": "button"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":34,"result":{"nodes":[{"type":"node","sharedId":"e0687672-6987-41f1-9ee9-aa8dcb571212","value":{"nodeType":1,"localName":"button","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":1,"attributes":{"type":"submit","auto-id":"btn-submit","class":"text-input","aria-label":"Submit the mega form"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 35,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "e0687672-6987-41f1-9ee9-aa8dcb571212"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":35,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"btn-submit"}}}
-- OK (0.02s)
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 36,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
--           Button - span with explicit role override: [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":36,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 37,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Span acting as button",
--                 "role": "button"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":37,"result":{"nodes":[{"type":"node","sharedId":"c440e2ba-9708-452c-b295-d0f67da92490","value":{"nodeType":1,"localName":"span","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":1,"attributes":{"role":"button","tabindex":"0","auto-id":"btn-span-role","class":"span-button","aria-label":"Span acting as button"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 38,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "c440e2ba-9708-452c-b295-d0f67da92490"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":38,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"btn-span-role"}}}
-- OK (0.02s)
--           Button - link with explicit role override: [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 39,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":39,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 40,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Link acting as button",
--                 "role": "button"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":40,"result":{"nodes":[{"type":"node","sharedId":"6ab6a6fc-d68d-4a1b-82bc-5e590180e669","value":{"nodeType":1,"localName":"a","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":1,"attributes":{"role":"button","href":"#","auto-id":"btn-link-role","class":"link-button","aria-label":"Link acting as button"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 41,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "6ab6a6fc-d68d-4a1b-82bc-5e590180e669"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":41,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"btn-link-role"}}}
-- OK (0.02s)
--           Textbox - Nickname via aria-label:         [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 42,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":42,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 43,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Nickname",
--                 "role": "textbox"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":43,"result":{"nodes":[{"type":"node","sharedId":"5ee35d85-56e4-4495-be67-f9a6a7b15cf6","value":{"nodeType":1,"localName":"input","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":0,"attributes":{"type":"text","aria-label":"Nickname","auto-id":"edt-nickname","class":"text-input","value":"Jay"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 44,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "5ee35d85-56e4-4495-be67-f9a6a7b15cf6"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":44,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"edt-nickname"}}}
-- OK (0.02s)
--           Checkbox - Read documents via aria-label:  [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 45,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":45,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 46,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Read documents",
--                 "role": "checkbox"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":46,"result":{"nodes":[{"type":"node","sharedId":"253b2b91-87d7-46e0-98a9-dfe694bd9067","value":{"nodeType":1,"localName":"input","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":0,"attributes":{"type":"checkbox","auto-id":"chk-docs-read","aria-label":"Read documents","checked":""},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 47,
--     "method": "script.callFunction",
--     "params": {
--         "arguments": [
--             {
--                 "sharedId": "253b2b91-87d7-46e0-98a9-dfe694bd9067"
--             },
--             {
--                 "type": "string",
--                 "value": "auto-id"
--             }
--         ],
--         "awaitPromise": false,
--         "functionDeclaration": "function(el, name) { return el.getAttribute(name); }",
--         "target": {
--             "context": "d35e2a18-3105-4466-aba2-41470f77fa2a"
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":47,"result":{"realm":"fa9a1b4b-6f4f-4735-a9ac-aff96be545a5","type":"success","result":{"type":"string","value":"chk-docs-read"}}}
-- OK (0.02s)
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 48,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
--           Img - div with explicit role override:     [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":48,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 49,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "name": "Abstract coloured shape",
--                 "role": "img"
--             }
--         }
--     }
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":49,"result":{"nodes":[]}}
-- [2026-10-26 20:01:06][Error] ElementNotFound
--   { description = "No element found matching locator."
--   , locator =
--       Role
--         { role = RoleFull { role = Img , name = "Abstract coloured shape" }
--         }
--   }
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 50,
--     "method": "browsingContext.getTree",
--     "params": {}
-- }
-- [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":50,"result":{"contexts":[{"children":[],"context":"d35e2a18-3105-4466-aba2-41470f77fa2a","originalOpener":null,"url":"file:///home/john-walker/repos/webdriver/webdriver-precore/test-resources/testFiles/locator-landmark-roles.html","userContext":"default","clientWindow":"939f2693-2e1e-45d9-88cd-2f89207385e9","parent":null,"moz:scope":"content","moz:name":""}]}}
-- [2026-10-26 20:01:06][Debug] Sending Message: {
--     "id": 51,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "d35e2a18-3105-4466-aba2-41470f77fa2a",
--         "locator": {
--             "type": "accessibility",
--             "value": {
--                 "role": "navigation"
--             }
--         }
--     }
-- }
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
--             Use -p '/Landmark and Role Tests/&&/Img - div with explicit role override/' to rerun this test only.
--         Multi-element role types
--           Navigation - finds both nav landmarks:     [2026-10-26 20:01:06][Debug] Received raw data: {"type":"success","id":51,"result":{"nodes":[{"type":"node","sharedId":"58320967-69ea-4778-9520-a9bc4d5696f8","value":{"nodeType":1,"localName":"nav","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":3,"attributes":{"aria-label":"Main navigation","auto-id":"nav-main"},"shadowRoot":null}},{"type":"node","sharedId":"1ebb3e95-2077-4105-9901-b5e7beaeb421","value":{"nodeType":1,"localName":"nav","namespaceURI":"http://www.w3.org/1999/xhtml","childNodeCount":3,"attributes":{"aria-label":"Breadcrumb","auto-id":"nav-breadcrumb"},"shadowRoot":null}}]}}
-- [2026-10-26 20:01:06][Debug] HTTP DELETE Url Http ("d6bbef55-4a84-4b0b-91ae-4df708adcdfd" :| ["session","127.0.0.1"])
-- [2026-10-26 20:01:06][Debug] Response: 200
-- OK
-- 
-- 1 out of 17 tests failed (1.70s)
