module BiDi.Demos.NetworkDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log)
import Const (second)
import TestServer (withTestServer)

-- >>> runDemo networkDataCollectorDemo
networkDataCollectorDemo :: BiDiDemo
networkDataCollectorDemo =
  demo "Network I - Data Collector Management" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Test 1: Add data collector with minimal parameters"
      collector1 <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [MkDataType "response"],
              maxEncodedDataSize = MkJSUInt 1024,
              collectorType = Nothing,
              contexts = Nothing,
              userContexts = Nothing
            }
      logShow "Basic data collector added" collector1
      pause

      -- not allowable in spec yet MkDataType must always be: request
      -- leaving this code here, expecting spec change 
      -- logTxt "Test 2: Add data collector with specific collector type"
      -- collector2 <-
      --   networkAddDataCollector $
      --     MkAddDataCollector
      --       { dataTypes = [MkDataType "request", MkDataType "response"],
      --         maxEncodedDataSize = MkJSUInt 2048,
      --         collectorType = Just (MkCollectorType "blob"),
      --         contexts = Nothing,
      --         userContexts = Nothing
      --       }
      -- logShow "Typed data collector added" collector2
      -- pause

      -- logTxt "Test 3: Add data collector targeting specific browsing context"
      -- collector3 <-
      --   networkAddDataCollector $
      --     MkAddDataCollector
      --       { dataTypes = [MkDataType "response"],
      --         maxEncodedDataSize = MkJSUInt 4096,
      --         collectorType = Just (MkCollectorType "stream"),
      --         contexts = Just [bc],
      --         userContexts = Nothing
      --       }
      -- logShow "Context-specific data collector added" collector3
      -- pause

      logTxt "Test 4: Create user context for targeted data collection"
      userContext <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User context created" userContext
      pause

      -- logTxt "Test 5: Add data collector targeting specific user context"
      -- collector4 <-
      --   networkAddDataCollector $
      --     MkAddDataCollector
      --       { dataTypes = [MkDataType "request"],
      --         maxEncodedDataSize = MkJSUInt 8192,
      --         collectorType = Nothing,
      --         contexts = Nothing,
      --         userContexts = Just [userContext]
      --       }
      -- logShow "User context-specific data collector added" collector4
      -- pause

      -- logTxt "Test 6: Add data collector with multiple data types and large size"
      -- collector5 <-
      --   networkAddDataCollector $
      --     MkAddDataCollector
      --       { dataTypes = [MkDataType "request", MkDataType "response", MkDataType "websocket"],
      --         maxEncodedDataSize = MkJSUInt 16384,
      --         collectorType = Just (MkCollectorType "buffer"),
      --         contexts = Just [bc],
      --         userContexts = Just [userContext]
      --       }
      -- logShow "Multi-type data collector added" collector5
      -- pause

      logTxt "Navigation to trigger some network activity for data collection"
      navResult <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = "data:text/html,<html><head><title>Network Test</title></head><body><h1>Network Activity Test</h1><script>fetch('data:text/plain,hello').then(r=>r.text()).then(console.log)</script></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result" navResult
      pauseAtLeast $ 1 * second
      pause

      logTxt "Test 7: Remove data collectors"
      let MkAddDataCollectorResult collectorId1 = collector1
      removeResult1 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId1
      logShow "Removed basic data collector" removeResult1
      pause

      -- let MkAddDataCollectorResult collectorId2 = collector2
      -- removeResult2 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId2
      -- logShow "Removed typed data collector" removeResult2
      -- pause

      -- let MkAddDataCollectorResult collectorId3 = collector3
      -- removeResult3 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId3
      -- logShow "Removed context-specific data collector" removeResult3
      -- pause

      -- let MkAddDataCollectorResult collectorId4 = collector4
      -- removeResult4 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId4
      -- logShow "Removed user context-specific data collector" removeResult4
      -- pause

      -- let MkAddDataCollectorResult collectorId5 = collector5
      -- removeResult5 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId5
      -- logShow "Removed multi-type data collector" removeResult5
      -- pause

      logTxt "Cleanup - remove user context"
      removeUC <- browserRemoveUserContext $ MkRemoveUserContext userContext
      logShow "Removed user context" removeUC
      pause

-- >>> runDemo networkInterceptDemo
networkInterceptDemo :: BiDiDemo
networkInterceptDemo =
  demo "Network II - Request/Response Interception" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Test 1: Add intercept for BeforeRequestSent phase"
      intercept1 <-
        networkAddIntercept $
          MkAddIntercept
            { phases = [BeforeRequestSent],
              contexts = Just [bc],
              urlPatterns = Nothing
            }
      logShow "BeforeRequestSent intercept added" intercept1
      pause

      logTxt "Test 2: Add intercept for ResponseStarted phase with URL patterns"
      intercept2 <-
        networkAddIntercept $
          MkAddIntercept
            { phases = [ResponseStarted],
              contexts = Just [bc],
              urlPatterns =
                Just
                  [ UrlPatternPattern
                      ( MkUrlPatternPattern
                          { protocol = Just "https",
                            hostname = Nothing,
                            port = Nothing,
                            pathname = Nothing,
                            search = Nothing
                          }
                      )
                  ]
            }
      logShow "ResponseStarted intercept with HTTPS pattern added" intercept2
      pause

      logTxt "Test 3: Add intercept for AuthRequired phase with specific hostname (using string pattern)"
      intercept3 <-
        networkAddIntercept $
          MkAddIntercept
            { phases = [AuthRequired],
              contexts = Just [bc],
              urlPatterns =
                Just
                  [ UrlPatternString (MkUrlPatternString "https://example.com/\\*")
                  ]
            }
      logShow "AuthRequired intercept for example.com added" intercept3
      pause

      logTxt "Test 4: Add intercept for multiple phases"
      intercept4 <-
        networkAddIntercept $
          MkAddIntercept
            { phases = [BeforeRequestSent, ResponseStarted],
              contexts = Just [bc],
              urlPatterns = Nothing
            }
      logShow "Multi-phase intercept added" intercept4
      pause

      logTxt "Test 5: Add intercept with comprehensive URL pattern"
      intercept5 <-
        networkAddIntercept $
          MkAddIntercept
            { phases = [BeforeRequestSent],
              contexts = Just [bc],
              urlPatterns =
                Just
                  [ UrlPatternPattern
                      ( MkUrlPatternPattern
                          { protocol = Just "https",
                            hostname = Just "api.example.com",
                            port = Just "443",
                            pathname = Just "/v1/\\*",
                            search = Just "key=\\*"
                          }
                      )
                  ]
            }
      logShow "Comprehensive URL pattern intercept added" intercept5
      pause

      logTxt "Test 6: Add intercept with multiple URL patterns (demonstrating both pattern types)"
      intercept6 <-
        networkAddIntercept $
          MkAddIntercept
            { phases = [ResponseStarted],
              contexts = Just [bc],
              urlPatterns =
                Just
                  [ UrlPatternPattern
                      ( MkUrlPatternPattern
                          { protocol = Just "http",
                            hostname = Nothing,
                            port = Nothing,
                            pathname = Nothing,
                            search = Nothing
                          }
                      ),
                    UrlPatternString (MkUrlPatternString "https://api.example.com/")
                  ]
            }
      logShow "Multi-pattern intercept added" intercept6
      pause

      logTxt "Test 7: Add intercept with no contexts (global intercept)"
      intercept7 <-
        networkAddIntercept $
          MkAddIntercept
            { phases = [BeforeRequestSent],
              contexts = Nothing,
              urlPatterns = Nothing
            }
      logShow "Global intercept added" intercept7
      pause

      logTxt "Navigation to trigger potential intercepts (data URL - won't match most patterns)"
      navResult <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = "data:text/html,<html><head><title>Intercept Test</title></head><body><h1>Testing Network Intercepts</h1><script>fetch('data:text/plain,test').then(r=>r.text()).then(console.log)</script></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result" navResult
      pauseAtLeast $ 1 * second
      pause

      logTxt "Test 7: Remove intercepts"
      let MkAddInterceptResult interceptId1 = intercept1
      removeResult1 <- networkRemoveIntercept $ MkRemoveIntercept interceptId1
      logShow "Removed BeforeRequestSent intercept" removeResult1
      pause

      let MkAddInterceptResult interceptId2 = intercept2
      removeResult2 <- networkRemoveIntercept $ MkRemoveIntercept interceptId2
      logShow "Removed ResponseStarted intercept" removeResult2
      pause

      let MkAddInterceptResult interceptId3 = intercept3
      removeResult3 <- networkRemoveIntercept $ MkRemoveIntercept interceptId3
      logShow "Removed AuthRequired intercept" removeResult3
      pause

      let MkAddInterceptResult interceptId4 = intercept4
      removeResult4 <- networkRemoveIntercept $ MkRemoveIntercept interceptId4
      logShow "Removed multi-phase intercept" removeResult4
      pause

      let MkAddInterceptResult interceptId5 = intercept5
      removeResult5 <- networkRemoveIntercept $ MkRemoveIntercept interceptId5
      logShow "Removed comprehensive pattern intercept" removeResult5
      pause

      let MkAddInterceptResult interceptId6 = intercept6
      removeResult6 <- networkRemoveIntercept $ MkRemoveIntercept interceptId6
      logShow "Removed multi-pattern intercept" removeResult6
      pause

      let MkAddInterceptResult interceptId7 = intercept7
      removeResult7 <- networkRemoveIntercept $ MkRemoveIntercept interceptId7
      logShow "Removed global intercept" removeResult7
      pause

-- handleNoSuchRequestError :: (Text -> IO ()) -> IO () -> IO ()
-- handleNoSuchRequestError log action = catch action $ \e -> do
--   unless (exceptionTextIncludes "no such request" e) $
--     throwIO e
--   log "Expected \"no such request\" error ~ request not initialised"

-- >>> runDemo networkRequestResponseModificationDemo
networkRequestResponseModificationDemo :: BiDiDemo
networkRequestResponseModificationDemo =
  demo "Network III - Request and Response Modification" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds = do
      _bc <- rootContext utils cmds

      logTxt "Note: This demo demonstrates network request/response modification commands."
      logTxt "These commands require active intercepts and real network requests to function properly."

      withTestServer $ do
        logTxt "Test Server running - ready to intercept and modify network traffic"
        pause

        -- TODO: Implement actual intercept handlers to capture real request IDs
        -- The following tests demonstrate the parameter structure for each command
        -- but cannot execute without real intercepted requests

        logTxt "Test 1: networkContinueRequest with basic parameters (requires intercepted request)"
        logTxt "This would continue an intercepted request without modifications"
        pause

        logTxt "Test 2: networkContinueRequest with modified headers and method"
        logTxt "Example: Adding X-Custom-Header and changing method to POST"
        pause

        logTxt "Test 3: networkContinueRequest with body and URL modification"
        logTxt "Example: Modifying request body and redirecting to different URL"
        pause

        logTxt "Test 4: networkContinueRequest with cookies"
        logTxt "Example: Modifying request cookies (session_id, user_pref)"
        pause

        logTxt "Test 5: networkContinueResponse with basic parameters"
        logTxt "This would continue an intercepted response without modifications"
        pause

        logTxt "Test 6: networkContinueResponse with status code and reason phrase"
        logTxt "Example: Changing status to 200 Modified OK"
        pause

        logTxt "Test 7: networkContinueResponse with modified response body and headers"
        logTxt "Example: Changing Content-Type, adding custom headers, modifying body"
        pause

        logTxt "Test 8: networkContinueResponse with response cookies"
        logTxt "Example: Setting response cookies with security attributes"
        pause

-- >>> runDemo networkAuthAndFailureDemo
networkAuthAndFailureDemo :: BiDiDemo
networkAuthAndFailureDemo =
  demo "Network IV - Authentication and Request Failure Handling" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds = do
      _bc <- rootContext utils cmds

      logTxt "Note: This demo demonstrates auth and failure handling commands."
      logTxt "These commands require active intercepts and real network requests to function properly."

      withTestServer $ do
        logTxt "Test Server running - ready for auth and failure scenarios"
        pause

        -- TODO: Implement actual intercept handlers to capture real request IDs
        -- The following tests demonstrate the parameter structure for each command

        logTxt "Test 1: networkContinueWithAuth with default response (no credentials)"
        logTxt "This would use browser's default auth handling"
        pause

        logTxt "Test 2: networkContinueWithAuth with cancel response"
        logTxt "This would cancel the auth request"
        pause

        logTxt "Test 3: networkContinueWithAuth with provided credentials"
        logTxt "Example: username='test_user', password='test_password_123'"
        pause

        logTxt "Test 4: networkContinueWithAuth with different credentials"
        logTxt "Example: username='admin@example.com', password='super_secure_password_456'"
        pause

        logTxt "Test 5: networkFailRequest with basic request"
        logTxt "This would fail an intercepted request"
        pause

        logTxt "Test 6: networkFailRequest with different request"
        logTxt "Example: Simulating network failure"
        pause

        logTxt "Test 7: networkFailRequest with another request"
        logTxt "Example: Simulating timeout"
        pause

        logTxt "Test 8: networkFailRequest with final request"
        logTxt "Example: Simulating SSL error"
        pause

-- >>> runDemo networkProvideResponseDemo
networkProvideResponseDemo :: BiDiDemo
networkProvideResponseDemo =
  demo "Network V - Custom Response Provision" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds = do
      _bc <- rootContext utils cmds

      logTxt "Note: This demo demonstrates custom response provision commands."
      logTxt "These commands require active intercepts and real network requests to function properly."

      withTestServer $ do
        logTxt "Test Server running - ready to provide custom responses"
        pause

        -- TODO: Implement actual intercept handlers to capture real request/intercept IDs
        -- The following tests demonstrate the parameter structure for each command

        logTxt "Test 1: networkProvideResponse with minimal parameters"
        logTxt "Example: Basic 200 OK response"
        pause

        logTxt "Test 2: networkProvideResponse with JSON content"
        logTxt "Example: JSON response with custom headers"
        pause

        logTxt "Test 3: networkProvideResponse with HTML content and redirect"
        logTxt "Example: 302 redirect with HTML body and Location header"
        pause

        logTxt "Test 4: networkProvideResponse with binary content (base64)"
        logTxt "Example: PNG image served from base64 data"
        pause

        logTxt "Test 5: networkProvideResponse with error status and cookies"
        logTxt "Example: 401 Unauthorized with auth cookies"
        pause

        logTxt "Test 6: networkProvideResponse with server error"
        logTxt "Example: 500 Internal Server Error with retry-after header"
        pause

-- not supported in geckodriver yet
-- >>> runDemo networkDataRetrievalDemo
networkDataRetrievalDemo :: BiDiDemo
networkDataRetrievalDemo =
  demo "Network VI - Data Retrieval and Ownership" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Test 1: Add data collector to generate some data"
      collector <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [MkDataType "response"],
              maxEncodedDataSize = MkJSUInt 2048,
              collectorType = Just (MkCollectorType "buffer"),
              contexts = Just [bc],
              userContexts = Nothing
            }
      logShow "Data collector for retrieval demo" collector
      pause

      let MkAddDataCollectorResult collectorId = collector
      let exampleRequest = MkRequest "example-request-id-for-data"

      logTxt "Test 2: networkGetData with basic parameters (disown=False)"
      getData1 <-
        networkGetData $
          MkGetData
            { dataType = MkDataType "response",
              collector = Just collectorId,
              disown = Just False,
              request = exampleRequest
            }
      logShow "Get data without disowning result" getData1
      pause

      logTxt "Test 3: networkGetData with disown=True"
      getData2 <-
        networkGetData $
          MkGetData
            { dataType = MkDataType "response",
              collector = Just collectorId,
              disown = Just True,
              request = exampleRequest
            }
      logShow "Get data with disowning result" getData2
      pause

      logTxt "Test 4: networkGetData without specifying collector or disown (default False)"
      getData3 <-
        networkGetData $
          MkGetData
            { dataType = MkDataType "response",
              collector = Nothing,
              disown = Nothing,
              request = exampleRequest
            }
      logShow "Get data without collector specification result" getData3
      pause

      logTxt "Test 5: networkGetData for different data type"
      getData4 <-
        networkGetData $
          MkGetData
            { dataType = MkDataType "request",
              collector = Just collectorId,
              disown = Just False,
              request = exampleRequest
            }
      logShow "Get request data result" getData4
      pause

      logTxt "Test 6: networkDisownData - explicitly disown specific data"
      disownResult <-
        networkDisownData $
          MkDisownData
            { dataType = MkDataType "response",
              collector = collectorId,
              request = exampleRequest
            }
      logShow "Disown data result" disownResult
      pause

      logTxt "Test 7: networkDisownData for different data type"
      disownResult2 <-
        networkDisownData $
          MkDisownData
            { dataType = MkDataType "request",
              collector = collectorId,
              request = exampleRequest
            }
      logShow "Disown request data result" disownResult2
      pause

      logTxt "Cleanup - remove data collector"
      removeResult <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId
      logShow "Removed data collector" removeResult
      pause

-- >>> runDemo networkCacheBehaviorDemo
networkCacheBehaviorDemo :: BiDiDemo
networkCacheBehaviorDemo =
  demo "Network VII - Cache Behavior Management" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Test 1: Set cache behavior to default (global)"
      cacheResult1 <-
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = DefaultCacheBehavior,
              contexts = Nothing
            }
      logShow "Set default cache behavior (global) result" cacheResult1
      pause

      logTxt "Test 2: Set cache behavior to bypass cache (global)"
      cacheResult2 <-
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Nothing
            }
      logShow "Set bypass cache behavior (global) result" cacheResult2
      pause

      logTxt "Test 3: Set cache behavior to default for specific context"
      cacheResult3 <-
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = DefaultCacheBehavior,
              contexts = Just [bc]
            }
      logShow "Set default cache behavior (context-specific) result" cacheResult3
      pause

      logTxt "Test 4: Set cache behavior to bypass cache for specific context"
      cacheResult4 <-
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
      logShow "Set bypass cache behavior (context-specific) result" cacheResult4
      pause

      logTxt "Test 5: Create new context to test cache behavior isolation"
      newContext <- newWindowContext utils cmds

      logTxt "Test 6: Set different cache behavior for new context"
      cacheResult5 <-
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [newContext]
            }
      logShow "Set bypass cache behavior for new context result" cacheResult5
      pause

      logTxt "Test 7: Set cache behavior for multiple contexts"
      cacheResult6 <-
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = DefaultCacheBehavior,
              contexts = Just [bc, newContext]
            }
      logShow "Set default cache behavior for multiple contexts result" cacheResult6
      pause

      logTxt "Test 8: Navigate both contexts to test cache behavior"
      navResult1 <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = "data:text/html,<html><head><title>Cache Test 1</title></head><body><h1>Testing Cache Behavior - Context 1</h1></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result - original context" navResult1
      pause

      navResult2 <-
        browsingContextNavigate $
          MkNavigate
            { context = newContext,
              url = "data:text/html,<html><head><title>Cache Test 2</title></head><body><h1>Testing Cache Behavior - Context 2</h1></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result - new context" navResult2
      pause

      logTxt "Test 9: Reset cache behavior to default for all contexts"
      cacheResult7 <-
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = DefaultCacheBehavior,
              contexts = Nothing
            }
      logShow "Reset to default cache behavior (global) result" cacheResult7
      pause

      logTxt "Cleanup - close new context"
      closeContext utils cmds newContext
