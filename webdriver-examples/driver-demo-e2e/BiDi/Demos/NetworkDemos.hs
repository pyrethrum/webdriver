module BiDi.Demos.NetworkDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Control.Exception
import Control.Monad (unless)
import Data.Text (Text)
import IOUtils (DemoUtils (..), exceptionTextIncludes)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..), StringValue (MkStringValue))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log)
import WebDriverPreCore.BiDi.Network (SameSite(SameSiteNone))

{-

##### Network #####
1. networkAddDataCollector :: DONE
2. networkAddIntercept :: DONE
3. networkContinueRequest :: DONE
4. networkContinueResponse :: DONE
5. networkContinueWithAuth :: DONE
6. networkDisownData :: DONE
7. networkFailRequest :: DONE
8. networkGetData :: DONE
9. networkProvideResponse :: DONE
10. networkRemoveDataCollector :: DONE
11. networkRemoveIntercept :: DONE
12. networkSetCacheBehavior :: DONE

-}


-- not supported in geckodriver yet
-- >>> runDemo networkDataCollectorDemo
networkDataCollectorDemo :: BiDiDemo
networkDataCollectorDemo =
  demo "Network I - Data Collector Management" action
  where
    action :: DemoUtils -> Commands -> IO ()
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

      logTxt "Test 2: Add data collector with specific collector type"
      collector2 <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [MkDataType "request", MkDataType "response"],
              maxEncodedDataSize = MkJSUInt 2048,
              collectorType = Just (MkCollectorType "blob"),
              contexts = Nothing,
              userContexts = Nothing
            }
      logShow "Typed data collector added" collector2
      pause

      logTxt "Test 3: Add data collector targeting specific browsing context"
      collector3 <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [MkDataType "response"],
              maxEncodedDataSize = MkJSUInt 4096,
              collectorType = Just (MkCollectorType "stream"),
              contexts = Just [bc],
              userContexts = Nothing
            }
      logShow "Context-specific data collector added" collector3
      pause

      logTxt "Test 4: Create user context for targeted data collection"
      userContext <-
        browserCreateUserContext
          MkCreateUserContext
            { acceptInsecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User context created" userContext
      pause

-- not supported in geckodriver yet
      logTxt "Test 5: Add data collector targeting specific user context"
      collector4 <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [MkDataType "request"],
              maxEncodedDataSize = MkJSUInt 8192,
              collectorType = Nothing,
              contexts = Nothing,
              userContexts = Just [userContext]
            }
      logShow "User context-specific data collector added" collector4
      pause

      logTxt "Test 6: Add data collector with multiple data types and large size"
      collector5 <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [MkDataType "request", MkDataType "response", MkDataType "websocket"],
              maxEncodedDataSize = MkJSUInt 16384,
              collectorType = Just (MkCollectorType "buffer"),
              contexts = Just [bc],
              userContexts = Just [userContext]
            }
      logShow "Multi-type data collector added" collector5
      pause

      logTxt "Navigation to trigger some network activity for data collection"
      navResult <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = "data:text/html,<html><head><title>Network Test</title></head><body><h1>Network Activity Test</h1><script>fetch('data:text/plain,hello').then(r=>r.text()).then(console.log)</script></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result" navResult
      pauseMinMs 1000 -- Allow time for network activity
      pause

      logTxt "Test 7: Remove data collectors"
      let MkAddDataCollectorResult collectorId1 = collector1
      removeResult1 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId1
      logShow "Removed basic data collector" removeResult1
      pause

      let MkAddDataCollectorResult collectorId2 = collector2
      removeResult2 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId2
      logShow "Removed typed data collector" removeResult2
      pause

      let MkAddDataCollectorResult collectorId3 = collector3
      removeResult3 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId3
      logShow "Removed context-specific data collector" removeResult3
      pause

      let MkAddDataCollectorResult collectorId4 = collector4
      removeResult4 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId4
      logShow "Removed user context-specific data collector" removeResult4
      pause

      let MkAddDataCollectorResult collectorId5 = collector5
      removeResult5 <- networkRemoveDataCollector $ MkRemoveDataCollector collectorId5
      logShow "Removed multi-type data collector" removeResult5
      pause

      logTxt "Cleanup - remove user context"
      removeUC <- browserRemoveUserContext $ MkRemoveUserContext userContext
      logShow "Removed user context" removeUC
      pause

-- >>> runDemo networkInterceptDemo
networkInterceptDemo :: BiDiDemo
networkInterceptDemo =
  demo "Network II - Request/Response Interception" action
  where
    action :: DemoUtils -> Commands -> IO ()
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
      pauseMinMs 1000 -- Allow time for potential intercepts
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

handleNoSuchRequestError :: (Text -> IO ()) -> IO () -> IO ()
handleNoSuchRequestError log action = catch action $ \e -> do
  unless (exceptionTextIncludes "no such request" e) $
    throwIO e
  log "Expected \"no such request\" error ~ request not initialised"

-- >>> runDemo networkRequestResponseModificationDemo
networkRequestResponseModificationDemo :: BiDiDemo
networkRequestResponseModificationDemo =
  demo "Network III - Request and Response Modification" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      _bc <- rootContext utils cmds

      logTxt "Note: This demo shows parameter usage. Actual request/response modification requires active intercepts and network events."

      logTxt "Test 1: networkContinueRequest with basic parameters"
      let exampleRequestId = MkRequestId "example-request-id-001"
          handleNoSuchRequest = handleNoSuchRequestError logTxt

      -- this will fail because we are continuing a non-existant bogus request
      handleNoSuchRequest $ do
        continueReq1 <-
          networkContinueRequest $
            MkContinueRequest
              { request = exampleRequestId,
                body = Nothing,
                cookies = Nothing,
                headers = Nothing,
                method = Nothing,
                url = Nothing
              }
        logShow "Basic request continuation result" continueReq1
      pause

      logTxt "Test 2: networkContinueRequest with modified headers and method"
      handleNoSuchRequest $ do
        continueReq2 <-
          networkContinueRequest $
            MkContinueRequest
              { request = exampleRequestId,
                body = Nothing,
                cookies = Nothing,
                headers =
                  Just
                    [ MkHeader
                        { headerName = "X-Custom-Header",
                          headerValue = TextBytesValue $ MkStringValue "modified-request"
                        },
                      MkHeader
                        { headerName = "User-Agent",
                          headerValue = TextBytesValue $ MkStringValue "BiDi-WebDriver-Test/1.0"
                        }
                    ],
                method = Just "POST",
                url = Nothing
              }
        logShow "Modified headers and method continuation result" continueReq2
      pause

      logTxt "Test 3: networkContinueRequest with body and URL modification"
      handleNoSuchRequest $ do
        continueReq3 <-
          networkContinueRequest $
            MkContinueRequest
              { request = exampleRequestId,
                body = Just (TextBytesValue $ MkStringValue "modified request body"),
                cookies = Nothing,
                headers = Nothing,
                method = Nothing,
                url = Just "https://modified.example.com/api/test"
              }
        logShow "Body and URL modification continuation result" continueReq3
      pause

      logTxt "Test 4: networkContinueRequest with cookies"
      handleNoSuchRequest $ do
        continueReq4 <-
          networkContinueRequest $
            MkContinueRequest
              { request = exampleRequestId,
                body = Nothing,
                cookies =
                  Just
                    [ MkCookieHeader
                        { cookieHeaderName = "session_id",
                          cookieHeaderValue = TextBytesValue $ MkStringValue "modified-session-123"
                        },
                      MkCookieHeader
                        { cookieHeaderName = "user_pref",
                          cookieHeaderValue = Base64Value "bW9kaWZpZWQ="
                        }
                    ],
                headers = Nothing,
                method = Nothing,
                url = Nothing
              }
        logShow "Cookie modification continuation result" continueReq4
      pause

      logTxt "Test 5: networkContinueResponse with basic parameters"
      handleNoSuchRequest $ do
        continueResp1 <-
          networkContinueResponse $
            MkContinueResponse
              { request = exampleRequestId,
                body = Nothing,
                cookies = Nothing,
                headers = Nothing,
                reasonPhrase = Nothing,
                statusCode = Nothing
              }
        logShow "Basic response continuation result" continueResp1
      pause

      logTxt "Test 6: networkContinueResponse with status code and reason phrase"
      handleNoSuchRequest $ do
        continueResp2 <-
          networkContinueResponse $
            MkContinueResponse
              { request = exampleRequestId,
                body = Nothing,
                cookies = Nothing,
                headers = Nothing,
                reasonPhrase = Just "Modified OK",
                statusCode = Just (MkJSUInt 200)
              }
        logShow "Status modification continuation result" continueResp2
      pause

      logTxt "Test 7: networkContinueResponse with modified response body and headers"
      handleNoSuchRequest $ do
        continueResp3 <-
          networkContinueResponse $
            MkContinueResponse
              { request = exampleRequestId,
                body = Just (TextBytesValue (MkStringValue "Modified response body content")),
                cookies = Nothing,
                headers =
                  Just
                    [ MkHeader
                        { headerName = "Content-Type",
                          headerValue = TextBytesValue (MkStringValue "text/plain; charset=utf-8")
                        },
                      MkHeader
                        { headerName = "X-Modified-Response",
                          headerValue = TextBytesValue (MkStringValue "true")
                        },
                      MkHeader
                        { headerName = "Cache-Control",
                          headerValue = TextBytesValue (MkStringValue "no-cache, no-store")
                        }
                    ],
                reasonPhrase = Just "Custom Response",
                statusCode = Just (MkJSUInt 202)
              }
        logShow "Complete response modification result" continueResp3
      pause

      logTxt "Test 8: networkContinueResponse with response cookies"
      handleNoSuchRequest $ do
        continueResp4 <-
          networkContinueResponse $
            MkContinueResponse
              { request = exampleRequestId,
                body = Nothing,
                cookies =
                  Just
                    [ MkSetCookieHeader
                        { name = "response_token",
                          value = TextBytesValue (MkStringValue "resp-token-456"),
                          domain = Just "api.example.com",
                          path = Just "/",
                          httpOnly = Just True,
                          secure = Just True,
                          -- Gheckodriver: 'default' is not accepted, must use 'lax', 'none', or 'strict'
                          sameSite = Just SameSiteNone,
                          expiry = Just "1767225600", -- Note: expiry is now Text, not Word
                          maxAge = Nothing
                        }
                    ],
                headers = Nothing,
                reasonPhrase = Nothing,
                statusCode = Nothing
              }
        logShow "Response cookie modification result" continueResp4
      pause

-- >>> runDemo networkAuthAndFailureDemo
networkAuthAndFailureDemo :: BiDiDemo
networkAuthAndFailureDemo =
  demo "Network IV - Authentication and Request Failure Handling" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      _bc <- rootContext utils cmds

      logTxt "Note: This demo shows parameter usage. Actual auth/failure handling requires active intercepts and auth events."

      let exampleRequest = MkRequest "example-request-auth-001"
          handleNoSuchRequest = handleNoSuchRequestError logTxt

      logTxt "Test 1: networkContinueWithAuth with default response (no credentials)"
      handleNoSuchRequest $ do
        authResult1 <-
          networkContinueWithAuth $
            MkContinueWithAuth
              { request = exampleRequest,
                authAction = DefaultAuth
              }
        logShow "Default auth response result" authResult1
      pause

      logTxt "Test 2: networkContinueWithAuth with cancel response"
      handleNoSuchRequest $ do
        authResult2 <-
          networkContinueWithAuth $
            MkContinueWithAuth
              { request = exampleRequest,
                authAction = CancelAuth
              }
        logShow "Cancel auth response result" authResult2
      pause

      logTxt "Test 3: networkContinueWithAuth with provided credentials"
      handleNoSuchRequest $ do
        authResult3 <-
          networkContinueWithAuth $
            MkContinueWithAuth
              { request = exampleRequest,
                authAction =
                  ProvideCredentials
                    MkAuthCredentials
                      { username = "test_user",
                        password = "test_password_123"
                      }
              }
        logShow "Provided credentials auth result" authResult3
      pause

      logTxt "Test 4: networkContinueWithAuth with different credentials"
      handleNoSuchRequest $ do
        authResult4 <-
          networkContinueWithAuth $
            MkContinueWithAuth
              { request = exampleRequest,
                authAction =
                  ProvideCredentials
                    MkAuthCredentials
                      { username = "admin@example.com",
                        password = "super_secure_password_456"
                      }
              }
        logShow "Admin credentials auth result" authResult4
      pause

      logTxt "Test 5: networkFailRequest with basic request"
      handleNoSuchRequest $ do
        failResult1 <-
          networkFailRequest $
            MkFailRequest
              { request = MkRequest "example-request-fail-001"
              }
        logShow "Basic failure result" failResult1
      pause

      logTxt "Test 6: networkFailRequest with different request"
      handleNoSuchRequest $ do
        failResult2 <-
          networkFailRequest $
            MkFailRequest
              { request = MkRequest "example-request-fail-002"
              }
        logShow "Second failure result" failResult2
      pause

      logTxt "Test 7: networkFailRequest with another request"
      handleNoSuchRequest $ do
        failResult3 <-
          networkFailRequest $
            MkFailRequest
              { request = MkRequest "example-request-fail-003"
              }
        logShow "Third failure result" failResult3
      pause

      logTxt "Test 8: networkFailRequest with final request"
      handleNoSuchRequest $ do
        failResult4 <-
          networkFailRequest $
            MkFailRequest
              { request = MkRequest "example-request-fail-004"
              }
        logShow "SSL failure result" failResult4
      pause

-- >>> runDemo networkProvideResponseDemo
networkProvideResponseDemo :: BiDiDemo
networkProvideResponseDemo =
  demo "Network V - Custom Response Provision" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      _bc <- rootContext utils cmds

      logTxt "Note: This demo shows parameter usage. Actual response provision requires active intercepts."

      let 
        intercept = MkIntercept "example-intercept-002"
        request = MkRequest "fake-request-002"
        handleNoSuchRequest = handleNoSuchRequestError logTxt

      logTxt "Test 1: networkProvideResponse with minimal parameters"
      handleNoSuchRequest $ do
        provideResult1 <-
          networkProvideResponse $
            MkProvideResponse
              { request,
                intercept,
                body = Nothing,
                cookies = Nothing,
                headers = Nothing,
                reasonPhrase = "OK",
                statusCode = 200
              }
        logShow "Minimal custom response result" provideResult1
      pause

      logTxt "Test 2: networkProvideResponse with JSON content"
      handleNoSuchRequest $ do
        provideResult2 <-
          networkProvideResponse $
            MkProvideResponse
              { request,
                intercept,
                body = Just (TextBytesValue (MkStringValue "{\"message\": \"Custom JSON response\", \"status\": \"success\"}")),
                cookies = Nothing,
                headers =
                  Just
                    [ MkHeader
                        { headerName = "Content-Type",
                          headerValue = TextBytesValue (MkStringValue "application/json")
                        },
                      MkHeader
                        { headerName = "X-Custom-Provider",
                          headerValue = TextBytesValue (MkStringValue "BiDi-WebDriver")
                        }
                    ],
                reasonPhrase = "OK",
                statusCode = 200
              }
        logShow "JSON custom response result" provideResult2
      pause

      logTxt "Test 3: networkProvideResponse with HTML content and redirect"
      handleNoSuchRequest $ do
        provideResult3 <-
          networkProvideResponse $
            MkProvideResponse
              { request,
                intercept,
                body = Just (TextBytesValue (MkStringValue "<html><body><h1>Redirected Page</h1><p>This is a custom redirect response.</p></body></html>")),
                cookies = Nothing,
                headers =
                  Just
                    [ MkHeader
                        { headerName = "Content-Type",
                          headerValue = TextBytesValue (MkStringValue "text/html; charset=utf-8")
                        },
                      MkHeader
                        { headerName = "Location",
                          headerValue = TextBytesValue (MkStringValue "https://custom.example.com/redirected")
                        }
                    ],
                reasonPhrase = "Found",
                statusCode = 302
              }
        logShow "HTML redirect response result" provideResult3
      pause

      logTxt "Test 4: networkProvideResponse with binary content (base64)"
      handleNoSuchRequest $ do
        provideResult4 <-
          networkProvideResponse $
            MkProvideResponse
              { request,
                intercept,
                body = Just (Base64Value "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg=="),
                cookies = Nothing,
                headers =
                  Just
                    [ MkHeader
                        { headerName = "Content-Type",
                          headerValue = TextBytesValue (MkStringValue "image/png")
                        },
                      MkHeader
                        { headerName = "Content-Length",
                          headerValue = TextBytesValue (MkStringValue "95")
                        }
                    ],
                reasonPhrase = "OK",
                statusCode = 200
              }
        logShow "Binary content response result" provideResult4
      pause

      logTxt "Test 5: networkProvideResponse with error status and cookies"
      handleNoSuchRequest $ do
        provideResult5 <-
          networkProvideResponse $
            MkProvideResponse
              { request,
                intercept,
                body = Just (TextBytesValue (MkStringValue "{\"error\": \"Unauthorized access\", \"code\": 401}")),
                cookies =
                  Just
                    [ MkCookie
                        { name = "auth_error",
                          value = TextBytesValue (MkStringValue "unauthorized_attempt"),
                          domain = "secure.example.com",
                          path = "/",
                          size = 120,
                          httpOnly = True,
                          secure = True,
                          sameSite = Strict,
                          expiry = Nothing
                        }
                    ],
                headers =
                  Just
                    [ MkHeader
                        { headerName = "Content-Type",
                          headerValue = TextBytesValue (MkStringValue "application/json")
                        },
                      MkHeader
                        { headerName = "WWW-Authenticate",
                          headerValue = TextBytesValue (MkStringValue "Bearer realm=\"secure\"")
                        }
                    ],
                reasonPhrase = "Unauthorized",
                statusCode = 401
              }
        logShow "Error status with cookies response result" provideResult5
      pause

      logTxt "Test 6: networkProvideResponse with server error"
      handleNoSuchRequest $ do
        provideResult6 <-
          networkProvideResponse $
            MkProvideResponse
              { request,
                intercept,
                body = Just (TextBytesValue (MkStringValue "Internal Server Error - Custom maintenance page")),
                cookies = Nothing,
                headers =
                  Just
                    [ MkHeader
                        { headerName = "Content-Type",
                          headerValue = TextBytesValue (MkStringValue "text/plain")
                        },
                      MkHeader
                        { headerName = "Retry-After",
                          headerValue = TextBytesValue (MkStringValue "3600")
                        }
                    ],
                reasonPhrase = "Internal Server Error",
                statusCode = 500
              }
        logShow "Server error response result" provideResult6
      pause

-- not supported in geckodriver yet
-- >>> runDemo networkDataRetrievalDemo
networkDataRetrievalDemo :: BiDiDemo
networkDataRetrievalDemo =
  demo "Network VI - Data Retrieval and Ownership" action
  where
    action :: DemoUtils -> Commands -> IO ()
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
    action :: DemoUtils -> Commands -> IO ()
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
