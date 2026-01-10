module BiDi.Demos.NetworkDemos where

import BiDi.Actions (BiDiActions (..))
import BiDi.DemoUtils
  ( BiDiDemo,
    closeContext,
    demo,
    newWindowContext,
    rootContext,
    runDemo
  )
import IOUtils (DemoActions (..))
import TestServerAPI (withTestServer)
import TestServerAPI qualified as URLs
import UnliftIO (putTMVar, readTMVar)
import UnliftIO.STM (atomically, newEmptyTMVarIO, newTVarIO, readTVar, writeTVar)
import WebDriverPreCore.BiDi.Protocol
  ( AddDataCollector (..),
    AddDataCollectorResult (..),
    AddIntercept (..),
    AddInterceptResult (..),
    AuthAction (..),
    AuthCredentials (..),
    AuthRequired (..),
    BeforeRequestSent (..),
    BytesValue (..),
    CacheBehavior (..),
    CollectorType (..),
    Command,
    ContinueRequest (..),
    ContinueResponse (..),
    ContinueWithAuth (..),
    Cookie (..),
    CreateUserContext (..),
    DataType (..),
    DisownData (..),
    FailRequest (..),
    GetData (..),
    Header (..),
    InterceptPhase (..),
    JSUInt (..),
    KnownCommand (..),
    KnownSubscriptionType (..),
    Navigate (..),
    NavigateResult,
    ProvideResponse (..),
    ReadinessState (..),
    RemoveDataCollector (..),
    RemoveIntercept (..),
    RemoveUserContext (..),
    Request (..),
    RequestData (..),
    ResponseCompleted (..),
    ResponseStarted (..),
    SameSite (..),
    SetCacheBehavior (..),
    SetCookieHeader (..),
    SetExtraHeaders (..),
    StringValue (..),
    URL (..),
    UrlPattern (..),
    UrlPatternPattern (..),
    UrlPatternString (..),
    mkCommand,
  )
import Utils (txt)
import Prelude hiding (log)


-- stop warning for unused demo (its used in eval)
_rundemo :: BiDiDemo -> IO ()
_rundemo = runDemo


authTestUrl :: URL
authTestUrl = MkUrl URLs.authTestUrl

boringHelloUrl :: URL
boringHelloUrl = MkUrl URLs.boringHelloUrl

boringHelloUrl2 :: URL
boringHelloUrl2 = MkUrl URLs.boringHelloUrl2

testServerHomeUrl :: URL
testServerHomeUrl = MkUrl URLs.testServerHomeUrl

-- >>> runDemo networkDataCollectorDemo
networkDataCollectorDemo :: BiDiDemo
networkDataCollectorDemo =
  demo
    "Network I - Data Collector Management - Response data contructor added https://www.w3.org/TR/2025/WD-webdriver-bidi-20251001"
    action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      withTestServer $ do
        bc <- rootContext utils bidi

        logTxt "Test 1: Add data collector with minimal parameters"
        collector1 <-
          networkAddDataCollector $
            MkAddDataCollector
              { dataTypes = [Response],
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
              { dataTypes = [Request, Response],
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
              { dataTypes = [Response],
                maxEncodedDataSize = MkJSUInt 4096,
                collectorType = Just (MkCollectorType "blob"),
                contexts = Just [bc],
                userContexts = Nothing
              }
        logShow "Context-specific data collector added" collector3
        pause

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

        logTxt "Test 5: Add data collector targeting specific user context"
        collector4 <-
          networkAddDataCollector $
            MkAddDataCollector
              { dataTypes = [Request],
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
              { dataTypes = [Request, Response],
                maxEncodedDataSize = MkJSUInt 16384,
                collectorType = Just (MkCollectorType "blob"),
                contexts = Just [bc],
                userContexts = Nothing
              }
        logShow "Multi-type data collector added" collector5
        pause

        logTxt "Navigation to trigger some network activity for data collection"
        navResult <-
          browsingContextNavigate $
            MkNavigate
              { context = bc,
                url = boringHelloUrl,
                wait = Just Complete
              }
        logShow "Navigation result" navResult
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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

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
              url = MkUrl "data:text/html,<html><head><title>Intercept Test</title></head><body><h1>Testing Network Intercepts</h1><script>fetch('data:text/plain,test').then(r=>r.text()).then(console.log)</script></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result" navResult
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

-- >>> runDemo networkRequestModificationDemo
networkRequestModificationDemo :: BiDiDemo
networkRequestModificationDemo =
  demo "Network III - Request Modification" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Subscribe first, then intercept and modify request headers and method"

        (beforeReqFired2, waitBeforeReq2) <- timeLimitLog NetworkBeforeRequestSent

        reqIdMVar <- newEmptyTMVarIO
        subscribeNetworkBeforeRequestSent
          ( \event -> do
              let MkBeforeRequestSent {request = MkRequestData {request = reqId}} = event
              atomically $ putTMVar reqIdMVar reqId
              beforeReqFired2 event
          )

        -- Add intercept AFTER subscription
        intercept2 <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId2 = intercept2
        logShow "BeforeRequestSent intercept added" interceptId2
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl,
              wait = Just Complete
            }

        reqId <- atomically $ readTMVar reqIdMVar

        logShow ("Switching urls to " <> boringHelloUrl2.url) reqId

        networkContinueRequest $
          MkContinueRequest
            { request = reqId,
              body = Nothing,
              cookies = Nothing,
              headers = Nothing,
              method = Just "GET",
              url = Just boringHelloUrl2
            }

        waitBeforeReq2
        pause

        removeIntercept2 <- networkRemoveIntercept $ MkRemoveIntercept interceptId2
        logShow "Removed intercept" removeIntercept2
        pause

-- >>> runDemo networkResponseModificationDemo
networkResponseModificationDemo :: BiDiDemo
networkResponseModificationDemo =
  demo "Network IV - Response Modification (Headers & Status Only) - modified in subscription" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Disable cache to ensure we see network requests"
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
        pause

        (respStartedFired, waitRespStarted) <- timeLimitLog NetworkResponseStarted

        subscribeNetworkResponseStarted
          ( \event -> do
              let MkResponseStarted {request = MkRequestData {request = req}} = event

              logShow "Modifying response: status 404, custom headers" req

              networkContinueResponse $
                MkContinueResponse
                  { request = req,
                    cookies =
                      Just
                        [ MkSetCookieHeader
                            { name = "bidi-inserted-cookie",
                              value = TextBytesValue $ MkStringValue "HELLLLO FROMM BIDI",
                              domain = Nothing,
                              path = Nothing,
                              expiry = Nothing,
                              httpOnly = Just True,
                              secure = Just True,
                              maxAge = Nothing,
                              sameSite = Nothing
                            }
                        ],
                    credentials = Nothing,
                    headers =
                      Just
                        [ MkHeader "X-Modified-By" (TextBytesValue $ MkStringValue "WebDriver-BiDi-Demo"),
                          MkHeader "X-Custom-Status" (TextBytesValue $ MkStringValue "Mocked-404"),
                          MkHeader "X-Intercept-Time" (TextBytesValue $ MkStringValue "2025-10-22")
                        ],
                    reasonPhrase = Just "Not Found",
                    statusCode = Just (MkJSUInt 404)
                  }

              respStartedFired event
          )

        logTxt "Subscribe to Response Completed - to see the effect of modification"
        (responseEndFired, waitRespCompleted) <- timeLimitLog NetworkResponseCompleted
        subscribeNetworkResponseCompleted responseEndFired

        intercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [ResponseStarted],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId = intercept
        logShow "ResponseStarted intercept added" interceptId
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl,
              wait = Just Complete
            }

        sequence_ [waitRespStarted, waitRespCompleted]

        removeIntercept <- networkRemoveIntercept $ MkRemoveIntercept interceptId
        logShow "Removed intercept" removeIntercept
        pause

-- >>> runDemo networkResponseModificationDemo
networkResponseModificationDemo2 :: BiDiDemo
networkResponseModificationDemo2 =
  demo "Network IV - Response Modification - with pre-generated id" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Disable cache to ensure we see network requests"
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
        pause

        (responseEndFired, waitRespCompleted) <- timeLimitLog NetworkResponseCompleted
        subscribeNetworkResponseCompleted responseEndFired

        intercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [ResponseStarted],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId = intercept
        logShow "ResponseStarted intercept added" interceptId
        pause

        let requestId = MkJSUInt 9999

        logTxt "Modifying response: status 404, custom headers"
        logShow "Modifying response: status 404, custom headers" 999

        networkContinueResponse $
          MkContinueResponse
            { request = MkRequest $ txt requestId,
              cookies =
                Just
                  [ MkSetCookieHeader
                      { name = "bidi-inserted-cookie",
                        value = TextBytesValue $ MkStringValue "HELLLLO FROMM BIDI",
                        domain = Nothing,
                        path = Nothing,
                        expiry = Nothing,
                        httpOnly = Just True,
                        secure = Just True,
                        maxAge = Nothing,
                        sameSite = Nothing
                      }
                  ],
              credentials = Nothing,
              headers =
                Just
                  [ MkHeader "X-Modified-By" (TextBytesValue $ MkStringValue "WebDriver-BiDi-Demo"),
                    MkHeader "X-Custom-Status" (TextBytesValue $ MkStringValue "Mocked-404"),
                    MkHeader "X-Intercept-Time" (TextBytesValue $ MkStringValue "2025-10-22")
                  ],
              reasonPhrase = Just "Not Found",
              statusCode = Just (MkJSUInt 404)
            }

        let navigateCmd :: Command NavigateResult
            navigateCmd =
              mkCommand BrowsingContextNavigate $
                MkNavigate
                  { context = bc,
                    url = boringHelloUrl,
                    wait = Just Complete
                  }
        sendCommand' requestId navigateCmd

        waitRespCompleted

        removeIntercept <- networkRemoveIntercept $ MkRemoveIntercept interceptId
        logShow "Removed intercept" removeIntercept
        pause

-- >>> runDemo networkAuthCancelDemo
networkAuthCancelDemo :: BiDiDemo
networkAuthCancelDemo =
  demo "Network V-A - Authentication: Cancel" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        authIntercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [AuthRequired],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult authInterceptId = authIntercept
        logShow "AuthRequired intercept added" authInterceptId
        pause

        (authReqFired, waitAuthReq) <- timeLimitLog NetworkAuthRequired

        sub <- subscribeNetworkAuthRequired $ \event -> do
          let MkAuthRequired {request = MkRequestData {request = reqId}} = event
          logShow "AuthRequired event captured (will cancel)" reqId

          networkContinueWithAuth $
            MkContinueWithAuth
              { request = reqId,
                authAction = CancelAuth
              }
          logTxt "Auth request cancelled"
          authReqFired event
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = authTestUrl,
              wait = Nothing
            }
        waitAuthReq
        unsubscribe sub
        pause

        networkRemoveIntercept $ MkRemoveIntercept authInterceptId
        pause

-- >>> runDemo networkAuthWithCredentialsDemo
networkAuthWithCredentialsDemo :: BiDiDemo
networkAuthWithCredentialsDemo =
  demo "Network V-B - Authentication: Credentials" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        authIntercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [AuthRequired],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult authInterceptId = authIntercept
        logShow "AuthRequired intercept added" authInterceptId
        pause

        (authReqFired, waitAuthReq) <- timeLimitLog NetworkAuthRequired

        sub <- subscribeNetworkAuthRequired $ \event -> do
          let MkAuthRequired {request = MkRequestData {request = reqId}} = event
          logShow "AuthRequired event captured (will provide credentials)" reqId

          networkContinueWithAuth $
            MkContinueWithAuth
              { request = reqId,
                authAction =
                  ProvideCredentials $
                    MkAuthCredentials
                      { username = "test_user",
                        password = "test_password_123"
                      }
              }
          logTxt "Provided credentials: test_user / test_password_123"
          authReqFired event
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = authTestUrl,
              wait = Nothing
            }
        waitAuthReq
        unsubscribe sub
        pause

        networkRemoveIntercept $ MkRemoveIntercept authInterceptId
        pause

-- >>> runDemo networkFailRequestDemo
networkFailRequestDemo :: BiDiDemo
networkFailRequestDemo =
  demo "Network VI - Request Failure Handling" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Test 1: Add intercept for BeforeRequestSent phase"
        failIntercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult failInterceptId = failIntercept
        logShow "BeforeRequestSent intercept added for failure demo" failInterceptId
        pause

        logTxt "Test 2: Subscribe to BeforeRequestSent and fail the request"
        (beforeReqFired1, waitBeforeReq1) <- timeLimitLog NetworkBeforeRequestSent
        failReqIdVar1 <- newEmptyTMVarIO
        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {request = reqId}} = event
          logShow "BeforeRequestSent captured (will fail request)" reqId
          atomically $ putTMVar failReqIdVar1 reqId

          networkFailRequest $ MkFailRequest {request = reqId}
          logTxt "Request failed intentionally"
          beforeReqFired1 event
        pause

        logTxt "Test 3: Navigate to trigger request failure"
        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl,
              wait = Nothing
            }
        waitBeforeReq1
        pause

        logTxt "Test 4: Fail another request (second demonstration)"
        (beforeReqFired2, waitBeforeReq2) <- timeLimitLog NetworkBeforeRequestSent
        failReqIdVar2 <- newEmptyTMVarIO
        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {request = reqId}} = event
          logShow "BeforeRequestSent captured (will fail second request)" reqId
          atomically $ putTMVar failReqIdVar2 reqId

          networkFailRequest $ MkFailRequest {request = reqId}
          logTxt "Second request failed intentionally"
          beforeReqFired2 event
        pause

        logTxt "Test 5: Navigate to trigger second request failure"
        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl2,
              wait = Nothing
            }
        waitBeforeReq2
        pause

        logTxt "Cleanup: Remove failure intercept"
        removeFailIntercept <- networkRemoveIntercept $ MkRemoveIntercept failInterceptId
        logShow "Removed BeforeRequestSent intercept" removeFailIntercept
        pause

-- >>> runDemo networkProvideResponseJSONDemo
networkProvideResponseJSONDemo :: BiDiDemo
networkProvideResponseJSONDemo =
  demo "Network VII-A - Provide JSON Response" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Disable cache to ensure we see network requests"
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
        pause

        logTxt "Provide custom JSON response - status 200"

        (beforeReqFired, waitBeforeReq) <- timeLimitLog NetworkBeforeRequestSent

        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {request = reqId}, intercepts = maybeIntercepts} = event
          case maybeIntercepts of
            Just (interceptId : _) -> do
              logShow "Captured request for JSON response" reqId

              let jsonBody = "{\"message\": \"Hello from BiDi!\", \"status\": \"success\", \"data\": [1, 2, 3]}"
              networkProvideResponse $
                MkProvideResponse
                  { request = reqId,
                    intercept = interceptId,
                    body = Just $ TextBytesValue $ MkStringValue jsonBody,
                    cookies = Nothing,
                    headers = Just [MkHeader "Content-Type" (TextBytesValue $ MkStringValue "application/json")],
                    reasonPhrase = "OK",
                    statusCode = 200
                  }
              logTxt "Provided custom JSON response"
            _ -> pure ()
          beforeReqFired event

        intercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId = intercept
        logShow "BeforeRequestSent intercept added" interceptId
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl,
              wait = Just Complete
            }

        waitBeforeReq
        pause

        networkRemoveIntercept $ MkRemoveIntercept interceptId
        logTxt "Removed intercept"
        pause

-- >>> runDemo networkProvideResponseHTMLDemo
networkProvideResponseHTMLDemo :: BiDiDemo
networkProvideResponseHTMLDemo =
  demo "Network VII-B - Provide HTML Response" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Disable cache to ensure we see network requests"
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
        pause

        logTxt "Provide HTML response with custom status"

        (beforeReqFired, waitBeforeReq) <- timeLimitLog NetworkBeforeRequestSent

        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {request = reqId}, intercepts = maybeIntercepts} = event
          case maybeIntercepts of
            Just (interceptId : _) -> do
              logShow "Captured request for HTML response" reqId

              let htmlBody = "<html><head><title>Custom Response</title></head><body><h1>This is a custom HTML response from BiDi!</h1><p>Status: 201 Created</p></body></html>"
              networkProvideResponse $
                MkProvideResponse
                  { request = reqId,
                    intercept = interceptId,
                    body = Just $ TextBytesValue $ MkStringValue htmlBody,
                    cookies = Nothing,
                    headers =
                      Just
                        [ MkHeader "Content-Type" (TextBytesValue $ MkStringValue "text/html; charset=utf-8"),
                          MkHeader "X-Custom-Header" (TextBytesValue $ MkStringValue "BiDi-Generated")
                        ],
                    reasonPhrase = "Created",
                    statusCode = 201
                  }
              logTxt "Provided custom HTML response with status 201"
            _ -> pure ()
          beforeReqFired event

        intercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId = intercept
        logShow "BeforeRequestSent intercept added" interceptId
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl2,
              wait = Just Complete
            }

        waitBeforeReq
        pause

        networkRemoveIntercept $ MkRemoveIntercept interceptId
        logTxt "Removed intercept"
        pause

-- >>> runDemo networkProvideResponseWithCookiesDemo
networkProvideResponseWithCookiesDemo :: BiDiDemo
networkProvideResponseWithCookiesDemo =
  demo "Network VII-C - Provide Response with Cookies" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Disable cache to ensure we see network requests"
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
        pause

        logTxt "Provide error response with cookies"

        (beforeReqFired, waitBeforeReq) <- timeLimitLog NetworkBeforeRequestSent

        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {request = reqId}, intercepts = maybeIntercepts} = event
          case maybeIntercepts of
            Just (interceptId : _) -> do
              logShow "Captured request for error response with cookies" reqId

              let errorBody = "{\"error\": \"Unauthorized\", \"message\": \"Please provide valid credentials\"}"
              networkProvideResponse $
                MkProvideResponse
                  { request = reqId,
                    intercept = interceptId,
                    body = Just $ TextBytesValue $ MkStringValue errorBody,
                    cookies =
                      Just
                        [ MkCookie
                            { name = "session_expired",
                              value = TextBytesValue $ MkStringValue "true",
                              domain = "localhost",
                              path = "/",
                              size = 4,
                              httpOnly = True,
                              secure = False,
                              sameSite = Strict,
                              expiry = Nothing
                            }
                        ],
                    headers =
                      Just
                        [ MkHeader "Content-Type" (TextBytesValue $ MkStringValue "application/json"),
                          MkHeader "WWW-Authenticate" (TextBytesValue $ MkStringValue "Bearer")
                        ],
                    reasonPhrase = "Unauthorized",
                    statusCode = 401
                  }
              logTxt "Provided 401 error response with cookies"
            _ -> pure ()
          beforeReqFired event

        intercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId = intercept
        logShow "BeforeRequestSent intercept added" interceptId
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = testServerHomeUrl,
              wait = Just Complete
            }

        waitBeforeReq
        pause

        networkRemoveIntercept $ MkRemoveIntercept interceptId
        logTxt "Removed intercept"
        pause

-- >>> runDemo networkProvideResponseBase64Demo
networkProvideResponseBase64Demo :: BiDiDemo
networkProvideResponseBase64Demo =
  demo "Network VII-D - Provide Base64 Response" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Disable cache to ensure we see network requests"
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
        pause

        logTxt "Provide base64 encoded response (simulated image)"

        (beforeReqFired, waitBeforeReq) <- timeLimitLog NetworkBeforeRequestSent

        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {request = reqId}, intercepts = maybeIntercepts} = event
          case maybeIntercepts of
            Just (interceptId : _) -> do
              logShow "Captured request for base64 response" reqId

              -- Tiny 1x1 red PNG in base64
              let pngBase64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg=="
              networkProvideResponse $
                MkProvideResponse
                  { request = reqId,
                    intercept = interceptId,
                    body = Just $ Base64Value pngBase64,
                    cookies = Nothing,
                    headers =
                      Just
                        [ MkHeader "Content-Type" (TextBytesValue $ MkStringValue "image/png"),
                          MkHeader "Cache-Control" (TextBytesValue $ MkStringValue "no-cache")
                        ],
                    reasonPhrase = "OK",
                    statusCode = 200
                  }
              logTxt "Provided base64 encoded PNG response"
            _ -> pure ()
          beforeReqFired event

        intercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId = intercept
        logShow "BeforeRequestSent intercept added" interceptId
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl,
              wait = Just Complete
            }

        waitBeforeReq
        pause

        networkRemoveIntercept $ MkRemoveIntercept interceptId
        logTxt "Removed intercept"
        pause

-- >>> runDemo networkProvideResponseErrorDemo
networkProvideResponseErrorDemo :: BiDiDemo
networkProvideResponseErrorDemo =
  demo "Network VII-E - Provide Error Response" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Disable cache to ensure we see network requests"
        networkSetCacheBehavior $
          MkSetCacheBehavior
            { cacheBehavior = BypassCache,
              contexts = Just [bc]
            }
        pause

        logTxt "Provide server error response"

        (beforeReqFired, waitBeforeReq) <- timeLimitLog NetworkBeforeRequestSent

        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {request = reqId}, intercepts = maybeIntercepts} = event
          case maybeIntercepts of
            Just (interceptId : _) -> do
              logShow "Captured request for server error response" reqId

              let errorBody = "<html><body><h1>500 Internal Server Error</h1><p>Something went wrong on our end.</p></body></html>"
              networkProvideResponse $
                MkProvideResponse
                  { request = reqId,
                    intercept = interceptId,
                    body = Just $ TextBytesValue $ MkStringValue errorBody,
                    cookies = Nothing,
                    headers =
                      Just
                        [ MkHeader "Content-Type" (TextBytesValue $ MkStringValue "text/html"),
                          MkHeader "Retry-After" (TextBytesValue $ MkStringValue "3600")
                        ],
                    reasonPhrase = "Internal Server Error",
                    statusCode = 500
                  }
              logTxt "Provided 500 server error response"
            _ -> pure ()
          beforeReqFired event

        intercept <-
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
        let MkAddInterceptResult interceptId = intercept
        logShow "BeforeRequestSent intercept added" interceptId
        pause

        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl2,
              wait = Just Complete
            }

        waitBeforeReq
        pause

        networkRemoveIntercept $ MkRemoveIntercept interceptId
        logTxt "Removed intercept"
        pause

-- >>> runDemo networkDataRetrievalDemo
networkDataRetrievalDemo :: BiDiDemo
networkDataRetrievalDemo =
  demo "Network VIII - Data Retrieval and Ownership" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Add data collector to capture network data"
      MkAddDataCollectorResult collectorId <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [Response],
              maxEncodedDataSize = MkJSUInt 2048,
              collectorType = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      logShow "Data collector created" collectorId
      pause

      logTxt "Test 2: Subscribe to network.responseCompleted event to capture request ID"
      requestIdVar <- newTVarIO Nothing
      (responseCompletedFired, waitResponseCompleted) <- timeLimitLog NetworkResponseCompleted
      subscribeNetworkResponseCompleted $ \event -> do
        let MkResponseCompleted {request = MkRequestData {request = requestId}} = event
        logShow "Captured request ID from event" requestId
        atomically $ writeTVar requestIdVar (Just requestId)
        responseCompletedFired event
      pause

      logTxt "Test 3: Navigate to trigger a real network request"
      withTestServer $ do
        navResult <-
          browsingContextNavigate $
            MkNavigate
              { context = bc,
                url = testServerHomeUrl,
                wait = Just Complete
              }
        logShow "Navigation result" navResult

        logTxt "Waiting for response completed event..."
        waitResponseCompleted

      logTxt "Test 4: Retrieve the captured request ID"
      maybeRequestId <- atomically $ readTVar requestIdVar
      case maybeRequestId of
        Nothing -> logTxt "ERROR: No request ID was captured from events"
        Just (MkRequest requestIdText) -> do
          let capturedRequest = MkRequest requestIdText
          logShow "Using captured request ID" capturedRequest
          pause

          logTxt "Test 6: networkGetData with disown=True"
          getData2 <-
            networkGetData $
              MkGetData
                { dataType = Response,
                  collector = Just collectorId,
                  disown = Just True,
                  request = capturedRequest
                }
          logShow "Get data with disowning result" getData2
          pause

-- >>> runDemo networkDisownDataDemo
networkDisownDataDemo :: BiDiDemo
networkDisownDataDemo =
  demo "Network IX - Data Retrieval and Disown" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Add data collector to capture network data"
      MkAddDataCollectorResult collectorId <-
        networkAddDataCollector $
          MkAddDataCollector
            { dataTypes = [Response],
              maxEncodedDataSize = MkJSUInt 2048,
              collectorType = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      logShow "Data collector created" collectorId
      pause

      logTxt "Test 2: Subscribe to network.responseCompleted event to capture request ID"
      requestIdVar <- newTVarIO Nothing
      (responseCompletedFired, waitResponseCompleted) <- timeLimitLog NetworkResponseCompleted
      subscribeNetworkResponseCompleted $ \event -> do
        let MkResponseCompleted {request = MkRequestData {request = requestId}} = event
        logShow "Captured request ID from event" requestId
        atomically $ writeTVar requestIdVar (Just requestId)
        responseCompletedFired event
      pause

      logTxt "Test 3: Navigate to trigger a real network request"
      withTestServer $ do
        navResult <-
          browsingContextNavigate $
            MkNavigate
              { context = bc,
                url = testServerHomeUrl,
                wait = Just Complete
              }
        logShow "Navigation result" navResult

        logTxt "Waiting for response completed event..."
        waitResponseCompleted

      logTxt "Test 4: Retrieve the captured request ID"
      maybeRequestId <- atomically $ readTVar requestIdVar
      case maybeRequestId of
        Nothing -> logTxt "ERROR: No request ID was captured from events"
        Just (MkRequest requestIdText) -> do
          let capturedRequest = MkRequest requestIdText
          logShow "Using captured request ID" capturedRequest
          pause

          logTxt "Test 5: networkGetData with disown=False"
          getData1 <-
            networkGetData $
              MkGetData
                { dataType = Response,
                  collector = Just collectorId,
                  disown = Just False,
                  request = capturedRequest
                }
          logShow "Get data without disowning result" getData1
          pause

          logTxt "Test 6: networkDisownData - explicitly disown specific data"
          disownResult <-
            networkDisownData $
              MkDisownData
                { dataType = Response,
                  collector = collectorId,
                  request = capturedRequest
                }
          logShow "Disown data result" disownResult
          pause

-- >>> runDemo networkCacheBehaviorDemo
networkCacheBehaviorDemo :: BiDiDemo
networkCacheBehaviorDemo =
  demo "Network X - Cache Behavior Management" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

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
      newContext <- newWindowContext utils bidi

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
              url = MkUrl "data:text/html,<html><head><title>Cache Test 1</title></head><body><h1>Testing Cache Behavior - Context 1</h1></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result - original context" navResult1
      pause

      navResult2 <-
        browsingContextNavigate $
          MkNavigate
            { context = newContext,
              url = MkUrl "data:text/html,<html><head><title>Cache Test 2</title></head><body><h1>Testing Cache Behavior - Context 2</h1></body></html>",
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
      closeContext utils bidi newContext


-- >>> runDemo networkSetExtraHeadersDemo
networkSetExtraHeadersDemo :: BiDiDemo
networkSetExtraHeadersDemo =
  demo "Network XI - Set Extra Headers -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251106" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Test 1: Set extra headers globally"
        setExtraHeaders1 <-
          networkSetExtraHeaders $
            MkSetExtraHeaders
              { headers =
                  [ MkHeader "X-Custom-Header-1" (TextBytesValue $ MkStringValue "global-value"),
                    MkHeader "X-Test-Token" (TextBytesValue $ MkStringValue "abc123")
                  ],
                contexts = Nothing,
                userContexts = Nothing
              }
        logShow "Set extra headers globally result" setExtraHeaders1
        pause

        logTxt "Test 2: Set extra headers for specific browsing context"
        setExtraHeaders2 <-
          networkSetExtraHeaders $
            MkSetExtraHeaders
              { headers =
                  [ MkHeader "X-Context-Specific" (TextBytesValue $ MkStringValue "context-value"),
                    MkHeader "Authorization" (TextBytesValue $ MkStringValue "Bearer token123")
                  ],
                contexts = Just [bc],
                userContexts = Nothing
              }
        logShow "Set extra headers for context result" setExtraHeaders2
        pause

        logTxt "Test 3: Create user context for targeted header setting"
        userContext <-
          browserCreateUserContext
            MkCreateUserContext
              { insecureCerts = Nothing,
                proxy = Nothing,
                unhandledPromptBehavior = Nothing
              }
        logShow "User context created" userContext
        pause

        logTxt "Test 4: Set extra headers for specific user context"
        setExtraHeaders3 <-
          networkSetExtraHeaders $
            MkSetExtraHeaders
              { headers =
                  [ MkHeader "X-User-Context-Header" (TextBytesValue $ MkStringValue "user-context-value")
                  ],
                contexts = Nothing,
                userContexts = Just [userContext]
              }
        logShow "Set extra headers for user context result" setExtraHeaders3
        pause

        logTxt "Test 5: Set extra headers with base64 values"
        setExtraHeaders4 <-
          networkSetExtraHeaders $
            MkSetExtraHeaders
              { headers =
                  [ MkHeader "X-Base64-Header" (Base64Value "SGVsbG8gV29ybGQ=")
                  ],
                contexts = Just [bc],
                userContexts = Nothing
              }
        logShow "Set extra headers with base64 result" setExtraHeaders4
        pause

        logTxt "Test 6: Subscribe to network events to verify headers"
        (beforeReqFired, waitBeforeReq) <- timeLimitLog NetworkBeforeRequestSent
        subscribeNetworkBeforeRequestSent $ \event -> do
          let MkBeforeRequestSent {request = MkRequestData {headers = requestHeaders}} = event
          logShow "Request headers" requestHeaders
          beforeReqFired event
        pause

        logTxt "Test 7: Navigate to trigger request with extra headers"
        sendCommandNoWait . mkCommand BrowsingContextNavigate $
          MkNavigate
            { context = bc,
              url = boringHelloUrl,
              wait = Just Complete
            }
        waitBeforeReq
        pause

        logTxt "Test 8: Clear extra headers by setting empty list"
        setExtraHeaders5 <-
          networkSetExtraHeaders $
            MkSetExtraHeaders
              { headers = [],
                contexts = Nothing,
                userContexts = Nothing
              }
        logShow "Cleared extra headers result" setExtraHeaders5
        pause

        logTxt "Cleanup - remove user context"
        removeUC <- browserRemoveUserContext $ MkRemoveUserContext userContext
        logShow "Removed user context" removeUC
        pause
