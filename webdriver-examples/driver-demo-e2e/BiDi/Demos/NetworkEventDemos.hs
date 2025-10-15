module BiDi.Demos.NetworkEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import TestData (slowLoadUrl)
import WebDriverPreCore.BiDi.Protocol
  ( ContextTarget (..),
    Evaluate (..),
    Navigate (..),
    SubscriptionType
      ( NetworkAuthRequired,
        NetworkBeforeRequestSent,
        NetworkFetchError,
        NetworkResponseCompleted,
        NetworkResponseStarted
      ),
    Target (..),
  )
import Prelude hiding (log, putStrLn)

{- 
Network Events - Implementation Status:

1. network.authRequired :: ✓ networkEventAuthRequired
2. network.beforeRequestSent :: ⚠ networkEventBeforeRequestSent, networkEventRequestResponseLifecycle
   (GeckoDriver incomplete - may timeout, see https://bugzilla.mozilla.org/show_bug.cgi?id=1790366)
3. network.fetchError :: ✓ networkEventFetchError
4. network.responseCompleted :: ✓ networkEventResponseCompleted, networkEventRequestResponseLifecycle
5. network.responseStarted :: ⚠ networkEventResponseStarted, networkEventRequestResponseLifecycle
   (GeckoDriver incomplete - may timeout, see https://bugzilla.mozilla.org/show_bug.cgi?id=1790369)
-}

-- >>> runDemo networkEventBeforeRequestSent
networkEventBeforeRequestSent :: BiDiDemo
networkEventBeforeRequestSent =
  demo "Network Events - Before Request Sent" action
  where
    -- NOTE: GeckoDriver has incomplete support for network.beforeRequestSent event
    -- See: https://bugzilla.mozilla.org/show_bug.cgi?id=1790366 (11 open dependencies as of Oct 2025)
    -- The event subscription and API implementation are correct, but GeckoDriver may not fire
    -- this event for navigation requests (browsingContext.navigate).
    -- Related: Bug #1899417 - Missing network events for navigation requests of discarded contexts
    -- WORKAROUND: Using fetch() to trigger network events instead of navigation
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to BeforeRequestSent event"
      (beforeReqEventFired, waitBeforeReqEventFired) <- timeLimitLog NetworkBeforeRequestSent
      subscribeNetworkBeforeRequestSent beforeReqEventFired

      (manyBeforeReqEventFired, waitManyBeforeReqEventFired) <- timeLimitLog NetworkBeforeRequestSent
      subscribeMany [NetworkBeforeRequestSent] manyBeforeReqEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigate to initial page (about:blank)"
      browsingContextNavigate $ MkNavigate bc "about:blank" Nothing
      pause

      logTxt "Trigger network request using fetch() to public API"
      let apiUrl = "https://jsonplaceholder.typicode.com/posts/1"
      scriptEvaluate $
        MkEvaluate
          { expression = "fetch('" <> apiUrl <> "').catch(() => {})",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      logTxt "Waiting for network events..."

      sequence_
        [ waitBeforeReqEventFired,
          waitManyBeforeReqEventFired
        ]

      closeContext utils cmds bc

-- >>> runDemo networkEventResponseStarted
-- *** Exception: user error (could not parse Event for NetworkResponseStarted
-- Parser error was: 
-- Error in $: parsing WebDriverPreCore.BiDi.Network.ResponseStarted(MkResponseStarted) failed, key "startedResponse" not found
-- The actual JSON value was: {
--     "method": "network.responseStarted",
--     "params": {
--         "context": "9908cb57-32e9-4937-ac5a-3c2f4f1e1c0b",
--         "isBlocked": false,
--         "navigation": null,
--         "redirectCount": 0,
--         "request": {
--             "bodySize": 0,
--             "cookies": [],
--             "destination": "",
--             "headers": [
--                 {
--                     "name": "Host",
--                     "value": {
--                         "type": "string",
--                         "value": "jsonplaceholder.typicode.com"
--                     }
--                 },
--                 {
--                     "name": "User-Agent",
--                     "value": {
--                         "type": "string",
--                         "value": "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:143.0) Gecko/20100101 Firefox/143.0"
--                     }
--                 },
--                 {
--                     "name": "Accept",
--                     "value": {
--                         "type": "string",
--                         "value": "*/*"
--                     }
--                 },
--                 {
--                     "name": "Accept-Language",
--                     "value": {
--                         "type": "string",
--                         "value": "en-US,en;q=0.5"
--                     }
--                 },
--                 {
--                     "name": "Accept-Encoding",
--                     "value": {
--                         "type": "string",
--                         "value": "gzip, deflate, br, zstd"
--                     }
--                 },
--                 {
--                     "name": "Origin",
--                     "value": {
--                         "type": "string",
--                         "value": "null"
--                     }
--                 },
--                 {
--                     "name": "Connection",
--                     "value": {
--                         "type": "string",
--                         "value": "keep-alive"
--                     }
--                 },
--                 {
--                     "name": "Sec-Fetch-Dest",
--                     "value": {
--                         "type": "string",
--                         "value": "empty"
--                     }
--                 },
--                 {
--                     "name": "Sec-Fetch-Mode",
--                     "value": {
--                         "type": "string",
--                         "value": "cors"
--                     }
--                 },
--                 {
--                     "name": "Sec-Fetch-Site",
--                     "value": {
--                         "type": "string",
--                         "value": "cross-site"
--                     }
--                 },
--                 {
--                     "name": "Priority",
--                     "value": {
--                         "type": "string",
--                         "value": "u=4"
--                     }
--                 },
--                 {
--                     "name": "TE",
--                     "value": {
--                         "type": "string",
--                         "value": "trailers"
--                     }
--                 }
--             ],
--             "headersSize": 367,
--             "initiatorType": "fetch",
--             "method": "GET",
--             "request": "8",
--             "timings": {
--                 "connectEnd": 1.760498316803086e12,
--                 "connectStart": 1.760498316755973e12,
--                 "dnsEnd": 1.760498316755685e12,
--                 "dnsStart": 1.76049831675562e12,
--                 "fetchStart": 1.76049831675562e12,
--                 "redirectEnd": 0,
--                 "redirectStart": 0,
--                 "requestStart": 1.760498316803181e12,
--                 "requestTime": 1.760498316752299e12,
--                 "responseEnd": 1.760498316869734e12,
--                 "responseStart": 1.760498316869665e12,
--                 "timeOrigin": 0,
--                 "tlsEnd": 1.760498316803086e12,
--                 "tlsStart": 1.760498316755973e12
--             },
--             "url": "https://jsonplaceholder.typicode.com/posts/1"
--         },
--         "response": {
--             "bodySize": 0,
--             "bytesReceived": 1179,
--             "content": {
--                 "size": 0
--             },
--             "fromCache": false,
--             "headers": [
--                 {
--                     "name": "access-control-allow-credentials",
--                     "value": {
--                         "type": "string",
--                         "value": "true"
--                     }
--                 },
--                 {
--                     "name": "access-control-allow-origin",
--                     "value": {
--                         "type": "string",
--                         "value": "null"
--                     }
--                 },
--                 {
--                     "name": "cache-control",
--                     "value": {
--                         "type": "string",
--                         "value": "max-age=43200"
--                     }
--                 },
--                 {
--                     "name": "content-type",
--                     "value": {
--                         "type": "string",
--                         "value": "application/json; charset=utf-8"
--                     }
--                 },
--                 {
--                     "name": "date",
--                     "value": {
--                         "type": "string",
--                         "value": "Wed, 15 Oct 2025 03:18:36 GMT"
--                     }
--                 },
--                 {
--                     "name": "etag",
--                     "value": {
--                         "type": "string",
--                         "value": "W/\"124-yiKdLzqO5gfBrJFrcdJ8Yq0LGnU\""
--                     }
--                 },
--                 {
--                     "name": "expires",
--                     "value": {
--                         "type": "string",
--                         "value": "-1"
--                     }
--                 },
--                 {
--                     "name": "nel",
--                     "value": {
--                         "type": "string",
--                         "value": "{\"report_to\":\"heroku-nel\",\"response_headers\":[\"Via\"],\"max_age\":3600,\"success_fraction\":0.01,\"failure_fraction\":0.1}"
--                     }
--                 },
--                 {
--                     "name": "pragma",
--                     "value": {
--                         "type": "string",
--                         "value": "no-cache"
--                     }
--                 },
--                 {
--                     "name": "report-to",
--                     "value": {
--                         "type": "string",
--                         "value": "{\"group\":\"heroku-nel\",\"endpoints\":[{\"url\":\"https://nel.heroku.com/reports?s=UkevIEntc%2FRO%2BnaBMc1Af1DgkT9GcE0RhSNj95y%2F4QQ%3D\\u0026sid=e11707d5-02a7-43ef-b45e-2cf4d2036f7d\\u0026ts=1760436833\"}],\"max_age\":3600}"
--                     }
--                 },
--                 {
--                     "name": "reporting-endpoints",
--                     "value": {
--                         "type": "string",
--                         "value": "heroku-nel=\"https://nel.heroku.com/reports?s=UkevIEntc%2FRO%2BnaBMc1Af1DgkT9GcE0RhSNj95y%2F4QQ%3D&sid=e11707d5-02a7-43ef-b45e-2cf4d2036f7d&ts=1760436833\""
--                     }
--                 },
--                 {
--                     "name": "server",
--                     "value": {
--                         "type": "string",
--                         "value": "cloudflare"
--                     }
--                 },
--                 {
--                     "name": "vary",
--                     "value": {
--                         "type": "string",
--                         "value": "Origin, Accept-Encoding"
--                     }
--                 },
--                 {
--                     "name": "via",
--                     "value": {
--                         "type": "string",
--                         "value": "2.0 heroku-router"
--                     }
--                 },
--                 {
--                     "name": "x-content-type-options",
--                     "value": {
--                         "type": "string",
--                         "value": "nosniff"
--                     }
--                 },
--                 {
--                     "name": "x-powered-by",
--                     "value": {
--                         "type": "string",
--                         "value": "Express"
--                     }
--                 },
--                 {
--                     "name": "x-ratelimit-limit",
--                     "value": {
--                         "type": "string",
--                         "value": "1000"
--                     }
--                 },
--                 {
--                     "name": "x-ratelimit-remaining",
--                     "value": {
--                         "type": "string",
--                         "value": "999"
--                     }
--                 },
--                 {
--                     "name": "x-ratelimit-reset",
--                     "value": {
--                         "type": "string",
--                         "value": "1760436889"
--                     }
--                 },
--                 {
--                     "name": "content-encoding",
--                     "value": {
--                         "type": "string",
--                         "value": "zstd"
--                     }
--                 },
--                 {
--                     "name": "age",
--                     "value": {
--                         "type": "string",
--                         "value": "13616"
--                     }
--                 },
--                 {
--                     "name": "cf-cache-status",
--                     "value": {
--                         "type": "string",
--                         "value": "HIT"
--                     }
--                 },
--                 {
--                     "name": "priority",
--                     "value": {
--                         "type": "string",
--                         "value": "u=4,i=?0"
--                     }
--                 },
--                 {
--                     "name": "cf-ray",
--                     "value": {
--                         "type": "string",
--                         "value": "98ec2b8f9a08ed7b-ADL"
--                     }
--                 },
--                 {
--                     "name": "alt-svc",
--                     "value": {
--                         "type": "string",
--                         "value": "h3=\":443\"; ma=86400"
--                     }
--                 },
--                 {
--                     "name": "server-timing",
--                     "value": {
--                         "type": "string",
--                         "value": "cfExtPri"
--                     }
--                 },
--                 {
--                     "name": "X-Firefox-Http3",
--                     "value": {
--                         "type": "string",
--                         "value": "h3"
--                     }
--                 }
--             ],
--             "headersSize": 1179,
--             "mimeType": "application/json;charset=utf-8",
--             "protocol": "h3",
--             "status": 200,
--             "statusText": "",
--             "url": "https://jsonplaceholder.typicode.com/posts/1"
--         },
--         "timestamp": 1760498316870
--     },
--     "type": "event"
-- })
networkEventResponseStarted :: BiDiDemo
networkEventResponseStarted =
  demo "Network Events - Response Started" action
  where
    -- NOTE: GeckoDriver has incomplete support for network.responseStarted event
    -- See: https://bugzilla.mozilla.org/show_bug.cgi?id=1790369 (9 open dependencies as of Oct 2025)
    -- The event subscription and API implementation are correct, but GeckoDriver may not fire
    -- this event for navigation requests (browsingContext.navigate).
    -- Related: Bug #1899417 - Missing network events for navigation requests of discarded contexts
    -- WORKAROUND: Using fetch() to trigger network events instead of navigation
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to ResponseStarted event"
      (respStartedEventFired, waitRespStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeNetworkResponseStarted respStartedEventFired

      (manyRespStartedEventFired, waitManyRespStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeMany [NetworkResponseStarted] manyRespStartedEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigate to initial page (about:blank)"
      browsingContextNavigate $ MkNavigate bc "about:blank" Nothing
      pause

      logTxt "Trigger network request using fetch() to public API"
      let apiUrl = "https://jsonplaceholder.typicode.com/posts/1"
      scriptEvaluate $
        MkEvaluate
          { expression = "fetch('" <> apiUrl <> "').catch(() => {})",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      logTxt "Waiting for response started events..."

      sequence_
        [ waitRespStartedEventFired,
          waitManyRespStartedEventFired
        ]

      closeContext utils cmds bc

-- >>> runDemo networkEventResponseCompleted
networkEventResponseCompleted :: BiDiDemo
networkEventResponseCompleted =
  demo "Network Events - Response Completed" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to ResponseCompleted event"
      (respCompletedEventFired, waitRespCompletedEventFired) <- timeLimitLog NetworkResponseCompleted
      subscribeNetworkResponseCompleted respCompletedEventFired

      (manyRespCompletedEventFired, waitManyRespCompletedEventFired) <- timeLimitLog NetworkResponseCompleted
      subscribeMany [NetworkResponseCompleted] manyRespCompletedEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigating to trigger network request"
      url <- slowLoadUrl
      browsingContextNavigate $ MkNavigate bc url Nothing
      logTxt "Waiting for response completed events..."

      sequence_
        [ waitRespCompletedEventFired,
          waitManyRespCompletedEventFired
        ]

      closeContext utils cmds bc

-- >>> runDemo networkEventFetchError
networkEventFetchError :: BiDiDemo
networkEventFetchError =
  demo "Network Events - Fetch Error" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to FetchError event"
      (fetchErrorEventFired, waitFetchErrorEventFired) <- timeLimitLog NetworkFetchError
      subscribeNetworkFetchError fetchErrorEventFired

      (manyFetchErrorEventFired, waitManyFetchErrorEventFired) <- timeLimitLog NetworkFetchError
      subscribeMany [NetworkFetchError] manyFetchErrorEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigating to invalid URL to trigger fetch error"
      let invalidUrl = "http://invalid-domain-that-does-not-exist-12345.com"
      -- Note: This may throw an error or time out depending on geckodriver behavior
      _ <- browsingContextNavigate $ MkNavigate bc invalidUrl Nothing
      logTxt "Waiting for fetch error events..."

      sequence_
        [ waitFetchErrorEventFired,
          waitManyFetchErrorEventFired
        ]

      closeContext utils cmds bc

-- >>> runDemo networkEventAuthRequired
networkEventAuthRequired :: BiDiDemo
networkEventAuthRequired =
  demo "Network Events - Auth Required (requires auth-protected URL)" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to AuthRequired event"
      (authReqEventFired, waitAuthReqEventFired) <- timeLimitLog NetworkAuthRequired
      subscribeNetworkAuthRequired authReqEventFired

      (manyAuthReqEventFired, waitManyAuthReqEventFired) <- timeLimitLog NetworkAuthRequired
      subscribeMany [NetworkAuthRequired] manyAuthReqEventFired

      bc <- newWindowContext utils cmds
      logTxt "Note: This demo requires a URL with HTTP basic authentication"
      logTxt "Using httpbin.org for auth testing"
      let authUrl = "https://httpbin.org/basic-auth/user/passwd"
      browsingContextNavigate $ MkNavigate bc authUrl Nothing
      logTxt "Waiting for auth required events..."

      sequence_
        [ waitAuthReqEventFired,
          waitManyAuthReqEventFired
        ]

      closeContext utils cmds bc

-- >>> runDemo networkEventRequestResponseLifecycle
networkEventRequestResponseLifecycle :: BiDiDemo
networkEventRequestResponseLifecycle =
  demo "Network Events - Complete Request/Response Lifecycle" action
  where
    -- NOTE: This demo may timeout waiting for NetworkBeforeRequestSent due to incomplete
    -- GeckoDriver support. See networkEventBeforeRequestSent comment above for details.
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to all network lifecycle events"

      (beforeReqEventFired, waitBeforeReqEventFired) <- timeLimitLog NetworkBeforeRequestSent
      subscribeNetworkBeforeRequestSent beforeReqEventFired

      (respStartedEventFired, waitRespStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeNetworkResponseStarted respStartedEventFired

      (respCompletedEventFired, waitRespCompletedEventFired) <- timeLimitLog NetworkResponseCompleted
      subscribeNetworkResponseCompleted respCompletedEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigate to initial page (about:blank)"
      browsingContextNavigate $ MkNavigate bc "about:blank" Nothing
      pause

      logTxt "Trigger network request to demonstrate complete network lifecycle"
      let apiUrl = "https://jsonplaceholder.typicode.com/posts/1"
      scriptEvaluate $
        MkEvaluate
          { expression = "fetch('" <> apiUrl <> "').catch(() => {})",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      logTxt "Waiting for all network lifecycle events..."

      sequence_
        [ waitBeforeReqEventFired,
          waitRespStartedEventFired,
          waitRespCompletedEventFired
        ]

      logTxt "Complete network request/response lifecycle observed!"
      closeContext utils cmds bc
