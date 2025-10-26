module BiDi.Demos.NetworkEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import TestServerAPI (authTestUrl, testServerHomeUrl, withTestServer, malformedResponseUrl)
import WebDriverPreCore.BiDi.Command (CommandMethod(..))
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
    mkCommand,
  )
import Prelude hiding (log, putStrLn)

-- >>> runDemo networkEventRequestResponseLifecycle
networkEventRequestResponseLifecycle :: BiDiDemo
networkEventRequestResponseLifecycle =
  demo "Network Events - Complete Request/Response Lifecycle" action
  where
    -- NOTE: This demo may timeout waiting for NetworkBeforeRequestSent due to incomplete
    -- GeckoDriver support. See networkEventBeforeRequestSent comment above for details.
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to all network lifecycle events"

      (beforeReqEventFired, waitBeforeReqEventFired) <- timeLimitLog NetworkBeforeRequestSent
      subscribeNetworkBeforeRequestSent beforeReqEventFired

      (respStartedEventFired, waitRespStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeNetworkResponseStarted respStartedEventFired

      (respCompletedEventFired, waitNetworkResponseCompletedEventFired) <- timeLimitLog NetworkResponseCompleted
      subscribeNetworkResponseCompleted respCompletedEventFired

      bc <- rootContext utils bidi

      withTestServer $ do
        logTxt "Trigger network request to demonstrate complete network lifecycle"
        scriptEvaluate $
          MkEvaluate
            { expression = "fetch('" <> testServerHomeUrl <> "')",
              target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }
        logTxt "Waiting for all network lifecycle events..."

        sequence_
          [ waitBeforeReqEventFired,
            waitRespStartedEventFired,
            waitNetworkResponseCompletedEventFired
          ]

-- >>> runDemo networkEventFetchError
networkEventFetchError :: BiDiDemo
networkEventFetchError =
  demo "Network Events - Fetch Error" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      withTestServer $ do
        logTxt "Subscribe to FetchError event"
        (fetchErrorEventFired, waitFetchErrorEventFired) <- timeLimitLog NetworkFetchError
        subscribeNetworkFetchError fetchErrorEventFired

        (manyFetchErrorEventFired, waitManyFetchErrorEventFired) <- timeLimitLogMany NetworkFetchError
        subscribeMany [NetworkFetchError] manyFetchErrorEventFired

        bc <- rootContext utils bidi

        logTxt "Trigger fetch error using invalid URL"
        scriptEvaluate $
          MkEvaluate
            { expression = "fetch('" <> malformedResponseUrl <> "')",
              target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }
        logTxt "Waiting for fetch error events..."

        sequence_
          [ waitFetchErrorEventFired,
            waitManyFetchErrorEventFired
          ]

-- >>> runDemo networkEventAuthRequired
networkEventAuthRequired :: BiDiDemo
networkEventAuthRequired =
  demo "Network Events - Auth Required (requires auth-protected URL)" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to AuthRequired event"
      (authReqEventFired, waitAuthReqEventFired) <- timeLimitLog NetworkAuthRequired
      subscribeNetworkAuthRequired authReqEventFired

      (manyAuthReqEventFired, waitManyAuthReqEventFired) <- timeLimitLogMany NetworkAuthRequired
      subscribeMany [NetworkAuthRequired] manyAuthReqEventFired

      bc <- rootContext utils bidi

      logTxt "Navigate to auth-protected URL to trigger AuthRequired event"

      withTestServer $ do
        sendCommandNoWait . mkCommand BrowsingContextNavigate $ MkNavigate {context = bc, url = authTestUrl, wait = Nothing}

        logTxt "Waiting for auth required events..."
        pause

        sequence_
          [ waitAuthReqEventFired,
            waitManyAuthReqEventFired
          ]
