module BiDi.Demos.NetworkEventDemos where

import BiDi.BiDiRunner (BiDiActions (..), sendCommandNoWait)
import BiDi.DemoUtils
import Data.Text (Text)
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
    Target (..), mkCommand,
  )
import Prelude hiding (log, putStrLn)
import qualified WebDriverPreCore.BiDi.API as P

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

apiUrl :: Text
apiUrl = "https://jsonplaceholder.typicode.com/posts/1"

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
      scriptEvaluate $
        MkEvaluate
          { expression = "fetch('" <> apiUrl <> "')",
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
      (respStartedEventFired, waitNetworkResponseStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeNetworkResponseStarted respStartedEventFired

      (manyRespStartedEventFired, waitManyNetworkResponseStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeMany [NetworkResponseStarted] manyRespStartedEventFired

      bc <- rootContext utils cmds

      logTxt "Trigger network request using fetch() to public API"
      scriptEvaluate $
        MkEvaluate
          { expression = "fetch('" <> apiUrl <> "')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      logTxt "Waiting for response started events..."

      sequence_
        [ waitNetworkResponseStartedEventFired,
          waitManyNetworkResponseStartedEventFired
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
      (respCompletedEventFired, waitNetworkResponseCompletedEventFired) <- timeLimitLog NetworkResponseCompleted
      subscribeNetworkResponseCompleted respCompletedEventFired

      (manyRespCompletedEventFired, waitManyNetworkResponseCompletedEventFired) <- timeLimitLog NetworkResponseCompleted
      subscribeMany [NetworkResponseCompleted] manyRespCompletedEventFired

      bc <- rootContext utils cmds

      logTxt "Trigger network request using fetch() to public API"
      scriptEvaluate $
        MkEvaluate
          { expression = "fetch('" <> apiUrl <> "')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      logTxt "Waiting for response started events..."

      sequence_
        [ waitNetworkResponseCompletedEventFired,
          waitManyNetworkResponseCompletedEventFired
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

      bc <- rootContext utils cmds

      logTxt "Trigger fetch error using invalid URL"
      let invalidUrl = "http://invalid-domain-that-does-not-exist-12345.com"
      scriptEvaluate $
        MkEvaluate
          { expression = "fetch('" <> invalidUrl <> "').catch(() => {})",
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

      bc <- rootContext utils cmds
      logTxt "Note: This demo requires a URL with HTTP basic authentication"
      logTxt "Using httpbin.org for auth testing"
      let authUrl = "https://httpbin.org/basic-auth/user/passwd"
  
      -- need to hand roll a custom command to avoid waiting for navigation to complete
      -- navigation is blocked by the auth challenge popup
      sendCommandNoWait . mkCommand "browsingContext.navigate" $ MkNavigate { context = bc, url = authUrl, wait = Nothing }

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

      (respCompletedEventFired, waitNetworkResponseCompletedEventFired) <- timeLimitLog NetworkResponseCompleted
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
          waitNetworkResponseCompletedEventFired
        ]

      logTxt "Complete network request/response lifecycle observed!"
      closeContext utils cmds bc
