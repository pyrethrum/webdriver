module BiDi.Demos.NetworkEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import TestData (checkboxesUrl, slowLoadUrl, textAreaUrl)
import WebDriverPreCore.BiDi.Protocol
  ( Navigate (..),
    SubscriptionType
      ( NetworkAuthRequired,
        NetworkBeforeRequestSent,
        NetworkFetchError,
        NetworkResponseCompleted,
        NetworkResponseStarted
      ),
  )
import Prelude hiding (log, putStrLn)

{- 
Network Events - Implementation Status:

1. network.authRequired :: ✓ networkEventAuthRequired
2. network.beforeRequestSent :: ✓ networkEventBeforeRequestSent, networkEventRequestResponseLifecycle
3. network.fetchError :: ✓ networkEventFetchError
4. network.responseCompleted :: ✓ networkEventResponseCompleted, networkEventRequestResponseLifecycle
5. network.responseStarted :: ✓ networkEventResponseStarted, networkEventRequestResponseLifecycle
-}

-- >>> runDemo networkEventBeforeRequestSent
-- *** Exception: user error (Timeout - Expected event did not fire: NetworkBeforeRequestSent)
networkEventBeforeRequestSent :: BiDiDemo
networkEventBeforeRequestSent =
  demo "Network Events - Before Request Sent" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to BeforeRequestSent event"
      (beforeReqEventFired, waitBeforeReqEventFired) <- timeLimitLog NetworkBeforeRequestSent
      subscribeNetworkBeforeRequestSent beforeReqEventFired

      (manyBeforeReqEventFired, waitManyBeforeReqEventFired) <- timeLimitLog NetworkBeforeRequestSent
      subscribeMany [NetworkBeforeRequestSent] manyBeforeReqEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigating to trigger network request"
      url <- textAreaUrl
      browsingContextNavigate $ MkNavigate bc url Nothing
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
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to ResponseStarted event"
      (respStartedEventFired, waitRespStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeNetworkResponseStarted respStartedEventFired

      (manyRespStartedEventFired, waitManyRespStartedEventFired) <- timeLimitLog NetworkResponseStarted
      subscribeMany [NetworkResponseStarted] manyRespStartedEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigating to trigger network request"
      url <- checkboxesUrl
      browsingContextNavigate $ MkNavigate bc url Nothing
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
      logTxt "Navigating to demonstrate complete network lifecycle"
      url <- textAreaUrl
      browsingContextNavigate $ MkNavigate bc url Nothing
      logTxt "Waiting for all network lifecycle events..."

      sequence_
        [ waitBeforeReqEventFired,
          waitRespStartedEventFired,
          waitRespCompletedEventFired
        ]

      logTxt "Complete network request/response lifecycle observed!"
      closeContext utils cmds bc
