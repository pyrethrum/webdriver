module BiDi.Demos.SessionDemos where

import BiDi.Actions (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoActions (..))
import WebDriverPreCore.BiDi.Protocol
  ( Capabilities (..),
    Capability (..),
    CreateUserContext (..),
    KnownSubscriptionType (..),
    ProxyConfiguration (..),
    SessionNewResult (..),
    SessionStatusResult (..),
    SessionSubscibe (..),
    SessionUnsubscribe (..),
    SubscriptionType (..),
    UserPromptHandler (..),
    UserPromptHandlerType (..),
  )
import Prelude hiding (log, putStrLn)

-- TODO: change from text to typed events

-- >>> runDemo sessionStatusDemo
sessionStatusDemo :: BiDiDemo
sessionStatusDemo =
  demo "Session - Status Check" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Checking session status"
      status <- sessionStatus
      logShow "Session status" status
      pause

      logTxt "Interpreting status result"
      case status of
        MkSessionStatusResult True msg -> logTxt $ "✓ Session is ready: " <> msg
        MkSessionStatusResult False msg -> logTxt $ "✗ Session not ready: " <> msg
      pause

-- >>> runDemo sessionNewDemo

-- *** Exception: Error executing BiDi command: MkCommand

--   { method = "session.new"
--   , params =
--       MkCapabilities { alwaysMatch = Nothing , firstMatch = [] }
--   , extended = Nothing
--   }
-- With JSON:
-- {
--     "id": 1,
--     "method": "session.new",
--     "params": {
--         "capabilities": {},
--         "firstMatch": []
--     }
-- }
-- BiDi driver error:
-- MkDriverError
--   { id = Just 1
--   , error = SessionNotCreated
--   , description = "Failed to create a new session"
--   , message = "Maximum number of active sessions"
--   , stacktrace =
--       Just
--         "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nSessionNotCreatedError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:814:5\ncreateSession@chrome://remote/content/webdriver-bidi/WebDriverBiDi.sys.mjs:127:13\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:206:55\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n"
--   , extensions = MkEmptyResult { extensible = fromList [] }
--   }
sessionNewDemo :: BiDiDemo
sessionNewDemo =
  demo "Session - New Session Creation" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Creating new BiDi session with basic capabilities"
      let basicCapabilities =
            MkCapabilities
              { alwaysMatch = Nothing,
                firstMatch = []
              }
      newSession <- sessionNew basicCapabilities
      logShow "New session created" newSession
      pause

      logTxt "Session information:"
      logTxt $ "Session ID: " <> newSession.sessionId
      logShow "Capabilities result" newSession.capabilities
      pause

-- TODO: Add orchestration between receive loop and session.end to suppress ConnectionClosed when connection closes gracefully

-- >>> runDemo sessionEndDemo
sessionEndDemo :: BiDiDemo
sessionEndDemo =
  demo "Session - End Session" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "⚠️  WARNING: This will end the current session!"
      logTxt "Ending current session gracefully"
      result <- sessionEnd
      logShow "Session end result" result
      pause

-- >>> runDemo sessionSubscribeDemo
sessionSubscribeDemo :: BiDiDemo
sessionSubscribeDemo =
  demo "Session - Event Subscription" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Subscribe to browsing context events globally"
      let globalSubscription =
            MkSessionSubscribe
              { events = KnownSubscriptionType <$> [BrowsingContextContextCreated, BrowsingContextContextDestroyed],
                contexts = Nothing,
                userContexts = Nothing
              }
      sub1 <- sessionSubscribe globalSubscription
      logShow "Global subscription" sub1
      pause

      logTxt "Test 2: Subscribe to network events for specific context"
      let contextSubscription =
            MkSessionSubscribe
              { events = KnownSubscriptionType <$> [NetworkFetchError, NetworkResponseCompleted],
                contexts = Just [bc],
                userContexts = Nothing
              }
      sub2 <- sessionSubscribe contextSubscription
      logShow "Context-specific subscription" sub2
      pause

      logTxt "Test 3: Subscribe to script events for user context"
      -- Get current user contexts or create a new one if needed
      userContextsResult <- browserGetUserContexts
      logShow "Current user contexts" userContextsResult

      -- Create a new user context for demonstration
      currentUserContext <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "Created user context" currentUserContext

      let userContextSubscription =
            MkSessionSubscribe
              { events = [KnownSubscriptionType ScriptRealmCreated],
                contexts = Nothing,
                userContexts = Just [currentUserContext]
              }
      sub3 <- sessionSubscribe userContextSubscription
      logShow "User context subscription" sub3
      pause

-- >>> runDemo sessionUnsubscribeDemo
sessionUnsubscribeDemo :: BiDiDemo
sessionUnsubscribeDemo =
  demo "Session - Event Unsubscription" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "First, create a subscription to demonstrate unsubscription"
      let subscription =
            MkSessionSubscribe
              { events = [KnownSubscriptionType BrowsingContextContextCreated],
                contexts = Nothing,
                userContexts = Nothing
              }
      subResult <- sessionSubscribe subscription
      logShow "Created subscription" subResult
      pause

      logTxt "Test 1: Unsubscribe by subscription ID"
      let unsubByID =
            UnsubscribeById
              { subscriptions = [subResult]
              }
      result1 <- sessionUnsubscribe unsubByID
      logShow "Unsubscribed by ID" result1
      pause

      logTxt "Now, Subscribe to network events for specific context"
      let contextSubscription =
            MkSessionSubscribe
              { events = [KnownSubscriptionType NetworkResponseCompleted],
                contexts = Nothing,
                userContexts = Nothing
              }
      sub2 <- sessionSubscribe contextSubscription
      logShow "Context-specific subscription" sub2
      pause

      logTxt "Test 2: Unsubscribe by attributes (alternative method)"
      let unsubByAttrs =
            UnsubscribeByAttributes
              { unsubEvents = [KnownSubscriptionType NetworkResponseCompleted]
              }
      result2 <- sessionUnsubscribe unsubByAttrs
      logShow "Unsubscribed by attributes" result2
      pause

-- >>> runDemo sessionCapabilityNegotiationDemo

-- *** Exception: Error executing BiDi command: MkCommand

--   { method = "session.new"
--   , params =
--       MkCapabilities
--         { alwaysMatch =
--             Just
--               MkCapability
--                 { acceptInsecureCerts = Just True
--                 , browserName = Just "firefox"
--                 , browserVersion = Nothing
--                 , webSocketUrl = True
--                 , platformName = Just "linux"
--                 , proxy = Nothing
--                 , unhandledPromptBehavior = Nothing
--                 }
--         , firstMatch = []
--         }
--   , extended = Nothing
--   }
-- With JSON:
-- {
--     "id": 1,
--     "method": "session.new",
--     "params": {
--         "capabilities": {
--             "alwaysMatch": {
--                 "acceptInsecureCerts": true,
--                 "browserName": "firefox",
--                 "browserVersion": null,
--                 "platformName": "linux",
--                 "proxy": null,
--                 "unhandledPromptBehavior": null,
--                 "webSocketUrl": true
--             }
--         },
--         "firstMatch": []
--     }
-- }
-- Failed to decode the 'result' property of JSON returned by driver to response type:
-- {
--     "error": "session not created",
--     "id": 1,
--     "message": "Maximum number of active sessions",
--     "stacktrace": "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nSessionNotCreatedError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:814:5\ncreateSession@chrome://remote/content/webdriver-bidi/WebDriverBiDi.sys.mjs:127:13\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:206:55\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n",
--     "type": "error"
-- }
-- Error message:
-- key "result" not found
sessionCapabilityNegotiationDemo :: BiDiDemo
sessionCapabilityNegotiationDemo =
  demo "Session - Capability Negotiation" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Test 1: Session with alwaysMatch capabilities"
      let alwaysMatchCap =
            MkCapability
              { acceptInsecureCerts = Just True,
                browserName = Just "firefox",
                browserVersion = Nothing,
                webSocketUrl = True,
                platformName = Just "linux",
                proxy = Nothing,
                unhandledPromptBehavior = Nothing
              }
      let alwaysMatchCapabilities =
            MkCapabilities
              { alwaysMatch = Just alwaysMatchCap,
                firstMatch = []
              }
      session1 <- sessionNew alwaysMatchCapabilities
      logShow "Session with alwaysMatch" session1
      pause

      logTxt "Test 2: Session with firstMatch capabilities"
      let firstMatchCap1 =
            MkCapability
              { acceptInsecureCerts = Just False,
                browserName = Just "firefox",
                browserVersion = Just "130.0",
                webSocketUrl = True,
                platformName = Just "linux",
                proxy = Just DirectProxyConfiguration,
                unhandledPromptBehavior = Nothing
              }
      let firstMatchCap2 =
            MkCapability
              { acceptInsecureCerts = Just True,
                browserName = Nothing,
                browserVersion = Nothing,
                webSocketUrl = True,
                platformName = Nothing,
                proxy = Nothing,
                unhandledPromptBehavior =
                  Just $
                    MkUserPromptHandler
                      { alert = Just Accept,
                        beforeUnload = Just Dismiss,
                        confirm = Just Accept,
                        defaultHandler = Just Ignore,
                        fileHandler = Nothing,
                        prompt = Just Accept
                      }
              }
      let firstMatchCapabilities =
            MkCapabilities
              { alwaysMatch = Nothing,
                firstMatch = [firstMatchCap1, firstMatchCap2]
              }
      session2 <- sessionNew firstMatchCapabilities
      logShow "Session with firstMatch" session2
      pause

-- >>> runDemo sessionCompleteLifecycleDemo
sessionCompleteLifecycleDemo :: BiDiDemo
sessionCompleteLifecycleDemo =
  demo "Session - Complete Lifecycle Management" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Step 1: Check initial session status"
      initialStatus <- sessionStatus
      logShow "Initial status" initialStatus
      pause

      logTxt "Step 2: Subscribe to key events"
      let subscription =
            MkSessionSubscribe
              { events = KnownSubscriptionType <$> [BrowsingContextContextCreated, BrowsingContextNavigationStarted],
                contexts = Nothing,
                userContexts = Nothing
              }
      subResult <- sessionSubscribe subscription
      logShow "Event subscription" subResult
      pause

      logTxt "Step 3: Perform some operations (context creation)"
      -- This would normally create contexts and generate events
      logTxt "Events would be generated during normal operations..."
      pause

      logTxt "Step 4: Check status after operations"
      operationStatus <- sessionStatus
      logShow "Status after operations" operationStatus
      pause

      logTxt "Step 5: Clean up subscriptions"
      let cleanup =
            UnsubscribeById
              { subscriptions = [subResult]
              }
      cleanupResult <- sessionUnsubscribe cleanup
      logShow "Cleanup result" cleanupResult
      pause

      logTxt "Session lifecycle demo complete"
