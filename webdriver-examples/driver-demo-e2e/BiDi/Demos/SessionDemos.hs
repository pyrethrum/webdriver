module BiDi.Demos.SessionDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.BiDi.Capabilities (UserPromptHandler(..), UserPromptHandlerType(..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)

{-
Session Module Commands (5 total):

1. session.status - Returns information about whether a remote end is in a state to create new sessions
2. session.new - Creates a new BiDi session (static command)
3. session.end - Ends the current session
4. session.subscribe - Enables certain events either globally or for a set of navigables
5. session.unsubscribe - Disables events either globally or for a set of navigables

Session Module Types:
- session.CapabilitiesRequest - Capabilities requested for a session
- session.CapabilityRequest - Specific set of requested capabilities
- session.ProxyConfiguration - Proxy configuration with various types (direct, manual, pac, etc.)
- session.UserPromptHandler - Configuration for user prompt handling
- session.UserPromptHandlerType - Behavior of user prompt handler (accept/dismiss/ignore)
- session.Subscription - Unique subscription identifier
- session.SubscriptionRequest - Request to subscribe/unsubscribe from events
- session.UnsubscribeByIDRequest - Remove subscriptions by ID
- session.UnsubscribeByAttributesRequest - Remove subscriptions by attributes

Key Concepts:
- Session lifecycle management
- BiDi vs HTTP session differences
- Event subscription and management
- Capability negotiation and matching
- Proxy configuration types
- User prompt automation
- WebSocket connection management

Complexity factors:
- Session creation and negotiation
- Event subscription filtering by context/user context
- Complex capability matching algorithms
- Proxy configuration validation
- User prompt handler configuration
- Subscription management and cleanup
-}

-- >>> runDemo sessionStatusDemo
sessionStatusDemo :: BiDiDemo
sessionStatusDemo =
  demo "Session - Status Check" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
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
sessionNewDemo :: BiDiDemo
sessionNewDemo =
  demo "Session - New Session Creation" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Creating new BiDi session with basic capabilities"
      let basicCapabilities = MkCapabilities
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

-- >>> runDemo sessionEndDemo  
sessionEndDemo :: BiDiDemo
sessionEndDemo =
  demo "Session - End Session" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
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
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Test 1: Subscribe to browsing context events globally"
      let globalSubscription = MkSessionSubscriptionRequest
            { events = ["browsingContext.contextCreated", "browsingContext.contextDestroyed"],
              contexts = Nothing,
              userContexts = Nothing
            }
      sub1 <- sessionSubScribe globalSubscription
      logShow "Global subscription" sub1
      pause

      logTxt "Test 2: Subscribe to network events for specific context"
      let contextSubscription = MkSessionSubscriptionRequest
            { events = ["network.requestWillBeSent", "network.responseReceived"],
              contexts = Just [bc.context],
              userContexts = Nothing
            }
      sub2 <- sessionSubScribe contextSubscription
      logShow "Context-specific subscription" sub2
      pause

      logTxt "Test 3: Subscribe to script events for user context"
      let userContextSubscription = MkSessionSubscriptionRequest
            { events = ["script.realmCreated"],
              contexts = Nothing,
              userContexts = Just ["default"]
            }
      sub3 <- sessionSubScribe userContextSubscription
      logShow "User context subscription" sub3
      pause

-- >>> runDemo sessionUnsubscribeDemo
sessionUnsubscribeDemo :: BiDiDemo
sessionUnsubscribeDemo =
  demo "Session - Event Unsubscription" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "First, create a subscription to demonstrate unsubscription"
      let subscription = MkSessionSubscriptionRequest
            { events = ["browsingContext.contextCreated"],
              contexts = Nothing,
              userContexts = Nothing
            }
      subResult <- sessionSubScribe subscription
      logShow "Created subscription" subResult
      pause

      logTxt "Test 1: Unsubscribe by subscription ID"
      let unsubByID = UnsubscribeByID $ MkSessionUnsubscribeByIDRequest
            { subscriptions = [subResult.subscription]
            }
      result1 <- sessionUnsubscribe unsubByID
      logShow "Unsubscribed by ID" result1
      pause

      logTxt "Test 2: Unsubscribe by attributes (alternative method)"
      let unsubByAttrs = UnsubscribeByAttributes $ MkSessionUnsubscribeByAttributesRequest
            { unsubEvents = ["network.requestWillBeSent"],
              unsubContexts = Nothing
            }
      result2 <- sessionUnsubscribe unsubByAttrs
      logShow "Unsubscribed by attributes" result2
      pause

-- >>> runDemo sessionCapabilityNegotiationDemo
sessionCapabilityNegotiationDemo :: BiDiDemo
sessionCapabilityNegotiationDemo =
  demo "Session - Capability Negotiation" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Test 1: Session with alwaysMatch capabilities"
      let alwaysMatchCap = MkCapability
            { acceptInsecureCerts = Just True,
              browserName = Just "firefox",
              browserVersion = Nothing,
              webSocketUrl = True,
              platformName = Just "linux",
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      let alwaysMatchCapabilities = MkCapabilities
            { alwaysMatch = Just alwaysMatchCap,
              firstMatch = []
            }
      session1 <- sessionNew alwaysMatchCapabilities
      logShow "Session with alwaysMatch" session1
      pause

      logTxt "Test 2: Session with firstMatch capabilities"
      let firstMatchCap1 = MkCapability
            { acceptInsecureCerts = Just False,
              browserName = Just "firefox",
              browserVersion = Just "130.0",
              webSocketUrl = True,
              platformName = Just "linux",
              proxy = Just DirectProxyConfiguration,
              unhandledPromptBehavior = Nothing
            }
      let firstMatchCap2 = MkCapability
            { acceptInsecureCerts = Just True,
              browserName = Nothing,
              browserVersion = Nothing,
              webSocketUrl = True,
              platformName = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Just $ MkUserPromptHandler
                { alert = Just Accept,
                  beforeUnload = Just Dismiss,
                  confirm = Just Accept,
                  defaultHandler = Just Ignore,
                  fileHandler = Nothing,
                  prompt = Just Accept
                }
            }
      let firstMatchCapabilities = MkCapabilities
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
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Step 1: Check initial session status"
      initialStatus <- sessionStatus
      logShow "Initial status" initialStatus
      pause

      logTxt "Step 2: Subscribe to key events"
      let subscription = MkSessionSubscriptionRequest
            { events = ["browsingContext.contextCreated", "browsingContext.navigationStarted"],
              contexts = Nothing,
              userContexts = Nothing
            }
      subResult <- sessionSubScribe subscription
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
      let cleanup = UnsubscribeByID $ MkSessionUnsubscribeByIDRequest
            { subscriptions = [subResult.subscription]
            }
      cleanupResult <- sessionUnsubscribe cleanup
      logShow "Cleanup result" cleanupResult
      pause

      logTxt "Session lifecycle demo complete"