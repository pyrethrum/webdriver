module BiDi.Demos.SessionDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.BiDi.Protocol
import WebDriverPreCore.Internal.Utils (txt)
import Data.Text (Text)
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

-- TODO: Implement session.status demo
-- Demonstrates checking remote end readiness
-- Should show:
-- - Basic status check
-- - Ready state interpretation
-- - Implementation-specific messages

-- TODO: Implement session.new demo (BiDi-only session)
-- Demonstrates creating new BiDi sessions
-- Should show:
-- - Basic session creation
-- - Capability negotiation (acceptInsecureCerts, proxy, etc.)
-- - WebSocket URL handling
-- - Session ID management

-- TODO: Implement session.end demo
-- Demonstrates proper session termination
-- Should show:
-- - Graceful session shutdown
-- - Resource cleanup
-- - WebSocket connection closure

-- TODO: Implement session.subscribe demo
-- Demonstrates event subscription management
-- Should show:
-- - Global event subscription
-- - Context-specific subscriptions
-- - User context filtering
-- - Multiple event types
-- - Subscription ID tracking

-- TODO: Implement session.unsubscribe demo
-- Demonstrates event unsubscription
-- Should show:
-- - Unsubscribe by subscription ID
-- - Unsubscribe by attributes (deprecated)
-- - Partial unsubscription
-- - Subscription cleanup

-- TODO: Implement capability negotiation demo
-- Demonstrates various capability scenarios
-- Should show:
-- - alwaysMatch vs firstMatch processing
-- - Proxy configuration types
-- - Browser-specific capabilities
-- - Capability validation and errors

-- TODO: Implement user prompt handler demo
-- Demonstrates automated prompt handling
-- Should show:
-- - Different prompt types (alert, confirm, beforeUnload, etc.)
-- - Handler behaviors (accept, dismiss, ignore)
-- - Default vs specific handlers

-- Demo helper for session management
-- TODO: Implement helper functions for session lifecycle

-- Demo helper for event subscription
-- TODO: Implement helper functions for subscription management