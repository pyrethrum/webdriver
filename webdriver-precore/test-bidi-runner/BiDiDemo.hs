{-|
BiDi Demo module for webdriver-precore-bidi-runner tests

This module contains demos migrated from the main test suite.
-}
module BiDiDemo where

import BiDiActions (BiDiActions (..))
import BiDiDemoUtils
  ( BiDiDemo,
    chkDomContains,
    demo,
    rootContext,
    runDemo,
  )
import Utils (txt)
import WebDriverPreCore.BiDi.Protocol
  ( Activate (..),
    Close (..),
    ContextTarget (..),
    Create (..),
    CreateType (..),
    CreateUserContext (..),
    Evaluate (..),
    GetTree (..),
    JSUInt (..),
    KnownSubscriptionType (..),
    Navigate (..),
    ReadinessState (..),
    RemoveUserContext (..),
    SessionStatusResult (..),
    SessionSubscibe (..),
    SessionUnsubscribe (..),
    SubscriptionType (..),
    Target (..),
  )
import WebDriverPreCore.Test.IOUtils (DemoActions (..))
import WebDriverPreCore.Test.TestData (indexUrl, loginUrl)
import Prelude hiding (log)

-- Suppress unused import warnings for demos
_stopDemoUnusedWarning :: BiDiDemo -> IO ()
_stopDemoUnusedWarning = runDemo

-- ===============================================
-- Session Demos
-- ===============================================

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
      userContextsResult <- browserGetUserContexts
      logShow "Current user contexts" userContextsResult

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

-- ===============================================
-- Browsing Context Demos
-- ===============================================

-- >>> runDemo browsingContextCreateActivateCloseDemo
browsingContextCreateActivateCloseDemo :: BiDiDemo
browsingContextCreateActivateCloseDemo =
  demo "Browsing Context - Create, Activate, Close" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "New browsing context - Tab"
      let bcParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      bc <- browsingContextCreate bcParams
      logShow "Browsing context - Tab" bc
      pause

      logTxt "New browsing context - Window"
      bcWin <- browsingContextCreate bcParams {createType = Window}
      logShow "Browsing context - Window" bcWin
      pause

      logTxt "New browsing context - Tab with reference context"
      bcWithContext <- browsingContextCreate bcParams {referenceContext = Just bc}
      logShow "Browsing context - Tab with reference context" bcWithContext
      pause

      logTxt "New browsing context - Background"
      bg <-
        browsingContextCreate
          bcParams
            { background = True,
              referenceContext = Just bcWin
            }
      logShow "Background browsing context created on front window" bg
      pause

      logTxt "New user context"
      uc <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User context created" uc
      pause

      logTxt "New browsing context - Window with user context"
      bcWithUc <- browsingContextCreate bcParams {createType = Window, userContext = Just uc}
      logShow "Browsing context - Window with user context" bcWithUc
      pause

      logTxt "Activate the background context"
      browsingContextActivate $ MkActivate bc
      logTxt "Activated background context"
      pause

      logTxt "Close the contexts"
      browsingContextClose $ MkClose {context = bc, promptUnload = Nothing}
      browsingContextClose $ MkClose {context = bcWin, promptUnload = Nothing}
      browsingContextClose $ MkClose {context = bcWithContext, promptUnload = Nothing}
      browsingContextClose $ MkClose {context = bg, promptUnload = Nothing}
      browsingContextClose $ MkClose {context = bcWithUc, promptUnload = Nothing}
      logTxt "All contexts closed"

-- >>> runDemo browsingContextNavigateDemo
browsingContextNavigateDemo :: BiDiDemo
browsingContextNavigateDemo =
  demo "Browsing Context - Navigate" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      
      url <- indexUrl
      logTxt $ "Navigating to: " <> txt url
      navResult <- browsingContextNavigate $
        MkNavigate
          { context = bc,
            url = url,
            wait = Just Complete
          }
      logShow "Navigation result" navResult
      pause

      url2 <- loginUrl
      logTxt $ "Navigating to: " <> txt url2
      navResult2 <- browsingContextNavigate $
        MkNavigate
          { context = bc,
            url = url2,
            wait = Just Complete
          }
      logShow "Navigation result 2" navResult2
      pause

-- >>> runDemo browsingContextGetTreeDemo
browsingContextGetTreeDemo :: BiDiDemo
browsingContextGetTreeDemo =
  demo "Browsing Context - Get Tree" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Get browsing context tree (all contexts)"
      tree <- browsingContextGetTree $ MkGetTree Nothing Nothing
      logShow "Browsing context tree" tree
      pause

      logTxt "Create a new window context"
      let bcParams =
            MkCreate
              { createType = Window,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      newBc <- browsingContextCreate bcParams
      logShow "New context" newBc
      pause

      logTxt "Get tree with depth limit"
      tree2 <- browsingContextGetTree $ MkGetTree (Just $ MkJSUInt 1) Nothing
      logShow "Tree with depth 1" tree2
      pause

      logTxt "Get tree for specific context"
      tree3 <- browsingContextGetTree $ MkGetTree Nothing (Just newBc)
      logShow "Tree for specific context" tree3
      pause

      browsingContextClose $ MkClose {context = newBc, promptUnload = Nothing}
      logTxt "Context closed"

-- >>> runDemo scriptEvaluateDemo
scriptEvaluateDemo :: BiDiDemo
scriptEvaluateDemo =
  demo "Script - Evaluate" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      let baseEval =
            MkEvaluate
              { expression = "1 + 1",
                target =
                  ContextTarget $
                    MkContextTarget
                      { context = bc,
                        sandbox = Nothing
                      },
                awaitPromise = True,
                resultOwnership = Nothing,
                serializationOptions = Nothing
              }

      logTxt "Test 1: Simple arithmetic"
      r1 <- scriptEvaluate baseEval
      logShow "1 + 1 =" r1
      pause

      logTxt "Test 2: String evaluation"
      r2 <- scriptEvaluate baseEval {expression = "'Hello from BiDi!'"}
      logShow "String result" r2
      pause

      logTxt "Test 3: Object evaluation"
      r3 <- scriptEvaluate baseEval {expression = "({name: 'test', value: 42})"}
      logShow "Object result" r3
      pause

      logTxt "Test 4: Array evaluation"
      r4 <- scriptEvaluate baseEval {expression = "[1, 2, 3, 'four']"}
      logShow "Array result" r4
      pause

      logTxt "Test 5: DOM access"
      url <- indexUrl
      _ <- browsingContextNavigate $
        MkNavigate
          { context = bc,
            url = url,
            wait = Just Complete
          }
      r5 <- scriptEvaluate baseEval {expression = "document.title"}
      logShow "Document title" r5
      pause

-- >>> runDemo browserUserContextDemo
browserUserContextDemo :: BiDiDemo
browserUserContextDemo =
  demo "Browser - User Context Management" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Get existing user contexts"
      contexts <- browserGetUserContexts
      logShow "User contexts" contexts
      pause

      logTxt "Create new user context"
      newUc <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "Created user context" newUc
      pause

      logTxt "Verify new context appears in list"
      contexts2 <- browserGetUserContexts
      logShow "User contexts after creation" contexts2
      pause

      logTxt "Remove the user context"
      browserRemoveUserContext $ MkRemoveUserContext newUc
      logTxt "User context removed"
      pause

      logTxt "Verify removal"
      contexts3 <- browserGetUserContexts
      logShow "User contexts after removal" contexts3

-- >>> runDemo browserClientWindowsDemo
browserClientWindowsDemo :: BiDiDemo
browserClientWindowsDemo =
  demo "Browser - Client Windows" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Get client windows"
      windows <- browserGetClientWindows
      logShow "Client windows" windows
      pause

-- >>> runDemo chkDomContainsDemo
chkDomContainsDemo :: BiDiDemo
chkDomContainsDemo =
  demo "DOM - Check Text Contains" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      
      url <- indexUrl
      logTxt $ "Navigating to: " <> txt url
      _ <- browsingContextNavigate $
        MkNavigate
          { context = bc,
            url = url,
            wait = Just Complete
          }
      pause

      logTxt "Checking if page contains expected text"
      chkDomContains utils bidi bc "Welcome"
      logTxt "Text validation passed!"
