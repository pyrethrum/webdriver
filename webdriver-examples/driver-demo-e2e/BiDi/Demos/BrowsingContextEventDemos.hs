module BiDi.Demos.BrowsingContextEventDemos where

import BiDi.BiDiRunner (BiDiActions (..), BiDiMethods (unsubscribe))
import BiDi.DemoUtils
import Const (second, seconds)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent (NavigationStarted), Close (..), Navigate (..))
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    Create (..),
    CreateType (..),
    CreateUserContext (..),
    SubscriptionType (BrowsingContextContextCreated, BrowsingContextContextDestroyed, BrowsingContextNavigationStarted),
    UserContext (..),
  )
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

{-
BrowsingContext Events TODO:

1. browsingContext.contextCreated :: TODO
2. browsingContext.contextDestroyed :: TODO
3. browsingContext.domContentLoaded :: TODO
4. browsingContext.downloadEnd :: TODO
5. browsingContext.downloadWillBegin :: TODO
6. browsingContext.fragmentNavigated :: TODO
7. browsingContext.historyUpdated :: TODO
8. browsingContext.load :: TODO
9. browsingContext.navigationAborted :: TODO
10. browsingContext.navigationCommitted :: TODO
11. browsingContext.navigationFailed :: TODO
12. browsingContext.navigationStarted :: TODO
13. browsingContext.userPromptClosed :: TODO
14. browsingContext.userPromptOpened :: TODO
-}

--  TODO aake sure all demos only use Protocol, not sub modules

-- >>> runDemo browsingContextEventDemo
browsingContextEventDemo :: BiDiDemo
browsingContextEventDemo =
  demo "Browsing Context Create - Subscribe Unsubscribe" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      subId <- subscribeBrowsingContextCreated (logShow "Event Subscription Fired: browsingContext.contextCreated")
      logShow "Subscription id:" subId

      logTxt "New browsing context - Tab"
      let bcParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      bc <- browsingContextCreate bcParams
      logShow "Browsing context - Tab II" bc

      logShow "Unsubscribing from browsingContext.contextCreated event" subId
      unsubscribe subId

      bc2 <- browsingContextCreate bcParams
      logShow "Browsing context - Tab III (No Event Triggered)" bc2
      pause

-- >>> runDemo browsingContextEventDemoMulti
browsingContextEventDemoMulti :: BiDiDemo
browsingContextEventDemoMulti =
  demo "Browsing Context Events - Subscribe Unsubscribe Using subscribeMany" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      subId <- subscribeMany [BrowsingContextContextCreated, BrowsingContextContextDestroyed] (logShow "Event Subscription Fired: browsingContext.contextCreated or contextDestroyed")
      logShow "Subscription id:" subId

      logTxt "New browsing context - Tab"
      let bcParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      bc <- browsingContextCreate bcParams
      logShow "Browsing context - Tab II" bc

      logShow "Closing browsing context - Tab II" bc
      browsingContextClose
        MkClose
          { context = bc,
            promptUnload = Nothing
          }

      logShow "Unsubscribing from events" subId
      unsubscribe subId

      bc2 <- browsingContextCreate bcParams
      logShow "Browsing context created - Tab III (No Event Triggered)" bc2

      browsingContextClose $ MkClose bc2 Nothing
      logShow "Browsing context destroyed - Tab III (No Event Triggered)" bc2

-- >>> runDemo browsingContextEventDemoFilteredSubscriptions
browsingContextEventDemoFilteredSubscriptions :: BiDiDemo
browsingContextEventDemoFilteredSubscriptions =
  demo "Browsing Context Events - Filtered Navigation Subscriptions - Filter not working - not working: https://github.com/mozilla/geckodriver/issues/2236" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Creating two browsing contexts"

      let createParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }

      -- Create first browsing context
      bc1 <- browsingContextCreate createParams
      logShow "Created browsing context 1:" bc1

      -- Create second browsing context
      bc2 <- browsingContextCreate createParams
      logShow "Created browsing context 2:" bc2

      logTxt "Subscribing to navigationStarted events only for browsing context 1"

      -- Subscribe to navigationStarted events only for parentContext1
      subId <-
        subscribeBrowsingContextNavigationStarted' [bc1] [] $
          logShow "Navigation Started Event Fired (should only fire for browsing context 1)"
      logShow "Subscribed to navigationStarted for browsing context 1:" subId
      pause

      logTxt "Navigating both contexts to different URLs"

      -- Navigate browsing context 1 (should trigger event)
      logTxt "Navigating browsing context 1 to checkboxes.html (SHOULD trigger event)"
      browsingContextNavigate $ MkNavigate bc1 "file:///home/john-walker/repos/webdriver/webdriver-examples/driver-demo-e2e/TestFiles/checkboxes.html" Nothing
      pause

      -- Navigate browsing context 2 (should NOT trigger event)
      logTxt "Navigating browsing context 2 to textArea.html (should NOT trigger event)"
      browsingContextNavigate $ MkNavigate bc2 "file:///home/john-walker/repos/webdriver/webdriver-examples/driver-demo-e2e/TestFiles/textArea.html" Nothing
      pause

-- >>> runDemo browsingContextEventDemoUserContextFiltered
browsingContextEventDemoUserContextFiltered :: BiDiDemo
browsingContextEventDemoUserContextFiltered =
  demo "Browsing Context Events - Filtered User Context Subscriptions" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Creating two user contexts"
      uc1 <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "Created user context 1:" uc1

      uc2 <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "Created user context 2:" uc2

      logTxt "Subscribing to contextCreated events only for user context 1"
      subId <-
        subscribeBrowsingContextCreated' [] [uc1] $
          logShow $
            "Context Created Event Fired - should only fire for user context: " <> txt uc1.userContext
      logShow "Subscribed to contextCreated for user context 1:" subId
      pause

      logTxt "Creating browsing contexts in both user contexts"

      logTxt "Creating browsing context in user context 1 (SHOULD trigger event)"
      let createParams1 =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Just uc1
              }
      bc1 <- browsingContextCreate createParams1
      logShow "Created browsing context 1:" bc1
      pause

      logTxt "Creating browsing context in user context 2 (should NOT trigger event)"
      let createParams2 =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Just uc2
              }
      bc2 <- browsingContextCreate createParams2
      logShow "Created browsing context 2:" bc2
      pause

      logShow "Unsubscribing from events" subId
      unsubscribe subId

      logTxt "Creating browsing context after unsubscribe (should NOT trigger event)"
      bc4 <- browsingContextCreate createParams1
      logShow "Created browsing context 4 (no event):" bc4

-- >>> runDemo browsingContextEventCreatedestroyed
browsingContextEventCreatedestroyed :: BiDiDemo
browsingContextEventCreatedestroyed =
  demo "Browsing Context Events - Created and Destroyed" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do

      logTxt "Subscribe to ContextCreated event"
      (createdEventFired, waitCreateEventFired) <- timeLimitLog BrowsingContextContextCreated
      subscribeBrowsingContextCreated createdEventFired

      (manyCreatedEventFired, waitManyCreatedEventFired) <- timeLimitLog BrowsingContextContextCreated
      subscribeMany [BrowsingContextContextCreated] manyCreatedEventFired

      logTxt "Creating a browsing context"
      let createParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      bc <- browsingContextCreate createParams
      logShow "Created browsing context:" bc

      sequence_
        [ waitCreateEventFired,
          waitManyCreatedEventFired
        ]

      logTxt "Subscribe to ContextDestroyed event"

      (destroyedEventFired, waitDestroyedEventFired) <- timeLimitLog BrowsingContextContextDestroyed
      subscribeBrowsingContextDestroyed destroyedEventFired

      (manyDestroyedEventFired, waitManyDestroyedEventFired) <- timeLimitLog BrowsingContextContextDestroyed
      subscribeMany [BrowsingContextContextDestroyed] manyDestroyedEventFired

      logTxt "Closing the browsing context"
      browsingContextClose $ MkClose bc Nothing
      logShow "Closed browsing context:" bc

      sequence_
        [ waitDestroyedEventFired,
          waitManyDestroyedEventFired
        ]
