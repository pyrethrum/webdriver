module BiDi.Demos.BrowsingContextEventDemos where

import BiDi.BiDiRunner (BiDiActions (..), BiDiMethods (unsubscribe))
import BiDi.DemoUtils
import Const (second, seconds)
import IOUtils (DemoUtils (..))
import TestData (checkboxesUrl, textAreaUrl, promptUrl, fragmentUrl, downloadUrl)
import WebDriverPreCore.BiDi.BrowsingContext 
  ( BrowsingContextEvent (NavigationStarted), 
    Close (..), 
    Navigate (..), 
    HandleUserPrompt (..),
    UserPromptType (..),
    DownloadWillBegin (..),
    UserPromptOpened (..),
    UserPromptClosed (..),
    NavigationInfo (..),
    HistoryUpdated (..)
  )
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    Create (..),
    CreateType (..),
    CreateUserContext (..),
    SubscriptionType 
      ( BrowsingContextContextCreated, 
        BrowsingContextContextDestroyed, 
        BrowsingContextNavigationStarted,
        BrowsingContextNavigationCommitted,
        BrowsingContextNavigationFailed,
        BrowsingContextNavigationAborted,
        BrowsingContextDomContentLoaded,
        BrowsingContextLoad,
        BrowsingContextFragmentNavigated,
        BrowsingContextHistoryUpdated,
        BrowsingContextUserPromptOpened,
        BrowsingContextUserPromptClosed,
        BrowsingContextDownloadWillBegin,
        BrowsingContextDownloadEnd
      ),
    UserContext (..),
    ContextTarget (..),
    Evaluate (..)
  )
import WebDriverPreCore.BiDi.Script 
  ( EvaluateResult (..), 
    PrimitiveProtocolValue (..), 
    RemoteValue (..),
    Target (ContextTarget)
  )
import WebDriverPreCore.BiDi.CoreTypes (StringValue (..))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

{-
BrowsingContext Events Implemented:

1. browsingContext.contextCreated :: ✓ browsingContextEventCreatedestroyed
2. browsingContext.contextDestroyed :: ✓ browsingContextEventCreatedestroyed
3. browsingContext.navigationStarted :: ✓ browsingContextEventNavigationLifecycle
4. browsingContext.navigationCommitted :: ✓ browsingContextEventNavigationLifecycle
5. browsingContext.domContentLoaded :: ✓ browsingContextEventNavigationLifecycle
6. browsingContext.load :: ✓ browsingContextEventNavigationLifecycle
7. browsingContext.fragmentNavigated :: ✓ browsingContextEventFragmentNavigation
8. browsingContext.userPromptOpened :: ✓ browsingContextEventUserPrompts, browsingContextEventUserPromptsVariants
9. browsingContext.userPromptClosed :: ✓ browsingContextEventUserPrompts, browsingContextEventUserPromptsVariants
10. browsingContext.historyUpdated :: ✓ browsingContextEventHistoryUpdated

Events TODO:
11. browsingContext.navigationAborted :: TODO (requires page that aborts navigation)
12. browsingContext.navigationFailed :: TODO (requires invalid URL/network error)
13. browsingContext.downloadWillBegin :: TODO (requires actual download trigger)
14. browsingContext.downloadEnd :: TODO (requires actual download completion)
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

-- >>> runDemo browsingContextEventNavigationLifecycle
browsingContextEventNavigationLifecycle :: BiDiDemo
browsingContextEventNavigationLifecycle =
  demo "Browsing Context Events - Navigation Lifecycle (Started, Committed, DomContentLoaded, Load)" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Subscribe to navigation lifecycle events"
      
      (startedEventFired, waitStartedEventFired) <- timeLimitLog BrowsingContextNavigationStarted
      subscribeBrowsingContextNavigationStarted startedEventFired

      (committedEventFired, waitCommittedEventFired) <- timeLimitLog BrowsingContextNavigationCommitted
      subscribeBrowsingContextNavigationCommitted committedEventFired

      (domContentLoadedEventFired, waitDomContentLoadedEventFired) <- timeLimitLog BrowsingContextDomContentLoaded
      subscribeBrowsingContextDomContentLoaded domContentLoadedEventFired

      (loadEventFired, waitLoadEventFired) <- timeLimitLog BrowsingContextLoad
      subscribeBrowsingContextLoad loadEventFired

      (manyEventFired, waitManyEventFired) <- timeLimitLog BrowsingContextNavigationStarted
      subscribeMany 
        [ BrowsingContextNavigationStarted,
          BrowsingContextNavigationCommitted,
          BrowsingContextDomContentLoaded,
          BrowsingContextLoad
        ] 
        manyEventFired

      logTxt "Navigating to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext MkDemoUtils {..} MkCommands {..}
      browsingContextNavigate $ MkNavigate bc url Nothing

      sequence_
        [ waitStartedEventFired,
          waitCommittedEventFired,
          waitDomContentLoadedEventFired,
          waitLoadEventFired,
          waitManyEventFired
        ]

-- >>> runDemo browsingContextEventFragmentNavigation
browsingContextEventFragmentNavigation :: BiDiDemo
browsingContextEventFragmentNavigation =
  demo "Browsing Context Events - Fragment Navigation" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Navigate to fragment page"
      url <- fragmentUrl
      bc <- rootContext MkDemoUtils {..} MkCommands {..}
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to FragmentNavigated event"
      
      (fragmentEventFired, waitFragmentEventFired) <- timeLimitLog BrowsingContextFragmentNavigated
      subscribeBrowsingContextFragmentNavigated fragmentEventFired

      (manyFragmentEventFired, waitManyFragmentEventFired) <- timeLimitLog BrowsingContextFragmentNavigated
      subscribeMany [BrowsingContextFragmentNavigated] manyFragmentEventFired

      logTxt "Navigate to fragment #section2"
      browsingContextNavigate $ MkNavigate bc (url <> "#section2") Nothing

      sequence_
        [ waitFragmentEventFired,
          waitManyFragmentEventFired
        ]

-- >>> runDemo browsingContextEventUserPrompts
browsingContextEventUserPrompts :: BiDiDemo
browsingContextEventUserPrompts =
  demo "Browsing Context Events - User Prompt Opened and Closed" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Navigate to prompt page"
      url <- promptUrl
      bc <- rootContext MkDemoUtils {..} MkCommands {..}
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to UserPromptOpened event"
      
      (openedEventFired, waitOpenedEventFired) <- timeLimitLog BrowsingContextUserPromptOpened
      subscribeBrowsingContextUserPromptOpened openedEventFired

      (manyOpenedEventFired, waitManyOpenedEventFired) <- timeLimitLog BrowsingContextUserPromptOpened
      subscribeMany [BrowsingContextUserPromptOpened] manyOpenedEventFired

      logTxt "Trigger alert prompt via script"
      scriptEvaluate $
        MkEvaluate
          { expression = "alert('Test alert')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      sequence_
        [ waitOpenedEventFired,
          waitManyOpenedEventFired
        ]

      logTxt "Subscribe to UserPromptClosed event"
      
      (closedEventFired, waitClosedEventFired) <- timeLimitLog BrowsingContextUserPromptClosed
      subscribeBrowsingContextUserPromptClosed closedEventFired

      (manyClosedEventFired, waitManyClosedEventFired) <- timeLimitLog BrowsingContextUserPromptClosed
      subscribeMany [BrowsingContextUserPromptClosed] manyClosedEventFired

      logTxt "Accept the alert prompt"
      browsingContextHandleUserPrompt $
        MkHandleUserPrompt
          { context = bc,
            accept = Just True,
            userText = Nothing
          }

      sequence_
        [ waitClosedEventFired,
          waitManyClosedEventFired
        ]

-- >>> runDemo browsingContextEventUserPromptsVariants
browsingContextEventUserPromptsVariants :: BiDiDemo
browsingContextEventUserPromptsVariants =
  demo "Browsing Context Events - User Prompt Types (Alert, Confirm, Prompt)" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Navigate to prompt page"
      url <- promptUrl
      bc <- rootContext MkDemoUtils {..} MkCommands {..}
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to UserPromptOpened and UserPromptClosed events"
      subscribeBrowsingContextUserPromptOpened $ logShow "UserPromptOpened"
      subscribeBrowsingContextUserPromptClosed $ logShow "UserPromptClosed"

      -- Test Alert
      logTxt "Testing Alert prompt"
      scriptEvaluate $
        MkEvaluate
          { expression = "alert('Alert message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just True) Nothing
      pause

      -- Test Confirm - Accept
      logTxt "Testing Confirm prompt - Accept"
      scriptEvaluate $
        MkEvaluate
          { expression = "confirm('Confirm message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just True) Nothing
      pause

      -- Test Confirm - Dismiss
      logTxt "Testing Confirm prompt - Dismiss"
      scriptEvaluate $
        MkEvaluate
          { expression = "confirm('Confirm dismiss message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just False) Nothing
      pause

      -- Test Prompt - with user text
      logTxt "Testing Prompt with user text"
      scriptEvaluate $
        MkEvaluate
          { expression = "prompt('Enter your name:', 'default')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just True) (Just "John Doe")
      pause

      -- Test Prompt - dismissed
      logTxt "Testing Prompt dismissed"
      scriptEvaluate $
        MkEvaluate
          { expression = "prompt('This will be dismissed')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just False) Nothing
      pause

-- >>> runDemo browsingContextEventHistoryUpdated
browsingContextEventHistoryUpdated :: BiDiDemo
browsingContextEventHistoryUpdated =
  demo "Browsing Context Events - History Updated" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Navigate to checkboxes page"
      url1 <- checkboxesUrl
      bc <- rootContext MkDemoUtils {..} MkCommands {..}
      browsingContextNavigate $ MkNavigate bc url1 Nothing
      pause

      logTxt "Navigate to fragment page"
      url2 <- fragmentUrl
      browsingContextNavigate $ MkNavigate bc url2 Nothing
      pause

      logTxt "Subscribe to HistoryUpdated event"
      
      (historyEventFired, waitHistoryEventFired) <- timeLimitLog BrowsingContextHistoryUpdated
      subscribeBrowsingContextHistoryUpdated historyEventFired

      (manyHistoryEventFired, waitManyHistoryEventFired) <- timeLimitLog BrowsingContextHistoryUpdated
      subscribeMany [BrowsingContextHistoryUpdated] manyHistoryEventFired

      logTxt "Navigate back in history"
      scriptEvaluate $
        MkEvaluate
          { expression = "history.back()",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      sequence_
        [ waitHistoryEventFired,
          waitManyHistoryEventFired
        ]
