module BiDi.Demos.BrowsingContextEventDemos where

import BiDi.BiDiRunner (BiDiActions (..), BiDiMethods (unsubscribe))
import BiDi.DemoUtils
import Const (Timeout (..), minute, second, seconds)
import IOUtils (DemoUtils (..))
import TestData (checkboxesUrl, downloadLinkUrl, downloadUrl, fragmentUrl, promptUrl, slowLoadUrl, textAreaUrl)
import WebDriverPreCore.BiDi.BrowsingContext
  ( BrowsingContextEvent (NavigationStarted),
    Close (..),
    DownloadEnd (..),
    DownloadWillBegin (..),
    HandleUserPrompt (..),
    HistoryUpdated (..),
    Navigate (..),
    NavigationInfo (..),
    Reload (..),
    UserPromptClosed (..),
    UserPromptOpened (..),
    UserPromptType (..),
  )
import WebDriverPreCore.BiDi.CoreTypes (StringValue (..))
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    ContextTarget (..),
    Create (..),
    CreateType (..),
    CreateUserContext (..),
    Evaluate (..),
    SubscriptionType
      ( BrowsingContextContextCreated,
        BrowsingContextContextDestroyed,
        BrowsingContextDomContentLoaded,
        BrowsingContextDownloadEnd,
        BrowsingContextDownloadWillBegin,
        BrowsingContextFragmentNavigated,
        BrowsingContextHistoryUpdated,
        BrowsingContextLoad,
        BrowsingContextNavigationAborted,
        BrowsingContextNavigationCommitted,
        BrowsingContextNavigationFailed,
        BrowsingContextNavigationStarted,
        BrowsingContextUserPromptClosed,
        BrowsingContextUserPromptOpened
      ),
    UserContext (..),
  )
import WebDriverPreCore.BiDi.Script
  ( EvaluateResult (..),
    PrimitiveProtocolValue (..),
    RemoteValue (..),
    Target (ContextTarget),
  )
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

{-
BrowsingContext Events - All Implemented ✓

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
11. browsingContext.navigationAborted :: ✓ browsingContextEventNavigationAborted
12. browsingContext.navigationFailed :: ✓ browsingContextEventNavigationFailed
13. browsingContext.downloadWillBegin :: ✓ browsingContextEventDownloadWillBegin
14. browsingContext.downloadEnd :: ✓ browsingContextEventDownloadEnd
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
      logShow "Subscription id" subId

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
      logShow "Subscription id" subId

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
  demo "Browsing Context Events - Filtered Navigation Subscriptions - Filter not working - not working: https://bugzilla.mozilla.org/show_bug.cgi?id=1994293" action
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
      logShow "Created browsing context 1" bc1

      -- Create second browsing context
      bc2 <- browsingContextCreate createParams
      logShow "Created browsing context 2" bc2

      logTxt "Subscribing to navigationStarted events only for browsing context 1"

      -- Subscribe to navigationStarted events only for parentContext1
      subId <-
        subscribeBrowsingContextNavigationStarted' [bc1] [] $
          logShow $
            "Navigation Started Event Fired (should only fire for browsing context 1: " <> bc1.context <> ")"
      logShow "Subscribed to navigationStarted for browsing context 1" subId
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
      logShow "Created user context 1" uc1

      uc2 <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "Created user context 2" uc2

      logTxt "Subscribing to contextCreated events only for user context 1"
      subId <-
        subscribeBrowsingContextCreated' [] [uc1] $
          logShow $
            "Context Created Event Fired - should only fire for user context: " <> txt uc1.userContext
      logShow "Subscribed to contextCreated for user context 1" subId
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
      logShow "Created browsing context 1" bc1
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
      logShow "Created browsing context 2" bc2
      pause

      logShow "Unsubscribing from events" subId
      unsubscribe subId

      logTxt "Creating browsing context after unsubscribe (should NOT trigger event)"
      bc4 <- browsingContextCreate createParams1
      logShow "Created browsing context 4 (no event)" bc4

-- >>> runDemo browsingContextEventCreateDestroy
browsingContextEventCreateDestroy :: BiDiDemo
browsingContextEventCreateDestroy =
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
      logShow "Created browsing context" bc

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
      logShow "Closed browsing context" bc

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
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to navigation lifecycle events"

      (startedEventFired, waitStartedEventFired) <- timeLimitLog BrowsingContextNavigationStarted
      subscribeBrowsingContextNavigationStarted startedEventFired

      (manyStartedEventFired, waitManyStartedEventFired) <- timeLimitLog BrowsingContextNavigationStarted
      subscribeMany [BrowsingContextNavigationStarted] manyStartedEventFired

      (committedEventFired, waitCommittedEventFired) <- timeLimitLog BrowsingContextNavigationCommitted
      subscribeBrowsingContextNavigationCommitted committedEventFired

      (manyCommittedEventFired, waitManyCommittedEventFired) <- timeLimitLog BrowsingContextNavigationCommitted
      subscribeMany [BrowsingContextNavigationCommitted] manyCommittedEventFired

      (domContentLoadedEventFired, waitDomContentLoadedEventFired) <- timeLimitLog BrowsingContextDomContentLoaded
      subscribeBrowsingContextDomContentLoaded domContentLoadedEventFired

      (manyDomContentLoadedEventFired, waitManyDomContentLoadedEventFired) <- timeLimitLog BrowsingContextDomContentLoaded
      subscribeMany [BrowsingContextDomContentLoaded] manyDomContentLoadedEventFired

      (loadEventFired, waitLoadEventFired) <- timeLimitLog BrowsingContextLoad
      subscribeBrowsingContextLoad loadEventFired

      (manyLoadEventFired, waitManyLoadEventFired) <- timeLimitLog BrowsingContextLoad
      subscribeMany [BrowsingContextLoad] manyLoadEventFired

      logTxt "Navigating to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing

      sequence_
        [ waitStartedEventFired,
          waitManyStartedEventFired,
          waitCommittedEventFired,
          waitManyCommittedEventFired,
          waitDomContentLoadedEventFired,
          waitManyDomContentLoadedEventFired,
          waitLoadEventFired,
          waitManyLoadEventFired
        ]

-- >>> runDemo browsingContextEventFragmentNavigation
browsingContextEventFragmentNavigation :: BiDiDemo
browsingContextEventFragmentNavigation =
  demo "Browsing Context Events - Fragment Navigation" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to fragment page"
      url <- fragmentUrl
      bc <- rootContext utils cmds
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
-- *** Exception: CloseRequest 1000 ""

-- *** Exception: Error executing BiDi command: MkCommand

--   { method = "browsingContext.handleUserPrompt"
--   , params =
--       MkHandleUserPrompt
--         { context =
--             MkBrowsingContext
--               { context = "65607047-c172-4220-8d45-7dab42362abb" }
--         , accept = Just True
--         , userText = Nothing
--         }
--   , extended = Nothing
--   }
-- With JSON:
-- {
--     "id": 8,
--     "method": "browsingContext.handleUserPrompt",
--     "params": {
--         "accept": true,
--         "context": "65607047-c172-4220-8d45-7dab42362abb"
--     }
-- }
-- BiDi driver error:
-- MkDriverError
--   { id = Just 8
--   , error = NoSuchAlert
--   , description =
--       "Tried to interact with an alert that doesn't exist"
--   , message = ""
--   , stacktrace =
--       Just
--         "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nNoSuchAlertError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:538:5\nhandleUserPrompt@chrome://remote/content/webdriver-bidi/modules/root/browsingContext.sys.mjs:933:11\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:260:33\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:410:32\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n"
--   , extensions = MkEmptyResult { extensible = fromList [] }
--   }
browsingContextEventUserPrompts :: BiDiDemo
browsingContextEventUserPrompts =
  demo "Browsing Context Events - User Prompt Opened and Closed" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to prompt page"
      url <- promptUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to UserPromptOpened event"

      (openedEventFired, waitOpenedEventFired) <- timeLimitLog BrowsingContextUserPromptOpened
      subscribeBrowsingContextUserPromptOpened openedEventFired

      (manyOpenedEventFired, waitManyOpenedEventFired) <- timeLimitLog BrowsingContextUserPromptOpened
      subscribeMany [BrowsingContextUserPromptOpened] manyOpenedEventFired

      logTxt "Click alert button to trigger prompt"
      scriptEvaluate $
        MkEvaluate
          { expression = "document.getElementById('alertBtn').click()",
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

      pauseAtLeast $ 5 * seconds
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
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to prompt page"
      url <- promptUrl
      bc <- rootContext utils cmds
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
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to checkboxes page"
      url1 <- checkboxesUrl
      bc <- rootContext utils cmds
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

-- >>> runDemo browsingContextEventNavigationAborted
browsingContextEventNavigationAborted :: BiDiDemo
browsingContextEventNavigationAborted =
  demo "Browsing Context Events - Navigation Aborted" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to NavigationAborted event"

      (abortedEventFired, waitAbortedEventFired) <- timeLimitLog BrowsingContextNavigationAborted
      subscribeBrowsingContextNavigationAborted abortedEventFired

      (manyAbortedEventFired, waitManyAbortedEventFired) <- timeLimitLog BrowsingContextNavigationAborted
      subscribeMany [BrowsingContextNavigationAborted] manyAbortedEventFired

      logTxt "Start navigation to slow loading page"
      url <- slowLoadUrl
      bc <- rootContext utils cmds

      -- Start navigation in background (non-blocking)
      scriptEvaluate $
        MkEvaluate
          { expression = "setTimeout(() => { window.location.href = '" <> url <> "'; }, 100)",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      -- Give the navigation a moment to start
      pauseAtLeast (MkTimeout 200)

      logTxt "Abort navigation by navigating to different page"
      url2 <- checkboxesUrl
      browsingContextNavigate $ MkNavigate bc url2 Nothing

      sequence_
        [ waitAbortedEventFired,
          waitManyAbortedEventFired
        ]

-- >>> runDemo browsingContextEventNavigationFailed
browsingContextEventNavigationFailed :: BiDiDemo
browsingContextEventNavigationFailed =
  demo "Browsing Context Events - Navigation Failed" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to NavigationFailed event"

      (failedEventFired, waitFailedEventFired) <- timeLimitLog BrowsingContextNavigationFailed
      subscribeBrowsingContextNavigationFailed failedEventFired

      (manyFailedEventFired, waitManyFailedEventFired) <- timeLimitLog BrowsingContextNavigationFailed
      subscribeMany [BrowsingContextNavigationFailed] manyFailedEventFired

      logTxt "Navigate to invalid URL to trigger navigation failure"
      bc <- rootContext utils cmds

      -- Try to navigate to an invalid URL
      browsingContextNavigate $ MkNavigate bc "http://invalid-domain-that-does-not-exist-12345.test" Nothing

      sequence_
        [ waitFailedEventFired,
          waitManyFailedEventFired
        ]

-- >>> runDemo browsingContextEventDownloadWillBegin
browsingContextEventDownloadWillBegin :: BiDiDemo
browsingContextEventDownloadWillBegin =
  demo "Browsing Context Events - Download Will Begin" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to download link page"
      url <- downloadLinkUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to DownloadWillBegin event"

      (downloadEventFired, waitDownloadEventFired) <- timeLimitLog BrowsingContextDownloadWillBegin
      subscribeBrowsingContextDownloadWillBegin downloadEventFired

      (manyDownloadEventFired, waitManyDownloadEventFired) <- timeLimitLog BrowsingContextDownloadWillBegin
      subscribeMany [BrowsingContextDownloadWillBegin] manyDownloadEventFired

      logTxt "Click download link via script"
      scriptEvaluate $
        MkEvaluate
          { expression = "document.getElementById('downloadLink').click()",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      sequence_
        [ waitDownloadEventFired,
          waitManyDownloadEventFired
        ]

-- >>> runDemo browsingContextEventDownloadEnd
browsingContextEventDownloadEnd :: BiDiDemo
browsingContextEventDownloadEnd =
  demo "Browsing Context Events - Download End (Complete and Canceled)" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to download link page"
      url <- downloadLinkUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to DownloadWillBegin and DownloadEnd events"

      subscribeBrowsingContextDownloadWillBegin $ logShow "DownloadWillBegin"

      (downloadEndEventFired, waitDownloadEndEventFired) <- timeLimitLog BrowsingContextDownloadEnd
      subscribeBrowsingContextDownloadEnd downloadEndEventFired

      (manyDownloadEndEventFired, waitManyDownloadEndEventFired) <- timeLimitLog BrowsingContextDownloadEnd
      subscribeMany [BrowsingContextDownloadEnd] manyDownloadEndEventFired

      logTxt "Trigger download via JavaScript (blob download should complete quickly)"
      scriptEvaluate $
        MkEvaluate
          { expression = "triggerDownload()",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      sequence_
        [ waitDownloadEndEventFired,
          waitManyDownloadEndEventFired
        ]
