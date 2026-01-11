module BiDi.Demos.BrowsingContextEventDemos where

import BiDi.Actions (BiDiActions (..))
import BiDi.DemoUtils
import Const (Timeout (..), milliseconds)
import Data.Text (unpack)
import IOUtils (DemoActions (..))
import TestData (checkboxesUrl, downloadLinkUrl, fragmentUrl, promptUrl, slowLoadUrl, textAreaUrl)
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    Close (..),
    ContextTarget (..),
    Create (..),
    CreateType (..),
    CreateUserContext (..),
    Evaluate (..),
    HandleUserPrompt (..),
    KnownSubscriptionType (..),
    Navigate (..),
    Target (..),
    URL (..),
    UserContext (..),
  )
import Utils (txt)
import Prelude hiding (log, putStrLn)

-- >>> runDemo browsingContextEventDemo
browsingContextEventDemo :: BiDiDemo
browsingContextEventDemo =
  demo "Browsing Context Create - Subscribe Unsubscribe" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
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
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      subId <-
        subscribeMany
          [BrowsingContextContextCreated, BrowsingContextContextDestroyed]
          (logShow "Event Subscription Fired: browsingContext.contextCreated or contextDestroyed")

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
  demo "Browsing Context Events - Filtered Navigation Subscriptions" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
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
      chkBxsUrl <- checkboxesUrl
      logTxt "Navigating browsing context 1 to checkboxes.html (SHOULD trigger event)"
      browsingContextNavigate $ MkNavigate bc1 chkBxsUrl Nothing
      pause

      logTxt "Resubscribe singleton and many filtered by bc1 (should not fire)"

      subIdre <-
        subscribeBrowsingContextNavigationStarted' [bc1] [] $
          \_n -> error $ "Navigation Started Event Fired (should only fire for browsing context 1 this should not happen filtering on bc1: " <> unpack bc1.context <> ")"
      logShow "Subscribed to navigationStarted for browsing context 1" subIdre
      pause

      subIdm <-
        subscribeMany'
          [bc1]
          []
          [BrowsingContextContextCreated, BrowsingContextContextDestroyed]
          (logShow "Event Subscription Fired: browsingContext.contextCreated or contextDestroyed")

      logShow "Subscribed to multi navigation started for browsing context 1" subIdm

      -- Navigate browsing context 2 (should NOT trigger event)
      txturl <- textAreaUrl
      logTxt "Navigating browsing context 2 to textArea.html (should NOT trigger event)"
      browsingContextNavigate $
        MkNavigate
          { context = bc2,
            url = txturl,
            wait = Nothing
          }

      -- make sure negative tests have time to fail
      pauseAtLeast $ 500 * milliseconds

-- >>> runDemo browsingContextEventDemoUserContextFiltered
browsingContextEventDemoUserContextFiltered :: BiDiDemo
browsingContextEventDemoUserContextFiltered =
  demo "Browsing Context Events - Filtered User Context Subscriptions" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
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


      -- Navigate browsing context 1 (should trigger event)
      chkBxsUrl <- checkboxesUrl
      logTxt "Navigating browsing context 1 to checkboxes.html (SHOULD trigger event)"
      browsingContextNavigate $ MkNavigate bc1 chkBxsUrl Nothing
      pause
-- >>> runDemo browsingContextEventCreateDestroy
browsingContextEventCreateDestroy :: BiDiDemo
browsingContextEventCreateDestroy =
  demo "Browsing Context Events - Created and Destroyed" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Subscribe to ContextCreated event"
      (createdEventFired, waitCreateEventFired) <- timeLimitLog BrowsingContextContextCreated
      subscribeBrowsingContextCreated createdEventFired

      (manyCreatedEventFired, waitManyCreatedEventFired) <- timeLimitLogMany BrowsingContextContextCreated
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

      (manyDestroyedEventFired, waitManyDestroyedEventFired) <- timeLimitLogMany BrowsingContextContextDestroyed
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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to navigation lifecycle events"

      (startedEventFired, waitStartedEventFired) <- timeLimitLog BrowsingContextNavigationStarted
      subscribeBrowsingContextNavigationStarted startedEventFired

      (manyStartedEventFired, waitManyStartedEventFired) <- timeLimitLogMany BrowsingContextNavigationStarted
      subscribeMany [BrowsingContextNavigationStarted] manyStartedEventFired

      (committedEventFired, waitCommittedEventFired) <- timeLimitLog BrowsingContextNavigationCommitted
      subscribeBrowsingContextNavigationCommitted committedEventFired

      (manyCommittedEventFired, waitManyCommittedEventFired) <- timeLimitLogMany BrowsingContextNavigationCommitted
      subscribeMany [BrowsingContextNavigationCommitted] manyCommittedEventFired

      (domContentLoadedEventFired, waitDomContentLoadedEventFired) <- timeLimitLog BrowsingContextDomContentLoaded
      subscribeBrowsingContextDomContentLoaded domContentLoadedEventFired

      (manyDomContentLoadedEventFired, waitManyDomContentLoadedEventFired) <- timeLimitLogMany BrowsingContextDomContentLoaded
      subscribeMany [BrowsingContextDomContentLoaded] manyDomContentLoadedEventFired

      (loadEventFired, waitLoadEventFired) <- timeLimitLog BrowsingContextLoad
      subscribeBrowsingContextLoad loadEventFired

      (manyLoadEventFired, waitManyLoadEventFired) <- timeLimitLogMany BrowsingContextLoad
      subscribeMany [BrowsingContextLoad] manyLoadEventFired

      logTxt "Navigating to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext utils bidi
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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Navigate to fragment page"
      url <- fragmentUrl
      bc <- rootContext utils bidi
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to FragmentNavigated event"

      (fragmentEventFired, waitFragmentEventFired) <- timeLimitLog BrowsingContextFragmentNavigated
      subscribeBrowsingContextFragmentNavigated fragmentEventFired

      (manyFragmentEventFired, waitManyFragmentEventFired) <- timeLimitLogMany BrowsingContextFragmentNavigated
      subscribeMany [BrowsingContextFragmentNavigated] manyFragmentEventFired

      logTxt "Navigate to fragment #section2"
      browsingContextNavigate $ MkNavigate { context = bc, url = MkUrl(url.url <> "#section2"), wait = Nothing }

      sequence_
        [ waitFragmentEventFired,
          waitManyFragmentEventFired
        ]

-- >>> runDemo browsingContextEventUserPrompts
browsingContextEventUserPrompts :: BiDiDemo
browsingContextEventUserPrompts =
  demo "Browsing Context Events - User Prompt Opened and Closed" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Navigate to prompt page"
      url <- promptUrl
      bc <- rootContext utils bidi
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to UserPromptOpened event"

      (openedEventFired, waitOpenedEventFired) <- timeLimitLog BrowsingContextUserPromptOpened
      subscribeBrowsingContextUserPromptOpened openedEventFired

      (manyOpenedEventFired, waitManyOpenedEventFired) <- timeLimitLogMany BrowsingContextUserPromptOpened
      subscribeMany [BrowsingContextUserPromptOpened] manyOpenedEventFired

      logTxt "Click alert button to trigger prompt"
      scriptEvaluateNoWait $
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

      (manyClosedEventFired, waitManyClosedEventFired) <- timeLimitLogMany BrowsingContextUserPromptClosed
      subscribeMany [BrowsingContextUserPromptClosed] manyClosedEventFired

      pauseAtLeast $ 500 * milliseconds
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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Navigate to prompt page"
      url <- promptUrl
      bc <- rootContext utils bidi
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to UserPromptOpened and UserPromptClosed events"
      subscribeBrowsingContextUserPromptOpened $ logShow "UserPromptOpened"
      subscribeBrowsingContextUserPromptClosed $ logShow "UserPromptClosed"

      -- Test Alert
      logTxt "Testing Alert prompt"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "alert('Alert message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just True) Nothing
      pause

      -- Test Confirm - Accept
      logTxt "Testing Confirm prompt - Accept"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "confirm('Confirm message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just True) Nothing
      pause

      -- Test Confirm - Dismiss
      logTxt "Testing Confirm prompt - Dismiss"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "confirm('Confirm dismiss message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just False) Nothing
      pause

      -- Test Prompt - with user text
      logTxt "Testing Prompt with user text"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "prompt('Enter your name:', 'default')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      -- TODO: get rid of pauseAtleast in tests
      pauseAtLeast $ 500 * milliseconds
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just True) (Just "John Doe")
      pause

      -- Test Prompt - dismissed
      logTxt "Testing Prompt dismissed"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "prompt('This will be dismissed')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds
      browsingContextHandleUserPrompt $ MkHandleUserPrompt bc (Just False) Nothing
      pause

-- >>> runDemo browsingContextEventHistoryUpdated
-- *** Exception: user error (Timeout - Expected event did not fire: BrowsingContextHistoryUpdated after 10000 milliseconds)
browsingContextEventHistoryUpdated :: BiDiDemo
browsingContextEventHistoryUpdated =
  demo "Browsing Context Events - History Updated" action
  where
    -- NOTE: browsingContext.historyUpdated event is not yet implemented in geckodriver
    -- See: https://bugzilla.mozilla.org/show_bug.cgi?id=1906050
    -- Status: NEW (as of 2025-06-03)
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to HistoryUpdated event"

      (historyEventFired, waitHistoryEventFired) <- timeLimitLog BrowsingContextHistoryUpdated
      subscribeBrowsingContextHistoryUpdated historyEventFired

      (manyHistoryEventFired, waitManyHistoryEventFired) <- timeLimitLogMany BrowsingContextHistoryUpdated
      subscribeMany [BrowsingContextHistoryUpdated] manyHistoryEventFired

      logTxt "Navigate to checkboxes page"
      url1 <- checkboxesUrl
      bc <- rootContext utils bidi
      browsingContextNavigate $ MkNavigate bc url1 Nothing
      pause

      -- not implemnted in geckodriver yet this will fail
      logTxt "Navigate to textArea page"
      url2 <- textAreaUrl
      browsingContextNavigate $ MkNavigate bc url2 Nothing
      pause

      -- back button
      logTxt "Click back button"
      scriptEvaluate $
        MkEvaluate
          { expression = "window.history.back()",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      -- this would work in geckodriver but it is a fudge
      -- logTxt "Use pushState to modify browser history"
      -- scriptEvaluate $
      --   MkEvaluate
      --     { expression = "window.history.pushState({page: 2}, 'Page 2', '?page=2')",
      --       target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
      --       awaitPromise = False,
      --       resultOwnership = Nothing,
      --       serializationOptions = Nothing
      --     }
      -- logTxt "History modified with pushState"
      sequence_
        [ waitHistoryEventFired,
          waitManyHistoryEventFired
        ]

-- >>> runDemo browsingContextEventNavigationAborted
-- *** Exception: user error (Timeout - Expected event did not fire: BrowsingContextNavigationAborted after 10000 milliseconds)
browsingContextEventNavigationAborted :: BiDiDemo
browsingContextEventNavigationAborted =
  demo "Browsing Context Events - Navigation Aborted" action
  where
    -- NOTE: browsingContext.navigationAborted event support varies by driver:
    -- 
    -- geckodriver: Not implemented. The subscription fails with InvalidArgument error:
    --   "browsingContext.navigationAborted is not a valid event name"
    --   See: https://bugzilla.mozilla.org/show_bug.cgi?id=1874362
    --   Status: NEW (as of 2025-09-17)
    --
    -- chromedriver: Partially implemented. Accepts subscriptions but does not emit the event.
    --   The test times out waiting for the event that never fires.
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to NavigationAborted event"

      (abortedEventFired, waitAbortedEventFired) <- timeLimitLog BrowsingContextNavigationAborted
      subscribeBrowsingContextNavigationAborted abortedEventFired

      (manyAbortedEventFired, waitManyAbortedEventFired) <- timeLimitLogMany BrowsingContextNavigationAborted
      subscribeMany [BrowsingContextNavigationAborted] manyAbortedEventFired

      logTxt "Start navigation to slow loading page"
      url <- slowLoadUrl
      bc <- rootContext utils bidi

      -- Start navigation in background (non-blocking)
      scriptEvaluate $
        MkEvaluate
          { expression = "setTimeout(() => { window.location.href = '" <> url.url <> "'; }, 100)",
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
-- *** Exception: BiDIError (ProtocolException {error = UnknownError, description = "An unknown error occurred in the remote end while processing the command", message = "net::ERR_NAME_NOT_RESOLVED", stacktrace = Just "Error\n    at new UnknownErrorException (<anonymous>:65:5630)\n    at BrowsingContextImpl.navigate (<anonymous>:679:14660)\n    at async #processCommand (<anonymous>:485:5805)\n    at async CommandProcessor.processCommand (<anonymous>:485:12768)", errorData = Nothing, response = Object (fromList [("error",String "unknown error"),("id",Number 4.0),("message",String "net::ERR_NAME_NOT_RESOLVED"),("stacktrace",String "Error\n    at new UnknownErrorException (<anonymous>:65:5630)\n    at BrowsingContextImpl.navigate (<anonymous>:679:14660)\n    at async #processCommand (<anonymous>:485:5805)\n    at async CommandProcessor.processCommand (<anonymous>:485:12768)"),("type",String "error")])})
browsingContextEventNavigationFailed :: BiDiDemo
browsingContextEventNavigationFailed =
  demo "Browsing Context Events - Navigation Failed (NOT WORKING - driver issue)" action
  where
    -- NOTE: browsingContext.navigationFailed event is implemented in the library,
    -- but both geckodriver and chromedriver throw an error on the navigate command 
    -- itself for DNS failures rather than starting the navigation and then firing
    -- a navigationFailed event.
    --
    -- geckodriver throws: NS_ERROR_UNKNOWN_HOST
    -- chromedriver throws: ERR_NAME_NOT_RESOLVED
    --
    -- This appears to be a driver behavior issue. The navigate command returns
    -- an error before the navigation lifecycle begins, so no navigationFailed event
    -- is emitted.
    --
    -- Possible alternatives to test this event:
    -- 1. Use a URL that resolves but returns a server error (may trigger different event)
    -- 2. Use network interception to force a failure during navigation
    -- 3. Wait for geckodriver fix to properly emit navigationFailed events
    --
    -- The library implementation is correct and will handle the event when it fires.
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to NavigationFailed event"

      (failedEventFired, waitFailedEventFired) <- timeLimitLog BrowsingContextNavigationFailed
      subscribeBrowsingContextNavigationFailed failedEventFired

      (manyFailedEventFired, waitManyFailedEventFired) <- timeLimitLogMany BrowsingContextNavigationFailed
      subscribeMany [BrowsingContextNavigationFailed] manyFailedEventFired

      logTxt "Attempting to navigate to invalid URL"
      logTxt "NOTE: This will throw an error instead of firing navigationFailed event"
      bc <- rootContext utils bidi

      -- This will throw NS_ERROR_UNKNOWN_HOST error from geckodriver
      -- instead of firing a navigationFailed event
      browsingContextNavigate $ MkNavigate { context = bc, url = MkUrl "https://invalid-domain-that-does-not-exist-12345", wait = Nothing }

      sequence_
        [ waitFailedEventFired,
          waitManyFailedEventFired
        ]

-- >>> runDemo browsingContextEventDownloadWillBegin
browsingContextEventDownloadWillBegin :: BiDiDemo
browsingContextEventDownloadWillBegin =
  demo "Browsing Context Events - Download Will Begin (NOT IMPLEMENTED)" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Navigate to download link page"
      url <- downloadLinkUrl
      bc <- rootContext utils bidi
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to DownloadWillBegin event"

      (downloadEventFired, waitDownloadEventFired) <- timeLimitLog BrowsingContextDownloadWillBegin
      subscribeBrowsingContextDownloadWillBegin downloadEventFired

      (manyDownloadEventFired, waitManyDownloadEventFired) <- timeLimitLogMany BrowsingContextDownloadWillBegin
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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Navigate to download link page"
      url <- downloadLinkUrl
      bc <- rootContext utils bidi
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      (downloadEndEventFired, waitDownloadEndEventFired) <- timeLimitLog BrowsingContextDownloadEnd
      subscribeBrowsingContextDownloadEnd downloadEndEventFired

      (manyDownloadEndEventFired, waitManyDownloadEndEventFired) <- timeLimitLogMany BrowsingContextDownloadEnd
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
