module BiDi.Demos.LogEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import Const (milliseconds)
import IOUtils (DemoUtils (..))
import TestData (checkboxesUrl, consoleLogUrl)
import WebDriverPreCore.BiDi.Log (LogEntry (..))
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    ContextTarget (..),
    Evaluate (..),
    Navigate (..),
    SubscriptionType (LogEntryAdded),
    Target (..)
  )
import Prelude hiding (log, putStrLn)

{-
Log Events - Implementation Status

1. log.entryAdded (generic) :: ✓ logEventGenericEntry
2. log.entryAdded (console) :: ✓ logEventConsoleEntries
3. log.entryAdded (javascript) :: ✓ logEventJavascriptErrors
4. All log levels :: ✓ logEventConsoleLevels
5. Multiple args :: ✓ logEventConsoleMultipleArgs
-}

-- >>> runDemo logEventConsoleEntries
-- *** Exception: user error (could not parse Event for LogEntryAdded
-- Parser error was: 
-- Error in $: Unknown log entry type: event
-- The actual JSON value was: {
--     "method": "log.entryAdded",
--     "params": {
--         "args": [
--             {
--                 "type": "string",
--                 "value": "Console log test page loaded"
--             }
--         ],
--         "level": "info",
--         "method": "log",
--         "source": {
--             "context": "60af8cb2-2bb0-4b17-a2e0-5bb1af2f1224",
--             "realm": "0bcbcdba-74c5-4238-8e8d-4936e1d9a93d"
--         },
--         "text": "Console log test page loaded",
--         "timestamp": 1760511242278,
--         "type": "console"
--     },
--     "type": "event"
-- })
logEventConsoleEntries :: BiDiDemo
logEventConsoleEntries =
  demo "Log Events - Console Log Entries" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to LogEntryAdded event"

      (logEventFired, waitLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeLogEntryAdded logEventFired

      (manyLogEventFired, waitManyLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeMany [LogEntryAdded] manyLogEventFired

      logTxt "Navigate to console log test page"
      url <- consoleLogUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing

      -- Page automatically logs on load, so events should fire
      logTxt "Waiting for console log events from page load..."

      sequence_
        [ waitLogEventFired,
          waitManyLogEventFired
        ]

-- >>> runDemo logEventConsoleLevels
-- *** Exception: user error (could not parse event params for LogEntryAdded
-- Parser error was: 
-- Error in $: parsing WebDriverPreCore.BiDi.Log.ConsoleLogEntry(MkConsoleLogEntry) failed, key "baseEntry" not found
-- The actual JSON value was: {
--     "args": [
--         {
--             "type": "string",
--             "value": "Console log test page loaded"
--         }
--     ],
--     "level": "info",
--     "method": "log",
--     "source": {
--         "context": "710ae8cc-10b8-429a-8897-7c67a2c45399",
--         "realm": "020280cb-2bf9-41c1-8735-d4abd81c84fd"
--     },
--     "text": "Console log test page loaded",
--     "timestamp": 1760505862819,
--     "type": "console"
-- })
logEventConsoleLevels :: BiDiDemo
logEventConsoleLevels =
  demo "Log Events - Console Log Levels (Debug, Info, Warn, Error)" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to console log test page"
      url <- consoleLogUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to LogEntryAdded event"
      subscribeLogEntryAdded $ logShow "Console Log Event"

      -- Test Debug level
      logTxt "Triggering console.debug()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.debug('Debug level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds

      -- Test Info level
      logTxt "Triggering console.info()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.info('Info level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds

      -- Test Warn level
      logTxt "Triggering console.warn()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.warn('Warning level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds

      -- Test Error level
      logTxt "Triggering console.error()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.error('Error level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

-- >>> runDemo logEventConsoleMultipleArgs
-- *** Exception: user error (could not parse event params for LogEntryAdded
-- Parser error was: 
-- Error in $: parsing WebDriverPreCore.BiDi.Log.ConsoleLogEntry(MkConsoleLogEntry) failed, key "baseEntry" not found
-- The actual JSON value was: {
--     "args": [
--         {
--             "type": "string",
--             "value": "Console log test page loaded"
--         }
--     ],
--     "level": "info",
--     "method": "log",
--     "source": {
--         "context": "4bcf5b35-cc8d-489d-908a-f76ac29fbc0e",
--         "realm": "2c88b0b2-1d99-4b53-8889-a29e70676581"
--     },
--     "text": "Console log test page loaded",
--     "timestamp": 1760505868414,
--     "type": "console"
-- })
logEventConsoleMultipleArgs :: BiDiDemo
logEventConsoleMultipleArgs =
  demo "Log Events - Console Log with Multiple Arguments" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to console log test page"
      url <- consoleLogUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to LogEntryAdded event"
      subscribeLogEntryAdded $ logShow "Console Log Event with Args"

      logTxt "Triggering console.log() with multiple arguments"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.log('String:', 123, true, null, {key: 'value'}, [1, 2, 3])",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

-- >>> runDemo logEventJavascriptErrors
logEventJavascriptErrors :: BiDiDemo
logEventJavascriptErrors =
  demo "Log Events - JavaScript Errors" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to console log test page"
      url <- consoleLogUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to LogEntryAdded event"
      subscribeLogEntryAdded $ logShow "JavaScript Error Event"

      logTxt "Triggering JavaScript Error"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "throw new Error('Test error for log event')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      logTxt "Triggering TypeError"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "throw new TypeError('Test type error')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      logTxt "Triggering ReferenceError"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "nonExistentFunction()",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

-- >>> runDemo logEventGenericEntry
-- *** Exception: user error (could not parse event params for LogEntryAdded
-- Parser error was: 
-- Error in $: parsing WebDriverPreCore.BiDi.Log.ConsoleLogEntry(MkConsoleLogEntry) failed, key "baseEntry" not found
-- The actual JSON value was: {
--     "args": [
--         {
--             "type": "string",
--             "value": "This generates a console log entry"
--         }
--     ],
--     "level": "info",
--     "method": "log",
--     "source": {
--         "context": "63ea773b-2679-4f85-97d9-d8027cce5030",
--         "realm": "ed33e509-3075-4485-bea7-9e730e4ee1f1"
--     },
--     "text": "This generates a console log entry",
--     "timestamp": 1760505884322,
--     "type": "console"
-- })
logEventGenericEntry :: BiDiDemo
logEventGenericEntry =
  demo "Log Events - Generic Log Entry" action
  where
    -- Generic log entries are less common and typically generated by browser internals
    -- or specific browser features. This demo shows how to subscribe to all log events
    -- and observe generic entries if they occur.
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to LogEntryAdded event (all types)"
      subscribeLogEntryAdded $ logShow "Log Event (any type)"

      logTxt "Navigate to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Generate various log entries"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.log('This generates a console log entry')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      logTxt "Note: Generic log entries are browser-generated and may not occur in this demo"
      logTxt "The subscription will capture console and javascript entries as demonstrated"

-- >>> runDemo logEventFilteredByContext
-- *** Exception: user error (could not parse event params for LogEntryAdded
-- Parser error was: 
-- Error in $: parsing WebDriverPreCore.BiDi.Log.ConsoleLogEntry(MkConsoleLogEntry) failed, key "baseEntry" not found
-- The actual JSON value was: {
--     "args": [
--         {
--             "type": "string",
--             "value": "Console log test page loaded"
--         }
--     ],
--     "level": "info",
--     "method": "log",
--     "source": {
--         "context": "8f92b91f-0ea6-49a4-8e39-44b1563fb094",
--         "realm": "2674338b-d5af-462b-93b5-74a5276b7500"
--     },
--     "text": "Console log test page loaded",
--     "timestamp": 1760505891971,
--     "type": "console"
-- })
logEventFilteredByContext :: BiDiDemo
logEventFilteredByContext =
  demo "Log Events - Filtered by Browsing Context" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Creating two browsing contexts"
      bc1 <- newWindowContext utils cmds
      bc2 <- newWindowContext utils cmds

      logTxt "Navigate both contexts to console log test page"
      url <- consoleLogUrl
      browsingContextNavigate $ MkNavigate bc1 url Nothing
      browsingContextNavigate $ MkNavigate bc2 url Nothing
      pause

      logTxt "Subscribe to LogEntryAdded events only for browsing context 1"
      subscribeLogEntryAdded' [bc1] [] $
        logShow $
          "Log Event from Context 1 (SHOULD fire - context: " <> bc1.context <> ")"

      pauseAtLeast $ 500 * milliseconds

      logTxt "Trigger console.log in browsing context 1 (SHOULD trigger event)"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.log('Log from context 1')",
            target = ContextTarget $ MkContextTarget {context = bc1, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      logTxt "Trigger console.log in browsing context 2 (should NOT trigger event)"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.log('Log from context 2 - should not be captured')",
            target = ContextTarget $ MkContextTarget {context = bc2, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

-- >>> runDemo logEventConsoleAndJavascriptCombined
-- *** Exception: user error (could not parse event params for LogEntryAdded
-- Parser error was: 
-- Error in $: parsing WebDriverPreCore.BiDi.Log.ConsoleLogEntry(MkConsoleLogEntry) failed, key "baseEntry" not found
-- The actual JSON value was: {
--     "args": [
--         {
--             "type": "string",
--             "value": "Console log test page loaded"
--         }
--     ],
--     "level": "info",
--     "method": "log",
--     "source": {
--         "context": "a5599baa-87bd-4fcd-a6b3-220321f3def9",
--         "realm": "e5676e4b-5612-408c-bbec-dcea699c6ef4"
--     },
--     "text": "Console log test page loaded",
--     "timestamp": 1760505919876,
--     "type": "console"
-- })
logEventConsoleAndJavascriptCombined :: BiDiDemo
logEventConsoleAndJavascriptCombined =
  demo "Log Events - Console and JavaScript Errors Combined" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to console log test page"
      url <- consoleLogUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to LogEntryAdded event (captures both console and javascript entries)"
      subscribeLogEntryAdded $ \entry ->
        case entry of
          ConsoleEntry consoleEntry ->
            logShow "Console Log Entry" consoleEntry
          JavascriptEntry jsEntry ->
            logShow "JavaScript Error Entry" jsEntry
          GenericEntry genericEntry ->
            logShow "Generic Log Entry" genericEntry

      logTxt "Trigger console.log"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.log('Console message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds

      logTxt "Trigger JavaScript error"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "throw new Error('JavaScript error message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds

      logTxt "Trigger console.warn"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.warn('Warning message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause
