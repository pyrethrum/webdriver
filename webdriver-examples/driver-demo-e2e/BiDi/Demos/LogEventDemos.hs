module BiDi.Demos.LogEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import Data.Maybe (fromJust)
import IOUtils (DemoUtils (..))
import TestData (checkboxesUrl, consoleLogUrl, badJavaScriptUrl)
import WebDriverPreCore.BiDi.BrowsingContext (Locator (..))
import WebDriverPreCore.BiDi.CoreTypes (NodeRemoteValue (..))
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Protocol
  ( ContextTarget (..),
    Evaluate (..),
    Navigate (..),
    SubscriptionType (LogEntryAdded),
    Target (..),
    LocateNodes (..),
    LocateNodesResult (..),
    SharedReference (..),
    SharedId
  )
import Prelude hiding (log, putStrLn)

{-
Log Events - Implementation Status

1. log.entryAdded (generic) :: ✓ logEventGenericEntry
2. log.entryAdded (console) :: ✓ logEventConsoleEntries
3. log.entryAdded (javascript) :: ✓ logEventJavascriptError, logEventJavascriptTypeError, logEventJavascriptReferenceError
4. Console log levels :: ✓ logEventConsoleLevelDebug, logEventConsoleLevelInfo, logEventConsoleLevelWarn, logEventConsoleLevelError
5. Multiple args :: ✓ logEventConsoleMultipleArgs
6. Context filtering :: ✓ logEventFilteredByContext
7. Combined tests :: ✓ logEventConsoleLogCombined, logEventJavascriptErrorCombined, logEventConsoleWarnCombined
-}

-- Helper function to create default pointer common properties
defaultPointerProps :: PointerCommonProperties
defaultPointerProps =
  MkPointerCommonProperties
    { width = Nothing,
      height = Nothing,
      pressure = Nothing,
      tangentialPressure = Nothing,
      twist = Nothing,
      altitudeAngle = Nothing,
      azimuthAngle = Nothing
    }

-- >>> runDemo logEventConsoleEntries
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


-- >>> runDemo logEventConsoleLevelDebug
logEventConsoleLevelDebug :: BiDiDemo
logEventConsoleLevelDebug =
  demo "Log Events - Console Debug Level" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
 
      logTxt "Subscribe to LogEntryAdded event"
      (logEventFired, waitLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeLogEntryAdded logEventFired

      (manyLogEventFired, waitManyLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeMany [LogEntryAdded] manyLogEventFired

      logTxt "Triggering console.debug()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.debug('Debug level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      sequence_
        [ waitLogEventFired,
          waitManyLogEventFired
        ]

-- >>> runDemo logEventConsoleLevelInfo
logEventConsoleLevelInfo :: BiDiDemo
logEventConsoleLevelInfo =
  demo "Log Events - Console Info Level" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Subscribe to LogEntryAdded event"
      (logEventFired, waitLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeLogEntryAdded logEventFired

      (manyLogEventFired, waitManyLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeMany [LogEntryAdded] manyLogEventFired

      logTxt "Triggering console.info()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.info('Info level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      sequence_
        [ waitLogEventFired,
          waitManyLogEventFired
        ]

-- >>> runDemo logEventConsoleLevelWarn
logEventConsoleLevelWarn :: BiDiDemo
logEventConsoleLevelWarn =
  demo "Log Events - Console Warn Level" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Subscribe to LogEntryAdded event"
      (logEventFired, waitLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeLogEntryAdded logEventFired

      (manyLogEventFired, waitManyLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeMany [LogEntryAdded] manyLogEventFired

      logTxt "Triggering console.warn()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.warn('Warning level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      sequence_
        [ waitLogEventFired,
          waitManyLogEventFired
        ]

-- >>> runDemo logEventConsoleLevelError
logEventConsoleLevelError :: BiDiDemo
logEventConsoleLevelError =
  demo "Log Events - Console Error Level" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Subscribe to LogEntryAdded event"
      (logEventFired, waitLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeLogEntryAdded logEventFired

      (manyLogEventFired, waitManyLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeMany [LogEntryAdded] manyLogEventFired

      logTxt "Triggering console.error()"
      scriptEvaluate $
        MkEvaluate
          { expression = "console.error('Error level message')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      sequence_
        [ waitLogEventFired,
          waitManyLogEventFired
        ]

-- >>> runDemo logEventGenericEntry
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
      (logEventFired, waitLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeLogEntryAdded logEventFired

      (manyLogEventFired, waitManyLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeMany [LogEntryAdded] manyLogEventFired

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
      sequence_
        [ waitLogEventFired,
          waitManyLogEventFired
        ]

      logTxt "Note: Generic log entries are browser-generated and may not occur in this demo"
      logTxt "The subscription will capture console and javascript entries as demonstrated"


-- >>> runDemo logEventJavascriptErrorFromButton
logEventJavascriptErrorFromButton :: BiDiDemo
logEventJavascriptErrorFromButton =
  demo "Log Events - JavaScript Error from Button Click" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Navigate to Bad JavaScript test page"
      url <- badJavaScriptUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Subscribe to LogEntryAdded event"
      (logEventFired, waitLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeLogEntryAdded logEventFired

      (manyLogEventFired, waitManyLogEventFired) <- timeLimitLog LogEntryAdded
      subscribeMany [LogEntryAdded] manyLogEventFired

      logTxt "Locate the 'Bad JS' button"
      buttonResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#badJsButton"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Button search result" buttonResult

      let MkLocateNodesResult nodes = buttonResult
          buttonId :: SharedId
          buttonId = case nodes of
            [button] -> fromJust button.sharedId
            _ -> error "Failed to locate Bad JS button"

      logTxt "Click the 'Bad JS' button to trigger JavaScript error"
      inputPerformActions $
        MkPerformActions
          { context = bc,
            actions =
              [ PointerSourceActions $
                  MkPointerSourceActions
                    { pointerId = "mouse1",
                      pointer = Just $ MkPointer {pointerType = Just MousePointer},
                      pointerActions =
                        [ PointerMove
                            { x = 0,
                              y = 0,
                              duration = Just 300,
                              origin =
                                Just $
                                  ElementOrigin $
                                    MkSharedReference
                                      { sharedId = buttonId,
                                        handle = Nothing,
                                        extensions = Nothing
                                      },
                              pointerCommonProperties = defaultPointerProps
                            },
                          PointerDown
                            { button = 0,
                              pointerCommonProperties = defaultPointerProps
                            },
                          PointerUp
                            { button = 0
                            }
                        ]
                    }
              ]
          }

      logTxt "Waiting for JavaScript error log events..."
      sequence_
        [ waitLogEventFired,
          waitManyLogEventFired
        ]

