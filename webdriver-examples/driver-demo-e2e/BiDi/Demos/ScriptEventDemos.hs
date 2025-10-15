module BiDi.Demos.ScriptEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import Const (milliseconds)
import IOUtils (DemoUtils (..))
import TestData (checkboxesUrl, scriptRealmUrl)
import WebDriverPreCore.BiDi.Protocol
  ( AddPreloadScript (..),
    ContextTarget (..),
    Evaluate (..),
    Navigate (..),
    SubscriptionType (ScriptMessage, ScriptRealmCreated, ScriptRealmDestroyed),
    Target (..)
  )
import WebDriverPreCore.BiDi.Script
  ( Channel (..),
    ChannelProperties (..),
    ChannelValue (..),
    ResultOwnership (..)
  )
import Prelude hiding (log, putStrLn)

{-
Script Events - Implementation Status

1. script.message :: ✓ scriptEventMessage
2. script.realmCreated :: ✓ scriptEventRealmLifecycle
3. script.realmDestroyed :: ✓ scriptEventRealmLifecycle
-}

-- >>> runDemo scriptEventRealmLifecycle
-- *** Exception: user error (could not parse event params (in SingleSubscription) for ScriptRealmDestroyed
-- Parser error was: 
-- Error in $: parsing Text failed, expected String, but encountered Object
-- The actual JSON value was: {
--     "realm": "c5913f19-6681-46c3-a5ea-0e8279fbd5e9"
-- })
scriptEventRealmLifecycle :: BiDiDemo
scriptEventRealmLifecycle =
  demo "Script Events - Realm Created and Destroyed" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to RealmCreated and RealmDestroyed events"

      (realmCreatedEventFired, waitRealmCreatedEventFired) <- timeLimitLog ScriptRealmCreated
      subscribeScriptRealmCreated realmCreatedEventFired

      (manyRealmCreatedEventFired, waitManyRealmCreatedEventFired) <- timeLimitLog ScriptRealmCreated
      subscribeMany [ScriptRealmCreated] manyRealmCreatedEventFired

      (realmDestroyedEventFired, waitRealmDestroyedEventFired) <- timeLimitLog ScriptRealmDestroyed
      subscribeScriptRealmDestroyed realmDestroyedEventFired

      (manyRealmDestroyedEventFired, waitManyRealmDestroyedEventFired) <- timeLimitLog ScriptRealmDestroyed
      subscribeMany [ScriptRealmDestroyed] manyRealmDestroyedEventFired

      logTxt "Navigate to script realm test page"
      url <- scriptRealmUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Create iframe to trigger realmCreated event"
      scriptEvaluate $
        MkEvaluate
          { expression = "createIframe()",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      logTxt "Waiting for realmCreated events..."
      sequence_
        [ waitRealmCreatedEventFired,
          waitManyRealmCreatedEventFired
        ]

      logTxt "Remove iframe to trigger realmDestroyed event"
      scriptEvaluate $
        MkEvaluate
          { expression = "removeIframe()",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      logTxt "Waiting for realmDestroyed events..."
      sequence_
        [ waitRealmDestroyedEventFired,
          waitManyRealmDestroyedEventFired
        ]

-- >>> runDemo scriptEventMessage
-- *** Exception: user error (could not parse Event for (in MultiSubscription) for ScriptMessage
-- Parser error was: 
-- Error in $.params: parsing WebDriverPreCore.BiDi.Script.Message(MkMessage) failed, key "messageData" not found
-- The actual JSON value was: {
--     "method": "script.message",
--     "params": {
--         "channel": "testChannel",
--         "data": {
--             "type": "string",
--             "value": "Message from preload script via channel"
--         },
--         "source": {
--             "context": "9dcfe98a-fcd2-4efb-9964-2c6fc677d4a5",
--             "realm": "4d83fc54-db98-4d1f-aaa7-d32205fc5cd2"
--         }
--     },
--     "type": "event"
-- })
scriptEventMessage :: BiDiDemo
scriptEventMessage =
  demo "Script Events - Message via Channel" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to Message event"

      (messageEventFired, waitMessageEventFired) <- timeLimitLog ScriptMessage
      subscribeScriptMessage messageEventFired

      (manyMessageEventFired, waitManyMessageEventFired) <- timeLimitLog ScriptMessage
      subscribeMany [ScriptMessage] manyMessageEventFired

      logTxt "Navigate to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      let channelName = "testChannel"
          channel = Channel {channelId = channelName}
          channelValue =
            MkChannelValue
              { value =
                  MkChannelProperties
                    { channel = channel,
                      serializationOptions = Nothing,
                      ownership = Just Root
                    }
              }

      logTxt "Add preload script with channel to send messages"
      scriptAddPreloadScript $
        MkAddPreloadScript
          { functionDeclaration =
              "function(channel) { channel('Message from preload script via channel'); }",
            arguments = Just [channelValue],
            contexts = Just [bc],
            userContexts = Nothing,
            sandbox = Nothing
          }

      logTxt "Navigate again to trigger preload script and message event"
      browsingContextNavigate $ MkNavigate bc url Nothing

      logTxt "Waiting for message events..."
      sequence_
        [ waitMessageEventFired,
          waitManyMessageEventFired
        ]

-- >>> runDemo scriptEventMessageRuntime
scriptEventMessageRuntime :: BiDiDemo
scriptEventMessageRuntime =
  demo "Script Events - Runtime Message via sendBidiMessage" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to Message event"
      subscribeScriptMessage $ logShow "Script Message Event"

      logTxt "Navigate to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext utils cmds
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      let channelName = "runtimeChannel"
          channel = Channel {channelId = channelName}
          channelValue =
            MkChannelValue
              { value =
                  MkChannelProperties
                    { channel = channel,
                      serializationOptions = Nothing,
                      ownership = Just Root
                    }
              }

      logTxt "Add preload script to set up sendBidiMessage function"
      scriptAddPreloadScript $
        MkAddPreloadScript
          { functionDeclaration =
              "function(channel) { \
              \  window.sendBidiMessage = function(msg) { channel(msg); }; \
              \}",
            arguments = Just [channelValue],
            contexts = Just [bc],
            userContexts = Nothing,
            sandbox = Nothing
          }

      logTxt "Navigate to trigger preload script setup"
      browsingContextNavigate $ MkNavigate bc url Nothing
      pause

      logTxt "Send message from JavaScript via channel"
      scriptEvaluate $
        MkEvaluate
          { expression = "window.sendBidiMessage('Hello from JavaScript runtime!')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      logTxt "Send another message with different content"
      scriptEvaluate $
        MkEvaluate
          { expression = "window.sendBidiMessage('Second message: ' + new Date().toISOString())",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause
