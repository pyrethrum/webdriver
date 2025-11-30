module BiDi.Demos.ScriptEventDemos where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoActions (..))
import TestData (checkboxesUrl, scriptRealmUrl)
import WebDriverPreCore.BiDi.Protocol
  ( AddPreloadScript (..),
    Channel (..),
    ChannelProperties (..),
    ChannelValue (..),
    ContextTarget (..),
    Evaluate (..),
    KnownSubscriptionType (ScriptMessage, ScriptRealmCreated, ScriptRealmDestroyed),
    Navigate (..),
    ResultOwnership (..),
    Target (..)
  )
import Prelude hiding (log, putStrLn)

-- >>> runDemo scriptEventRealmLifecycle
scriptEventRealmLifecycle :: BiDiDemo
scriptEventRealmLifecycle =
  demo "Script Events - Realm Created and Destroyed" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to RealmCreated and RealmDestroyed events"

      (realmCreatedEventFired, waitRealmCreatedEventFired) <- timeLimitLog ScriptRealmCreated
      subscribeScriptRealmCreated realmCreatedEventFired

      (manyRealmCreatedEventFired, waitManyRealmCreatedEventFired) <- timeLimitLogMany ScriptRealmCreated
      subscribeMany [ScriptRealmCreated] manyRealmCreatedEventFired

      (realmDestroyedEventFired, waitRealmDestroyedEventFired) <- timeLimitLog ScriptRealmDestroyed
      subscribeScriptRealmDestroyed realmDestroyedEventFired

      (manyRealmDestroyedEventFired, waitManyRealmDestroyedEventFired) <- timeLimitLogMany ScriptRealmDestroyed
      subscribeMany [ScriptRealmDestroyed] manyRealmDestroyedEventFired

      logTxt "Navigate to script realm test page"
      url <- scriptRealmUrl
      bc <- rootContext utils bidi
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
        [ 
          waitRealmCreatedEventFired,
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
        [ 
          waitRealmDestroyedEventFired,
          waitManyRealmDestroyedEventFired
        ]

-- >>> runDemo scriptEventMessage
scriptEventMessage :: BiDiDemo
scriptEventMessage =
  demo "Script Events - Message via Channel" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to Message event"

      (messageEventFired, waitMessageEventFired) <- timeLimitLog ScriptMessage
      subscribeScriptMessage messageEventFired

      (manyMessageEventFired, waitManyMessageEventFired) <- timeLimitLogMany ScriptMessage
      subscribeMany [ScriptMessage] manyMessageEventFired

      logTxt "Navigate to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext utils bidi
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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "Subscribe to Message event"
      subscribeScriptMessage $ logShow "Script Message Event"

      logTxt "Navigate to checkboxes page"
      url <- checkboxesUrl
      bc <- rootContext utils bidi
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
              "function(channel) {\
              \  window.sendBidiMessage = function(msg) { channel(msg); };\
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
