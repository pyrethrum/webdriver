module Main where

import ApiCoverageTest qualified as API
import BiDi.DemoUtils (BiDiDemo (..), runDemo')
import BiDi.Demos.BrowserDemos qualified as Browser
import BiDi.Demos.BrowsingContextDemos qualified as BrowsingContext
import BiDi.Demos.BrowsingContextEventDemos qualified as BrowsingContextEvent
import BiDi.Demos.EmulationDemos qualified as Emulation
import BiDi.Demos.InputDemos qualified as Input
import BiDi.Demos.InputEventDemos qualified as InputEvent
import BiDi.Demos.LogEventDemos qualified as LogEvent
import BiDi.Demos.NetworkDemos qualified as Network
import BiDi.Demos.NetworkEventDemos qualified as NetworkEvent
import BiDi.Demos.ScriptDemos qualified as Script
import BiDi.Demos.ScriptEventDemos qualified as ScriptEvent
import BiDi.Demos.SessionDemos qualified as Session
import BiDi.Demos.StorageDemos qualified as Storage
import BiDi.Demos.WebExtensionDemos qualified as WebExtension
import Const (Timeout (MkTimeout, microseconds))
import Control.Exception (SomeException)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import ErrorCoverageTest qualified as Error
import IOUtils (DemoUtils (..))
import JSONParsingTest qualified as JSON
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import UnliftIO (try)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ unitTests,
      propertyTests,
      bidiDemos
    ]

-- [ bidiTest
--     "Session"
--     [ Session.sessionStatusDemo,
--       biDiError
--         "Maximum number of active sessions"
--         Session.sessionNewDemo,
--       biDiError
--         "Ending a session started with WebDriver classic is not supported"
--         Session.sessionEndDemo,
--       Session.sessionSubscribeDemo,
--       Session.sessionUnsubscribeDemo,
--       biDiError
--         "Maximum number of active sessions"
--         Session.sessionCapabilityNegotiationDemo,
--       Session.sessionCompleteLifecycleDemo
--     ]
-- ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testGroup
        "API Coverage"
        [ testCase "All endpoints covered" API.unit_test_all_endpoints_covered
        ],
      testGroup
        "Error Coverage"
        [ testCase "All errors covered" Error.unit_test_all_errors_covered,
          testCase "Round trip error codes" Error.unit_round_trip_error_codes
        ],
      testGroup
        "JSON Parsing"
        [ testCase "WebSocket URL from JSON" JSON.unit_websocketUrlFromJSon
        ]
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ testGroup
        "JSON Parsing"
        [ JSON.test_round_trip
        ]
    ]

fromBidiDemo :: BiDiDemo -> TestTree
fromBidiDemo demo =
  testCase (unpack demo.name) $ runDemo' False MkTimeout {microseconds = 0} demo

bidiTest :: Text -> [BiDiDemo] -> TestTree
bidiTest title = testGroup (unpack title) . fmap fromBidiDemo

bidiDemos :: TestTree
bidiDemos =
  testGroup
    "BiDi Demos"
    [ testGroup
        "BiDi Commands"
        [ bidiTest
            "Browser"
            [ Browser.browserGetClientWindowsDemo,
              Browser.browserCreateUserContextDemo,
              Browser.browserGetUserContextsDemo,
              Browser.browserSetClientWindowStateDemo,
              Browser.browserRemoveUserContextDemo,
              Browser.browserCompleteWorkflowDemo,
              biDiError
                ( "Closing the browser in a session started with WebDriver classic is not supported. "
                    <> "Use the WebDriver classic \"Delete Session\" command instead which will also close the browser."
                )
                Browser.browserCloseDemo
            ],
          bidiTest
            "Browsing Context"
            [ BrowsingContext.browsingContextCreateActivateCloseDemo,
              BrowsingContext.browsingContextCaptureScreenshotCloseDemo,
              BrowsingContext.browsingContextClosePromptUnloadDemo,
              BrowsingContext.browsingContextGetTreeDemo,
              BrowsingContext.browsingContextHandleUserPromptDemo,
              BrowsingContext.browsingNavigateReloadTraverseHistoryDemo,
              BrowsingContext.browsingContextLocateNodesDemo,
              BrowsingContext.browsingContextPrintAndSetViewportDemo
            ],
          bidiTest
            "Emulation"
            [ Emulation.emulationSetGeolocationOverrideDemo,
              Emulation.emulationSetLocaleOverrideDemo,
              Emulation.emulationSetScreenOrientationOverrideDemo,
              Emulation.emulationSetTimezoneOverrideDemo,
              Emulation.emulationCompleteWorkflowDemo
            ],
          bidiTest
            "Input"
            [ Input.inputKeyboardDemo,
              Input.inputPointerDemo,
              Input.inputWheelDemo,
              Input.inputCombinedActionsDemo,
              Input.inputReleaseActionsDemo,
              Input.inputSetFilesDemo
            ],
          bidiTest
            "Network"
            [ Network.networkDataCollectorDemo,
              Network.networkInterceptDemo,
              Network.networkRequestResponseModificationDemo,
              Network.networkAuthAndFailureDemo,
              Network.networkProvideResponseDemo,
              Network.networkDataRetrievalDemo,
              Network.networkCacheBehaviorDemo
            ],
          bidiTest
            "Script"
            [ Script.scriptEvaluateAllPrimitiveTypesDemo,
              Script.scriptEvaluateAdvancedDemo,
              Script.serializationOptionsDemo,
              Script.scriptPreloadScriptDemo,
              Script.scriptPreloadScriptMultiContextDemo,
              Script.scriptChannelArgumentDemo,
              Script.scriptUserContextsDemo,
              Script.scriptCallFunctionDemo,
              Script.scriptGetRealmsAndDisownDemo
            ],
          bidiTest
            "Session"
            [ Session.sessionStatusDemo,
              biDiError
                "Maximum number of active sessions"
                Session.sessionNewDemo,
              biDiError
                "Ending a session started with WebDriver classic is not supported"
                Session.sessionEndDemo,
              Session.sessionSubscribeDemo,
              Session.sessionUnsubscribeDemo,
              biDiError
                "Maximum number of active sessions"
                Session.sessionCapabilityNegotiationDemo,
              Session.sessionCompleteLifecycleDemo
            ],
          bidiTest
            "Storage"
            [ Storage.storageGetCookiesDemo,
              Storage.storageSetCookieDemo,
              Storage.storageDeleteCookiesDemo,
              Storage.storagePartitionKeyDemo,
              Storage.storageCompleteWorkflowDemo
            ],
          bidiTest
            "WebExtension"
            [ WebExtension.webExtensionInstallPathDemo,
              WebExtension.webExtensionInstallArchiveDemo,
              WebExtension.webExtensionInstallBase64Demo,
              WebExtension.webExtensionValidationDemo
            ]
        ],
      testGroup
        "BiDi Events"
        [ bidiTest
            "Browsing Context Events"
            [ BrowsingContextEvent.browsingContextEventDemo,
              BrowsingContextEvent.browsingContextEventDemoMulti,
              BrowsingContextEvent.browsingContextEventDemoFilteredSubscriptions,
              BrowsingContextEvent.browsingContextEventDemoUserContextFiltered,
              BrowsingContextEvent.browsingContextEventCreateDestroy,
              BrowsingContextEvent.browsingContextEventNavigationLifecycle,
              BrowsingContextEvent.browsingContextEventFragmentNavigation,
              BrowsingContextEvent.browsingContextEventUserPrompts,
              BrowsingContextEvent.browsingContextEventUserPromptsVariants,
              biDiError
                "Expected event did not fire: BrowsingContextHistoryUpdated"
                BrowsingContextEvent.browsingContextEventHistoryUpdated,
              BrowsingContextEvent.browsingContextEventNavigationAborted,
              BrowsingContextEvent.browsingContextEventNavigationFailed,
              BrowsingContextEvent.browsingContextEventDownloadWillBegin,
              BrowsingContextEvent.browsingContextEventDownloadEnd
            ],
          bidiTest
            "Input Events"
            [ InputEvent.inputEventFileDialogOpened
            ],
          bidiTest
            "Log Events"
            [ LogEvent.logEventConsoleEntries,
              LogEvent.logEventConsoleLevelDebug,
              LogEvent.logEventConsoleLevelInfo,
              LogEvent.logEventConsoleLevelWarn,
              LogEvent.logEventConsoleLevelError,
              LogEvent.logEventJavascriptErrorFromButton
            ],
          bidiTest
            "Network Events"
            [ NetworkEvent.networkEventBeforeRequestSent,
              NetworkEvent.networkEventRequestResponseLifecycle,
              NetworkEvent.networkEventFetchError,
              NetworkEvent.networkEventAuthRequired
            ],
          bidiTest
            "Script Events"
            [ ScriptEvent.scriptEventRealmLifecycle,
              ScriptEvent.scriptEventMessage,
              ScriptEvent.scriptEventMessageRuntime
            ]
        ]
    ]

biDiError :: Text -> BiDiDemo -> BiDiDemo
biDiError errorFragment MkBiDiDemo {name, action} =
  MkBiDiDemo
    { name = name <> " - EXPECTED ERROR: " <> errorFragment,
      action = \utils cmds -> do
        result <- try (action utils cmds)
        case result of
          Left (e :: SomeException) -> do
            let errText = txt $ show e
            if errorFragment `T.isInfixOf` errText
              then utils.logTxt $ "Caught expected error: " <> errText
              else fail $ "Error did not contain expected fragment. Got: " <> unpack errText
          Right _ ->
            fail "Expected error, but action completed successfully."
    }
