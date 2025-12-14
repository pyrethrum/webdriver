{-# LANGUAGE CPP #-}

module Main where

import ApiCoverageTest qualified as API
import BiDi.DemoUtils (BiDiDemo (..), expectError, runDemo')
import BiDi.Demos.BrowserDemos qualified as Browser
import BiDi.Demos.BrowsingContextDemos qualified as BrowsingContext
import BiDi.Demos.BrowsingContextEventDemos qualified as BrowsingContextEvent
import BiDi.Demos.EmulationDemos qualified as Emulation
import BiDi.Demos.FallbackDemos qualified as Fallback
import BiDi.Demos.InputDemos qualified as Input
import BiDi.Demos.InputEventDemos qualified as InputEvent
import BiDi.Demos.LogEventDemos qualified as LogEvent
import BiDi.Demos.NetworkDemos qualified as Network
import BiDi.Demos.NetworkEventDemos qualified as NetworkEvent
import BiDi.Demos.OtherDemos qualified as Other
import BiDi.Demos.ScriptDemos qualified as Script
import BiDi.Demos.ScriptEventDemos qualified as ScriptEvent
import BiDi.Demos.SessionDemos qualified as Session
import BiDi.Demos.StorageDemos qualified as Storage
import BiDi.Demos.WebExtensionDemos qualified as WebExtension
import BiDi.ErrorDemo qualified as BiDiError
import Config (Config (..))
import ConfigLoader (loadConfig)
import Data.Text (Text, unpack)
import ErrorCoverageTest qualified as Error
import HTTP.DemoUtils (HttpDemo (..), runDemoWithConfig)
import HTTP.ErrorDemo qualified as HttpError
import HTTP.HttpDemo qualified as Http
#ifndef LEGACY_TEST
import HTTP.FallbackDemo qualified as HttpFallback
#endif
import JSONParsingTest qualified as JSON
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = do
  cfg <- loadConfig
  let testCfg =
        cfg
          { logging = False,
            pauseMS = 0
          }
  -- defaultMain tests
  defaultMain $
    tests testCfg

tests :: Config -> TestTree
tests cfg =
  testGroup
    "Tests"
#ifdef LEGACY_TEST
    [ 
      httpDemos cfg
    ]
#else
    [ unitTests,
      httpDemos cfg,
      propertyTests,
      bidiDemos cfg
    ]
#endif

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
          testCase "Round trip error codes" Error.unit_round_trip_error_codes,
          testCase "All BiDi errors covered" Error.unit_test_all_errors_covered,
          testCase "Round trip BiDi error codes" Error.unit_round_trip_error_codes
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

httpDemos :: Config -> TestTree
httpDemos cfg =
  testGroup
    "HTTP Demos"
    $ fromHttpDemo cfg
      <$> [ Http.newSessionDemo,
            Http.driverStatusDemo,
            Http.demoSendKeysClear,
            Http.demoForwardBackRefresh,
            -- this test is redundant but used in docs so run anyway
            Http.documentationDemo,
            Http.demoWindowHandles,
            Http.demoWindowSizes,
            Http.demoElementPageProps,
            Http.demoTimeouts,
            Http.demoWindowRecs,
            Http.demoWindowFindElement,
            Http.demoFrames,
            Http.demoShadowDom,
            Http.demoIsElementSelected,
            Http.demoGetPageSourceScreenShot,
            Http.demoPrintPage,
            Http.demoExecuteScript,
            Http.demoCookies,
            Http.demoCookiesWithDomain,
            Http.demoAlerts,
            Http.demoPointerNoneActions,
            Http.demoKeyAndReleaseActions,
            Http.demoWheelActions,
            Http.demoError,
            HttpError.errorDemo
-- fallback commands not implemented for legacy
#ifndef LEGACY_TEST
            , HttpFallback.demoFallbackActions
            , HttpFallback.demoFallbackCoercions
            , HttpFallback.demoExtendPost
#endif
          ]

httpTest :: Config -> Text -> [HttpDemo] -> TestTree
httpTest cfg title = testGroup (unpack title) . fmap (fromHttpDemo cfg)

fromHttpDemo :: Config -> HttpDemo -> TestTree
fromHttpDemo cfg demo = testCase (unpack demo.name) $ runDemoWithConfig cfg demo

-- testCase (unpack demo.name) $ runDemo' logNothingLogger MkTimeout {microseconds = 0} demo

bidiTest :: Config -> Text -> [BiDiDemo] -> TestTree
bidiTest cfg title =
  testGroup (unpack title) . fmap fromBidiDemo
  where
    fromBidiDemo demo = testCase (unpack demo.name) $ runDemo' cfg demo

bidiDemos :: Config -> TestTree
bidiDemos cfg =
  let run = bidiTest cfg
   in testGroup
        "BiDi Demos"
        [ testGroup
            "BiDi Commands"
            [ testGroup
                "BiDi Exception tests - threads rigged to explode"
                [ testCase "send exception" $ Other.sendFailDemo cfg,
                  testCase "get exception" $ Other.getFailDemo cfg,
                  testCase "event fail exception" $ Other.eventFailDemo cfg
                ],
              run
                "Browser"
                [ Browser.browserGetClientWindowsDemo,
                  Browser.browserCreateUserContextDemo,
                  Browser.browserGetUserContextsDemo,
                  unknownCommandError
                    Browser.browserSetClientWindowStateDemo,
                  Browser.browserRemoveUserContextDemo,
                  Browser.browserCompleteWorkflowDemo,
                  biDiError
                    "Closing the browser in a session started with WebDriver classic is not supported"
                    Browser.browserCloseDemo,
                  unknownCommandError
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20250918/#command-browser-seProtocolExceptiontDownloadBehavior
                    Browser.browserSetDownloadBehaviorDemo
                ],
              run
                "Browsing Context"
                [ BrowsingContext.browsingContextCreateActivateCloseDemo,
                  BrowsingContext.browsingContextCaptureScreenshotCloseDemo,
                  BrowsingContext.browsingContextClosePromptUnloadDemo,
                  BrowsingContext.browsingContextGetTreeDemo,
                  BrowsingContext.browsingContextHandleUserPromptDemo,
                  BrowsingContext.browsingNavigateReloadTraverseHistoryDemo,
                  BrowsingContext.browsingContextLocateNodesDemo,
                  BrowsingContext.browsingContextPrintDemo,
                  BrowsingContext.browsingContextSetViewportDemo,
                  BiDiError.errorDemo
                  -- TODO: WHEN NEW DRIVERS ADDED make conditional - hangs in firefox
                  -- , BrowsingContext.browsingContextSetViewportResetDemo
                ],
              run
                "Emulation"
                [ unknownCommandError
                    -- since https:\/\/www.w3.org\/TR\/2025\/WD-webdriver-bidi-20250729
                    Emulation.emulationSetForcedColorsModeThemeOverrideDemo,
                  Emulation.emulationSetGeolocationOverrideDemo,
                  Emulation.emulationSetLocaleOverrideDemo,
                  unknownCommandError
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007
                    Emulation.emulationSetNetworkConditionsDemo,
                  Emulation.emulationSetScreenOrientationOverrideDemo,
                  unknownCommandError
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120
                    Emulation.emulationSetScreenSettingsOverrideDemo,
                  unknownCommandError
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811
                    Emulation.emulationSetScriptingEnabledDemo,
                  Emulation.emulationSetTimezoneOverrideDemo,
                  Emulation.emulationSetUserAgentOverrideDemo,
                  Emulation.emulationCompleteWorkflowDemo
                ],
              run
                "Fallback"
                [ Fallback.fallbackExtendCommandDemo,
                  Fallback.fallbackOffSpecCommandDemo,
                  Fallback.fallbackCommandCoercionsDemo,
                  Fallback.fallbackSubscribeUnknownEventDemo,
                  Fallback.fallbackSubscribeUnknownEventFilteredDemo
                ],
              run
                "Input"
                [ Input.inputKeyboardDemo,
                  Input.inputPointerDemo,
                  Input.inputWheelDemo,
                  Input.inputCombinedActionsDemo,
                  Input.inputReleaseActionsDemo,
                  Input.inputSetFilesDemo
                ],
              run
                "Network"
                [ biDiError
                    "The arguments passed to a command are either invalid or malformed"
                    Network.networkDataCollectorDemo,
                  Network.networkInterceptDemo,
                  Network.networkRequestModificationDemo,
                  Network.networkResponseModificationDemo,
                  Network.networkAuthCancelDemo,
                  Network.networkAuthWithCredentialsDemo,
                  Network.networkFailRequestDemo,
                  Network.networkProvideResponseJSONDemo,
                  Network.networkProvideResponseHTMLDemo,
                  Network.networkProvideResponseWithCookiesDemo,
                  Network.networkProvideResponseBase64Demo,
                  Network.networkProvideResponseErrorDemo,
                  Network.networkDataRetrievalDemo,
                  Network.networkDisownDataDemo,
                  Network.networkCacheBehaviorDemo,
                  -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251106
                  Network.networkSetExtraHeadersDemo
                ],
              run
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
              run
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
              run
                "Storage"
                [ Storage.storageGetCookiesDemo,
                  Storage.storageSetCookieDemo,
                  Storage.storageDeleteCookiesDemo,
                  Storage.storagePartitionKeyDemo,
                  Storage.storageCompleteWorkflowDemo
                ],
              run
                "WebExtension"
                [ WebExtension.webExtensionInstallPathDemo,
                  WebExtension.webExtensionInstallArchiveDemo,
                  WebExtension.webExtensionInstallBase64Demo,
                  WebExtension.webExtensionValidationDemo
                ]
            ],
          testGroup
            "BiDi Events"
            [ run
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
                  -- not supporrted in geckodriver yet
                  biDiError
                    "browsingContext.navigationAborted is not a valid event name"
                    BrowsingContextEvent.browsingContextEventNavigationAborted,
                  biDiError
                    "Error: NS_ERROR_UNKNOWN_HOST"
                    BrowsingContextEvent.browsingContextEventNavigationFailed,
                  BrowsingContextEvent.browsingContextEventDownloadWillBegin,
                  BrowsingContextEvent.browsingContextEventDownloadEnd
                ],
              run
                "Input Events"
                [ biDiError
                    "input.fileDialogOpened is not a valid event name"
                    InputEvent.inputEventFileDialogOpened
                ],
              run
                "Log Events"
                [ LogEvent.logEventConsoleEntries,
                  LogEvent.logEventConsoleLevelDebug,
                  LogEvent.logEventConsoleLevelInfo,
                  LogEvent.logEventConsoleLevelWarn,
                  LogEvent.logEventConsoleLevelError,
                  LogEvent.logEventJavascriptErrorFromButton
                ],
              run
                "Network Events"
                [ NetworkEvent.networkEventRequestResponseLifecycle,
                  NetworkEvent.networkEventFetchError,
                  NetworkEvent.networkEventAuthRequired
                ],
              run
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
      action = \utils bidi -> expectError name errorFragment (action utils bidi)
    }

unknownCommandError :: BiDiDemo -> BiDiDemo
unknownCommandError = biDiError "unknown command"