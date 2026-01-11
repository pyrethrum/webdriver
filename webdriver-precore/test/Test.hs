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
import Config (Config (..), DemoBrowser (..))
import ConfigLoader (loadConfig)
import Control.Exception (SomeException, catch, throw)
import Data.Text (Text, unpack)
import ErrorCoverageTest qualified as Error
import Network.WebSockets (ConnectionException (..))
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
  testCfg <- loadConfig
  -- defaultMain $ httpDemoSingleIsolated testCfg 
  -- defaultMain $ bidiSingleForDebug testCfg 
  defaultMain $ tests testCfg

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


-- Single isolated HTTP demo for CI debugging 
httpDemoSingleIsolated :: Config -> TestTree
httpDemoSingleIsolated cfg =
  testGroup
    "HTTP Demos"
    $ fromHttpDemo cfg
      <$> [ 
            Http.demoForwardBackRefresh
          ]

-- Single isolated Bidi demo for CI debugging 
bidiSingleForDebug :: Config -> TestTree
bidiSingleForDebug cfg =
  let run = bidiTest cfg
   in testGroup
            "BiDi Single Demo"
            [ 
              run
                "Browser"
                [ ScriptEvent.scriptEventMessage
                ]
        ]

httpDemos :: Config -> TestTree
httpDemos cfg =
  let thisBrowser = cfg.browser
      expectHttpFail = httpError thisBrowser
   in testGroup
        "HTTP Demos"
        $ fromHttpDemo cfg
          <$> [ Http.newSessionDemo,
                -- W3C spec requires status.ready=false when sessions exist. Chrome diverges from spec.
                expectHttpFail [Chrome'] "status.ready expected to be False"
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
      thisBrowser = cfg.browser
      browserType = fromBrowser thisBrowser
      unknownCommand = unknownCommandError thisBrowser
      expectFail = biDiError thisBrowser
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
                  unknownCommand [FireFox', Chrome']
                    Browser.browserSetClientWindowStateDemo,
                  Browser.browserRemoveUserContextDemo,
                  Browser.browserCompleteWorkflowDemo,
                  expectFail [FireFox']
                    "Closing the browser in a session started with WebDriver classic is not supported"
                    Browser.browserCloseDemo,
                  unknownCommand [FireFox']
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
                [ unknownCommand [FireFox', Chrome']
                    -- since https:\/\/www.w3.org\/TR\/2025\/WD-webdriver-bidi-20250729
                    Emulation.emulationSetForcedColorsModeThemeOverrideDemo,
                  Emulation.emulationSetGeolocationOverrideDemo,
                  Emulation.emulationSetLocaleOverrideDemo,
                  unknownCommand [FireFox']
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007
                    Emulation.emulationSetNetworkConditionsDemo,
                  Emulation.emulationSetScreenOrientationOverrideDemo,
                  unknownCommand [FireFox']
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120
                    Emulation.emulationSetScreenSettingsOverrideDemo,
                  unknownCommand [FireFox', Chrome']
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
                [ 
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
                  expectFail [FireFox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "Maximum number of active sessions"
                      Chrome{} -> "session already exists"
                      )
                    Session.sessionNewDemo,
                    -- todo: - calling `session.end` on the BiDi runner can throw `ConnectionClosed` when 
                    -- the server closes the WebSocket after session termination - needs orchestration fix in bidi runner when sesssion is closed
                  expectFail [FireFox']
                    "Ending a session started with WebDriver classic is not supported"
                    (catchConnectionClosed browserType Session.sessionEndDemo),
                  Session.sessionSubscribeDemo,
                  Session.sessionUnsubscribeDemo,
                  expectFail [FireFox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "Maximum number of active sessions"
                      Chrome{} -> "session already exists"
                      )
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
                  expectFail [FireFox']
                    "Expected event did not fire: BrowsingContextHistoryUpdated"
                    BrowsingContextEvent.browsingContextEventHistoryUpdated,
                  -- not supporrted in geckodriver yet
                  expectFail [FireFox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "browsingContext.navigationAborted is not a valid event name"
                      Chrome{} -> "ERR_NAME_NOT_RESOLVED"
                      )
                    BrowsingContextEvent.browsingContextEventNavigationAborted,
                  expectFail [FireFox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "Error: NS_ERROR_UNKNOWN_HOST"
                      Chrome{} -> "Error: NS_ERROR_UNKNOWN_HOST"
                      )
                    BrowsingContextEvent.browsingContextEventNavigationFailed,
                  BrowsingContextEvent.browsingContextEventDownloadWillBegin,
                  BrowsingContextEvent.browsingContextEventDownloadEnd
                ],
              run
                "Input Events"
                [ expectFail [FireFox', Chrome']
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

expectFailure :: DemoBrowser -> [BrowserType] -> Bool
expectFailure actualBrowser failBrowsers = 
  fromBrowser actualBrowser `elem` failBrowsers

biDiError :: DemoBrowser -> [BrowserType] -> Text -> BiDiDemo -> BiDiDemo
biDiError actualBrowser failBrowsers errorFragment demo@MkBiDiDemo {name, action} =
  if expectFailure actualBrowser failBrowsers then
  MkBiDiDemo
    { name = name <> " - EXPECTED ERROR: " <> errorFragment,
      action = \utils bidi -> expectError name errorFragment (action utils bidi)
    }
  else demo

data BrowserType = FireFox' | Chrome' deriving (Eq, Show)

fromBrowser :: DemoBrowser -> BrowserType
fromBrowser = \case 
  Firefox {} -> FireFox'
  Chrome {} -> Chrome'

unknownCommandError :: DemoBrowser -> [BrowserType] -> BiDiDemo -> BiDiDemo
unknownCommandError actualBrowser failBrowsers  demo = 
  biDiError actualBrowser failBrowsers failFragement demo
  where
    failFragement = case actualBrowser of
      Chrome {} -> "not implemented"
      Firefox {} -> "unknown command"

httpError :: DemoBrowser -> [BrowserType] -> Text -> HttpDemo -> HttpDemo
httpError actualBrowser failBrowsers errorFragment demo =
  if expectFailure actualBrowser failBrowsers then
    case demo of
      Demo {name, action} ->
        Demo
          { name = name <> " - EXPECTED ERROR: " <> errorFragment,
            action = \demoActions httpActions ->
              catch
                ( do
                    action demoActions httpActions
                    error $ "Expected test to fail with error containing: " <> unpack errorFragment
                )
                (\(_ :: SomeException) -> pure ())
          }
      SessionDemo {name, sessionAction} ->
        SessionDemo
          { name = name <> " - EXPECTED ERROR: " <> errorFragment,
            sessionAction = \session demoActions httpActions ->
              catch
                ( do
                    sessionAction session demoActions httpActions
                    error $ "Expected test to fail with error containing: " <> unpack errorFragment
                )
                (\(_ :: SomeException) -> pure ())
          }
  else demo

catchConnectionClosed :: BrowserType-> BiDiDemo -> BiDiDemo
catchConnectionClosed browserType  demo@MkBiDiDemo {name, action} =
  if browserType == Chrome' then
    MkBiDiDemo
      { name = name <> " - catches ConnectionClosed",
        action = \utils bidi -> do 
           action utils bidi 
           error "Expected ConnectionClosed exception but none thrown - has bidirunner been fixed?"
        `catch` \case 
          ConnectionClosed -> pure ()
          e -> throw e
      }
  else demo

