{-|
Test suite for webdriver-precore-bidi-runner library

This module contains BiDi demos migrated from the main test suite,
excluding HTTP tests and unit tests.
-}
module Main where

import BiDiDemoUtils (BiDiDemo (..), runDemoWithConfig, expectError, FailTest (..), toText)
import BrowserDemos qualified as Browser
import BrowsingContextDemos qualified as BrowsingContext
import BrowsingContextEventDemos qualified as BrowsingContextEvent
import EmulationDemos qualified as Emulation
import ErrorDemo qualified as BiDiError
import FallbackDemos qualified as Fallback
import InputDemos qualified as Input
import InputEventDemos qualified as InputEvent
import LogEventDemos qualified as LogEvent
import NetworkDemos qualified as Network
import NetworkEventDemos qualified as NetworkEvent
import ScriptDemos qualified as Script
import ScriptEventDemos qualified as ScriptEvent
import SessionDemos qualified as Session
import StorageDemos qualified as Storage
import WebExtensionDemos qualified as WebExtension
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Runners (NumThreads (..))
import WebDriverPreCore.Test.Config (Config (..), DemoBrowser (..))
import WebDriverPreCore.Test.ConfigLoader (loadConfig)

main :: IO ()
main = do
  testCfg <- loadConfig
  -- defaultMain $ localOption (NumThreads 1) $ tests testCfg
  defaultMain $ tests testCfg

tests :: Config -> TestTree
tests cfg =
  testGroup
    "BiDi Runner Tests"
    [ bidiDemos cfg
    ]

-- Single isolated Bidi demo for CI debugging 
bidiSingleForDebug :: Config -> TestTree
bidiSingleForDebug cfg =
  let 
    run = bidiTest cfg
    thisBrowser = cfg.browser
    expectFail bts txt' = biDiError thisBrowser bts (Fragment txt')
   in testGroup
            "BiDi Single Demo"
            [ 
              run
                "Emulation"
                [ 
                  expectFail [Firefox']
                    "Expected \\\\\\\"coordinates\\\\\\\" to be an object"
                    Emulation.emulationSetGeolocationOverridePositionErrorDemo
                ]
        ]

bidiTest :: Config -> Text -> [BiDiDemo] -> TestTree
bidiTest cfg title =
  testGroup (unpack title) . fmap fromBidiDemo
  where
    fromBidiDemo demo' = testCase (unpack demo'.name) $ runDemoWithConfig cfg demo'

bidiDemos :: Config -> TestTree
bidiDemos cfg =
  let run = bidiTest cfg
      thisBrowser = cfg.browser
      browserType = fromBrowser thisBrowser
      unknownCommand = unknownCommandError thisBrowser
      expectFail bts txt' = biDiError thisBrowser bts (Fragment txt')
   in testGroup
        "BiDi Demos"
        [ testGroup
            "BiDi Commands"
            [ -- BiDi Exception tests require runDemoFail' which is not yet implemented
              -- TODO: Uncomment when runDemoFail' is implemented in BiDiDemoUtils
              -- testGroup
              --   "BiDi Exception tests - threads rigged to explode"
              --   [ testCase "send exception" $ Other.sendFailDemo cfg,
              --     testCase "get exception" $ Other.getFailDemo cfg,
              --     testCase "event fail exception" $ Other.eventFailDemo cfg
              --   ],
              run
                "Browser"
                [ Browser.browserGetClientWindowsDemo,
                  Browser.browserCreateUserContextDemo,
                  Browser.browserGetUserContextsDemo,
                  unknownCommand [Firefox', Chrome']
                    Browser.browserSetClientWindowStateDemo,
                  Browser.browserRemoveUserContextDemo,
                  Browser.browserCompleteWorkflowDemo,
                  expectFail [Firefox']
                    "Closing the browser in a session started with WebDriver classic is not supported"
                    Browser.browserCloseDemo,
                  unknownCommand [Firefox']
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
                  BrowsingContext.browsingContextContextLocatorDemo,
                  BrowsingContext.browsingContextPrintDemo,
                  BrowsingContext.browsingContextSetViewportDemo,
                  BiDiError.errorDemo
                  -- TODO: WHEN NEW DRIVERS ADDED make conditional - hangs in firefox
                  -- , BrowsingContext.browsingContextSetViewportResetDemo
                ],
              run
                "Emulation"
                [ unknownCommand [Firefox', Chrome']
                    -- since https:\/\/www.w3.org\/TR\/2025\/WD-webdriver-bidi-20250729
                    Emulation.emulationSetForcedColorsModeThemeOverrideDemo,
                  Emulation.emulationSetGeolocationOverrideDemo,
                  -- Geckodriver bug: incorrectly requires 'coordinates' when 'error' is provided
                  -- Spec section 7.4.2.2 states that 'error' and 'coordinates' are mutually exclusive
                  expectFail [Firefox']
                    "Expected \\\\\\\"coordinates\\\\\\\" to be an object"
                    Emulation.emulationSetGeolocationOverridePositionErrorDemo,
                  Emulation.emulationSetLocaleOverrideDemo,
                  unknownCommand [Firefox']
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007
                    Emulation.emulationSetNetworkConditionsDemo,
                  Emulation.emulationSetScreenOrientationOverrideDemo,
                  unknownCommand [Chrome']
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120
                    Emulation.emulationSetScreenSettingsOverrideDemo,
                  unknownCommand [Firefox']
                    -- since https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811
                    Emulation.emulationSetScriptingEnabledDemo,
                  Emulation.emulationSetTimezoneOverrideDemo,
                  unknownCommand [Firefox', Chrome']
                    -- since https://www.w3.org/TR/2026/WD-webdriver-bidi-20260109
                    Emulation.emulationSetTouchOverrideDemo,
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
                  -- Chromedriver does not support setting non-string header values
                  expectFail [Chrome']
                    "Only string headers values are supported"
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
                  expectFail [Firefox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "Maximum number of active sessions"
                      Chrome{} -> "session already exists"
                      )
                    Session.sessionNewDemo,

                  Session.sessionSubscribeDemo,
                  Session.sessionUnsubscribeDemo,
                  expectFail [Firefox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "Maximum number of active sessions"
                      Chrome{} -> "session already exists"
                      )
                    Session.sessionCapabilityNegotiationDemo,
                  Session.sessionCompleteLifecycleDemo
                ],
                run
                "Session - firefox only" (
                  if browserType == Firefox' then 
                  [
                  -- todo: - calling `session.end` on the test BiDi runner throws `ConnectionClosed` when 
                  -- the server closes the WebSocket after session termination - needs orchestration 
                  -- fix in bidi runner when sesssion is closed
                  expectFail [Firefox']
                    "Ending a session started with WebDriver classic is not supported"
                    Session.sessionEndDemo
                ] else []),
              run
                "Storage"
                [ Storage.storageGetCookiesDemo,
                  -- ChromeDriver does not support storageKey partition type in storage.setCookie
                  expectFail [Chrome']
                    "unable to set cookie"
                  Storage.storageSetCookieDemo,
                  Storage.storageDeleteCookiesDemo,
                  Storage.storagePartitionKeyDemo,
                  Storage.storageCompleteWorkflowDemo
                ],
              run
                "WebExtension"
                [ -- ChromeDriver doesn't support BiDi WebExtension methods
                  expectFail [Chrome']
                    "Method not available"
                    WebExtension.webExtensionInstallPathDemo,
                  expectFail [Chrome']
                    "Archived and Base64 extensions are not supported"
                    WebExtension.webExtensionInstallArchiveDemo,
                  expectFail [Chrome']
                    "Archived and Base64 extensions are not supported"
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
                  expectFail [Firefox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "Expected event did not fire: BrowsingContextHistoryUpdated"
                      Chrome{} -> "Timeout"
                      )
                    BrowsingContextEvent.browsingContextEventHistoryUpdated,
                  -- not supporrted in geckodriver yet
                  expectFail [Firefox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "browsingContext.navigationAborted is not a valid event name"
                      Chrome{} -> "Expected event did not fire: BrowsingContextNavigationAborted"
                      )
                      BrowsingContextEvent.browsingContextEventNavigationAborted,
                  expectFail [Firefox', Chrome']
                    (case thisBrowser of 
                      Firefox{} -> "NS_ERROR_UNKNOWN_HOST"
                      Chrome{} -> "ERR_NAME_NOT_RESOLVED"
                      )
                    BrowsingContextEvent.browsingContextEventNavigationFailed,
                  BrowsingContextEvent.browsingContextEventDownloadWillBegin,
                  BrowsingContextEvent.browsingContextEventDownloadEnd
                ],
              run
                "Input Events"
                [],
              run
                "Input Events - File Dialog Opened" (
                   case thisBrowser of 
                     Chrome {} -> [InputEvent.inputEventFileDialogOpened]
                     Firefox {headless = False} -> [InputEvent.inputEventFileDialogOpened]
                     -- firfox throws error on file dialog open in headless mode
                     -- ConnectionClosed is not coming from main thread so not being caught TODO: reinstate this when runner fixed
                     Firefox {headless = True} ->  [] -- [expectFail [Firefox'] "ConnectionClosed" InputEvent.inputEventFileDialogOpened] 
                ),
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
                [ expectFail [Chrome']
                    "Timeout - Expected event did not fire: NetworkResponseStarted"
                    NetworkEvent.networkEventRequestResponseLifecycle,
                  NetworkEvent.networkEventFetchError,
                  expectFail [Chrome']
                    "Timeout - Expected event did not fire: NetworkAuthRequired"
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

biDiError :: DemoBrowser -> [BrowserType] -> FailTest -> BiDiDemo -> BiDiDemo
biDiError actualBrowser failBrowsers failTest demo'@MkBiDiDemo {name, action} =
  if expectFailure actualBrowser failBrowsers then
  MkBiDiDemo
    { name = name <> " - EXPECTED ERROR: " <> toText failTest,
      action = \utils bidi -> expectError name failTest (action utils bidi)
    }
  else demo'

data BrowserType = Firefox' | Chrome' deriving (Eq, Show)

fromBrowser :: DemoBrowser -> BrowserType
fromBrowser = \case 
  Firefox {} -> Firefox'
  Chrome {} -> Chrome'

unknownCommandError :: DemoBrowser -> [BrowserType] -> BiDiDemo -> BiDiDemo
unknownCommandError actualBrowser failBrowsers  demo' = 
  biDiError actualBrowser failBrowsers failTest demo'
  where
    failTest = Predicate \txt -> "not implemented" `T.isInfixOf` txt || "unknown command" `T.isInfixOf` txt
