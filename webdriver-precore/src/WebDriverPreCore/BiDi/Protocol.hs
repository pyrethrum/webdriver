module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Object,
  )
import WebDriverPreCore.BiDi.Browser
import WebDriverPreCore.BiDi.BrowsingContext
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command 
import WebDriverPreCore.BiDi.Emulation
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Network qualified as Network
import WebDriverPreCore.BiDi.Script qualified as Script
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.BiDi.Storage qualified as Storage
import WebDriverPreCore.BiDi.WebExtensions qualified as WebExtensions



---- Session ----

sessionNew :: Capabilities -> Command Capabilities SessionNewResult
sessionNew = mkCommand "session.new"

sessionStatus :: Command Object SessionStatusResult
sessionStatus = emptyCommand "session.status"

sessionEnd :: Command Object Object
sessionEnd = emptyCommand "session.end"

sessionSubScribe :: SessionSubscriptionRequest -> Command SessionSubscriptionRequest SessionSubscribeResult
sessionSubScribe = mkCommand "session.subscribe"

sessionUnsubscribe :: SessionUnsubscribeParameters -> Command SessionUnsubscribeParameters Object
sessionUnsubscribe = mkCommand "session.unsubscribe"

---- Browsing Context ----

browsingContextActivate :: Activate -> Command Activate Object
browsingContextActivate = mkCommand "browsingContext.activate"

browsingContextCaptureScreenshot :: CaptureScreenshot -> Command CaptureScreenshot CaptureScreenshotResult
browsingContextCaptureScreenshot = mkCommand "browsingContext.captureScreenshot"

browsingContextClose :: Close -> Command Close Object
browsingContextClose = mkCommand "browsingContext.close"

browsingContextCreate :: Create -> Command Create CreateResult
browsingContextCreate = mkCommand "browsingContext.create"

browsingContextGetTree :: GetTree -> Command GetTree GetTreeResult
browsingContextGetTree = mkCommand "browsingContext.getTree"

browsingContextHandleUserPrompt :: HandleUserPrompt -> Command HandleUserPrompt Object
browsingContextHandleUserPrompt = mkCommand "browsingContext.handleUserPrompt"

browsingContextLocateNodes :: LocateNodes -> Command LocateNodes LocateNodesResult
browsingContextLocateNodes = mkCommand "browsingContext.locateNodes"

browsingContextNavigate :: Navigate -> Command Navigate NavigateResult
browsingContextNavigate = mkCommand "browsingContext.navigate"

browsingContextPrint :: Print -> Command Print PrintResult
browsingContextPrint = mkCommand "browsingContext.print"

browsingContextReload :: Reload -> Command Reload Object
browsingContextReload = mkCommand "browsingContext.reload"

browsingContextSetViewport :: SetViewport -> Command SetViewport Object
browsingContextSetViewport = mkCommand "browsingContext.setViewport"

browsingContextTraverseHistory :: TraverseHistory -> Command TraverseHistory TraverseHistoryResult
browsingContextTraverseHistory = mkCommand "browsingContext.traverseHistory"

---- Browser ----

browserClose :: Command Object Object
browserClose = emptyCommand "browser.close"

browserCreateUserContext :: CreateUserContext -> Command CreateUserContext UserContextInfo
browserCreateUserContext = mkCommand "browser.createUserContext"

browserGetClientWindows :: Command Object GetClientWindowsResult
browserGetClientWindows = emptyCommand "browser.getClientWindows"

browserGetUserContexts :: Command Object GetUserContextsResult
browserGetUserContexts = emptyCommand "browser.getUserContexts"

browserRemoveUserContext :: RemoveUserContext -> Command RemoveUserContext Object
browserRemoveUserContext = mkCommand "browser.removeUserContext"

browserSetClientWindowState :: SetClientWindowState -> Command SetClientWindowState ClientWindowInfo
browserSetClientWindowState = mkCommand "browser.setClientWindowState"

---- Emulation ----

emulationSetGeolocationOverride :: EmulationCommand -> Command EmulationCommand Object
emulationSetGeolocationOverride params = mkCommand "emulation.setGeolocationOverride" params

emulationSetLocaleOverride :: EmulationCommand -> Command EmulationCommand Object
emulationSetLocaleOverride params = mkCommand "emulation.setLocaleOverride" params

emulationSetScreenOrientationOverride :: EmulationCommand -> Command EmulationCommand Object
emulationSetScreenOrientationOverride params = mkCommand "emulation.setScreenOrientationOverride" params

emulationSetTimezoneOverride :: EmulationCommand -> Command EmulationCommand Object
emulationSetTimezoneOverride params = mkCommand "emulation.setTimezoneOverride" params

---- Input ----

inputPerformActions :: PerformActions -> Command PerformActions Object
inputPerformActions = mkCommand "input.performActions"

inputReleaseActions :: ReleaseActions -> Command ReleaseActions Object
inputReleaseActions = mkCommand "input.releaseActions"

inputSetFiles :: SetFiles -> Command SetFiles Object
inputSetFiles = mkCommand "input.setFiles"

---- Network ----

-- TODO: AddDataCollector type not exported from Network module
-- networkAddDataCollector :: Network.AddDataCollector -> Command Network.AddDataCollector Network.AddDataCollectorResult

networkAddIntercept :: Network.AddIntercept -> Command Network.AddIntercept Network.AddInterceptResult
networkAddIntercept = mkCommand "network.addIntercept"

networkContinueRequest :: Network.ContinueRequest -> Command Network.ContinueRequest Object
networkContinueRequest = mkCommand "network.continueRequest"

networkContinueResponse :: Network.ContinueResponse -> Command Network.ContinueResponse Object
networkContinueResponse = mkCommand "network.continueResponse"

networkContinueWithAuth :: Network.ContinueWithAuth -> Command Network.ContinueWithAuth Object
networkContinueWithAuth = mkCommand "network.continueWithAuth"

-- TODO: DisownData type not exported from Network module
-- networkDisownData :: Network.DisownData -> Command Network.DisownData Object

networkFailRequest :: Network.FailRequest -> Command Network.FailRequest Object
networkFailRequest = mkCommand "network.failRequest"

-- TODO: GetData type not exported from Network module
-- networkGetData :: Network.GetData -> Command Network.GetData Network.GetDataResult

networkProvideResponse :: Network.ProvideResponse -> Command Network.ProvideResponse Object
networkProvideResponse = mkCommand "network.provideResponse"

-- TODO: RemoveDataCollector type not exported from Network module
-- networkRemoveDataCollector :: Network.RemoveDataCollector -> Command Network.RemoveDataCollector Object

networkRemoveIntercept :: Network.RemoveIntercept -> Command Network.RemoveIntercept Object
networkRemoveIntercept = mkCommand "network.removeIntercept"

networkSetCacheBehavior :: Network.SetCacheBehavior -> Command Network.SetCacheBehavior Object
networkSetCacheBehavior = mkCommand "network.setCacheBehavior"

---- Script ----

scriptAddPreloadScript :: Script.AddPreloadScript -> Command Script.AddPreloadScript Script.AddPreloadScriptResult
scriptAddPreloadScript = mkCommand "script.addPreloadScript"

scriptCallFunction :: Script.CallFunction -> Command Script.CallFunction Script.CallFunctionResult
scriptCallFunction = mkCommand "script.callFunction"

scriptDisown :: Script.Disown -> Command Script.Disown Object
scriptDisown = mkCommand "script.disown"

scriptEvaluate :: Script.Evaluate -> Command Script.Evaluate Script.EvaluateResult
scriptEvaluate = mkCommand "script.evaluate"

scriptGetRealms :: Script.GetRealms -> Command Script.GetRealms Script.GetRealmsResult
scriptGetRealms = mkCommand "script.getRealms"

scriptRemovePreloadScript :: Script.RemovePreloadScript -> Command Script.RemovePreloadScript Object
scriptRemovePreloadScript = mkCommand "script.removePreloadScript"

---- Storage ----

storageDeleteCookies :: Storage.DeleteCookies -> Command Storage.DeleteCookies Storage.DeleteCookiesResult
storageDeleteCookies = mkCommand "storage.deleteCookies"

storageGetCookies :: Storage.GetCookies -> Command Storage.GetCookies Storage.GetCookiesResult
storageGetCookies = mkCommand "storage.getCookies"

storageSetCookie :: Storage.SetCookie -> Command Storage.SetCookie Storage.SetCookieResult
storageSetCookie = mkCommand "storage.setCookie"

---- WebExtension ----

webExtensionInstall :: WebExtensions.WebExtensionData -> Command WebExtensions.WebExtensionData WebExtensions.WebExtensionResult
webExtensionInstall = mkCommand "webExtension.install"

webExtensionUninstall :: WebExtensions.WebExtension -> Command WebExtensions.WebExtension Object
webExtensionUninstall = mkCommand "webExtension.uninstall"

{-
ANALYSIS: Command and Result Type Differences from WebDriver BiDi Specification

Completed command implementations following the existing pattern:
✓ Session: sessionNew, sessionStatus, sessionEnd, sessionSubScribe, sessionUnsubscribe
✓ BrowsingContext: All 12 commands implemented
✓ Browser: All 6 commands implemented  
✓ Emulation: All 4 commands implemented (setGeolocationOverride, setLocaleOverride, setScreenOrientationOverride, setTimezoneOverride)
✓ Input: All 3 commands implemented (performActions, releaseActions, setFiles)
✓ Script: All 6 commands implemented (addPreloadScript, callFunction, disown, evaluate, getRealms, removePreloadScript)
✓ Storage: All 3 commands implemented (deleteCookies, getCookies, setCookie)
✓ WebExtension: All 2 commands implemented (install, uninstall)

Network module issues:
⚠ Some Network command parameter types are not exported from the Network module:
  - AddDataCollector, DisownData, GetData, RemoveDataCollector
  These commands are commented out with TODO markers
✓ Implemented: addIntercept, continueRequest, continueResponse, continueWithAuth, failRequest, provideResponse, removeIntercept, setCacheBehavior

Differences from spec (beyond Haskell naming conventions):

1. Browser Commands:
   - Spec: browser.setClientWindowState returns browser.ClientWindowInfo
   - Haskell: browserSetClientWindowState returns ClientWindowInfo ✓ Correct

2. Emulation Commands:
   - All emulation commands in spec return EmptyResult
   - Haskell: All return Object ✓ Correct (Object represents EmptyResult)

3. Network Commands:
   - Missing exports prevent full implementation of some commands
   - Need to export AddDataCollector, DisownData, GetData, RemoveDataCollector types from Network module

4. Storage Commands:
   - All storage commands correctly return their respective result types
   - deleteCookies -> DeleteCookiesResult ✓
   - getCookies -> GetCookiesResult ✓ 
   - setCookie -> SetCookieResult ✓

5. Script Commands:
   - callFunction returns CallFunctionResult ✓
   - evaluate returns EvaluateResult ✓
   - addPreloadScript returns AddPreloadScriptResult ✓
   - getRealms returns GetRealmsResult ✓

6. WebExtension Commands:
   - install returns WebExtensionResult ✓
   - uninstall returns EmptyResult (Object) ✓

7. Command Parameter Usage:
   - Commands with parameters use mkCommand ✓
   - Commands without parameters use emptyCommand ✓
   - Empty command types correctly use Object ✓

All implemented commands follow the existing Protocol.hs pattern correctly.
The main outstanding issue is the Network module export list needs updating
to expose the missing parameter types for full Network command coverage.
-}


