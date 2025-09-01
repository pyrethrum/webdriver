module WebDriverPreCore.BiDi.Protocol
  ( -- * Session Commands
    sessionNew,
    sessionStatus,
    sessionEnd,
    sessionSubScribe,
    sessionUnsubscribe,

    -- * BrowsingContext Commands
    browsingContextActivate,
    browsingContextCaptureScreenshot,
    browsingContextClose,
    browsingContextCreate,
    browsingContextGetTree,
    browsingContextHandleUserPrompt,
    browsingContextLocateNodes,
    browsingContextNavigate,
    browsingContextPrint,
    browsingContextReload,
    browsingContextSetViewport,
    browsingContextTraverseHistory,

    -- * Browser Commands
    browserClose,
    browserCreateUserContext,
    browserGetClientWindows,
    browserGetUserContexts,
    browserRemoveUserContext,
    browserSetClientWindowState,

    -- * Emulation Commands
    emulationSetGeolocationOverride,
    emulationSetLocaleOverride,
    emulationSetScreenOrientationOverride,
    emulationSetTimezoneOverride,

    -- * Input Commands
    inputPerformActions,
    inputReleaseActions,
    inputSetFiles,

    -- * Network Commands
    networkAddDataCollector,
    networkAddIntercept,
    networkContinueRequest,
    networkContinueResponse,
    networkContinueWithAuth,
    networkDisownData,
    networkFailRequest,
    networkGetData,
    networkProvideResponse,
    networkRemoveDataCollector,
    networkRemoveIntercept,
    networkSetCacheBehavior,

    -- * Script Commands
    scriptAddPreloadScript,
    scriptCallFunction,
    scriptDisown,
    scriptEvaluate,
    scriptGetRealms,
    scriptRemovePreloadScript,

    -- * Storage Commands
    storageDeleteCookies,
    storageGetCookies,
    storageSetCookie,

    -- * WebExtension Commands
    webExtensionInstall,
    webExtensionUninstall,

    -- * Re-exported modules
    module BrowsingContext,
    module WebDriverPreCore.BiDi.Browser,
    module WebDriverPreCore.BiDi.BrowsingContext,
    module WebDriverPreCore.BiDi.Capabilities,
    module WebDriverPreCore.BiDi.Command,
    module WebDriverPreCore.BiDi.Emulation,
    module WebDriverPreCore.BiDi.Input,
    module WebDriverPreCore.BiDi.Network,
    module WebDriverPreCore.BiDi.Script,
    module WebDriverPreCore.BiDi.Session,
    module WebDriverPreCore.BiDi.Storage,
    module WebDriverPreCore.BiDi.WebExtensions,
  )
where

import Data.Aeson
  ( Object,
  )
import WebDriverPreCore.BiDi.Browser
  ( ClientWindowInfo(..),
    CreateUserContext(..),
    GetClientWindowsResult,
    GetUserContextsResult,
    RemoveUserContext,
    SetClientWindowState, 
  )
import WebDriverPreCore.BiDi.BrowsingContext
  ( 
    Activate(..),
    CaptureScreenshot,
    CaptureScreenshotResult,
    Close,
    Create(..),
    CreateType (..),
    GetTree,
    GetTreeResult,
    HandleUserPrompt,
    LocateNodes,
    LocateNodesResult,
    Navigate,
    NavigateResult,
    Print,
    PrintResult,
    Reload,
    SetViewport,
    TraverseHistory,
    TraverseHistoryResult,
  )
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command
  ( Command,
    emptyCommand,
    mkCommand,
  )
import WebDriverPreCore.BiDi.Emulation
  ( SetGeolocationOverride,
    SetLocaleOverride,
    SetScreenOrientationOverride,
    SetTimezoneOverride,
  )
import WebDriverPreCore.BiDi.Input
  ( PerformActions,
    ReleaseActions,
    SetFiles,
  )
import WebDriverPreCore.BiDi.Network
  ( AddDataCollector,
    AddDataCollectorResult (..),
    AddIntercept,
    AddInterceptResult (..),
    ContinueRequest,
    ContinueResponse,
    ContinueWithAuth,
    DisownData,
    FailRequest,
    GetData,
    GetDataResult (..),
    ProvideResponse,
    RemoveDataCollector,
    RemoveIntercept,
    SetCacheBehavior,
  )
import WebDriverPreCore.BiDi.Script
  ( AddPreloadScript,
    AddPreloadScriptResult (..),
    CallFunction,
    CallFunctionResult,
    Disown,
    Evaluate,
    EvaluateResult,
    GetRealms,
    GetRealmsResult,
    RemovePreloadScript,
  )
import WebDriverPreCore.BiDi.Session
  ( SessionNewResult,
    SessionStatusResult,
    SessionSubscribeResult,
    SessionSubscriptionRequest,
    SessionUnsubscribeParameters,
  )
import WebDriverPreCore.BiDi.Storage
  ( DeleteCookies,
    DeleteCookiesResult (..),
    GetCookies,
    GetCookiesResult (..),
    SetCookie,
    SetCookieResult (..),
  )
import WebDriverPreCore.BiDi.WebExtensions
  ( WebExtension,
    WebExtensionData,
    WebExtensionResult (..),
  )
import WebDriverPreCore.BiDi.CoreTypes as BrowsingContext (BrowsingContext(..), UserContext)

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

browsingContextCreate :: Create -> Command Create BrowsingContext
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

browserCreateUserContext :: CreateUserContext -> Command CreateUserContext UserContext
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

emulationSetGeolocationOverride :: SetGeolocationOverride -> Command SetGeolocationOverride Object
emulationSetGeolocationOverride = mkCommand "emulation.setGeolocationOverride"

emulationSetLocaleOverride :: SetLocaleOverride -> Command SetLocaleOverride Object
emulationSetLocaleOverride = mkCommand "emulation.setLocaleOverride"

emulationSetScreenOrientationOverride :: SetScreenOrientationOverride -> Command SetScreenOrientationOverride Object
emulationSetScreenOrientationOverride = mkCommand "emulation.setScreenOrientationOverride"

emulationSetTimezoneOverride :: SetTimezoneOverride -> Command SetTimezoneOverride Object
emulationSetTimezoneOverride = mkCommand "emulation.setTimezoneOverride"

---- Input ----

inputPerformActions :: PerformActions -> Command PerformActions Object
inputPerformActions = mkCommand "input.performActions"

inputReleaseActions :: ReleaseActions -> Command ReleaseActions Object
inputReleaseActions = mkCommand "input.releaseActions"

inputSetFiles :: SetFiles -> Command SetFiles Object
inputSetFiles = mkCommand "input.setFiles"

---- Network ----

networkAddDataCollector :: AddDataCollector -> Command AddDataCollector AddDataCollectorResult
networkAddDataCollector = mkCommand "network.addDataCollector"

networkAddIntercept :: AddIntercept -> Command AddIntercept AddInterceptResult
networkAddIntercept = mkCommand "network.addIntercept"

networkContinueRequest :: ContinueRequest -> Command ContinueRequest Object
networkContinueRequest = mkCommand "network.continueRequest"

networkContinueResponse :: ContinueResponse -> Command ContinueResponse Object
networkContinueResponse = mkCommand "network.continueResponse"

networkContinueWithAuth :: ContinueWithAuth -> Command ContinueWithAuth Object
networkContinueWithAuth = mkCommand "network.continueWithAuth"

networkDisownData :: DisownData -> Command DisownData Object
networkDisownData = mkCommand "network.disownData"

networkFailRequest :: FailRequest -> Command FailRequest Object
networkFailRequest = mkCommand "network.failRequest"

networkGetData :: GetData -> Command GetData GetDataResult
networkGetData = mkCommand "network.getData"

networkProvideResponse :: ProvideResponse -> Command ProvideResponse Object
networkProvideResponse = mkCommand "network.provideResponse"

networkRemoveDataCollector :: RemoveDataCollector -> Command RemoveDataCollector Object
networkRemoveDataCollector = mkCommand "network.removeDataCollector"

networkRemoveIntercept :: RemoveIntercept -> Command RemoveIntercept Object
networkRemoveIntercept = mkCommand "network.removeIntercept"

networkSetCacheBehavior :: SetCacheBehavior -> Command SetCacheBehavior Object
networkSetCacheBehavior = mkCommand "network.setCacheBehavior"

---- Script ----

scriptAddPreloadScript :: AddPreloadScript -> Command AddPreloadScript AddPreloadScriptResult
scriptAddPreloadScript = mkCommand "script.addPreloadScript"

scriptCallFunction :: CallFunction -> Command CallFunction CallFunctionResult
scriptCallFunction = mkCommand "script.callFunction"

scriptDisown :: Disown -> Command Disown Object
scriptDisown = mkCommand "script.disown"

scriptEvaluate :: Evaluate -> Command Evaluate EvaluateResult
scriptEvaluate = mkCommand "script.evaluate"

scriptGetRealms :: GetRealms -> Command GetRealms GetRealmsResult
scriptGetRealms = mkCommand "script.getRealms"

scriptRemovePreloadScript :: RemovePreloadScript -> Command RemovePreloadScript Object
scriptRemovePreloadScript = mkCommand "script.removePreloadScript"

---- Storage ----

storageDeleteCookies :: DeleteCookies -> Command DeleteCookies DeleteCookiesResult
storageDeleteCookies = mkCommand "storage.deleteCookies"

storageGetCookies :: GetCookies -> Command GetCookies GetCookiesResult
storageGetCookies = mkCommand "storage.getCookies"

storageSetCookie :: SetCookie -> Command SetCookie SetCookieResult
storageSetCookie = mkCommand "storage.setCookie"

---- WebExtension ----

webExtensionInstall :: WebExtensionData -> Command WebExtensionData WebExtensionResult
webExtensionInstall = mkCommand "webExtension.install"

webExtensionUninstall :: WebExtension -> Command WebExtension Object
webExtensionUninstall = mkCommand "webExtension.uninstall"
