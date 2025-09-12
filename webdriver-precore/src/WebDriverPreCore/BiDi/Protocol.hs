module WebDriverPreCore.BiDi.Protocol
  (
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

import WebDriverPreCore.BiDi.Browser
  ( ClientWindowInfo (..),
    CreateUserContext (..),
    GetClientWindowsResult (..),
    GetUserContextsResult (..),
    RemoveUserContext (..),
    SetClientWindowState (..),
  )
import WebDriverPreCore.BiDi.BrowsingContext
  ( Activate (..),
    CaptureScreenshot (..),
    CaptureScreenshotResult (..),
    ClipRectangle (..),
    Close (..),
    Create (..),
    CreateType (..),
    GetTree (..),
    GetTreeResult (..),
    HandleUserPrompt (..),
    ImageFormat (..),
    Info (..),
    LocateNodes (..),
    LocateNodesResult (..),
    Navigate (..),
    NavigateResult (..),
    Orientation (..),
    PageRange (..),
    Print (..),
    PrintResult (..),
    ReadinessState (..),
    Reload (..),
    ScreenShotOrigin (..),
    SetViewport (..),
    TraverseHistory (..),
    TraverseHistoryResult (..),
  )
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command
  ( Command,
    emptyCommand,
    mkCommand,
  )
import WebDriverPreCore.BiDi.CoreTypes as BrowsingContext (BrowsingContext (..), UserContext)
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
  ( AddPreloadScript (..),
    AddPreloadScriptResult (..),
    CallFunction (..),
    ContextTarget (..),
    Disown (..),
    Evaluate (..),
    EvaluateResult (..),
    GetRealms (..),
    GetRealmsResult (..),
    ChannelValue (..),
    ChannelProperties (..),
    Channel (..),
    LocalValue (..),
    MappingLocalValue (..),
    MapLocalValue (..),
    ObjectLocalValue (..),
    PrimitiveProtocolValue (..),
    Realm (..),
    RemoteValue (..),
    RemovePreloadScript (..),
    ResultOwnership (..),
    Sandbox (..),
    SerializationOptions (..),
    IncludeShadowTree (..),
    SharedId (..),
    SharedReference (..),
    SpecialNumber (..),
    Target (..),
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
