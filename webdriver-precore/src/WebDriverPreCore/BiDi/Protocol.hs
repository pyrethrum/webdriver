module WebDriverPreCore.BiDi.Protocol
  ( -- * Re-exported modules
    module BrowsingContext,
    module WebDriverPreCore.BiDi.Browser,
    module WebDriverPreCore.BiDi.BrowsingContext,
    module WebDriverPreCore.BiDi.Capabilities,
    module WebDriverPreCore.BiDi.Command,
    module WebDriverPreCore.BiDi.Event,
    module WebDriverPreCore.BiDi.Emulation,
    module WebDriverPreCore.BiDi.Input,
    module WebDriverPreCore.BiDi.Script,
    module WebDriverPreCore.BiDi.Session,
    module WebDriverPreCore.BiDi.Storage,
    module WebDriverPreCore.BiDi.WebExtensions,

    -- * Network types (exported directly to avoid conflicts)
    AddDataCollector (..),
    AddDataCollectorResult (..),
    AddIntercept (..),
    AddInterceptResult (..),
    ContinueRequest (..),
    ContinueResponse (..),
    ContinueWithAuth (..),
    DisownData (..),
    FailRequest (..),
    GetData (..),
    GetDataResult (..),
    ProvideResponse (..),
    RemoveDataCollector (..),
    RemoveIntercept (..),
    SetCacheBehavior (..),
    UrlPattern (..),
    UrlPatternPattern (..),
    UrlPatternString (..),
    InterceptPhase (..),
    Intercept (..),
    RequestId (..),
    Request (..),
    DataType (..),
    CollectorType (..),
    Collector (..),
    AuthAction (..),
    AuthCredentials (..),
    CacheBehavior (..),
    BytesValue (..),
    Cookie (..),
    SameSite (..), 
    Header (..),
    CookieHeader (..),
    SetCookieHeader (..),
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
import WebDriverPreCore.BiDi.Capabilities (
  Capabilities(..), 
  Capability(..), 
  ProxyConfiguration(..))
import WebDriverPreCore.BiDi.Command
  ( Command (..),
    emptyCommand,
    mkCommand,
  )
import WebDriverPreCore.BiDi.CoreTypes as BrowsingContext
  ( BrowsingContext (..),
    SharedId (..),
    UserContext (..),
  )
import WebDriverPreCore.BiDi.Emulation
  ( GeolocationCoordinates (..),
    GeolocationPositionError (..),
    ScreenOrientationNatural (..),
    ScreenOrientationType (..),
    ScreenOrientationOverride (..),
    SetGeolocationOverride (..),
    SetLocaleOverride (..),
    SetScreenOrientationOverride (..),
    SetTimezoneOverride (..),
  )

import WebDriverPreCore.BiDi.Event
  ( Event (..),
    SubscriptionType (..)
  
  )

import WebDriverPreCore.BiDi.Input
  ( PerformActions (..),
    ReleaseActions (..),
    SetFiles (..),
  )
import WebDriverPreCore.BiDi.Network
  ( AddDataCollector (..),
    AddDataCollectorResult (..),
    AddIntercept (..),
    AddInterceptResult (..),
    -- URL Pattern types

    -- Intercept types

    -- Request/Response types

    -- Data types

    -- Auth types
    AuthAction (..),
    AuthCredentials (..),
    -- Cache types

    -- Value types
    BytesValue (..),
    CacheBehavior (..),
    Collector (..),
    CollectorType (..),
    ContinueRequest (..),
    ContinueResponse (..),
    ContinueWithAuth (..),
    Cookie (..),
    -- Explicitly exclude None to avoid conflict with BrowsingContext.None

    CookieHeader (..),
    DataType (..),
    DisownData (..),
    FailRequest (..),
    GetData (..),
    GetDataResult (..),
    Header (..),
    Intercept (..),
    InterceptPhase (..),
    ProvideResponse (..),
    RemoveDataCollector (..),
    RemoveIntercept (..),
    Request (..),
    RequestId (..),
    SameSite (..),
    SetCacheBehavior (..),
    SetCookieHeader (..),
    UrlPattern (..),
    UrlPatternPattern (..),
    UrlPatternString (..),
  )
import WebDriverPreCore.BiDi.Script
  ( AddPreloadScript (..),
    AddPreloadScriptResult (..),
    CallFunction (..),
    Channel (..),
    ChannelProperties (..),
    ChannelValue (..),
    ContextTarget (..),
    Disown (..),
    Evaluate (..),
    EvaluateResult (..),
    GetRealms (..),
    GetRealmsResult (..),
    IncludeShadowTree (..),
    LocalValue (..),
    MapLocalValue (..),
    MappingLocalValue (..),
    ObjectLocalValue (..),
    PrimitiveProtocolValue (..),
    Realm (..),
    RemoteValue (..),
    RemovePreloadScript (..),
    ResultOwnership (..),
    Sandbox (..),
    SerializationOptions (..),
    SharedReference (..),
    SpecialNumber (..),
    Target (..),
  )
import WebDriverPreCore.BiDi.Session
  ( SessionNewResult (..),
    SessionStatusResult (..),
    SessionSubscribeResult (..),
    SessionSubscriptionRequest (..),
    SessionUnsubscribe (..),
    SubscriptionId (..)
  )
import WebDriverPreCore.BiDi.Storage
  ( DeleteCookies (..),
    DeleteCookiesResult (..),
    GetCookies (..),
    GetCookiesResult (..),
    SetCookie (..),
    SetCookieResult (..),
  )
import WebDriverPreCore.BiDi.WebExtensions
  ( WebExtensionID (..),
    WebExtensionInstall (..),
    WebExtensionUninstall (..),
    WebExtensionResult (..),
  )
