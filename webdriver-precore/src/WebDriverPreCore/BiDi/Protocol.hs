module WebDriverPreCore.BiDi.Protocol
  ( -- * Re-exported modules
    module CoreTypes,
    module WebDriverPreCore.BiDi.Browser,
    module WebDriverPreCore.BiDi.BrowsingContext,
    module WebDriverPreCore.BiDi.Capabilities,
    module WebDriverPreCore.BiDi.Command,
    module WebDriverPreCore.BiDi.Event,
    module WebDriverPreCore.BiDi.Emulation,
    module WebDriverPreCore.BiDi.Input,
    module WebDriverPreCore.BiDi.Log,
    module WebDriverPreCore.BiDi.Response,
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
    SetExtraHeaders (..),
    UrlPattern (..),
    UrlPatternPattern (..),
    UrlPatternString (..),
    InterceptPhase (..),
    Intercept (..),
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
    DownloadBehaviour (..),
    GetClientWindowsResult (..),
    GetUserContextsResult (..),
    RemoveUserContext (..),
    SetClientWindowState (..),
    SetDownloadBehavior (..),
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
    Locator (..),
    Navigate (..),
    NavigateResult (..),
    Orientation (..),
    PageRange (..),
    Print (..),
    PrintMargin (..),
    PrintPage (..),
    PrintResult (..),
    ReadinessState (..),
    Reload (..),
    ScreenShotOrigin (..),
    SetViewport (..),
    TraverseHistory (..),
    TraverseHistoryResult (..),
    Viewport (..)
  )
import WebDriverPreCore.BiDi.Capabilities
  ( Capabilities (..),
    Capability (..),
    ProxyConfiguration (..),
  )
import WebDriverPreCore.BiDi.Command
  ( Command (..),
  )
import WebDriverPreCore.BiDi.CoreTypes as CoreTypes
import WebDriverPreCore.BiDi.Emulation
  ( ForcedColorsModeTheme (..),
    GeolocationCoordinates (..),
    GeolocationPositionError (..),
    NetworkConditions (..),
    NetworkConditionsOffline (..),
    ScreenArea (..),
    ScreenOrientationNatural (..),
    ScreenOrientationOverride (..),
    ScreenOrientationType (..),
    SetForcedColorsModeThemeOverride (..),
    SetGeolocationOverride (..),
    SetLocaleOverride (..),
    SetNetworkConditions (..),
    SetScreenOrientationOverride (..),
    SetScreenSettingsOverride (..),
    SetScriptingEnabled (..),
    SetTimezoneOverride (..),
    SetUserAgentOverride (..),
  )
import WebDriverPreCore.BiDi.Event
  ( Event (..),
    Subscription (..),
  )
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Log
  ( BaseLogEntry (..),
    ConsoleLogEntry (..),
    GenericLogEntry (..),
    Level (..),
    LogEntry (..),
    LogEvent (..),
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
    SameSite (..),
    SetCacheBehavior (..),
    SetCookieHeader (..),
    SetExtraHeaders (..),
    UrlPattern (..),
    UrlPatternPattern (..),
    UrlPatternString (..),
  )
import WebDriverPreCore.BiDi.Response
  ( JSONDecodeError (..),
    MatchedResponse (..),
    ResponseObject (..),
    displayResponseError,
    parseResponse,
  )
import WebDriverPreCore.BiDi.Script
import WebDriverPreCore.BiDi.Session
  ( SessionNewResult (..),
    SessionStatusResult (..),
    SessionSubscibe (..),
    SessionSubscribeResult (..),
    SessionUnsubscribe (..),
    SubscriptionId (..),
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
    WebExtensionResult (..),
    WebExtensionUninstall (..),
  )
