

{-|
Module: WebDriverPreCore.Extended.Capabilities
Description: Universal capabilities abstraction for HTTP and BiDi protocols

Provides a unified capabilities interface that can be converted to protocol-specific
types. Uses sum types to represent HTTP and BiDi protocol variants.
-}
module WebDriverPreCore.Extended.Capabilities
  ( -- * Universal Capability Types
    CapabilitiesRequest (..),
    CapabilitiesResponse (..),
    FullCapabilitiesRequest (..),

    -- * Unified Property Types
    ProxyConfig (..),
    SocksProxyConfig (..),
    PromptBehavior (..),
    PromptAction (..),
    PageLoadStrategy (..),
    BrowserName (..),
    PlatformName (..),

    -- * Re-exports from HTTP (vendor-specific and HTTP-only types)
    HTTP.Timeouts (..),
    HTTP.VendorSpecific (..),
    HTTP.PerfLoggingPrefs (..),
    HTTP.MobileEmulation (..),
    HTTP.LogLevel (..),
    HTTP.LogSettings (..),
    HTTP.DeviceMetrics (..),

    -- * Conversions to Native Types
    toHttpCapabilities,
    toBiDiCapability,
    toHttpFullCapabilities,
    toBiDiCapabilities,

    -- * Conversions from Native Types
    fromHttpCapabilities,
    fromBiDiCapability,
    fromHttpFullCapabilities,
    fromBiDiCapabilities,

    -- * Response Conversions
    fromHttpSessionResponse,
    fromBiDiCapabilitiesResult,

    -- * Cross-Protocol Conversions
    convertRequestToHttp,
    convertRequestToBiDi,

    -- * Smart Constructors
    mkMinimalHttpRequest,
    mkMinimalBiDiRequest,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Word (Word8)
import WebDriverPreCore.HTTP.Protocol qualified as HTTP
import WebDriverPreCore.BiDi.Protocol qualified as BiDi

-- * Unified Property Types

-- | Unified proxy configuration (merges HTTP.Proxy and BiDi.ProxyConfiguration)
data ProxyConfig
  = DirectProxy
  | AutodetectProxy
  | ManualProxy
      { httpProxy :: Maybe Text,
        sslProxy :: Maybe Text,
        socksProxy :: Maybe SocksProxyConfig,
        noProxy :: Maybe [Text]
      }
  | PacProxy
      { proxyAutoconfigUrl :: Text
      }
  | SystemProxy
  deriving (Show, Eq)

-- | SOCKS proxy configuration
data SocksProxyConfig = MkSocksProxyConfig
  { socksProxy :: Text,
    socksVersion :: Word8
  }
  deriving (Show, Eq)

-- | Unified prompt behavior (supports both simple HTTP and detailed BiDi styles)
data PromptBehavior
  = -- | Simple behavior for all prompts (HTTP style)
    SimplePromptBehavior PromptAction
  | -- | Detailed per-prompt-type behavior (BiDi style)
    DetailedPromptBehavior
      { alert :: Maybe PromptAction,
        beforeUnload :: Maybe PromptAction,
        confirm :: Maybe PromptAction,
        defaultHandler :: Maybe PromptAction,
        prompt :: Maybe PromptAction
      }
  deriving (Show, Eq)

-- | Prompt action
data PromptAction
  = Accept
  | Dismiss
  | Ignore
  deriving (Show, Eq)

-- | Page load strategy (re-exported for convenience)
data PageLoadStrategy
  = None'
  | Eager
  | Normal
  deriving (Show, Eq)

-- | Browser name with extensibility for unknown browsers
data BrowserName
  = Chrome
  | Firefox
  | Safari
  | Edge
  | InternetExplorer
  | Other Text
  deriving (Show, Eq)

-- | Platform name with extensibility for unknown platforms
data PlatformName
  = Windows
  | Mac
  | Linux
  | Android
  | IOS
  | OtherPlatform Text
  deriving (Show, Eq)

-- * Universal Capability Request Types

-- | Universal capabilities request type with protocol-specific variants.
--
-- Protocol-specific fields (like timeouts for HTTP) are only present in the
-- corresponding constructor.
data CapabilitiesRequest
  = HttpCapabilitiesRequest
      { browserName :: Maybe BrowserName,
        browserVersion :: Maybe Text,
        platformName :: Maybe PlatformName,
        acceptInsecureCerts :: Maybe Bool,
        pageLoadStrategy :: Maybe PageLoadStrategy,
        proxy :: Maybe ProxyConfig,
        timeouts :: Maybe HTTP.Timeouts,
        strictFileInteractability :: Maybe Bool,
        unhandledPromptBehavior :: Maybe PromptBehavior,
        vendorSpecific :: Maybe HTTP.VendorSpecific
      }
  | BiDiCapabilitiesRequest
      { browserName :: Maybe BrowserName,
        browserVersion :: Maybe Text,
        platformName :: Maybe PlatformName,
        acceptInsecureCerts :: Maybe Bool,
        proxy :: Maybe ProxyConfig,
        unhandledPromptBehavior :: Maybe PromptBehavior
        -- webSocketUrl is always true for BiDi, handled in conversion
      }
  deriving (Show, Eq)

-- * Universal Capability Response Types

-- | Universal capabilities response type with protocol-specific variants.
--
-- Response types include fields that are only present in session responses.
data CapabilitiesResponse
  = HttpCapabilitiesResponse
      { browserName :: BrowserName,
        browserVersion :: Text,
        platformName :: PlatformName,
        acceptInsecureCerts :: Bool,
        pageLoadStrategy :: Maybe PageLoadStrategy,
        proxy :: Maybe ProxyConfig,
        httpSetWindowRect :: Maybe Bool,
        timeouts :: Maybe HTTP.Timeouts,
        strictFileInteractability :: Maybe Bool,
        unhandledPromptBehavior :: Maybe PromptBehavior,
        httpWebSocketUrl :: Maybe Bool,
        vendorSpecific :: Maybe HTTP.VendorSpecific
      }
  | BiDiCapabilitiesResponse
      { acceptInsecureCerts :: Bool,
        browserName :: BrowserName,
        browserVersion :: Text,
        platformName :: PlatformName,
        bidiSetWindowRect :: Bool,
        userAgent :: Text,
        proxy :: Maybe ProxyConfig,
        unhandledPromptBehavior :: Maybe PromptBehavior,
        bidiWebSocketUrl :: Maybe Text
      }
  deriving (Show, Eq)

-- * Full Capabilities Request

-- | Full capabilities request with alwaysMatch and firstMatch.
data FullCapabilitiesRequest = MkFullCapabilitiesRequest
  { alwaysMatch :: Maybe CapabilitiesRequest,
    firstMatch :: [CapabilitiesRequest]
  }
  deriving (Show, Eq)

-- * Proxy Conversions

-- | Convert unified proxy to HTTP proxy
proxyToHttp :: ProxyConfig -> HTTP.Proxy
proxyToHttp = \case
  DirectProxy -> HTTP.Direct
  AutodetectProxy -> HTTP.AutoDetect
  ManualProxy {httpProxy, sslProxy, socksProxy, noProxy} ->
    HTTP.Manual
      { HTTP.httpProxy = httpProxy,
        HTTP.sslProxy = sslProxy,
        HTTP.socksProxy = fmap socksToHttp socksProxy,
        HTTP.noProxy = noProxy
      }
  PacProxy {proxyAutoconfigUrl} ->
    HTTP.Pac {HTTP.proxyAutoconfigUrl = proxyAutoconfigUrl}
  SystemProxy -> HTTP.System

-- | Convert HTTP proxy to unified proxy
proxyFromHttp :: HTTP.Proxy -> ProxyConfig
proxyFromHttp = \case
  HTTP.Direct -> DirectProxy
  HTTP.AutoDetect -> AutodetectProxy
  HTTP.Manual {HTTP.httpProxy, HTTP.sslProxy, HTTP.socksProxy, HTTP.noProxy} ->
    ManualProxy
      { httpProxy = httpProxy,
        sslProxy = sslProxy,
        socksProxy = fmap socksFromHttp socksProxy,
        noProxy = noProxy
      }
  HTTP.Pac {HTTP.proxyAutoconfigUrl} ->
    PacProxy {proxyAutoconfigUrl = proxyAutoconfigUrl}
  HTTP.System -> SystemProxy

-- | Convert unified proxy to BiDi proxy
proxyToBiDi :: ProxyConfig -> BiDi.ProxyConfiguration
proxyToBiDi = \case
  DirectProxy -> BiDi.DirectProxyConfiguration
  AutodetectProxy -> BiDi.AutodetectProxyConfiguration
  ManualProxy {httpProxy, sslProxy, socksProxy, noProxy} ->
    BiDi.ManualProxyConfiguration
      { BiDi.httpProxy = httpProxy,
        BiDi.sslProxy = sslProxy,
        BiDi.socksProxyConfig = fmap socksToBiDi socksProxy,
        BiDi.noProxy = noProxy
      }
  PacProxy {proxyAutoconfigUrl} ->
    BiDi.PacProxyConfiguration {BiDi.proxyAutoconfigUrl = proxyAutoconfigUrl}
  SystemProxy -> BiDi.SystemProxyConfiguration

-- | Convert BiDi proxy to unified proxy
proxyFromBiDi :: BiDi.ProxyConfiguration -> ProxyConfig
proxyFromBiDi = \case
  BiDi.DirectProxyConfiguration -> DirectProxy
  BiDi.AutodetectProxyConfiguration -> AutodetectProxy
  BiDi.ManualProxyConfiguration {BiDi.httpProxy, BiDi.sslProxy, BiDi.socksProxyConfig, BiDi.noProxy} ->
    ManualProxy
      { httpProxy = httpProxy,
        sslProxy = sslProxy,
        socksProxy = fmap socksFromBiDi socksProxyConfig,
        noProxy = noProxy
      }
  BiDi.PacProxyConfiguration {BiDi.proxyAutoconfigUrl} ->
    PacProxy {proxyAutoconfigUrl = proxyAutoconfigUrl}
  BiDi.SystemProxyConfiguration -> SystemProxy

-- | Convert unified SOCKS to HTTP SOCKS
socksToHttp :: SocksProxyConfig -> HTTP.SocksProxy
socksToHttp (MkSocksProxyConfig {socksProxy, socksVersion}) =
  HTTP.MkSocksProxy
    { HTTP.socksProxy = socksProxy,
      HTTP.socksVersion = fromIntegral socksVersion
    }

-- | Convert HTTP SOCKS to unified SOCKS
socksFromHttp :: HTTP.SocksProxy -> SocksProxyConfig
socksFromHttp (HTTP.MkSocksProxy {HTTP.socksProxy, HTTP.socksVersion}) =
  MkSocksProxyConfig
    { socksProxy = socksProxy,
      socksVersion = fromIntegral socksVersion
    }

-- | Convert unified SOCKS to BiDi SOCKS
socksToBiDi :: SocksProxyConfig -> BiDi.SocksProxyConfiguration
socksToBiDi (MkSocksProxyConfig {socksProxy, socksVersion}) =
  BiDi.MkSocksProxyConfiguration
    { BiDi.socksProxy = socksProxy,
      BiDi.socksVersion = socksVersion
    }

-- | Convert BiDi SOCKS to unified SOCKS
socksFromBiDi :: BiDi.SocksProxyConfiguration -> SocksProxyConfig
socksFromBiDi (BiDi.MkSocksProxyConfiguration {BiDi.socksProxy, BiDi.socksVersion}) =
  MkSocksProxyConfig
    { socksProxy = socksProxy,
      socksVersion = socksVersion
    }

-- * Prompt Behavior Conversions

-- | Convert unified prompt behavior to HTTP unhandled prompt behavior
promptToHttp :: PromptBehavior -> HTTP.UnhandledPromptBehavior
promptToHttp = \case
  SimplePromptBehavior action -> promptActionToHttp action
  DetailedPromptBehavior {defaultHandler} ->
    -- HTTP only supports simple behavior, use default or Dismiss
    maybe HTTP.Dismiss promptActionToHttp defaultHandler

-- | Convert prompt action to HTTP behavior
promptActionToHttp :: PromptAction -> HTTP.UnhandledPromptBehavior
promptActionToHttp = \case
  Accept -> HTTP.Accept
  Dismiss -> HTTP.Dismiss
  Ignore -> HTTP.Ignore

-- | Convert HTTP unhandled prompt behavior to unified
promptFromHttp :: HTTP.UnhandledPromptBehavior -> PromptBehavior
promptFromHttp = SimplePromptBehavior . \case
  HTTP.Accept -> Accept
  HTTP.Dismiss -> Dismiss
  HTTP.AcceptAndNotify -> Accept
  HTTP.DismissAndNotify -> Dismiss
  HTTP.Ignore -> Ignore

-- | Convert unified prompt behavior to BiDi user prompt handler
promptToBiDi :: PromptBehavior -> BiDi.UserPromptHandler
promptToBiDi = \case
  SimplePromptBehavior action ->
    let bidiAction = Just $ promptActionToBiDi action
     in BiDi.MkUserPromptHandler
          { BiDi.alert = bidiAction,
            BiDi.beforeUnload = bidiAction,
            BiDi.confirm = bidiAction,
            BiDi.defaultHandler = bidiAction,
            BiDi.fileHandler = bidiAction,
            BiDi.prompt = bidiAction
          }
  DetailedPromptBehavior {alert, beforeUnload, confirm, defaultHandler, prompt} ->
    BiDi.MkUserPromptHandler
      { BiDi.alert = fmap promptActionToBiDi alert,
        BiDi.beforeUnload = fmap promptActionToBiDi beforeUnload,
        BiDi.confirm = fmap promptActionToBiDi confirm,
        BiDi.defaultHandler = fmap promptActionToBiDi defaultHandler,
        BiDi.fileHandler = Nothing,
        BiDi.prompt = fmap promptActionToBiDi prompt
      }

-- | Convert prompt action to BiDi action
promptActionToBiDi :: PromptAction -> BiDi.UserPromptHandlerType
promptActionToBiDi = \case
  Accept -> BiDi.Accept
  Dismiss -> BiDi.Dismiss
  Ignore -> BiDi.Ignore

-- | Convert BiDi user prompt handler to unified
promptFromBiDi :: BiDi.UserPromptHandler -> PromptBehavior
promptFromBiDi (BiDi.MkUserPromptHandler {BiDi.alert, BiDi.beforeUnload, BiDi.confirm, BiDi.defaultHandler, BiDi.prompt}) =
  DetailedPromptBehavior
    { alert = fmap promptActionFromBiDi alert,
      beforeUnload = fmap promptActionFromBiDi beforeUnload,
      confirm = fmap promptActionFromBiDi confirm,
      defaultHandler = fmap promptActionFromBiDi defaultHandler,
      prompt = fmap promptActionFromBiDi prompt
    }

-- | Convert BiDi action to prompt action
promptActionFromBiDi :: BiDi.UserPromptHandlerType -> PromptAction
promptActionFromBiDi = \case
  BiDi.Accept -> Accept
  BiDi.Dismiss -> Dismiss
  BiDi.Ignore -> Ignore

-- * Page Load Strategy Conversions

-- | Convert unified page load strategy to HTTP
pageLoadToHttp :: PageLoadStrategy -> HTTP.PageLoadStrategy
pageLoadToHttp = \case
  None' -> HTTP.None'
  Eager -> HTTP.Eager
  Normal -> HTTP.Normal

-- | Convert HTTP page load strategy to unified
pageLoadFromHttp :: HTTP.PageLoadStrategy -> PageLoadStrategy
pageLoadFromHttp = \case
  HTTP.None' -> None'
  HTTP.Eager -> Eager
  HTTP.Normal -> Normal

-- * Conversions to Native Types

-- | Convert universal capabilities request to HTTP capabilities
toHttpCapabilities :: CapabilitiesRequest -> HTTP.Capabilities
toHttpCapabilities (HttpCapabilitiesRequest {..}) =
  HTTP.MkCapabilities
    { HTTP.browserName = fmap browserNameToText browserName,
      HTTP.browserVersion = browserVersion,
      HTTP.platformName = fmap platformNameToText platformName,
      HTTP.acceptInsecureCerts = acceptInsecureCerts,
      HTTP.pageLoadStrategy = fmap pageLoadToHttp pageLoadStrategy,
      HTTP.proxy = fmap proxyToHttp proxy,
      HTTP.setWindowRect = Nothing,
      HTTP.timeouts = timeouts,
      HTTP.strictFileInteractability = strictFileInteractability,
      HTTP.unhandledPromptBehavior = fmap promptToHttp unhandledPromptBehavior,
      HTTP.webSocketUrl = Nothing,
      HTTP.vendorSpecific = vendorSpecific
    }
toHttpCapabilities (BiDiCapabilitiesRequest {..}) =
  -- Convert BiDi request to HTTP (lossy: no timeouts, vendor-specific)
  HTTP.MkCapabilities
    { HTTP.browserName = fmap browserNameToText browserName,
      HTTP.browserVersion = browserVersion,
      HTTP.platformName = fmap platformNameToText platformName,
      HTTP.acceptInsecureCerts = acceptInsecureCerts,
      HTTP.pageLoadStrategy = Nothing,
      HTTP.proxy = fmap proxyToHttp proxy,
      HTTP.setWindowRect = Nothing,
      HTTP.timeouts = Nothing,
      HTTP.strictFileInteractability = Nothing,
      HTTP.unhandledPromptBehavior = fmap promptToHttp unhandledPromptBehavior,
      HTTP.webSocketUrl = Nothing,
      HTTP.vendorSpecific = Nothing
    }

-- | Convert universal capabilities request to BiDi capability
toBiDiCapability :: CapabilitiesRequest -> BiDi.Capability
toBiDiCapability (HttpCapabilitiesRequest {..}) =
  -- Convert HTTP request to BiDi (lossy: no timeouts, vendor-specific)
  BiDi.MkCapability
    { BiDi.acceptInsecureCerts = acceptInsecureCerts,
      BiDi.browserName = fmap browserNameToText browserName,
      BiDi.browserVersion = browserVersion,
      BiDi.webSocketUrl = True, -- Always true for BiDi
      BiDi.platformName = fmap platformNameToText platformName,
      BiDi.proxy = fmap proxyToBiDi proxy,
      BiDi.unhandledPromptBehavior = fmap promptToBiDi unhandledPromptBehavior
    }
toBiDiCapability (BiDiCapabilitiesRequest {..}) =
  BiDi.MkCapability
    { BiDi.acceptInsecureCerts = acceptInsecureCerts,
      BiDi.browserName = fmap browserNameToText browserName,
      BiDi.browserVersion = browserVersion,
      BiDi.webSocketUrl = True,
      BiDi.platformName = fmap platformNameToText platformName,
      BiDi.proxy = fmap proxyToBiDi proxy,
      BiDi.unhandledPromptBehavior = fmap promptToBiDi unhandledPromptBehavior
    }

-- * Conversions from Native Types

-- | Convert HTTP capabilities to universal request
fromHttpCapabilities :: HTTP.Capabilities -> CapabilitiesRequest
fromHttpCapabilities (HTTP.MkCapabilities {..}) =
  HttpCapabilitiesRequest
    { browserName = fmap textToBrowserName browserName,
      browserVersion = browserVersion,
      platformName = fmap textToPlatformName platformName,
      acceptInsecureCerts = acceptInsecureCerts,
      pageLoadStrategy = fmap pageLoadFromHttp pageLoadStrategy,
      proxy = fmap proxyFromHttp proxy,
      timeouts = timeouts,
      strictFileInteractability = strictFileInteractability,
      unhandledPromptBehavior = fmap promptFromHttp unhandledPromptBehavior,
      vendorSpecific = vendorSpecific
    }

-- | Convert BiDi capability to universal request
fromBiDiCapability :: BiDi.Capability -> CapabilitiesRequest
fromBiDiCapability (BiDi.MkCapability {..}) =
  BiDiCapabilitiesRequest
    { browserName = fmap textToBrowserName browserName,
      browserVersion = browserVersion,
      platformName = fmap textToPlatformName platformName,
      acceptInsecureCerts = acceptInsecureCerts,
      proxy = fmap proxyFromBiDi proxy,
      unhandledPromptBehavior = fmap promptFromBiDi unhandledPromptBehavior
    }

-- * Response Conversions

-- | Convert HTTP session response to universal response
fromHttpSessionResponse :: HTTP.Capabilities -> CapabilitiesResponse
fromHttpSessionResponse (HTTP.MkCapabilities {..}) =
  HttpCapabilitiesResponse
    { browserName = maybe (Other "") textToBrowserName browserName,
      browserVersion = maybe "" id browserVersion,
      platformName = maybe (OtherPlatform "") textToPlatformName platformName,
      acceptInsecureCerts = maybe False id acceptInsecureCerts,
      pageLoadStrategy = fmap pageLoadFromHttp pageLoadStrategy,
      proxy = fmap proxyFromHttp proxy,
      httpSetWindowRect = setWindowRect,
      timeouts = timeouts,
      strictFileInteractability = strictFileInteractability,
      unhandledPromptBehavior = fmap promptFromHttp unhandledPromptBehavior,
      httpWebSocketUrl = webSocketUrl,
      vendorSpecific = vendorSpecific
    }

-- | Convert BiDi capabilities result to universal response
fromBiDiCapabilitiesResult :: BiDi.CapabilitiesResult -> CapabilitiesResponse
fromBiDiCapabilitiesResult (BiDi.MkCapabilitiesResult {..}) =
  BiDiCapabilitiesResponse
    { acceptInsecureCerts = acceptInsecureCerts,
      browserName = textToBrowserName browserName,
      browserVersion = browserVersion,
      platformName = textToPlatformName platformName,
      bidiSetWindowRect = setWindowRect,
      userAgent = userAgent,
      proxy = fmap proxyFromBiDi proxy,
      unhandledPromptBehavior = fmap promptFromBiDi unhandledPromptBehavior,
      bidiWebSocketUrl = webSocketUrl
    }

-- * Cross-Protocol Conversions

-- | Convert any request to HTTP variant (potentially lossy)
convertRequestToHttp :: CapabilitiesRequest -> CapabilitiesRequest
convertRequestToHttp req@(HttpCapabilitiesRequest {}) = req
convertRequestToHttp (BiDiCapabilitiesRequest {..}) =
  HttpCapabilitiesRequest
    { browserName = browserName,
      browserVersion = browserVersion,
      platformName = platformName,
      acceptInsecureCerts = acceptInsecureCerts,
      pageLoadStrategy = Nothing,
      proxy = proxy,
      timeouts = Nothing,
      strictFileInteractability = Nothing,
      unhandledPromptBehavior = unhandledPromptBehavior,
      vendorSpecific = Nothing
    }

-- | Convert any request to BiDi variant (potentially lossy)
convertRequestToBiDi :: CapabilitiesRequest -> CapabilitiesRequest
convertRequestToBiDi (HttpCapabilitiesRequest {..}) =
  BiDiCapabilitiesRequest
    { browserName = browserName,
      browserVersion = browserVersion,
      platformName = platformName,
      acceptInsecureCerts = acceptInsecureCerts,
      proxy = proxy,
      unhandledPromptBehavior = unhandledPromptBehavior
    }
convertRequestToBiDi req@(BiDiCapabilitiesRequest {}) = req

-- * Full Capabilities Conversions

-- | Convert universal full capabilities to HTTP full capabilities
toHttpFullCapabilities :: FullCapabilitiesRequest -> HTTP.FullCapabilities
toHttpFullCapabilities (MkFullCapabilitiesRequest {..}) =
  HTTP.MkFullCapabilities
    { HTTP.alwaysMatch = fmap toHttpCapabilities alwaysMatch,
      HTTP.firstMatch = fmap toHttpCapabilities firstMatch
    }

-- | Convert universal full capabilities to BiDi capabilities
toBiDiCapabilities :: FullCapabilitiesRequest -> BiDi.Capabilities
toBiDiCapabilities (MkFullCapabilitiesRequest {..}) =
  BiDi.MkCapabilities
    { BiDi.alwaysMatch = fmap toBiDiCapability alwaysMatch,
      BiDi.firstMatch = fmap toBiDiCapability firstMatch
    }

-- | Convert HTTP full capabilities to universal
fromHttpFullCapabilities :: HTTP.FullCapabilities -> FullCapabilitiesRequest
fromHttpFullCapabilities (HTTP.MkFullCapabilities {..}) =
  MkFullCapabilitiesRequest
    { alwaysMatch = fmap fromHttpCapabilities alwaysMatch,
      firstMatch = fmap fromHttpCapabilities firstMatch
    }

-- | Convert BiDi capabilities to universal
fromBiDiCapabilities :: BiDi.Capabilities -> FullCapabilitiesRequest
fromBiDiCapabilities (BiDi.MkCapabilities {..}) =
  MkFullCapabilitiesRequest
    { alwaysMatch = fmap fromBiDiCapability alwaysMatch,
      firstMatch = fmap fromBiDiCapability firstMatch
    }

-- * Smart Constructors

-- | Create minimal HTTP capabilities request
mkMinimalHttpRequest :: Maybe BrowserName -> CapabilitiesRequest
mkMinimalHttpRequest bName =
  HttpCapabilitiesRequest
    { browserName = bName,
      browserVersion = Nothing,
      platformName = Nothing,
      acceptInsecureCerts = Nothing,
      pageLoadStrategy = Nothing,
      proxy = Nothing,
      timeouts = Nothing,
      strictFileInteractability = Nothing,
      unhandledPromptBehavior = Nothing,
      vendorSpecific = Nothing
    }

-- | Create minimal BiDi capabilities request
mkMinimalBiDiRequest :: Maybe BrowserName -> CapabilitiesRequest
mkMinimalBiDiRequest bName =
  BiDiCapabilitiesRequest
    { browserName = bName,
      browserVersion = Nothing,
      platformName = Nothing,
      acceptInsecureCerts = Nothing,
      proxy = Nothing,
      unhandledPromptBehavior = Nothing
    }

-- * Helper Functions for Browser/Platform Names

-- | ToJSON instance for BrowserName
instance ToJSON BrowserName where
  toJSON :: BrowserName -> Value
  toJSON = String . browserNameToText

-- | FromJSON instance for BrowserName
instance FromJSON BrowserName where
  parseJSON :: Value -> Parser BrowserName
  parseJSON = withText "BrowserName" $ pure . textToBrowserName

-- | ToJSON instance for PlatformName
instance ToJSON PlatformName where
  toJSON :: PlatformName -> Value
  toJSON = String . platformNameToText

-- | FromJSON instance for PlatformName
instance FromJSON PlatformName where
  parseJSON :: Value -> Parser PlatformName
  parseJSON = withText "PlatformName" $ pure . textToPlatformName

-- * Helper Functions for Browser/Platform Names

-- | Convert text to BrowserName
textToBrowserName :: Text -> BrowserName
textToBrowserName = \case
  "chrome" -> Chrome
  "firefox" -> Firefox
  "safari" -> Safari
  "edge" -> Edge
  "internet explorer" -> InternetExplorer
  other -> Other other

-- | Convert BrowserName to text
browserNameToText :: BrowserName -> Text
browserNameToText = \case
  Chrome -> "chrome"
  Firefox -> "firefox"
  Safari -> "safari"
  Edge -> "edge"
  InternetExplorer -> "internet explorer"
  Other t -> t

-- | Convert text to PlatformName
textToPlatformName :: Text -> PlatformName
textToPlatformName = \case
  "windows" -> Windows
  "mac" -> Mac
  "linux" -> Linux
  "android" -> Android
  "ios" -> IOS
  other -> OtherPlatform other

-- | Convert PlatformName to text
platformNameToText :: PlatformName -> Text
platformNameToText = \case
  Windows -> "windows"
  Mac -> "mac"
  Linux -> "linux"
  Android -> "android"
  IOS -> "ios"
  OtherPlatform t -> t
