{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module: WebDriverPreCore.Extended.Capabilities
-- Description: Universal capabilities abstraction for HTTP and BiDi protocols
--
-- Provides a unified capabilities interface that can be converted to protocol-specific
-- types. Uses sum types to represent HTTP and BiDi protocol variants.
module WebDriverPreCore.Extended.Capabilities
  ( -- * Universal Capability Types
    HttpCapability (..),
    BiDiCapability (..),
    HttpSessionResponse (..),
    BiDiSessionResponse (..),
    FullCapabilities (..),
    UserPromptHandler (..),
    UserPromptHandlerType (..),

    -- * Unified Property Types
    Proxy (..),
    SocksProxyConfig (..),
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
    HTTP.Session (..),
    HTTP.UnhandledPromptBehavior(..),

    -- * Full Capabilities
    HttpCapabilities,
    BiDiCapabilities,

    -- * Conversions from Native Types
    fromHttpCapability,
    fromBiDiCapability,
    fromHttpCapabilities,
    fromBiDiCapabilities,

    -- * Conversions to Native Types
    toHttpCapability,
    toBiDiCapability,
    toHttpCapabilities,
    toBiDiCapabilities,

    -- * Response Conversions
    fromHttpSessionResponse,
    fromBiDiSessionResponse,

    -- * Cross-Protocol Conversions
    BiDiPromptHandlerSource (..),
    biDiCapabilityToHttp,
    httpCapabilityToBiDi,

    -- * Session Management
    Runner,
    newHttpSession,
    newHttpSessionResponse,
  )
where

import Data.Aeson (Value)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import WebDriverPreCore.BiDi.Protocol qualified as BiDi
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as Actions
import WebDriverPreCore.HTTP.Protocol (Session (..), BrowserName (..), PlatformName (..))
import WebDriverPreCore.HTTP.Protocol qualified as HTTP
import GHC.Generics (Generic)

type HttpCapabilities = FullCapabilities HttpCapability

type BiDiCapabilities = FullCapabilities BiDiCapability
-- TODO : review whole file esp for lossy conversions

-- | HTTP-specific capabilities
data HttpCapability = MkHttpCapability
  { browserName :: Maybe BrowserName,
    -- browserVersion :: Text,
    platformName :: Maybe PlatformName,
    acceptInsecureCerts :: Bool,
    pageLoadStrategy :: Maybe PageLoadStrategy,
    proxy :: Maybe Proxy,
    -- httpSetWindowRect :: Maybe Bool,
    timeouts :: Maybe HTTP.Timeouts,
    strictFileInteractability :: Maybe Bool,
    unhandledPromptBehavior :: Maybe HTTP.UnhandledPromptBehavior,
    httpWebSocketUrl :: Maybe Bool,
    vendorSpecific :: Maybe HTTP.VendorSpecific
  }
  deriving (Show, Eq)

-- | Page load strategy (re-exported for convenience)
data PageLoadStrategy
  = None'
  | Eager
  | Normal
  deriving (Show, Eq)

-- | SOCKS proxy configuration
data SocksProxyConfig = MkSocksProxyConfig
  { socksProxy :: Text,
    socksVersion :: Word8
  }
  deriving (Show, Eq)

-- | Unified proxy configuration (merges HTTP.Proxy and BiDi.ProxyConfiguration)
data Proxy
  = Direct
  | Autodetect
  | Manual
      { httpProxy :: Maybe Text,
        sslProxy :: Maybe Text,
        socksProxy :: Maybe SocksProxyConfig,
        noProxy :: Maybe [Text]
      }
  | Pac
      { proxyAutoconfigUrl :: Text
      }
  | System
  deriving (Show, Eq)

data HttpSessionResponse = MkHttpSessionResponse
  { session :: HTTP.Session,
    websocketUrl :: Maybe Text,
    browserVersion :: Text,
    httpSetWindowRect :: Maybe Bool,
    -- TODO should be object
    extensions :: Maybe (M.Map Text Value),
    httpCapability :: HttpCapability
  }

-- | BiDi-specific capabilities
data BiDiCapability = MkBiDiCapability
  { acceptInsecureCerts :: Maybe Bool,
    browserName :: Maybe BrowserName,
    browserVersion :: Maybe Text,
    platformName :: Maybe PlatformName,
    proxy :: Maybe Proxy,
    unhandledPromptBehavior :: Maybe UserPromptHandler
  }
  deriving (Show, Eq)

-- All the cruft below is to avoid name conflicts with HTTP.UnhandledPromptBehavior, which is a different type with the same purpose in BiDi. 
-- We want to be able to convert between them, but they are not directly compatible, so we define our own type and conversion functions.
data UserPromptHandler = MkUserPromptHandler
  { alert :: Maybe UserPromptHandlerType,
    beforeUnload :: Maybe UserPromptHandlerType,
    confirm :: Maybe UserPromptHandlerType,
    defaultHandler :: Maybe UserPromptHandlerType,
    fileHandler :: Maybe UserPromptHandlerType,
    prompt :: Maybe UserPromptHandlerType
  }
  deriving (Show, Eq, Generic)

fromBidiPromptHandler :: BiDi.UserPromptHandler -> UserPromptHandler
fromBidiPromptHandler (BiDi.MkUserPromptHandler {..}) =
  MkUserPromptHandler
    { alert = mapHT alert,
      beforeUnload = mapHT beforeUnload,
      confirm = mapHT confirm,
      defaultHandler = mapHT defaultHandler,
      fileHandler = mapHT fileHandler,
      prompt = mapHT prompt
    }
    where 
      mapHT :: Maybe BiDi.UserPromptHandlerType -> Maybe UserPromptHandlerType
      mapHT = fmap fromBidiPromptHandlerType

toBidiPromptHandler :: UserPromptHandler -> BiDi.UserPromptHandler
toBidiPromptHandler (MkUserPromptHandler {..}) =
  BiDi.MkUserPromptHandler
    { alert = mapHT alert,
      beforeUnload = mapHT beforeUnload,
      confirm = mapHT confirm,
      defaultHandler = mapHT defaultHandler,
      fileHandler = mapHT fileHandler,
      prompt = mapHT prompt
    }
    where 
      mapHT :: Maybe UserPromptHandlerType -> Maybe BiDi.UserPromptHandlerType
      mapHT = fmap toBidiPromptHandlerType

data UserPromptHandlerType
  = AcceptPrompt
  | DismissPrompt
  | IgnorePrompt
  deriving (Show, Eq)

toBidiPromptHandlerType :: UserPromptHandlerType -> BiDi.UserPromptHandlerType
toBidiPromptHandlerType = \case
  AcceptPrompt -> BiDi.Accept
  DismissPrompt -> BiDi.Dismiss
  IgnorePrompt -> BiDi.Ignore

fromBidiPromptHandlerType :: BiDi.UserPromptHandlerType -> UserPromptHandlerType
fromBidiPromptHandlerType = \case
  BiDi.Accept -> AcceptPrompt
  BiDi.Dismiss -> DismissPrompt
  BiDi.Ignore -> IgnorePrompt

data BiDiSessionResponse = MkBiDiSessionResponse
  { -- TODO - make base BIDI Session use the same newtype as HTTP
    session :: HTTP.Session,
    acceptInsecureCerts :: Bool,
    browserName :: BrowserName,
    browserVersion :: Text,
    platformName :: PlatformName,
    setWindowRect :: Bool,
    userAgent :: Text,
    proxy :: Maybe Proxy,
    unhandledPromptBehavior :: Maybe UserPromptHandler,
    webSocketUrl :: Maybe Text
  }
  deriving (Show, Eq)

-- * Full Capabilities Request

-- | Full capabilities request with alwaysMatch and firstMatch.
data FullCapabilities a = MkFullCapabilities
  { alwaysMatch :: Maybe a,
    firstMatch :: [a]
  }
  deriving (Show, Eq)

-- ######################################################################
-- ################# Conversion from Native Types #######################
-- ######################################################################

-- | Convert native HTTP capabilities to local HTTP capability
fromHttpCapability :: HTTP.Capabilities -> HttpCapability
fromHttpCapability HTTP.MkCapabilities {..} =
  MkHttpCapability
    { browserName,
      platformName,
      acceptInsecureCerts = fromMaybe False acceptInsecureCerts,
      pageLoadStrategy = pageLoadFromHttp <$> pageLoadStrategy,
      proxy = proxyFromHttp <$> proxy,
      timeouts,
      strictFileInteractability,
      unhandledPromptBehavior,
      httpWebSocketUrl = webSocketUrl,
      vendorSpecific
    }

-- | Convert native BiDi capability to local BiDi capability
fromBiDiCapability :: BiDi.Capability -> BiDiCapability
fromBiDiCapability BiDi.MkCapability {..} =
  MkBiDiCapability
    { acceptInsecureCerts,
      browserName,
      browserVersion,
      platformName,
      proxy = proxyFromBiDi <$> proxy,
      unhandledPromptBehavior = fromBidiPromptHandler <$> unhandledPromptBehavior
    }

-- | Convert native HTTP full capabilities to local HTTP full capabilities
fromHttpCapabilities :: HTTP.FullCapabilities -> FullCapabilities HttpCapability
fromHttpCapabilities HTTP.MkFullCapabilities {..} =
  MkFullCapabilities
    { alwaysMatch = fromHttpCapability <$> alwaysMatch,
      firstMatch = fromHttpCapability <$> firstMatch
    }

-- | Convert native BiDi capabilities to local BiDi full capabilities
fromBiDiCapabilities :: BiDi.Capabilities -> FullCapabilities BiDiCapability
fromBiDiCapabilities BiDi.MkCapabilities {..} =
  MkFullCapabilities
    { alwaysMatch = fromBiDiCapability <$> alwaysMatch,
      firstMatch = fromBiDiCapability <$> firstMatch
    }

-- ** Property from Subtypes

-- | Convert HTTP proxy to unified proxy
proxyFromHttp :: HTTP.Proxy -> Proxy
proxyFromHttp = \case
  HTTP.Direct -> Direct
  HTTP.AutoDetect -> Autodetect
  HTTP.Manual {HTTP.httpProxy, HTTP.sslProxy, HTTP.socksProxy, HTTP.noProxy} ->
    Manual
      { httpProxy,
        sslProxy,
        socksProxy = socksFromHttp <$> socksProxy,
        noProxy
      }
  HTTP.Pac {HTTP.proxyAutoconfigUrl} ->
    Pac {proxyAutoconfigUrl}
  HTTP.System -> System

-- | Convert HTTP SOCKS to unified SOCKS
socksFromHttp :: HTTP.SocksProxy -> SocksProxyConfig
socksFromHttp (HTTP.MkSocksProxy {HTTP.socksProxy, HTTP.socksVersion}) =
  MkSocksProxyConfig
    { socksProxy,
      socksVersion = fromIntegral socksVersion
    }

-- | Convert BiDi proxy to unified proxy
proxyFromBiDi :: BiDi.ProxyConfiguration -> Proxy
proxyFromBiDi = \case
  BiDi.DirectProxyConfiguration -> Direct
  BiDi.AutodetectProxyConfiguration -> Autodetect
  BiDi.ManualProxyConfiguration {BiDi.httpProxy, BiDi.sslProxy, BiDi.socksProxyConfig, BiDi.noProxy} ->
    Manual
      { httpProxy,
        sslProxy,
        socksProxy = socksFromBiDi <$> socksProxyConfig,
        noProxy
      }
  BiDi.PacProxyConfiguration {BiDi.proxyAutoconfigUrl} ->
    Pac {proxyAutoconfigUrl}
  BiDi.SystemProxyConfiguration -> System

-- | Convert BiDi SOCKS to unified SOCKS
socksFromBiDi :: BiDi.SocksProxyConfiguration -> SocksProxyConfig
socksFromBiDi (BiDi.MkSocksProxyConfiguration {BiDi.socksProxy, BiDi.socksVersion}) =
  MkSocksProxyConfig
    { socksProxy,
      socksVersion
    }

-- | Convert HTTP page load strategy to unified
pageLoadFromHttp :: HTTP.PageLoadStrategy -> PageLoadStrategy
pageLoadFromHttp = \case
  HTTP.None' -> None'
  HTTP.Eager -> Eager
  HTTP.Normal -> Normal

-- ** Response Conversions

-- | Convert native HTTP session response to local HTTP session response
fromHttpSessionResponse :: HTTP.SessionResponse -> HttpSessionResponse
fromHttpSessionResponse (HTTP.MkSessionResponse {sessionId = session, webSocketUrl = wsUrl, capabilities = HTTP.MkCapabilities {..}, extensions = exts}) =
  MkHttpSessionResponse
    { session,
      websocketUrl = wsUrl,
      browserVersion = fromMaybe "" browserVersion,
      httpSetWindowRect = setWindowRect,
      extensions = exts,
      httpCapability =
        MkHttpCapability
          { browserName,
            platformName,
            acceptInsecureCerts = fromMaybe False acceptInsecureCerts,
            pageLoadStrategy = pageLoadFromHttp <$> pageLoadStrategy,
            proxy = proxyFromHttp <$> proxy,
            timeouts,
            strictFileInteractability,
            unhandledPromptBehavior,
            httpWebSocketUrl = webSocketUrl,
            vendorSpecific
          }
    }

-- | Convert native BiDi session response to local BiDi session response
fromBiDiSessionResponse :: BiDi.SessionNewResult -> BiDiSessionResponse
fromBiDiSessionResponse (BiDi.MkSessionNewResult {sessionId = session, capabilities = BiDi.MkCapabilitiesResult {..}}) =
  MkBiDiSessionResponse
    { session,
      acceptInsecureCerts,
      browserName,
      browserVersion,
      platformName,
      setWindowRect,
      userAgent,
      proxy = proxyFromBiDi <$> proxy,
      unhandledPromptBehavior = fromBidiPromptHandler <$> unhandledPromptBehavior,
      webSocketUrl
    }

-- ######################################################################
-- ################## Conversion to Native Types ########################
-- ######################################################################

-- | Convert local HTTP capability to native HTTP capabilities
toHttpCapability :: HttpCapability -> HTTP.Capabilities
toHttpCapability (MkHttpCapability {..}) =
  HTTP.MkCapabilities
    { browserName,
      -- response only
      browserVersion = Nothing,
      platformName,
      acceptInsecureCerts = Just acceptInsecureCerts,
      pageLoadStrategy = pageLoadToHttp <$> pageLoadStrategy,
      proxy = proxyToHttp <$> proxy,
      -- response only
      setWindowRect = Nothing,
      timeouts,
      strictFileInteractability,
      unhandledPromptBehavior,
      webSocketUrl = httpWebSocketUrl,
      vendorSpecific
    }

-- ** Property to Subtypes

-- | Convert unified proxy to HTTP proxy
proxyToHttp :: Proxy -> HTTP.Proxy
proxyToHttp = \case
  Direct -> HTTP.Direct
  Autodetect -> HTTP.AutoDetect
  Manual {httpProxy, sslProxy, socksProxy, noProxy} ->
    HTTP.Manual
      { httpProxy,
        sslProxy,
        socksProxy = socksToHttp <$> socksProxy,
        noProxy
      }
  Pac {proxyAutoconfigUrl} ->
    HTTP.Pac {proxyAutoconfigUrl}
  System -> HTTP.System

-- | Convert unified SOCKS to HTTP SOCKS
socksToHttp :: SocksProxyConfig -> HTTP.SocksProxy
socksToHttp (MkSocksProxyConfig {socksProxy, socksVersion}) =
  HTTP.MkSocksProxy
    { socksProxy,
      socksVersion = fromIntegral socksVersion
    }

-- | Convert unified proxy to BiDi proxy
proxyToBiDi :: Proxy -> BiDi.ProxyConfiguration
proxyToBiDi = \case
  Direct -> BiDi.DirectProxyConfiguration
  Autodetect -> BiDi.AutodetectProxyConfiguration
  Manual {httpProxy, sslProxy, socksProxy, noProxy} ->
    BiDi.ManualProxyConfiguration
      { httpProxy,
        sslProxy,
        socksProxyConfig = socksToBiDi <$> socksProxy,
        noProxy
      }
  Pac {proxyAutoconfigUrl} ->
    BiDi.PacProxyConfiguration {proxyAutoconfigUrl}
  System -> BiDi.SystemProxyConfiguration

-- | Convert unified SOCKS to BiDi SOCKS
socksToBiDi :: SocksProxyConfig -> BiDi.SocksProxyConfiguration
socksToBiDi (MkSocksProxyConfig {socksProxy, socksVersion}) =
  BiDi.MkSocksProxyConfiguration
    { socksProxy,
      socksVersion
    }

-- | Convert unified page load strategy to HTTP
pageLoadToHttp :: PageLoadStrategy -> HTTP.PageLoadStrategy
pageLoadToHttp = \case
  None' -> HTTP.None'
  Eager -> HTTP.Eager
  Normal -> HTTP.Normal

-- | Convert local BiDi capability to native BiDi capability
toBiDiCapability :: BiDiCapability -> BiDi.Capability
toBiDiCapability (MkBiDiCapability {..}) =
  BiDi.MkCapability
    { acceptInsecureCerts,
      browserName,
      browserVersion,
      platformName,
      proxy = proxyToBiDi <$> proxy,
      unhandledPromptBehavior = toBidiPromptHandler <$> unhandledPromptBehavior
    }

-- | Convert universal full capabilities to HTTP full capabilities
toHttpCapabilities :: FullCapabilities HttpCapability -> HTTP.FullCapabilities
toHttpCapabilities (MkFullCapabilities {..}) =
  HTTP.MkFullCapabilities
    { alwaysMatch = toHttpCapability <$> alwaysMatch,
      firstMatch = toHttpCapability <$> firstMatch
    }

-- | Convert universal full capabilities to BiDi capabilities
toBiDiCapabilities :: FullCapabilities BiDiCapability -> BiDi.Capabilities
toBiDiCapabilities (MkFullCapabilities {..}) =
  BiDi.MkCapabilities
    { alwaysMatch = toBiDiCapability <$> alwaysMatch,
      firstMatch = toBiDiCapability <$> firstMatch
    }

-- * Cross-Protocol Conversions

-- | Convert BiDi capability to local HTTP capability (potentially lossy)
biDiCapabilityToHttp :: BiDiCapability -> Maybe HTTP.UnhandledPromptBehavior -> HttpCapability
biDiCapabilityToHttp (MkBiDiCapability {..}) mUnhandledPromptBehavior =
  MkHttpCapability
    { browserName,
      platformName,
      acceptInsecureCerts = fromMaybe False acceptInsecureCerts,
      pageLoadStrategy = Nothing,
      proxy,
      timeouts = Nothing,
      strictFileInteractability = Nothing,
      unhandledPromptBehavior = mUnhandledPromptBehavior,
      httpWebSocketUrl = Nothing,
      vendorSpecific = Nothing
    }

data BiDiPromptHandlerSource
  = Coerce
  | Clear
  | SetValue UserPromptHandler

-- | Convert HTTP capability to local BiDi capability (potentially lossy)
httpCapabilityToBiDi :: HttpCapability -> BiDiPromptHandlerSource -> BiDiCapability
httpCapabilityToBiDi (MkHttpCapability {unhandledPromptBehavior = httpPromptHandler, ..}) promptSource =
  MkBiDiCapability
    { acceptInsecureCerts = Just acceptInsecureCerts,
      browserName,
      browserVersion = Nothing,
      platformName,
      proxy,
      unhandledPromptBehavior
    }
  where
    unhandledPromptBehavior :: Maybe UserPromptHandler
    unhandledPromptBehavior = case promptSource of
      Coerce -> mkPromptHandler <$> httpPromptHandler
      Clear -> Nothing
      SetValue handler -> Just handler

    convertHandlerType :: HTTP.UnhandledPromptBehavior -> UserPromptHandlerType
    convertHandlerType = \case
      HTTP.Accept -> AcceptPrompt
      HTTP.Dismiss -> DismissPrompt
      HTTP.Ignore -> IgnorePrompt
      HTTP.DismissAndNotify -> DismissPrompt
      HTTP.AcceptAndNotify -> AcceptPrompt

    mkPromptHandler :: HTTP.UnhandledPromptBehavior -> UserPromptHandler
    mkPromptHandler hpb =
      MkUserPromptHandler
        { alert = promptHandlerType,
          beforeUnload = promptHandlerType,
          confirm = promptHandlerType,
          defaultHandler = promptHandlerType,
          fileHandler = promptHandlerType,
          prompt = promptHandlerType
        }
      where
        promptHandlerType = Just $ convertHandlerType hpb

-- ######################################################################
-- ######################## Session Management ##########################
-- ######################################################################

-- | A 'Runner' is a function that executes a 'Command' in a monadic context.
-- This allows the Extended module to work with different execution strategies.
type Runner m a = HTTP.Command a -> m a

-- | Create a new HTTP WebDriver session with the given capabilities.
--
-- Specification Entry: [HTMLSpecURL#new-session](https://www.w3.org/TR/webdriver/#new-session)
--
-- @POST \/session New Session@
newHttpSessionResponse ::
  forall m.
  (Functor m) =>
  Runner m HTTP.SessionResponse ->
  FullCapabilities HttpCapability ->
  m HttpSessionResponse
newHttpSessionResponse runner =
  fmap fromHttpSessionResponse . Actions.newSession runner . toHttpCapabilities

newHttpSession ::
  forall m.
  (Functor m) =>
  Runner m HTTP.SessionResponse ->
  FullCapabilities HttpCapability ->
  m Session
newHttpSession runner =
  fmap ((.session) . fromHttpSessionResponse) . Actions.newSession runner . toHttpCapabilities
