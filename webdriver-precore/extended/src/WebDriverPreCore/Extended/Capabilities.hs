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

    -- * Unified Property Types
    Proxy (..),
    SocksProxyConfig (..),
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
    HTTP.Session (..),

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
    convertCapabilityToHttp,
    convertCapabilityToBiDi,

    -- * Session Management
    Runner,
    newHttpSession,
    newHttpSessionResponse,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Types (Parser)
import Data.Coerce (coerce)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import WebDriverPreCore.BiDi.Protocol qualified as BiDi
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as Actions
import WebDriverPreCore.HTTP.Protocol (Session (..))
import WebDriverPreCore.HTTP.Protocol qualified as HTTP

type HttpCapabilities = FullCapabilities HttpCapability

type BiDiCapabilities = FullCapabilities BiDiCapability

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
    unhandledPromptBehavior :: Maybe PromptAction,
    httpWebSocketUrl :: Maybe Bool,
    vendorSpecific :: Maybe HTTP.VendorSpecific
  }
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

-- | Prompt action
data PromptAction
  = Accept
  | Dismiss
  | Ignore
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

data UserPromptHandler = MkUserPromptHandler
  { alert :: Maybe PromptAction,
    beforeUnload :: Maybe PromptAction,
    confirm :: Maybe PromptAction,
    defaultHandler :: Maybe PromptAction,
    fileHandler :: Maybe PromptAction,
    prompt :: Maybe PromptAction
  }
  deriving (Show, Eq)

data BiDiSessionResponse = MkBiDiSessionResponse
  { -- TODO - make base BIDI Session use the same newtype as HTTP
    session :: HTTP.Session,
    acceptInsecureCerts :: Bool,
    browserName :: Text,
    browserVersion :: Text,
    platformName :: Text,
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

-- * Proxy Conversions

-- | Convert unified proxy to HTTP proxy
proxyToHttp :: Proxy -> HTTP.Proxy
proxyToHttp = \case
  Direct -> HTTP.Direct
  Autodetect -> HTTP.AutoDetect
  Manual {httpProxy, sslProxy, socksProxy, noProxy} ->
    HTTP.Manual
      { HTTP.httpProxy = httpProxy,
        HTTP.sslProxy = sslProxy,
        HTTP.socksProxy = socksToHttp <$> socksProxy,
        HTTP.noProxy = noProxy
      }
  Pac {proxyAutoconfigUrl} ->
    HTTP.Pac {HTTP.proxyAutoconfigUrl = proxyAutoconfigUrl}
  System -> HTTP.System

-- | Convert HTTP proxy to unified proxy
proxyFromHttp :: HTTP.Proxy -> Proxy
proxyFromHttp = \case
  HTTP.Direct -> Direct
  HTTP.AutoDetect -> Autodetect
  HTTP.Manual {HTTP.httpProxy, HTTP.sslProxy, HTTP.socksProxy, HTTP.noProxy} ->
    Manual
      { httpProxy = httpProxy,
        sslProxy = sslProxy,
        socksProxy = socksFromHttp <$> socksProxy,
        noProxy = noProxy
      }
  HTTP.Pac {HTTP.proxyAutoconfigUrl} ->
    Pac {proxyAutoconfigUrl = proxyAutoconfigUrl}
  HTTP.System -> System

-- | Convert unified proxy to BiDi proxy
proxyToBiDi :: Proxy -> BiDi.ProxyConfiguration
proxyToBiDi = \case
  Direct -> BiDi.DirectProxyConfiguration
  Autodetect -> BiDi.AutodetectProxyConfiguration
  Manual {httpProxy, sslProxy, socksProxy, noProxy} ->
    BiDi.ManualProxyConfiguration
      { BiDi.httpProxy = httpProxy,
        BiDi.sslProxy = sslProxy,
        BiDi.socksProxyConfig = socksToBiDi <$> socksProxy,
        BiDi.noProxy = noProxy
      }
  Pac {proxyAutoconfigUrl} ->
    BiDi.PacProxyConfiguration {BiDi.proxyAutoconfigUrl = proxyAutoconfigUrl}
  System -> BiDi.SystemProxyConfiguration

-- | Convert BiDi proxy to unified proxy
proxyFromBiDi :: BiDi.ProxyConfiguration -> Proxy
proxyFromBiDi = \case
  BiDi.DirectProxyConfiguration -> Direct
  BiDi.AutodetectProxyConfiguration -> Autodetect
  BiDi.ManualProxyConfiguration {BiDi.httpProxy, BiDi.sslProxy, BiDi.socksProxyConfig, BiDi.noProxy} ->
    Manual
      { httpProxy = httpProxy,
        sslProxy = sslProxy,
        socksProxy = socksFromBiDi <$> socksProxyConfig,
        noProxy = noProxy
      }
  BiDi.PacProxyConfiguration {BiDi.proxyAutoconfigUrl} ->
    Pac {proxyAutoconfigUrl = proxyAutoconfigUrl}
  BiDi.SystemProxyConfiguration -> System

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

-- | Convert prompt action to HTTP unhandled prompt behavior
promptToHttp :: PromptAction -> HTTP.UnhandledPromptBehavior
promptToHttp = promptActionToHttp

-- | Convert prompt action to HTTP behavior
promptActionToHttp :: PromptAction -> HTTP.UnhandledPromptBehavior
promptActionToHttp = \case
  Accept -> HTTP.Accept
  Dismiss -> HTTP.Dismiss
  Ignore -> HTTP.Ignore

-- TODO - review this is lossy

-- | Convert HTTP unhandled prompt behavior to prompt action
promptFromHttp :: HTTP.UnhandledPromptBehavior -> PromptAction
promptFromHttp = \case
  HTTP.Accept -> Accept
  HTTP.Dismiss -> Dismiss
  HTTP.AcceptAndNotify -> Accept
  HTTP.DismissAndNotify -> Dismiss
  HTTP.Ignore -> Ignore

-- | Convert prompt action to BiDi user prompt handler
promptActionToUserPromptHandler :: PromptAction -> UserPromptHandler
promptActionToUserPromptHandler action =
  MkUserPromptHandler
    { alert = Just action,
      beforeUnload = Just action,
      confirm = Just action,
      defaultHandler = Just action,
      fileHandler = Just action,
      prompt = Just action
    }

-- | Convert prompt action to BiDi action
promptActionToBiDi :: PromptAction -> BiDi.UserPromptHandlerType
promptActionToBiDi = \case
  Accept -> BiDi.Accept
  Dismiss -> BiDi.Dismiss
  Ignore -> BiDi.Ignore

-- | Convert BiDi user prompt handler to local user prompt handler
userPromptHandlerFromBiDi :: BiDi.UserPromptHandler -> UserPromptHandler
userPromptHandlerFromBiDi (BiDi.MkUserPromptHandler {BiDi.alert, BiDi.beforeUnload, BiDi.confirm, BiDi.defaultHandler, BiDi.fileHandler, BiDi.prompt}) =
  MkUserPromptHandler
    { alert = mapPrompt alert,
      beforeUnload = mapPrompt beforeUnload,
      confirm = mapPrompt confirm,
      defaultHandler = mapPrompt defaultHandler,
      fileHandler = mapPrompt fileHandler,
      prompt = mapPrompt prompt
    }
  where
    mapPrompt = fmap promptActionFromBiDi

-- | Convert local user prompt handler to a single prompt action
userPromptHandlerToPromptAction :: UserPromptHandler -> PromptAction
userPromptHandlerToPromptAction MkUserPromptHandler {alert, beforeUnload, confirm, defaultHandler, fileHandler, prompt} =
  fromMaybe Dismiss (defaultHandler <|> alert <|> beforeUnload <|> confirm <|> fileHandler <|> prompt)

-- | Convert local user prompt handler to BiDi prompt handler
userPromptHandlerToBiDi :: UserPromptHandler -> BiDi.UserPromptHandler
userPromptHandlerToBiDi MkUserPromptHandler {alert, beforeUnload, confirm, defaultHandler, fileHandler, prompt} =
  BiDi.MkUserPromptHandler
    { BiDi.alert = mapPrompt alert,
      BiDi.beforeUnload = mapPrompt beforeUnload,
      BiDi.confirm = mapPrompt confirm,
      BiDi.defaultHandler = mapPrompt defaultHandler,
      BiDi.fileHandler = mapPrompt fileHandler,
      BiDi.prompt = mapPrompt prompt
    }
  where
    mapPrompt = fmap promptActionToBiDi

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

-- | Convert local HTTP capability to native HTTP capabilities
toHttpCapability :: HttpCapability -> HTTP.Capabilities
toHttpCapability (MkHttpCapability {..}) =
  HTTP.MkCapabilities
    { HTTP.browserName = browserNameToText <$> browserName,
      -- response only
      HTTP.browserVersion = Nothing,
      HTTP.platformName = platformNameToText <$> platformName,
      HTTP.acceptInsecureCerts = Just acceptInsecureCerts,
      HTTP.pageLoadStrategy = pageLoadToHttp <$> pageLoadStrategy,
      HTTP.proxy = proxyToHttp <$> proxy,
      -- response only
      HTTP.setWindowRect = Nothing,
      HTTP.timeouts = timeouts,
      HTTP.strictFileInteractability = strictFileInteractability,
      HTTP.unhandledPromptBehavior = promptToHttp <$> unhandledPromptBehavior,
      HTTP.webSocketUrl = httpWebSocketUrl,
      HTTP.vendorSpecific = vendorSpecific
    }

-- | Convert local BiDi capability to native BiDi capability
toBiDiCapability :: BiDiCapability -> BiDi.Capability
toBiDiCapability (MkBiDiCapability {..}) =
  BiDi.MkCapability
    { BiDi.acceptInsecureCerts = acceptInsecureCerts,
      BiDi.browserName = browserNameToText <$> browserName,
      BiDi.browserVersion = browserVersion,
      BiDi.platformName = platformNameToText <$> platformName,
      BiDi.proxy = proxyToBiDi <$> proxy,
      BiDi.unhandledPromptBehavior = userPromptHandlerToBiDi <$> unhandledPromptBehavior
    }

-- * Response Conversions

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
          { browserName = textToBrowserName <$> browserName,
            platformName = textToPlatformName <$> platformName,
            acceptInsecureCerts = fromMaybe False acceptInsecureCerts,
            pageLoadStrategy = pageLoadFromHttp <$> pageLoadStrategy,
            proxy = proxyFromHttp <$> proxy,
            timeouts = timeouts,
            strictFileInteractability = strictFileInteractability,
            unhandledPromptBehavior = promptFromHttp <$> unhandledPromptBehavior,
            httpWebSocketUrl = webSocketUrl,
            vendorSpecific = vendorSpecific
          }
    }

-- | Convert native BiDi session response to local BiDi session response
fromBiDiSessionResponse :: BiDi.SessionNewResult -> BiDiSessionResponse
fromBiDiSessionResponse (BiDi.MkSessionNewResult {sessionId = session, capabilities = BiDi.MkCapabilitiesResult {..}}) =
  MkBiDiSessionResponse
    { session,
      acceptInsecureCerts = acceptInsecureCerts,
      browserName = browserName,
      browserVersion = browserVersion,
      platformName = platformName,
      setWindowRect = setWindowRect,
      userAgent = userAgent,
      proxy = proxyFromBiDi <$> proxy,
      unhandledPromptBehavior = userPromptHandlerFromBiDi <$> unhandledPromptBehavior,
      webSocketUrl = webSocketUrl
    }

-- * Cross-Protocol Conversions

-- | Convert BiDi capability to local HTTP capability (potentially lossy)
convertCapabilityToHttp :: BiDiCapability -> HttpCapability
convertCapabilityToHttp (MkBiDiCapability {..}) =
  MkHttpCapability
    { browserName = browserName,
      platformName = platformName,
      acceptInsecureCerts = fromMaybe False acceptInsecureCerts,
      pageLoadStrategy = Nothing,
      proxy = proxy,
      timeouts = Nothing,
      strictFileInteractability = Nothing,
      unhandledPromptBehavior = userPromptHandlerToPromptAction <$> unhandledPromptBehavior,
      httpWebSocketUrl = Nothing,
      vendorSpecific = Nothing
    }

-- | Convert HTTP capability to local BiDi capability (potentially lossy)
convertCapabilityToBiDi :: HttpCapability -> BiDiCapability
convertCapabilityToBiDi (MkHttpCapability {..}) =
  MkBiDiCapability
    { acceptInsecureCerts = Just acceptInsecureCerts,
      browserName = browserName,
      browserVersion = Nothing,
      platformName = platformName,
      proxy = proxy,
      unhandledPromptBehavior = promptActionToUserPromptHandler <$> unhandledPromptBehavior
    }

-- * Full Capabilities Conversions

-- | Convert universal full capabilities to HTTP full capabilities
toHttpCapabilities :: FullCapabilities HttpCapability -> HTTP.FullCapabilities
toHttpCapabilities (MkFullCapabilities {..}) =
  HTTP.MkFullCapabilities
    { HTTP.alwaysMatch = toHttpCapability <$> alwaysMatch,
      HTTP.firstMatch = toHttpCapability <$> firstMatch
    }

-- | Convert universal full capabilities to BiDi capabilities
toBiDiCapabilities :: FullCapabilities BiDiCapability -> BiDi.Capabilities
toBiDiCapabilities (MkFullCapabilities {..}) =
  BiDi.MkCapabilities
    { BiDi.alwaysMatch = toBiDiCapability <$> alwaysMatch,
      BiDi.firstMatch = toBiDiCapability <$> firstMatch
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
