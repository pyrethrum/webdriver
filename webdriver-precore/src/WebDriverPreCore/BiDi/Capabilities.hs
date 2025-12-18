module WebDriverPreCore.BiDi.Capabilities
  ( Capabilities (..),
    Capability (..),
    ProxyConfiguration (..),
    SocksProxyConfiguration (..),
    UserPromptHandler (..),
    UserPromptHandlerType (..),
    CapabilitiesResult (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (fromList)
import Data.Word (Word8)
import GHC.Generics (Generic)
import AesonUtils (opt, fromJSONCamelCase)
import Data.Aeson.Types (Parser)



-- | Capabilities Request
data Capabilities = MkCapabilities
  { alwaysMatch :: Maybe Capability,
    firstMatch :: [Capability]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Capabilities where
  toJSON :: Capabilities -> Value
  toJSON MkCapabilities {alwaysMatch, firstMatch} =
    object $
      [ "capabilities" .= (object $ catMaybes [opt "alwaysMatch" $ alwaysMatch]),
        "firstMatch" .= firstMatch'
      ]
    where
      firstMatch' :: Value
      firstMatch' = Array . Data.Vector.fromList $ toJSON <$> firstMatch

-- | Capability Request
data Capability = MkCapability
  { acceptInsecureCerts :: Maybe Bool,
    browserName :: Maybe Text,
    browserVersion :: Maybe Text,
    -- Always true for BiDi
    webSocketUrl :: Bool,
    platformName :: Maybe Text,
    proxy :: Maybe ProxyConfiguration,
    unhandledPromptBehavior :: Maybe UserPromptHandler
  }
  deriving (Show, Eq, Generic)

instance ToJSON Capability

-- | Proxy Configuration
data ProxyConfiguration
  = AutodetectProxyConfiguration
  | DirectProxyConfiguration
  | ManualProxyConfiguration
      { httpProxy :: Maybe Text,
        sslProxy :: Maybe Text,
        socksProxyConfig :: Maybe SocksProxyConfiguration,
        noProxy :: Maybe [Text]
      }
  | PacProxyConfiguration
      { proxyAutoconfigUrl :: Text
      }
  | SystemProxyConfiguration
  deriving (Show, Eq, Generic)

instance FromJSON ProxyConfiguration

instance ToJSON ProxyConfiguration where
  toJSON :: ProxyConfiguration -> Value
  toJSON = \case
    AutodetectProxyConfiguration -> object ["proxyType" .= ("autodetect" :: Text)]
    DirectProxyConfiguration -> object ["proxyType" .= ("direct" :: Text)]
    ManualProxyConfiguration {httpProxy, sslProxy, socksProxyConfig, noProxy} ->
      object
        [ "proxyType" .= ("manual" :: Text),
          "httpProxy" .= httpProxy,
          "sslProxy" .= sslProxy,
          "socksProxy" .= socksProxyConfig,
          "noProxy" .= noProxy
        ]
    PacProxyConfiguration proxyAutoconfigUrl ->
      object ["proxyType" .= ("pac" :: Text), "proxyAutoconfigUrl" .= proxyAutoconfigUrl]
    SystemProxyConfiguration -> object ["proxyType" .= ("system" :: Text)]

instance FromJSON SocksProxyConfiguration

instance ToJSON SocksProxyConfiguration where
  toJSON :: SocksProxyConfiguration -> Value
  toJSON (MkSocksProxyConfiguration socksProxy socksVersion) =
    object
      [ "socksProxy" .= socksProxy,
        "socksVersion" .= socksVersion
      ]

-- | Socks Proxy Configuration
data SocksProxyConfiguration = MkSocksProxyConfiguration
  { socksProxy :: Text,
    socksVersion :: Word8
  }
  deriving (Show, Eq, Generic)

-- | User Prompt Handler
data UserPromptHandler = MkUserPromptHandler
  { alert :: Maybe UserPromptHandlerType,
    beforeUnload :: Maybe UserPromptHandlerType,
    confirm :: Maybe UserPromptHandlerType,
    defaultHandler :: Maybe UserPromptHandlerType,
    fileHandler :: Maybe UserPromptHandlerType,
    prompt :: Maybe UserPromptHandlerType
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserPromptHandler

instance ToJSON UserPromptHandler where
  toJSON :: UserPromptHandler -> Value
  toJSON (MkUserPromptHandler alert beforeUnload confirm defaultHandler fileHandler prompt) =
    object
      [ "alert" .= alert,
        "beforeUnload" .= beforeUnload,
        "confirm" .= confirm,
        "defaultHandler" .= defaultHandler,
        "fileHandler" .= fileHandler,
        "prompt" .= prompt
      ]

-- | User Prompt Handler Type
data UserPromptHandlerType
  = Accept
  | Dismiss
  | Ignore
  deriving (Show, Eq, Generic)

instance FromJSON UserPromptHandlerType where
  parseJSON :: Value -> Parser UserPromptHandlerType
  parseJSON = fromJSONCamelCase

instance ToJSON UserPromptHandlerType where
  toJSON :: UserPromptHandlerType -> Value
  toJSON = \case
    Accept -> "accept"
    Dismiss -> "dismiss"
    Ignore -> "ignore"


-- | Capabilities Result
data CapabilitiesResult = MkCapabilitiesResult
  { acceptInsecureCerts :: Bool,
    browserName :: Text,
    browserVersion :: Text,
    platformName :: Text,
    setWindowRect :: Bool,
    userAgent :: Text,
    proxy :: Maybe ProxyConfiguration,
    unhandledPromptBehavior :: Maybe UserPromptHandler,
    webSocketUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CapabilitiesResult where
  toJSON :: CapabilitiesResult -> Value
  toJSON (MkCapabilitiesResult acceptInsecureCerts browserName browserVersion platformName setWindowRect userAgent proxy unhandledPromptBehavior webSocketUrl) =
    object
      [ "acceptInsecureCerts" .= acceptInsecureCerts,
        "browserName" .= browserName,
        "browserVersion" .= browserVersion,
        "platformName" .= platformName,
        "setWindowRect" .= setWindowRect,
        "userAgent" .= userAgent,
        "proxy" .= proxy,
        "unhandledPromptBehavior" .= unhandledPromptBehavior,
        "webSocketUrl" .= webSocketUrl
      ]

instance FromJSON CapabilitiesResult