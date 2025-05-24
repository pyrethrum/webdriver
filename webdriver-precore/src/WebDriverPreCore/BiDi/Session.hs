module WebDriverPreCore.BiDi.Session where

import Data.Text (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Prelude (Bool (..), Eq (..), Maybe (..), Show (..), undefined, ($), (<$>), Applicative ((<*>)))
import Data.Aeson (Encoding, ToJSON (..), object, (.=), Value, FromJSON (..), withObject, (.:))
import Data.ByteString (ByteString)
import Data.Aeson.Types (Parser)

-- webSocketUrl :: https://www.w3.org/TR/2025/WD-webdriver-bidi-20250514/#establishing

sessionNew :: Capabilities -> Encoding
sessionNew = undefined

sessionNewResult :: ByteString -> SessionNewResult
sessionNewResult = undefined

-- ######### Remote #########
data SessionCommand
  = SessionEnd
  | SessionNew Capabilities
  | SessionStatus
  | SessionSubscribe SessionSubscriptionRequest
  | SessionUnsubscribe SessionUnsubscribeParameters
  deriving (Show, Eq, Generic)


-- | Capabilities Request
data Capabilities = MkCapabilities
  { alwaysMatch :: Maybe Capability,
    firstMatch :: Maybe [Capability]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Capabilities where
  toJSON :: Capabilities -> Value
  toJSON (MkCapabilities alwaysMatch firstMatch) =
    object [ "alwaysMatch" .= alwaysMatch
           , "firstMatch" .= firstMatch
           ]

-- | Capability Request
data Capability = MkCapability
  { acceptInsecureCerts :: Maybe Bool,
    browserName :: Maybe Text,
    browserVersion :: Maybe Text,
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
      { ftpProxy :: Maybe Text,
        httpProxy :: Maybe Text,
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
    ManualProxyConfiguration ftpProxy httpProxy sslProxy socksProxyConfig noProxy ->
      object
        [ "proxyType" .= ("manual" :: Text),
          "ftpProxy" .= ftpProxy,
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

instance FromJSON UserPromptHandlerType 
instance ToJSON UserPromptHandlerType where
  toJSON :: UserPromptHandlerType -> Value
  toJSON = \case
    Accept -> "accept"
    Dismiss -> "dismiss"
    Ignore -> "ignore"

-- | Subscription
newtype Subscription = MkSubscription Text
  deriving (Show, Eq, Generic)

-- | Subscription Request
data SessionSubscriptionRequest = MkSessionSubscriptionRequest
  { events :: [Text],
    contexts :: Maybe [Text],
    userContexts :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

-- | Unsubscribe Parameters
data SessionUnsubscribeParameters
  = UnsubscribeByID SessionUnsubscribeByIDRequest
  | UnsubscribeByAttributes SessionUnsubscribeByAttributesRequest
  deriving (Show, Eq, Generic)

-- | Unsubscribe By ID Request
newtype SessionUnsubscribeByIDRequest = MkSessionUnsubscribeByIDRequest
  { subscriptions :: [Subscription]
  }
  deriving (Show, Eq, Generic)

-- | Unsubscribe By Attributes Request
data SessionUnsubscribeByAttributesRequest = MkSessionUnsubscribeByAttributesRequest
  { unsubEvents :: [Text],
    unsubContexts :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

-- ######### Local #########

-- | Session Result
data SessionResult
  = SessionNewResult SessionNewResult
  | SessionStatusResult SessionStatusResult
  | SessionSubscribeResult SessionSubscribeResult
  deriving (Show, Eq, Generic)

-- | Session New Result
data SessionNewResult = MkSessionNewResult
  { sessionId :: Text,
    capabilities :: CapabilitiesResult
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionNewResult where
  toJSON :: SessionNewResult -> Value
  toJSON (MkSessionNewResult sessionId capabilities) =
    object
      [ "sessionId" .= sessionId,
        "capabilities" .= capabilities
      ]

instance FromJSON SessionNewResult where
  parseJSON :: Value -> Parser SessionNewResult
  parseJSON = withObject "SessionNewResult" $ \v ->
    MkSessionNewResult
      <$> v .: "sessionId"
      <*> v .: "capabilities"

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

-- | Session Status Result
data SessionStatusResult = MkSessionStatusResult
  { ready :: Bool,
    message :: Text
  }
  deriving (Show, Eq, Generic)

-- | Session Subscribe Result
newtype SessionSubscribeResult = MkSessionSubscribeResult
  { subscription :: Subscription
  }
  deriving (Show, Eq, Generic)
