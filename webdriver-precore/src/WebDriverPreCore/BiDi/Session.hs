module WebDriverPreCore.BiDi.Session where

import Data.Aeson (FromJSON (..), Key, ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (fromList)
import Data.Word (Word8)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (BiDiMethod (..))
import WebDriverPreCore.Internal.AesonUtils (emptyObj, opt)
import Prelude (Applicative ((<*>)), Bool (..), Eq (..), Maybe (..), Show (..), ($), (.), (<$>))

webSocketUrlKey :: Key
webSocketUrlKey = "webSocketUrl"

-- ######### Remote #########

data SessionCommand
  = New Capabilities
  | Status
  | Subscribe SessionSubscriptionRequest
  | Unsubscribe SessionUnsubscribeParameters
  | End
  deriving (Show, Eq, Generic)

instance BiDiMethod SessionCommand where
  bidiMethod :: SessionCommand -> Text
  bidiMethod = \case
    New _ -> "session.new"
    Status -> "session.status"
    Subscribe _ -> "session.subscribe"
    Unsubscribe _ -> "session.unsubscribe"
    End -> "session.end"

instance ToJSON SessionCommand where
  toJSON :: SessionCommand -> Value
  toJSON = \case
    New capabilities -> toJSON capabilities
    Subscribe request -> toJSON request
    Unsubscribe params -> toJSON params
    Status -> emptyObj
    End -> emptyObj

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

instance FromJSON UserPromptHandlerType

instance ToJSON UserPromptHandlerType where
  toJSON :: UserPromptHandlerType -> Value
  toJSON = \case
    Accept -> "accept"
    Dismiss -> "dismiss"
    Ignore -> "ignore"

-- | Subscription
newtype Subscription = MkSubscription {subscriptionId :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Subscription where
  toJSON :: Subscription -> Value
  toJSON (MkSubscription subId) = String subId

-- | Subscription Request
data SessionSubscriptionRequest = MkSessionSubscriptionRequest
  { events :: [Text],
    contexts :: Maybe [Text],
    userContexts :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionSubscriptionRequest

-- | Unsubscribe Parameters
data SessionUnsubscribeParameters
  = UnsubscribeByID SessionUnsubscribeByIDRequest
  | UnsubscribeByAttributes SessionUnsubscribeByAttributesRequest
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribeParameters

-- | Unsubscribe By ID Request
newtype SessionUnsubscribeByIDRequest = MkSessionUnsubscribeByIDRequest
  { subscriptions :: [Subscription]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribeByIDRequest

-- | Unsubscribe By Attributes Request
data SessionUnsubscribeByAttributesRequest = MkSessionUnsubscribeByAttributesRequest
  { unsubEvents :: [Text],
    unsubContexts :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionUnsubscribeByAttributesRequest

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
        webSocketUrlKey .= webSocketUrl
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
