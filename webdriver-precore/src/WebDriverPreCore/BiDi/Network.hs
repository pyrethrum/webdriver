{-# OPTIONS_GHC -Wno-unused-imports #-}

module WebDriverPreCore.BiDi.Network
  ( -- * NetworkCommand
    AddDataCollector (..),
    AddIntercept (..),
    InterceptPhase (..),
    UrlPattern (..),
    UrlPatternPattern (..),
    UrlPatternString (..),
    ContinueRequest (..),
    RequestId (..),
    BytesValue (..),
    Cookie (..),
    SameSite (..),
    Header (..),
    ContinueResponse (..),
    ContinueWithAuth (..),
    Intercept (..),
    AuthCredentials (..),
    AuthAction (..),
    DisownData (..),
    FailRequest (..),
    GetData (..),
    ProvideResponse (..),
    RemoveDataCollector (..),
    RemoveIntercept (..),
    SetCacheBehavior (..),
    CacheBehavior (..),

    -- * Additional Network Types
    DataType (..),
    CollectorType (..),
    Collector (..),
    Request (..),

    -- * NetworkResult
    AddDataCollectorResult (..),
    AddInterceptResult (..),
    GetDataResult (..),

    -- * NetworkEvent
    NetworkEvent (..),
    AuthRequired (..),
    ResponseData (..),
    ResponseContent (..),
    AuthChallenge (..),
    BeforeRequestSent (..),
    Initiator (..),
    InitiatorType (..),
    FetchError (..),
    ResponseCompleted (..),
    ResponseStarted (..),
  )
where

-- This module provides functionality related to BiDi (Bidirectional) network operations
-- for WebDriverPreCore. It is currently a placeholder for future implementation.

-- Data structures for network protocol

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic (to))
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSUInt, UserContext, StringValue (..))
import WebDriverPreCore.BiDi.Script (StackTrace)
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase, objectOrThrow, toJSONOmitNothing)
import Prelude

-- ######### REMOTE #########

data AddDataCollector = MkAddDataCollector
  { dataTypes :: [DataType],
    maxEncodedDataSize :: JSUInt,
    collectorType :: Maybe CollectorType,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AddDataCollector

-- TODO - not sure what this is about
-- network.DataType = "response"
newtype DataType = MkDataType {dataType :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON DataType

instance ToJSON DataType

-- TODO - not sure what this is about
-- network.CollectorType = "blob"
newtype CollectorType = MkCollectorType {collectorType :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON CollectorType

instance ToJSON CollectorType

data DisownData = MkDisownData
  { dataType :: DataType,
    collector :: Collector,
    request :: Request
  }
  deriving (Show, Eq, Generic)

instance ToJSON DisownData

newtype Collector = MkCollector {collector :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON Collector

instance ToJSON Collector

newtype Request = MkRequest {request :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Request

data GetData = MkGetData
  { dataType :: DataType,
    collector :: Maybe Collector,
    disown :: Maybe Bool,
    request :: Request
  }
  deriving (Show, Eq, Generic)

instance ToJSON GetData

data RemoveDataCollector = MkRemoveDataCollector
  { collector :: Collector
  }
  deriving (Show, Eq, Generic)

instance ToJSON RemoveDataCollector

-- | AddIntercept parameters
data AddIntercept = MkAddIntercept
  { phases :: [InterceptPhase],
    contexts :: Maybe [BrowsingContext],
    urlPatterns :: Maybe [UrlPattern]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AddIntercept where
  toJSON :: AddIntercept -> Value
  toJSON = toJSONOmitNothing

-- | Intercept phases for network requests
data InterceptPhase
  = BeforeRequestSent
  | ResponseStarted
  | AuthRequired
  deriving (Show, Eq, Generic)

instance FromJSON InterceptPhase

instance ToJSON InterceptPhase where
  toJSON = enumCamelCase

data UrlPattern
  = UrlPatternPattern UrlPatternPattern
  | UrlPatternString UrlPatternString
  deriving (Show, Eq, Generic)

instance ToJSON UrlPattern where
  toJSON :: UrlPattern -> Value
  toJSON = \case
    UrlPatternPattern p -> toJSON p
    UrlPatternString s -> toJSON s

newtype UrlPatternString = MkUrlPatternString {patternString :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON UrlPatternString where
  toJSON :: UrlPatternString -> Value
  toJSON (MkUrlPatternString p) =
    object
      [ "type" .= ("string" :: Text),
        "pattern" .= p
      ]

-- | URL pattern for interception
data UrlPatternPattern = MkUrlPatternPattern
  { protocol :: Maybe Text,
    hostname :: Maybe Text,
    port :: Maybe Text,
    pathname :: Maybe Text,
    search :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UrlPatternPattern where
  toJSON :: UrlPatternPattern -> Value
  toJSON p =
    object $
      ["type" .= "pattern"]
        <> (KeyMap.toList . objectOrThrow "UrlPatternPattern" $ toJSONOmitNothing p)

-- | ContinueRequest parameters
data ContinueRequest = MkContinueRequest
  { request :: RequestId,
    body :: Maybe BytesValue,
    cookies :: Maybe [Cookie],
    headers :: Maybe [Header],
    method :: Maybe Text,
    url :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContinueRequest

newtype RequestId = MkRequestId {id :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | BytesValue can be either string or base64-encoded
data BytesValue
  = TextBytesValue StringValue
  | Base64Value Text
  deriving (Show, Eq, Generic)

instance FromJSON BytesValue

instance ToJSON BytesValue where
  toJSON :: BytesValue -> Value
  toJSON (TextBytesValue val) = toJSON val
  toJSON (Base64Value val) = object ["type" .= ("base64" :: Text), "value" .= val]

-- | Cookie information
data Cookie = MkCookie
  { name :: Text,
    value :: BytesValue,
    domain :: Text,
    path :: Text,
    size :: Word,
    httpOnly :: Bool,
    secure :: Bool,
    sameSite :: SameSite,
    expiry :: Maybe Word
  }
  deriving (Show, Eq, Generic)

instance FromJSON Cookie

instance ToJSON Cookie

data SameSite
  = Strict
  | Lax
  | None
  | Default
  deriving (Show, Eq, Generic)

instance FromJSON SameSite

instance ToJSON SameSite where
  toJSON = enumCamelCase

-- | Headers for requests and responses
data Header = MkHeader
  { headerName :: Text,
    headerValue :: BytesValue
  }
  deriving (Show, Eq, Generic)

instance FromJSON Header

instance ToJSON Header

-- | ContinueResponse parameters
data ContinueResponse = MkContinueResponse
  { request :: RequestId,
    body :: Maybe BytesValue,
    cookies :: Maybe [Cookie],
    headers :: Maybe [Header],
    reasonPhrase :: Maybe Text,
    statusCode :: Maybe JSUInt
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContinueResponse

-- | ContinueWithAuth parameters - using union type approach from spec
data ContinueWithAuth = MkContinueWithAuth
  { request :: Request,
    authAction :: AuthAction
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContinueWithAuth

-- | Auth action - matches spec's union type structure
data AuthAction
  = ProvideCredentials AuthCredentials
  | DefaultAuth
  | CancelAuth
  deriving (Show, Eq, Generic)

instance ToJSON AuthAction where
  toJSON (ProvideCredentials creds) = object ["action" .= ("provideCredentials" :: Text), "credentials" .= creds]
  toJSON DefaultAuth = object ["action" .= ("default" :: Text)]
  toJSON CancelAuth = object ["action" .= ("cancel" :: Text)]

-- | Network intercept identifier
newtype Intercept = MkIntercept Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Auth credentials for authentication
data AuthCredentials = MkAuthCredentials
  { username :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuthCredentials

-- | FailRequest parameters
data FailRequest = MkFailRequest
  { request :: Request
  }
  deriving (Show, Eq, Generic)

instance ToJSON FailRequest

-- | ProvideResponse parameters
data ProvideResponse = MkProvideResponse
  { intercept :: Intercept,
    body :: Maybe BytesValue,
    cookies :: Maybe [Cookie],
    headers :: Maybe [Header],
    reasonPhrase :: Text,
    statusCode :: Word
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProvideResponse

-- | RemoveIntercept parameters
newtype RemoveIntercept = MkRemoveIntercept
  { intercept :: Intercept
  }
  deriving (Show, Eq, Generic)

instance ToJSON RemoveIntercept

-- | SetCacheBehavior parameters
data SetCacheBehavior = MkSetCacheBehavior
  { cacheBehavior :: CacheBehavior,
    contexts :: Maybe [BrowsingContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetCacheBehavior

-- | Cache behavior options
data CacheBehavior
  = DefaultCacheBehavior
  | BypassCache
  deriving (Show, Eq, Generic)

instance ToJSON CacheBehavior where
  toJSON DefaultCacheBehavior = "default"
  toJSON BypassCache = "bypass"

-- ######### Local #########

newtype AddInterceptResult = MkAddInterceptResult
  { -- name changed to avoid conflict with field name in AddIntercept
    addedIntercept :: Intercept
  }
  deriving (Show, Eq, Generic)

instance FromJSON AddInterceptResult where
  parseJSON =
    withObject "AddInterceptResult" $ \obj ->
      MkAddInterceptResult <$> obj .: "intercept"

data NetworkEvent
  = AuthRequiredEvent AuthRequired
  | BeforeRequestSentEvent BeforeRequestSent
  | FetchError FetchError
  | ResponseCompleted ResponseCompleted
  | ResponseStartedEvent ResponseStarted
  deriving (Show, Eq, Generic)

-- | AuthRequired parameters
newtype AuthRequired = MkAuthRequired
  { authRequiredResponse :: ResponseData
  }
  deriving (Show, Eq, Generic)

-- | Response data
data ResponseData = MkResponseData
  { responseUrl :: Text,
    responseProtocol :: Text,
    responseStatus :: Word,
    responseStatusText :: Text,
    responseFromCache :: Bool,
    responseHeaders :: [Header],
    responseMimeType :: Text,
    responseBytesReceived :: Word,
    responseHeadersSize :: Maybe Word,
    responseBodySize :: Maybe Word,
    responseContent :: ResponseContent,
    responseAuthChallenges :: Maybe [AuthChallenge]
  }
  deriving (Show, Eq, Generic)

-- | Response content information
newtype ResponseContent = MkResponseContent
  { contentSize :: JSUInt
  }
  deriving (Show, Eq, Generic)

data AuthChallenge = MkAuthChallenge
  { authScheme :: Text,
    authRealm :: Text
  }
  deriving (Show, Eq, Generic)

-- | BeforeRequestSent parameters
newtype BeforeRequestSent = MkBeforeRequestSent
  { beforeRequestInitiator :: Maybe Initiator
  }
  deriving (Show, Eq, Generic)

data Initiator = MkInitiator
  { initiatorColumnNumber :: Maybe Word,
    initiatorLineNumber :: Maybe Word,
    initiatorRequest :: Maybe RequestId,
    initiatorStackTrace :: Maybe StackTrace,
    initiatorType :: Maybe InitiatorType
  }
  deriving (Show, Eq, Generic)

-- | Information about what initiated a request
data InitiatorType
  = Parser
  | Script
  | Preflight
  | Other
  deriving (Show, Eq, Generic)

-- | FetchError parameters
newtype FetchError = MkFetchError
  { fetchErrorText :: Text
  }
  deriving (Show, Eq, Generic)

-- | ResponseCompleted parameters
newtype ResponseCompleted = MkResponseCompleted
  { completedResponse :: ResponseData
  }
  deriving (Show, Eq, Generic)

-- | ResponseStarted parameters
data ResponseStarted = MkResponseStarted
  { startedResponse :: ResponseData
  }
  deriving (Show, Eq, Generic)

newtype GetDataResult = MkGetDataResult
  { bytes :: BytesValue
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetDataResult

newtype AddDataCollectorResult = MkAddDataCollectorResult
  { collector :: Collector
  }
  deriving (Show, Eq, Generic)

instance FromJSON AddDataCollectorResult
