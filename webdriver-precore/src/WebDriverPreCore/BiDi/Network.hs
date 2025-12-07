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
    BytesValue (..),
    Cookie (..),
    SameSite (..),
    Header (..),
    CookieHeader (..),
    SetCookieHeader (..),
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
    SetExtraHeaders (..),

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
    RequestData (..),
    ResponseData (..),
    ResponseContent (..),
    AuthChallenge (..),
    BeforeRequestSent (..),
    Initiator (..),
    InitiatorType (..),
    FetchError (..),
    ResponseCompleted (..),
    ResponseStarted (..),
    HTTPMethod (..),
    FetchTimingInfo (..),
  )
where

-- This module provides functionality related to BiDi (Bidirectional) network operations
-- for WebDriverPreCore. It is currently a placeholder for future implementation.

-- Data structures for network protocol

import Data.Aeson (FromArgs, FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.=), Options (..), defaultOptions, genericParseJSON, (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parse)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text, toLower)
import GHC.Generics (Generic (to))
import WebDriverPreCore.BiDi.BrowsingContext (Navigation)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSUInt, StringValue (..), SubscriptionType (..), UserContext, KnownSubscriptionType (..), URL)
import WebDriverPreCore.BiDi.Script (StackTrace)
import AesonUtils (addProps, enumCamelCase, fromJSONCamelCase, objectOrThrow, parseJSONOmitNothing, toJSONOmitNothing)

-- ######### REMOTE #########

data AddDataCollector = MkAddDataCollector
  { dataTypes :: [DataType],
    maxEncodedDataSize :: JSUInt,
    collectorType :: Maybe CollectorType,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON AddDataCollector where
  toJSON :: AddDataCollector -> Value
  toJSON = toJSONOmitNothing


data DataType = Request | Response
  deriving (Show, Eq, Generic)

instance ToJSON DataType where
  toJSON :: DataType -> Value
  toJSON =  enumCamelCase

instance FromJSON DataType where
  parseJSON :: Value -> Parser DataType
  parseJSON = fromJSONCamelCase
-- TODO - not sure what this is about
-- network.CollectorType = "blob"
newtype CollectorType = MkCollectorType {collectorType :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data DisownData = MkDisownData
  { dataType :: DataType,
    collector :: Collector,
    request :: Request
  }
  deriving (Show, Eq, Generic)

instance ToJSON DisownData

newtype Collector = MkCollector {collector :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Request = MkRequest {request :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON,ToJSON)

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
  toJSON p = addProps "UrlPatternPattern" ["type" .= "pattern"] $ toJSONOmitNothing p

-- | ContinueRequest parameters
data ContinueRequest = MkContinueRequest
  { request :: Request,
    body :: Maybe BytesValue,
    cookies :: Maybe [CookieHeader],
    headers :: Maybe [Header],
    method :: Maybe Text,
    url :: Maybe URL
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContinueRequest where
  toJSON :: ContinueRequest -> Value
  toJSON = toJSONOmitNothing

data CookieHeader = MkCookieHeader
  { cookieHeaderName :: Text,
    cookieHeaderValue :: BytesValue
  }
  deriving (Show, Eq, Generic)

instance ToJSON CookieHeader where
  toJSON :: CookieHeader -> Value
  toJSON h =
    object
      [ "name" .= h.cookieHeaderName,
        "value" .= h.cookieHeaderValue
      ]

instance FromJSON CookieHeader where
  parseJSON :: Value -> Parser CookieHeader
  parseJSON = withObject "CookieHeader" $ \obj -> do
    cookieHeaderName <- obj .: "name"
    cookieHeaderValue <- obj .: "value"
    pure $ MkCookieHeader {cookieHeaderName, cookieHeaderValue}

-- | BytesValue can be either string or base64-encoded
data BytesValue
  = TextBytesValue StringValue
  | Base64Value Text
  deriving (Show, Eq, Generic)

instance FromJSON BytesValue where
  parseJSON :: Value -> Parser BytesValue
  parseJSON v = do
    obj <- withObject "BytesValue" pure v
    typ <- obj .: "type"
    case typ of
      "string" -> TextBytesValue <$> parseJSON v
      "base64" -> Base64Value <$> obj .: "value"
      _ -> fail $ "Invalid BytesValue type: " <> show typ

instance ToJSON BytesValue where
  toJSON :: BytesValue -> Value
  toJSON = \case
    TextBytesValue val ->
      object
        [ "type" .= "string",
          "value" .= val
        ]
    Base64Value val ->
      object
        [ "type" .= "base64",
          "value" .= val
        ]

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
  | SameSiteNone
  | Default
  deriving (Show, Eq, Generic)

instance FromJSON SameSite where
  parseJSON :: Value -> Parser SameSite
  parseJSON =
    withText "SameSite" $ \case
      "strict" -> pure Strict
      "lax" -> pure Lax
      "none" -> pure SameSiteNone
      "default" -> pure Default
      _ -> fail "Invalid SameSite value"

instance ToJSON SameSite where
  toJSON :: SameSite -> Value
  toJSON = \case
    Strict -> "strict"
    Lax -> "lax"
    SameSiteNone -> "none"
    Default -> "default"

-- | Headers for requests and responses
data Header = MkHeader
  { headerName :: Text,
    headerValue :: BytesValue
  }
  deriving (Show, Eq, Generic)

instance FromJSON Header where
  parseJSON :: Value -> Parser Header
  parseJSON = withObject "Header" $ \obj -> do
    headerName <- obj .: "name"
    headerValue <- obj .: "value"
    pure $ MkHeader {headerName, headerValue}

instance ToJSON Header where
  toJSON :: Header -> Value
  toJSON h =
    object
      [ "type" .= "network.Header",
        "name" .= h.headerName,
        "value" .= h.headerValue
      ]

-- | ContinueResponse parameters
data ContinueResponse = MkContinueResponse
  { request :: Request,
    cookies :: Maybe [SetCookieHeader],
    credentials :: Maybe AuthCredentials,
    headers :: Maybe [Header],
    reasonPhrase :: Maybe Text,
    statusCode :: Maybe JSUInt
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContinueResponse where
  toJSON :: ContinueResponse -> Value
  toJSON = toJSONOmitNothing

-- | Partial cookie for setting
data SetCookieHeader = MkSetCookieHeader
  { name :: Text,
    value :: BytesValue,
    domain :: Maybe Text,
    httpOnly :: Maybe Bool,
    expiry :: Maybe Text,
    maxAge :: Maybe JSUInt,
    path :: Maybe Text,
    secure :: Maybe Bool,
    sameSite :: Maybe SameSite
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetCookieHeader where
  toJSON :: SetCookieHeader -> Value
  toJSON = toJSONOmitNothing

-- | ContinueWithAuth parameters - using union type approach from spec
data ContinueWithAuth = MkContinueWithAuth
  { request :: Request,
    authAction :: AuthAction
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContinueWithAuth where
  toJSON :: ContinueWithAuth -> Value
  toJSON (MkContinueWithAuth req action) =
    addProps "ContinueWithAuth" ["request" .= req] $ toJSON action

-- | Auth action - matches spec's union type structure
data AuthAction
  = ProvideCredentials AuthCredentials
  | DefaultAuth
  | CancelAuth
  deriving (Show, Eq, Generic)

instance ToJSON AuthAction where
  toJSON :: AuthAction -> Value
  toJSON (ProvideCredentials creds) = object ["action" .= "provideCredentials", "credentials" .= creds]
  toJSON DefaultAuth = object ["action" .= "default"]
  toJSON CancelAuth = object ["action" .= "cancel"]

-- | Network intercept identifier
newtype Intercept = MkIntercept Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Auth credentials for authentication
data AuthCredentials = MkAuthCredentials
  { username :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuthCredentials where
  toJSON :: AuthCredentials -> Value
  toJSON (MkAuthCredentials user pass) =
    object
      [ "type" .= "password",
        "username" .= user,
        "password" .= pass
      ]

-- | FailRequest parameters
data FailRequest = MkFailRequest
  { request :: Request
  }
  deriving (Show, Eq, Generic)

instance ToJSON FailRequest

-- | ProvideResponse parameters
data ProvideResponse = MkProvideResponse
  { request :: Request,
    intercept :: Intercept,
    body :: Maybe BytesValue,
    cookies :: Maybe [Cookie],
    headers :: Maybe [Header],
    reasonPhrase :: Text,
    statusCode :: Word
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProvideResponse where
  toJSON :: ProvideResponse -> Value
  toJSON = toJSONOmitNothing

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
  toJSON :: CacheBehavior -> Value
  toJSON DefaultCacheBehavior = "default"
  toJSON BypassCache = "bypass"

-- | SetExtraHeaders parameters
data SetExtraHeaders = MkSetExtraHeaders
  { headers :: [Header],
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetExtraHeaders where
  toJSON :: SetExtraHeaders -> Value
  toJSON = toJSONOmitNothing

-- ######### Local #########

newtype AddInterceptResult = MkAddInterceptResult
  { -- name changed to avoid conflict with field name in AddIntercept
    addedIntercept :: Intercept
  }
  deriving (Show, Eq, Generic)

instance FromJSON AddInterceptResult where
  parseJSON :: Value -> Parser AddInterceptResult
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

instance FromJSON NetworkEvent where
  parseJSON :: Value -> Parser NetworkEvent
  parseJSON val =
    val
      & ( withObject "NetworkEvent" $ \obj -> do
            eventType <- obj .: "method"
            params <- obj .: "params"
            let parseParams :: forall a b. (FromJSON a) => (a -> b) -> Parser b
                parseParams = (<&>) (parseJSON params)
            case eventType of
              NetworkAuthRequired -> parseParams AuthRequiredEvent
              NetworkBeforeRequestSent -> parseParams BeforeRequestSentEvent
              NetworkFetchError -> parseParams FetchError
              NetworkResponseCompleted -> parseParams ResponseCompleted
              NetworkResponseStarted -> parseParams ResponseStartedEvent
              _ -> fail $ "Unknown NetworkEvent type: " <> show eventType
        )


data HTTPMethod
  = GET
  | POST
  | PUT
  | DELETE
  | HEAD
  | OPTIONS
  | PATCH
  | TRACE
  | CONNECT
  deriving (Show, Eq, Generic)

instance FromJSON HTTPMethod where
  parseJSON :: Value -> Parser HTTPMethod
  parseJSON = withText "Method" $ \t ->
    case toLower t of
      "get" -> pure GET
      "post" -> pure POST
      "put" -> pure PUT
      "delete" -> pure DELETE
      "head" -> pure HEAD
      "options" -> pure OPTIONS
      "patch" -> pure PATCH
      "trace" -> pure TRACE
      "connect" -> pure CONNECT
      _ -> fail $ "Unknown HTTP method: " <> show t

data RequestData = MkRequestData
  { request :: Request,
    url :: Text,
    method :: HTTPMethod,
    headers :: [Header],
    -- not as per spec but geckodriver appears to be returning cookie headers rather than cookies
    cookies :: Maybe [CookieHeader],
    -- cookies :: Maybe [Cookie],
    headersSize :: JSUInt,
    bodySize :: Maybe JSUInt,
    destination :: Text,
    initiatorType :: Maybe InitiatorType,
    timings :: Maybe FetchTimingInfo
  }
  deriving (Show, Eq, Generic)

instance FromJSON RequestData where
  parseJSON :: Value -> Parser RequestData
  parseJSON = parseJSONOmitNothing

data FetchTimingInfo = MkFetchTimingInfo
  { timeOrigin :: Double,
    requestTime :: Double,
    redirectStart :: Double,
    redirectEnd :: Double,
    fetchStart :: Double,
    dnsStart :: Double,
    dnsEnd :: Double,
    connectStart :: Double,
    connectEnd :: Double,
    tlsStart :: Double,
    requestStart :: Double,
    responseStart :: Double,
    responseEnd :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON FetchTimingInfo

-- | AuthRequired parameters
data AuthRequired = MkAuthRequired
  { context :: BrowsingContext,
    isBlocked :: Bool,
    navigation :: Maybe Navigation,
    redirectCount :: JSUInt,
    request :: RequestData,
    timestamp :: JSUInt,
    intercepts :: Maybe [Intercept],
    response :: ResponseData
  }
  deriving (Show, Eq, Generic)

instance FromJSON AuthRequired

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

instance FromJSON ResponseData where
  parseJSON :: Value -> Parser ResponseData
  parseJSON = withObject "ResponseData" $ \obj -> do
    responseUrl <- obj .: "url"
    responseProtocol <- obj .: "protocol"
    responseStatus <- obj .: "status"
    responseStatusText <- obj .: "statusText"
    responseFromCache <- obj .: "fromCache"
    responseHeaders <- obj .: "headers"
    responseMimeType <- obj .: "mimeType"
    responseBytesReceived <- obj .: "bytesReceived"
    responseHeadersSize <- obj .:? "headersSize"
    responseBodySize <- obj .:? "bodySize"
    responseContent <- obj .: "content"
    responseAuthChallenges <- obj .:? "authChallenges"
    pure MkResponseData {..}

-- | Response content information
newtype ResponseContent = MkResponseContent
  { size :: JSUInt
  }
  deriving (Show, Eq, Generic)

instance FromJSON ResponseContent

data AuthChallenge = MkAuthChallenge
  { scheme :: Text,
    realm :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AuthChallenge

-- | BeforeRequestSent parameters
data BeforeRequestSent = MkBeforeRequestSent
  { context :: BrowsingContext,
    isBlocked :: Bool,
    navigation :: Maybe Navigation,
    redirectCount :: JSUInt,
    request :: RequestData,
    timestamp :: JSUInt,
    intercepts :: Maybe [Intercept],
    beforeRequestInitiator :: Maybe Initiator
  }
  deriving (Show, Eq, Generic)

instance FromJSON BeforeRequestSent where
  parseJSON :: Value -> Parser BeforeRequestSent
  parseJSON = parseJSONOmitNothing

data Initiator = MkInitiator
  { initiatorColumnNumber :: Maybe Word,
    initiatorLineNumber :: Maybe Word,
    initiatorRequest :: Maybe Request,
    initiatorStackTrace :: Maybe StackTrace,
    initiatorType :: Maybe InitiatorType
  }
  deriving (Show, Eq, Generic)

instance FromJSON Initiator where
  parseJSON :: Value -> Parser Initiator
  parseJSON = parseJSONOmitNothing

-- | Information about what initiated a request
data InitiatorType
  = Audio
  | Beacon
  | Body
  | CSSInitiatorType
  | EarlyHints
  | Embed
  | Fetch
  | Font
  | Frame
  | IFrame
  | Image
  | Img
  | Input
  | Link
  | ObjectElement
  | Ping
  | Script
  | Track
  | Video
  | XMLHttpRequest
  | Other
  deriving (Show, Eq, Generic)

instance FromJSON InitiatorType where
  parseJSON :: Value -> Parser InitiatorType
  parseJSON = withText "InitiatorType" $ \t ->
    case toLower t of
      "audio" -> pure Audio
      "beacon" -> pure Beacon
      "body" -> pure Body
      "css" -> pure CSSInitiatorType
      "early-hints" -> pure EarlyHints
      "embed" -> pure Embed
      "fetch" -> pure Fetch
      "font" -> pure Font
      "frame" -> pure Frame
      "iframe" -> pure IFrame
      "image" -> pure Image
      "img" -> pure Img
      "input" -> pure Input
      "link" -> pure Link
      "object" -> pure ObjectElement
      "ping" -> pure Ping
      "script" -> pure Script
      "track" -> pure Track
      "video" -> pure Video
      "xmlhttprequest" -> pure XMLHttpRequest
      "other" -> pure Other
      _ -> fail $ "Unknown InitiatorType: " <> show t

-- | FetchError parameters
data FetchError = MkFetchError
  { context :: BrowsingContext,
    isBlocked :: Bool,
    navigation :: Maybe Navigation,
    redirectCount :: JSUInt,
    request :: RequestData,
    timestamp :: JSUInt,
    intercepts :: Maybe [Intercept],
    errorText :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON FetchError where
  parseJSON :: Value -> Parser FetchError
  parseJSON = parseJSONOmitNothing

-- | ResponseCompleted parameters
data ResponseCompleted = MkResponseCompleted
  { context :: BrowsingContext,
    isBlocked :: Bool,
    navigation :: Maybe Navigation,
    redirectCount :: JSUInt,
    request :: RequestData,
    timestamp :: JSUInt,
    intercepts :: Maybe [Intercept],
    response :: ResponseData
  }
  deriving (Show, Eq, Generic)

instance FromJSON ResponseCompleted where
  parseJSON :: Value -> Parser ResponseCompleted
  parseJSON = parseJSONOmitNothing

-- | ResponseStarted parameters
data ResponseStarted = MkResponseStarted
  { context :: BrowsingContext,
    isBlocked :: Bool,
    navigation :: Maybe Navigation,
    redirectCount :: JSUInt,
    request :: RequestData,
    timestamp :: JSUInt,
    intercepts :: Maybe [Intercept],
    response :: ResponseData
  }
  deriving (Show, Eq, Generic)

instance FromJSON ResponseStarted where
  parseJSON :: Value -> Parser ResponseStarted
  parseJSON = parseJSONOmitNothing

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
