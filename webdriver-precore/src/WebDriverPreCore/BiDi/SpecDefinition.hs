{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module WebDriver.BidDi.SpecDefinition where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), object)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

-- | Common types used across commands

-- | BiDi command IDs are sequential integers
type CommandId = Integer

-- | BiDi messages have a standard format
data BidiRequest = BidiRequest
  { requestId :: CommandId
  , requestMethod :: Text
  , requestParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON BidiRequest where
  toJSON BidiRequest{..} = object $
    [ "id" .= requestId
    , "method" .= requestMethod
    ] ++ case requestParams of
          Just params -> ["params" .= params]
          Nothing -> []

-- | BiDi response structure
data BidiResponse = BidiResponse
  { responseId :: CommandId
  , responseResult :: Value
  } deriving (Show, Eq, Generic)

instance FromJSON BidiResponse where
  parseJSON = \case
    Object o -> BidiResponse
      <$> o .: "id"
      <*> o .: "result"
    _ -> fail "Expected object for BidiResponse"

-- | Error structure
data BidiError = BidiError
  { errorCode :: Text
  , errorMessage :: Text
  , errorStacktrace :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON BidiError where
  parseJSON = \case
    Object o -> BidiError
      <$> o .: "error"
      <*> o .: "message"
      <*> o .:? "stacktrace"
    _ -> fail "Expected object for BidiError"

-- | BiDi error response
data BidiErrorResponse = BidiErrorResponse
  { errorResponseId :: CommandId
  , errorDetails :: BidiError
  } deriving (Show, Eq, Generic)

instance FromJSON BidiErrorResponse where
  parseJSON = \case
    Object o -> BidiErrorResponse
      <$> o .: "id"
      <*> parseJSON (Object o)
    _ -> fail "Expected object for BidiErrorResponse"

-- | Universal command result parser
data BidiCommandResult a
  = BidiSuccess a
  | BidiFailure BidiError
  deriving (Show, Eq)

-- | ScriptEvaluationResult type for script evaluation
data ScriptEvaluationResultType 
  = LocalValue
  | RemoteValue
  | Exception
  deriving (Show, Eq, Generic)

instance FromJSON ScriptEvaluationResultType where
  parseJSON = \case
    String "local-value" -> pure LocalValue
    String "remote-value" -> pure RemoteValue
    String "exception" -> pure Exception
    _ -> fail "Invalid ScriptEvaluationResultType"

instance ToJSON ScriptEvaluationResultType where
  toJSON = \case
    LocalValue -> String "local-value"
    RemoteValue -> String "remote-value" 
    Exception -> String "exception"

-- | Remote reference type
data RemoteReference = RemoteReference
  { remoteRefHandle :: Text
  , remoteRefShared :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON RemoteReference where
  parseJSON = \case
    Object o -> RemoteReference
      <$> o .: "handle"
      <*> o .: "shared"
    _ -> fail "Expected object for RemoteReference"

instance ToJSON RemoteReference where
  toJSON RemoteReference{..} = object
    [ "handle" .= remoteRefHandle
    , "shared" .= remoteRefShared
    ]

-- | RemoteValue type
data RemoteValue = RemoteValue
  { remoteValueType :: Text
  , remoteValueValue :: Maybe Value
  , remoteValueInternalId :: Maybe Text
  , remoteValueHandle :: Maybe Text
  , remoteValueShared :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance FromJSON RemoteValue where
  parseJSON = \case
    Object o -> RemoteValue
      <$> o .: "type"
      <*> o .:? "value"
      <*> o .:? "internalId"
      <*> o .:? "handle"
      <*> o .:? "shared"
    _ -> fail "Expected object for RemoteValue"

instance ToJSON RemoteValue where
  toJSON RemoteValue{..} = object $ 
    [ "type" .= remoteValueType ]
    ++ maybe [] (\v -> ["value" .= v]) remoteValueValue
    ++ maybe [] (\v -> ["internalId" .= v]) remoteValueInternalId
    ++ maybe [] (\v -> ["handle" .= v]) remoteValueHandle
    ++ maybe [] (\v -> ["shared" .= v]) remoteValueShared

-- | ScriptEvaluationResult type
data ScriptEvaluationResult = ScriptEvaluationResult
  { resultType :: ScriptEvaluationResultType
  , resultValue :: Maybe Value
  , resultExceptionDetails :: Maybe Value
  , resultRealm :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ScriptEvaluationResult where
  parseJSON = \case
    Object o -> ScriptEvaluationResult
      <$> o .: "type"
      <*> o .:? "value"
      <*> o .:? "exceptionDetails"
      <*> o .:? "realm"
    _ -> fail "Expected object for ScriptEvaluationResult"

-- | Navigation target types
data NavigationTarget
  = UrlNavigationTarget Text
  | FragmentNavigationTarget Text
  deriving (Show, Eq)

-- | Root context type
data BrowsingContext = BrowsingContext 
  { contextId :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON BrowsingContext where
  parseJSON = \case
    Object o -> BrowsingContext <$> o .: "context"
    _ -> fail "Expected object for BrowsingContext"

instance ToJSON BrowsingContext where
  toJSON BrowsingContext{..} = object ["context" .= contextId]

-- | Navigation behavior/reload options
data NavigationBehavior = NavigationBehavior
  { waitForLoad :: Maybe Text
  , waitTimeout :: Maybe Scientific
  } deriving (Show, Eq, Generic)

instance ToJSON NavigationBehavior where
  toJSON NavigationBehavior{..} = object $
    maybe [] (\w -> ["wait" .= w]) waitForLoad ++
    maybe [] (\t -> ["timeout" .= t]) waitTimeout

-- | Command definitions for each BiDi command

-- | browsingContext.create command
data CreateContextParams = CreateContextParams
  { createType :: Text
  , createReferenceContext :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateContextParams where
  toJSON CreateContextParams{..} = object $
    [ "type" .= createType ]
    ++ maybe [] (\rc -> ["referenceContext" .= rc]) createReferenceContext

data CreateContextResult = CreateContextResult
  { createdContext :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateContextResult where
  parseJSON = \case
    Object o -> CreateContextResult <$> o .: "context"
    _ -> fail "Expected object for CreateContextResult"

mkCreateContext :: CreateContextParams -> (BidiRequest, Value -> Parser CreateContextResult)
mkCreateContext params = 
  ( BidiRequest 0 "browsingContext.create" (Just $ toJSON params)
  , \v -> case v of
      Object o -> parseJSON v
      _ -> fail "Expected object for CreateContextResult"
  )

-- | browsingContext.navigate command
data NavigateParams = NavigateParams
  { navContext :: Text
  , navUrl :: Text
  , navBehavior :: Maybe NavigationBehavior
  } deriving (Show, Eq, Generic)

instance ToJSON NavigateParams where
  toJSON NavigateParams{..} = object $
    [ "context" .= navContext
    , "url" .= navUrl
    ] ++ maybe [] (\b -> ["wait" .= toJSON b]) navBehavior

data NavigateResult = NavigateResult
  { navigationId :: Text
  , url :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON NavigateResult where
  parseJSON = \case
    Object o -> NavigateResult
      <$> o .: "navigation"
      <*> o .: "url"
    _ -> fail "Expected object for NavigateResult"

mkNavigate :: NavigateParams -> (BidiRequest, Value -> Parser NavigateResult)
mkNavigate params =
  ( BidiRequest 0 "browsingContext.navigate" (Just $ toJSON params)
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for NavigateResult"
  )

-- | browsingContext.reload command
data ReloadParams = ReloadParams
  { reloadContext :: Text
  , reloadBehavior :: Maybe NavigationBehavior
  } deriving (Show, Eq, Generic)

instance ToJSON ReloadParams where
  toJSON ReloadParams{..} = object $
    [ "context" .= reloadContext
    ] ++ maybe [] (\b -> ["wait" .= toJSON b]) reloadBehavior

data ReloadResult = ReloadResult
  { reloadNavigationId :: Text
  , reloadUrl :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON ReloadResult where
  parseJSON = \case
    Object o -> ReloadResult
      <$> o .: "navigation"
      <*> o .: "url"
    _ -> fail "Expected object for ReloadResult"

mkReload :: ReloadParams -> (BidiRequest, Value -> Parser ReloadResult)
mkReload params =
  ( BidiRequest 0 "browsingContext.reload" (Just $ toJSON params)
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for ReloadResult"
  )

-- | browsingContext.getTree command
data GetTreeParams = GetTreeParams
  { treeMaxDepth :: Maybe Int
  , treeRoot :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON GetTreeParams where
  toJSON GetTreeParams{..} = object $
    maybe [] (\d -> ["maxDepth" .= d]) treeMaxDepth ++
    maybe [] (\r -> ["root" .= r]) treeRoot

data ContextInfo = ContextInfo
  { infoContextId :: Text
  , infoParentContext :: Maybe Text
  , infoUrl :: Text
  , infoChildren :: [ContextInfo]
  } deriving (Show, Eq, Generic)

instance FromJSON ContextInfo where
  parseJSON = \case
    Object o -> ContextInfo
      <$> o .: "context"
      <*> o .:? "parent"
      <*> o .: "url"
      <*> o .: "children"
    _ -> fail "Expected object for ContextInfo"

data GetTreeResult = GetTreeResult
  { contextInfos :: [ContextInfo]
  } deriving (Show, Eq, Generic)

instance FromJSON GetTreeResult where
  parseJSON = \case
    Object o -> GetTreeResult <$> o .: "contexts"
    v@(Array _) -> GetTreeResult <$> parseJSON v
    _ -> fail "Expected object or array for GetTreeResult"

mkGetTree :: GetTreeParams -> (BidiRequest, Value -> Parser GetTreeResult)
mkGetTree params =
  ( BidiRequest 0 "browsingContext.getTree" (Just $ toJSON params)
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for GetTreeResult"
  )

-- | script.evaluate command
data EvaluateParams = EvaluateParams
  { evalTarget :: Value
  , evalExpression :: Text
  , evalAwaitPromise :: Maybe Bool
  , evalResultOwnership :: Maybe Text
  , evalSerializationOptions :: Maybe Value -- Implementation for serialization options
  } deriving (Show, Eq, Generic)

instance ToJSON EvaluateParams where
  toJSON EvaluateParams{..} = object $
    [ "target" .= evalTarget
    , "expression" .= evalExpression
    ] ++ maybe [] (\a -> ["awaitPromise" .= a]) evalAwaitPromise
    ++ maybe [] (\o -> ["resultOwnership" .= o]) evalResultOwnership
    ++ maybe [] (\s -> ["serializationOptions" .= s]) evalSerializationOptions

data EvaluateResult = EvaluateResult
  { evalResult :: ScriptEvaluationResult
  } deriving (Show, Eq, Generic)

instance FromJSON EvaluateResult where
  parseJSON = \case
    Object o -> EvaluateResult <$> o .: "result"
    _ -> fail "Expected object for EvaluateResult"

mkEvaluate :: EvaluateParams -> (BidiRequest, Value -> Parser EvaluateResult)
mkEvaluate params =
  ( BidiRequest 0 "script.evaluate" (Just $ toJSON params)
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for EvaluateResult"
  )

-- | script.callFunction command
data CallFunctionParams = CallFunctionParams
  { callTarget :: Value
  , callFunctionDeclaration :: Text
  , callThis :: Maybe Value
  , callArguments :: Maybe [Value]
  , callAwaitPromise :: Maybe Bool
  , callResultOwnership :: Maybe Text
  , callSerializationOptions :: Maybe Value -- Implementation for serialization options
  } deriving (Show, Eq, Generic)

instance ToJSON CallFunctionParams where
  toJSON CallFunctionParams{..} = object $
    [ "target" .= callTarget
    , "functionDeclaration" .= callFunctionDeclaration
    ] ++ maybe [] (\t -> ["this" .= t]) callThis
    ++ maybe [] (\a -> ["arguments" .= a]) callArguments
    ++ maybe [] (\a -> ["awaitPromise" .= a]) callAwaitPromise
    ++ maybe [] (\o -> ["resultOwnership" .= o]) callResultOwnership
    ++ maybe [] (\s -> ["serializationOptions" .= s]) callSerializationOptions

data CallFunctionResult = CallFunctionResult
  { callResult :: ScriptEvaluationResult
  } deriving (Show, Eq, Generic)

instance FromJSON CallFunctionResult where
  parseJSON = \case
    Object o -> CallFunctionResult <$> o .: "result"
    _ -> fail "Expected object for CallFunctionResult"

mkCallFunction :: CallFunctionParams -> (BidiRequest, Value -> Parser CallFunctionResult)
mkCallFunction params =
  ( BidiRequest 0 "script.callFunction" (Just $ toJSON params)
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for CallFunctionResult"
  )

-- | script.disown command
data DisownParams = DisownParams
  { disownHandles :: [Text]
  , disownTarget :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON DisownParams where
  toJSON DisownParams{..} = object
    [ "handles" .= disownHandles
    , "target" .= disownTarget
    ]

-- script.disown has empty result, just success indication
mkDisown :: DisownParams -> (BidiRequest, Value -> Parser ())
mkDisown params =
  ( BidiRequest 0 "script.disown" (Just $ toJSON params)
  , \_ -> pure ()
  )

-- | session.end command
mkEndSession :: (BidiRequest, Value -> Parser ())
mkEndSession =
  ( BidiRequest 0 "session.end" Nothing
  , \_ -> pure ()
  )

-- | session.new command
data NewSessionParams = NewSessionParams
  { newSessionCapabilities :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON NewSessionParams where
  toJSON NewSessionParams{..} = object $
    maybe [] (\c -> ["capabilities" .= c]) newSessionCapabilities

data NewSessionResult = NewSessionResult
  { sessionId :: Text
  , capabilities :: Value
  } deriving (Show, Eq, Generic)

instance FromJSON NewSessionResult where
  parseJSON = \case
    Object o -> NewSessionResult
      <$> o .: "sessionId"
      <*> o .: "capabilities"
    _ -> fail "Expected object for NewSessionResult"

mkNewSession :: NewSessionParams -> (BidiRequest, Value -> Parser NewSessionResult)
mkNewSession params =
  ( BidiRequest 0 "session.new" (Just $ toJSON params)
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for NewSessionResult"
  )

-- | session.status command
data StatusResult = StatusResult
  { statusMessage :: Text
  , statusReady :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON StatusResult where
  parseJSON = \case
    Object o -> StatusResult
      <$> o .: "message"
      <*> o .: "ready"
    _ -> fail "Expected object for StatusResult"

mkStatus :: (BidiRequest, Value -> Parser StatusResult)
mkStatus =
  ( BidiRequest 0 "session.status" Nothing
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for StatusResult"
  )

-- | session.subscribe command
data SubscribeParams = SubscribeParams
  { subscribeEvents :: [Text]
  , subscribeContexts :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON SubscribeParams where
  toJSON SubscribeParams{..} = object $
    [ "events" .= subscribeEvents ]
    ++ maybe [] (\c -> ["contexts" .= c]) subscribeContexts

mkSubscribe :: SubscribeParams -> (BidiRequest, Value -> Parser ())
mkSubscribe params =
  ( BidiRequest 0 "session.subscribe" (Just $ toJSON params)
  , \_ -> pure ()
  )

-- | session.unsubscribe command
data UnsubscribeParams = UnsubscribeParams
  { unsubscribeEvents :: [Text]
  , unsubscribeContexts :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON UnsubscribeParams where
  toJSON UnsubscribeParams{..} = object $
    [ "events" .= unsubscribeEvents ]
    ++ maybe [] (\c -> ["contexts" .= c]) unsubscribeContexts

mkUnsubscribe :: UnsubscribeParams -> (BidiRequest, Value -> Parser ())
mkUnsubscribe params =
  ( BidiRequest 0 "session.unsubscribe" (Just $ toJSON params)
  , \_ -> pure ()
  )

-- | browser.close command
mkCloseBrowser :: (BidiRequest, Value -> Parser ())
mkCloseBrowser =
  ( BidiRequest 0 "browser.close" Nothing
  , \_ -> pure ()
  )

-- | browser.createUserContext command
data CreateUserContextParams = CreateUserContextParams
  { userContextOptions :: Maybe Value -- Raw options value
  } deriving (Show, Eq, Generic)

instance ToJSON CreateUserContextParams where
  toJSON CreateUserContextParams{..} = object $
    maybe [] (\o -> ["userContext" .= o]) userContextOptions

data CreateUserContextResult = CreateUserContextResult
  { userContextId :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateUserContextResult where
  parseJSON = \case
    Object o -> CreateUserContextResult <$> o .: "userContext"
    _ -> fail "Expected object for CreateUserContextResult"

mkCreateUserContext :: CreateUserContextParams -> (BidiRequest, Value -> Parser CreateUserContextResult)
mkCreateUserContext params =
  ( BidiRequest 0 "browser.createUserContext" (Just $ toJSON params)
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for CreateUserContextResult"
  )

-- | browser.getUserContexts command
data UserContextInfo = UserContextInfo
  { userContextInfoId :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON UserContextInfo where
  parseJSON = \case
    Object o -> UserContextInfo <$> o .: "userContext"
    _ -> fail "Expected object for UserContextInfo"

data GetUserContextsResult = GetUserContextsResult
  { userContexts :: [UserContextInfo]
  } deriving (Show, Eq, Generic)

instance FromJSON GetUserContextsResult where
  parseJSON = \case
    Object o -> GetUserContextsResult <$> o .: "userContexts"
    _ -> fail "Expected object for GetUserContextsResult"

mkGetUserContexts :: (BidiRequest, Value -> Parser GetUserContextsResult)
mkGetUserContexts =
  ( BidiRequest 0 "browser.getUserContexts" Nothing
  , \v -> case v of
      Object _ -> parseJSON v
      _ -> fail "Expected object for GetUserContextsResult"
  )

-- | browser.removeUserContext command
data RemoveUserContextParams = RemoveUserContextParams
  { removeUserContextId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON RemoveUserContextParams where
  toJSON RemoveUserContextParams{..} = object
    [ "userContext" .= removeUserContextId ]

mkRemoveUserContext :: RemoveUserContextParams -> (BidiRequest, Value -> Parser ())
mkRemoveUserContext params =
  ( BidiRequest 0 "browser.removeUserContext" (Just $ toJSON params)
  , \_ -> pure ()
  )