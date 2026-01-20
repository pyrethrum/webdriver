{-|
Module: WebDriverPreCore.BiDiRunnerBase.Response
Description: Response parsing for BiDi WebSocket messages

This module provides types and functions for parsing BiDi WebSocket responses.
-}
module WebDriverPreCore.BiDiRunnerBase.Response
  ( -- * Response Types
    ResponseObject (..),
    MatchedResponse (..),
    ResponseException (..),
    Success (..),
    
    -- * Parsing Functions
    parseResponse,
    matchResponseId,
    decodeResponse,
  )
where

import Control.Exception (Exception (..))
import Data.Aeson (FromJSON (..), Object, Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDiRunnerBase.Types (JSUInt (..))
import WebDriverPreCore.Exception 
  ( WebDriverException (..),
    JSONEncodeException (..),
    parseWebDriverException,
  )

-- | A parsed response object from the WebSocket
data ResponseObject
  = NoID {object :: Object}  -- ^ Event message (no ID)
  | WithID {id :: JSUInt, object :: Object}  -- ^ Command response (has ID)
  deriving (Show, Generic)

-- | A matched response with parsed result
data MatchedResponse a = MkMatchedResponse
  { response :: a,
    object :: Object
  }
  deriving (Show, Generic)

-- | Exception during response parsing/matching
data ResponseException
  = BiDIError WebDriverException
  | BiDiTimeoutError {ms :: Int}
  deriving (Show, Eq, Generic)

instance Exception ResponseException where
  displayException :: ResponseException -> String
  displayException = \case
    BiDIError e -> displayException e
    BiDiTimeoutError {ms} -> "Timed out waiting for matching command response from driver (" <> show ms <> " milliseconds)"

-- | Successful command response
data Success a = MkSuccess
  { id :: JSUInt,
    result :: a,
    extensions :: Object
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Success a) where
  parseJSON :: Value -> Parser (Success a)
  parseJSON = withObject "Success" $ \o -> do
    id' <- o .: "id"
    result <- o .: "result"
    let extensions = KM.delete "id" $ KM.delete "result" o
    pure $ MkSuccess
      { id = id',
        result,
        extensions
      }

-- | Parse and match a response to a command ID
parseResponse :: forall r. (FromJSON r) 
  => JSUInt 
  -> Either JSONEncodeException ResponseObject 
  -> Maybe (Either ResponseException (MatchedResponse r))
parseResponse id' =
  either
    (Just . Left . BiDIError . JSONEncodeException)
    (matchResponseId id')

-- | Match a response object to a command ID
matchResponseId :: forall a. (FromJSON a) 
  => JSUInt 
  -> ResponseObject 
  -> Maybe (Either ResponseException (MatchedResponse a))
matchResponseId msgId = \case
  NoID {} -> Nothing  -- Events don't have IDs, skip
  WithID id' obj ->
    if id' == msgId
      then Just $ parseSuccessOrError obj
      else Nothing
  where
    parseSuccessOrError :: Object -> Either ResponseException (MatchedResponse a)
    parseSuccessOrError obj =
      case parseEither parseJSON (Object obj) :: Either String (Success a) of
        Right s -> Right $ MkMatchedResponse {response = s.result, object = obj}
        Left err -> Left $ BiDIError $ parseWebDriverException (pack err) (Object obj)

-- | Decode a raw WebSocket message into a ResponseObject
decodeResponse :: ByteString -> Either JSONEncodeException ResponseObject
decodeResponse bs =
  case eitherDecode bs of
    Left err -> Left $ MkJSONEncodeException "Failed to parse response" (pack err)
    Right obj -> parseResponseObj obj

parseResponseObj :: Object -> Either JSONEncodeException ResponseObject
parseResponseObj obj =
  case parseEither (\o -> o .:? "id") obj of
    Left err -> Left $ MkJSONEncodeException "Failed to parse response id" (pack err)
    Right mId -> Right $ maybe (NoID obj) (\i -> WithID i obj) mId
