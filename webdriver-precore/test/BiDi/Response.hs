module BiDi.Response
  ( parseResponse,
    matchResponseId,
    decodeResponse,
    MatchedResponse (..),
    ResponseObject (..),
    ResponseException (..),
    Success (..),
  )
where

import AesonUtils (parseObjectEither, subtractProps)
import Control.Exception (Exception (..))
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Utils (txt)
import WebDriverPreCore.BiDi.Protocol (EmptyResult (..), JSONEncodeException (..), JSUInt, WebDriverException (..), parseWebDriverException)

parseResponse :: forall r. (FromJSON r) => JSUInt -> Either JSONEncodeException ResponseObject -> Maybe (Either ResponseException (MatchedResponse r))
parseResponse id' =
  either
    (Just . Left . BiDIError . JSONEncodeException)
    (matchResponseId id')

matchResponseId :: forall a. (FromJSON a) => JSUInt -> ResponseObject -> Maybe (Either ResponseException (MatchedResponse a))
matchResponseId msgId = \case
  NoID {} -> Nothing
  WithID id' obj ->
    if id' == msgId
      then
        Just $
          bimap
            (\e -> BiDIError . parseWebDriverException e $ Object obj)
            (\s -> MkMatchedResponse {response = s.result, object = obj})
            (parseObjectEither obj :: Either Text (Success a))
      else
        Nothing

decodeResponse :: ByteString -> Either JSONEncodeException ResponseObject
decodeResponse =
  (=<<) parseResponseObj . packLeft . eitherDecode
  where
    packLeft = first (MkJSONEncodeException "Failed to parse response" . pack)

    parseResponseObj :: Object -> Either JSONEncodeException ResponseObject
    parseResponseObj =
      packLeft . parseEither (\o' -> maybe NoID WithID <$> o' .:? "id" <*> pure o')

data MatchedResponse a = MkMatchedResponse
  { response :: a,
    object :: Object
  }
  deriving (Show, Generic)

data ResponseObject
  = NoID {object :: Object}
  | WithID {id :: JSUInt, object :: Object}
  deriving (Show, Generic)

data ResponseException
  = BiDIError WebDriverException
  | BiDiTimeoutError {ms :: Int}
  deriving (Show, Eq, Generic)

instance Exception ResponseException where
  displayException :: ResponseException -> String
  displayException = \case
    BiDIError e -> displayException e
    BiDiTimeoutError {ms} -> unpack $ "Timed out waiting for matching command response from driver (" <> txt ms <> "milliseconds)"

data Success a = MkSuccess
  { id :: JSUInt,
    result :: a,
    extensions :: EmptyResult
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Success a) where
  parseJSON :: Value -> Parser (Success a)
  parseJSON = withObject "Success" $ \o -> do
    id' <- o .: "id"
    result <- o .: "result"
    pure $
      MkSuccess
        { id = id',
          result,
          extensions = MkEmptyResult $ subtractProps ["id", "result"] o
        }