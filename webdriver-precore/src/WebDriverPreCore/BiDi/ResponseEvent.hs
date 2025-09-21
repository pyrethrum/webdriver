module WebDriverPreCore.BiDi.ResponseEvent where

import Data.Aeson (FromJSON (parseJSON), Object, Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent)
import WebDriverPreCore.BiDi.Command (Command)
import WebDriverPreCore.BiDi.CoreTypes (EmptyResult (..), JSUInt)
import WebDriverPreCore.BiDi.Error (DriverError (..))
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.Internal.AesonUtils (jsonToText, parseObjectEither, parseObjectMaybe, subtractProps)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude

parseResponse :: (FromJSON r) => JSUInt -> Either JSONDecodeError ResponseObject -> Maybe (Either ResponseError (MatchedResponse r))
parseResponse id' =
  either
    (Just . Left . DecodeError)
    (matchResponseObject id')

matchResponseObject :: forall a. (FromJSON a) => JSUInt -> ResponseObject -> Maybe (Either ResponseError (MatchedResponse a))
matchResponseObject msgId = \case
  NoID {} -> Nothing
  WithID id' obj ->
    if id' == msgId
      then
        Just $
          bimap
            ( \e ->
                (parseObjectMaybe obj :: Maybe DriverError)
                  & maybe
                    (ParseError {object = obj, error = e})
                    BiDIError
            )
            (\s -> MkMatchedResponse {response = s.result, object = obj})
            (parseObjectEither obj :: Either Text (Success a))
      else
        Nothing

decodeResponse :: ByteString -> Either JSONDecodeError ResponseObject
decodeResponse =
  (=<<) parseResponseObj . packLeft . eitherDecode
  where
    packLeft = first (MkJSONDecodeError . pack)

    parseResponseObj :: Object -> Either JSONDecodeError ResponseObject
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

newtype JSONDecodeError = MkJSONDecodeError Text deriving (Show, Eq, Generic)

data ResponseError
  = BiDIError DriverError
  | DecodeError JSONDecodeError
  | ParseError
      { object :: Object,
        error :: Text
      }
  | BiDiTimeoutError {ms :: Int}
  deriving (Show, Eq, Generic)

displayResponseError :: (Show c) => Command c r -> Value -> ResponseError -> Text
displayResponseError c json err =
  "Error executing BiDi command: "
    <> txt c
    <> "\n"
    <> "With JSON: \n"
    <> jsonToText json
    <> "\n"
    <> case err of
      BiDIError e -> "BiDi driver error: \n" <> txt e
      DecodeError (MkJSONDecodeError e) -> "Failed to encode driver response to JSON: \n" <> txt e
      ParseError {object, error = e} ->
        "Failed to decode the 'result' property of JSON returned by driver to response type: \n"
          <> jsonToText (Object object)
          <> "\nError message: \n"
          <> e
      BiDiTimeoutError {ms} -> "Timed out waiting for matching command response from driver (" <> txt ms <> "milliseconds)"

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

-- typ :: Text, -- "event"
data Event = MkEvent
  { eventData :: EventData,
    extensions :: EmptyResult
  }
  deriving (Show, Generic)

data EventData
  = BrowsingContextEvent BrowsingContextEvent
  | -- | InputEvent InputEvent
    InputEvent FileDialogOpened
  | -- method: "log.entryAdded"
    LogEvent Entry
  deriving
    ( Show,
      Generic
    )
