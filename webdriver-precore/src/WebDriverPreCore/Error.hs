module WebDriverPreCore.Error
  ( ErrorType (..),
    WebDriverException (..),
    JSONEncodeException (..),
    errorDescription,
    toErrorType,
    toErrorCode,
    parseWebDriverException,
    parseErrorType,
  )
where

import AesonUtils (jsonToText)
import Control.Exception (Exception (..))
import Data.Aeson (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON, withObject)
import Data.Aeson.Types (Parser, parseEither, parseMaybe, (.:))
import Data.Bifunctor (first)
import Data.Char (isUpper, toLower)
import Data.Function ((&))
import Data.Text as T (Text, concat, pack, toTitle, unpack, words)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Text.Read (readEither)
import Prelude as P hiding (error, words)



-- | A JavaScript unsigned integer (0..9007199254740991), represented as Word64.
-- Used internally for parsing BiDi response IDs.
newtype JSUInt = MkJSUInt Word64
  deriving newtype (Show, Eq, Enum, FromJSON)

-- | Known WebDriver Error Types
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#errors)
data ErrorType
  = ElementClickIntercepted
  | ElementNotInteractable
  | InsecureCertificate
  | InvalidArgument
  | InvalidCookieDomain
  | InvalidElementState
  | InvalidSelector
  | InvalidSessionId
  | JavascriptError
  | MoveTargetOutOfBounds
  | NoSuchAlert
  | NoSuchCookie
  | NoSuchElement
  | NoSuchFrame
  | NoSuchWindow
  | NoSuchShadowRoot
  | ScriptTimeout
  | SessionNotCreated
  | StaleElementReference
  | DetachedShadowRoot
  | Timeout
  | UnableToSetCookie
  | UnableToCaptureScreen
  | UnexpectedAlertOpen
  | UnknownCommand
  | UnknownError
  | UnknownMethod
  | UnsupportedOperation
  | --- Bidi
    InvalidWebExtension
  | NoSuchClientWindow
  | NoSuchHandle
  | NoSuchHistoryEntry
  | NoSuchNetworkCollector
  | NoSuchIntercept
  | NoSuchNetworkData
  | NoSuchNode
  | NoSuchRequest
  | NoSuchScript
  | NoSuchStoragePartition
  | NoSuchUserContext
  | NoSuchWebExtension
  | UnableToCloseBrowser
  | UnableToSetFileInput
  | UnavailableNetworkData
  | UnderspecifiedStoragePartition
  deriving (Eq, Read, Show, Ord, Bounded, Enum)

instance FromJSON ErrorType where
  parseJSON :: Value -> Parser ErrorType
  parseJSON = withObject "ErrorType" $ \o -> do
    errCode <- o .: "error"
    case toErrorType errCode of
      Left e -> fail $ "Unknown ErrorType code: " <> unpack e
      Right et -> pure et

toErrorType :: Text -> Either Text ErrorType
toErrorType =
  first pack . readEither . unpack . T.concat . fmap T.toTitle . T.words

toErrorCode :: ErrorType -> Text
toErrorCode =
  pack . P.unwords . splitCamel . show
  where
    splitCamel :: String -> [String]
    splitCamel = \case
      [] -> []
      (x : xs) ->
        let (w, rest) = break isUpper xs
         in (toLower x : w) : splitCamel rest

errorDescription :: ErrorType -> Text
errorDescription = \case
  ElementClickIntercepted -> "The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked"
  ElementNotInteractable -> "A command could not be completed because the element is not pointer- or keyboard interactable"
  InsecureCertificate -> "Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate"
  InvalidArgument -> "The arguments passed to a command are either invalid or malformed"
  InvalidCookieDomain -> "An illegal attempt was made to set a cookie under a different domain than the current page"
  InvalidElementState -> "A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable"
  InvalidSelector -> "Argument was an invalid selector"
  InvalidSessionId -> "Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active"
  JavascriptError -> "An error occurred while executing JavaScript supplied by the user"
  MoveTargetOutOfBounds -> "The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport"
  NoSuchAlert -> "An attempt was made to operate on a modal dialog when one was not open"
  NoSuchCookie -> "No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document"
  NoSuchElement -> "An element could not be located on the page using the given search parameters"
  NoSuchFrame -> "A command to switch to a frame could not be satisfied because the frame could not be found"
  NoSuchWindow -> "A command to switch to a window could not be satisfied because the window could not be found"
  NoSuchShadowRoot -> "The element does not have a shadow root"
  ScriptTimeout -> "A script did not complete before its timeout expired"
  SessionNotCreated -> "A new session could not be created"
  StaleElementReference -> "A command failed because the referenced element is no longer attached to the DOM"
  DetachedShadowRoot -> "A command failed because the referenced shadow root is no longer attached to the DOM"
  Timeout -> "An operation did not complete before its timeout expired"
  UnableToSetCookie -> "Tried to create a cookie, but the user agent rejected it"
  UnableToCaptureScreen -> "A screen capture was made impossible"
  UnexpectedAlertOpen -> "A modal dialog was open, blocking this operation"
  UnknownCommand -> "A command could not be executed because the remote end is not aware of it"
  UnknownError -> "An unknown error occurred in the remote end while processing the command"
  UnknownMethod -> "The requested command matched a known URL but did not match any method for that URL"
  UnsupportedOperation -> "Indicates that a command that should have executed properly cannot be supported for some reason"
  -- Bidi
  InvalidWebExtension -> "Tried to install an invalid web extension"
  NoSuchClientWindow -> "Tried to interact with an unknown client window"
  NoSuchNetworkCollector -> "Tried to remove an unknown collector"
  NoSuchHandle -> "Tried to deserialize an unknown RemoteObjectReference"
  NoSuchHistoryEntry -> "Tried to navigate to an unknown session history entry"
  NoSuchIntercept -> "Tried to remove an unknown network intercept"
  NoSuchNetworkData -> "Tried to reference an unknown network data"
  NoSuchNode -> "Tried to deserialize an unknown SharedReference"
  NoSuchRequest -> "Tried to continue an unknown request"
  NoSuchScript -> "Tried to remove an unknown preload script"
  NoSuchStoragePartition -> "Tried to access data in a non-existent storage partition"
  NoSuchUserContext -> "Tried to reference an unknown user context"
  NoSuchWebExtension -> "Tried to reference an unknown web extension"
  UnableToCloseBrowser -> "Tried to close the browser, but failed to do so"
  UnableToSetFileInput -> "Tried to set a file input, but failed to do so"
  UnavailableNetworkData -> "Tried to get network data which was not collected or already evicted"
  UnderspecifiedStoragePartition -> "Tried to interact with data in a storage partition which was not adequately specified"

data WebDriverException
  = ResponseParseException
      { message :: Text,
        response :: Value
      }
  | UnrecognisedErrorTypeException {errorType :: Text, response :: Value}
  | JSONEncodeException JSONEncodeException
  | ProtocolException
      { error :: ErrorType,
        description :: Text,
        message :: Text,
        stacktrace :: Maybe Text,
        errorData :: Maybe Value,
        response :: Value
      }
  deriving (Eq, Show, Ord, Generic)

data JSONEncodeException = MkJSONEncodeException
  { message :: Text,
    responseText :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON JSONEncodeException

instance Exception WebDriverException where
  displayException :: WebDriverException -> String
  displayException =
    unpack . \case
      ResponseParseException {message, response} ->
        "Error parsing WebDriver response: "
          <> message
          <> "\nResponse was:\n"
          <> jsonToText response
      UnrecognisedErrorTypeException {response} ->
        "Unrecognised WebDriver error type in response:\n"
          <> jsonToText response
      JSONEncodeException MkJSONEncodeException {message, responseText} ->
        "Error converting WebDriver response to JSON: "
          <> message
          <> "\nResponse text was:\n"
          <> responseText
      ProtocolException {error, description, message, stacktrace} ->
        "Error executing WebDriver command: "
          <> "\nError Type: "
          <> toErrorCode error
          <> "\nDescription: "
          <> description
          <> "\nMessage: "
          <> message
          <> maybe "" (\st -> "\nStacktrace: \n" <> st) stacktrace


parseWebDriverException :: Text -> Value -> WebDriverException
parseWebDriverException errInfo response =
  parseMaybe getErrorProp response
    & maybe
      (parserErr (errInfo <> "\n" <> "Could not find 'error' property in response\n" <> jsonToText response))
      ( either
          (flip UnrecognisedErrorTypeException response)
          mkWebDriverException
          . toErrorType
      )
  where
    parserErr message = ResponseParseException {message, response}
    mkWebDriverException :: ErrorType -> WebDriverException
    mkWebDriverException et =
      parseEither parseJSON response
        & either
          (\msg -> parserErr $ "Error object parsing failed:\n" <> pack msg <> "\n" <> jsonToText response)
          \MkWebDriverExceptionRaw {..} ->
            ProtocolException
              { error = et,
                description = errorDescription et,
                response,
                message,
                stacktrace,
                errorData
              }

getErrorProp :: Value -> Parser Text
getErrorProp =
  withObject "error" (.: "error")

parseErrorType :: Value -> Maybe ErrorType
parseErrorType resp =
  case parseWebDriverException "parse error type result" resp of
    ProtocolException {error} -> Just error
    JSONEncodeException {} -> Nothing
    ResponseParseException {} -> Nothing
    UnrecognisedErrorTypeException {} -> Nothing

data WebDriverExceptionRaw = MkWebDriverExceptionRaw
  { error :: Text,
    message :: Text,
    biDiId :: Maybe JSUInt,
    stacktrace :: Maybe Text,
    errorData :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON WebDriverExceptionRaw where
  parseJSON :: Value -> Parser WebDriverExceptionRaw
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = \case
            "data" -> "errorData"
            "id" -> "biDiId"
            other -> other
        }
