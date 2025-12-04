{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Http.Error
  ( WebDriverErrorType (..),
    ErrorClassification (..),
    errorDescription,
    errorCodeToErrorType,
    errorTypeToErrorCode,
    parseWebDriverError,
    parseWebDriverErrorType,
  )
where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), Options (..), Value, defaultOptions, genericParseJSON, withObject)
import Data.Aeson.Types (Parser, parseMaybe, (.:))
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (error)

{-
Error Code 	HTTP Status 	JSON Error Code 	Description
element click intercepted 	400 	element click intercepted 	The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked.
element not interactable 	400 	element not interactable 	A command could not be completed because the element is not pointer- or keyboard interactable.
insecure certificate 	400 	insecure certificate 	Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate.
invalid argument 	400 	invalid argument 	The arguments passed to a command are either invalid or malformed.
invalid cookie domain 	400 	invalid cookie domain 	An illegal attempt was made to set a cookie under a different domain than the current page.
invalid element state 	400 	invalid element state 	A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable.
invalid selector 	400 	invalid selector 	Argument was an invalid selector.
invalid session id 	404 	invalid session id 	Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active.
javascript error 	500 	javascript error 	An error occurred while executing JavaScript supplied by the user.
move target out of bounds 	500 	move target out of bounds 	The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport.
no such alert 	404 	no such alert 	An attempt was made to operate on a modal dialog when one was not open.
no such cookie 	404 	no such cookie 	No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document.
no such element 	404 	no such element 	An element could not be located on the page using the given search parameters.
no such frame 	404 	no such frame 	A command to switch to a frame could not be satisfied because the frame could not be found.
no such window 	404 	no such window 	A command to switch to a window could not be satisfied because the window could not be found.
no such shadow root 	404 	no such shadow root 	The element does not have a shadow root.
script timeout error 	500 	script timeout 	A script did not complete before its timeout expired.
session not created 	500 	session not created 	A new session could not be created.
stale element reference 	404 	stale element reference 	A command failed because the referenced element is no longer attached to the DOM.
detached shadow root 	404 	detached shadow root 	A command failed because the referenced shadow root is no longer attached to the DOM.
timeout 	500 	timeout 	An operation did not complete before its timeout expired.
unable to set cookie 	500 	unable to set cookie 	A command to set a cookie's value could not be satisfied.
unable to capture screen 	500 	unable to capture screen 	A screen capture was made impossible.
unexpected alert open 	500 	unexpected alert open 	A modal dialog was open, blocking this operation.
unknown command 	404 	unknown command 	A command could not be executed because the remote end is not aware of it.
unknown error 	500 	unknown error 	An unknown error occurred in the remote end while processing the command.
unknown method 	405 	unknown method 	The requested command matched a known URL but did not match any method for that URL.
unsupported operation 	500 	unsupported operation 	Indicates that a command that should have executed properly cannot be supported for some reason.
-}

-- | Known WevDriver Error Types
--
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#errors)
data WebDriverErrorType
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
  | ScriptTimeoutError
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
  deriving (Eq, Show, Ord, Bounded, Enum)

errorCodeToErrorType :: Text -> Either Text WebDriverErrorType
errorCodeToErrorType errCode =
  case errCode of
    "element click intercepted" -> r ElementClickIntercepted
    "element not interactable" -> r ElementNotInteractable
    "insecure certificate" -> r InsecureCertificate
    "invalid argument" -> r InvalidArgument
    "invalid cookie domain" -> r InvalidCookieDomain
    "invalid element state" -> r InvalidElementState
    "invalid selector" -> r InvalidSelector
    "invalid session id" -> r InvalidSessionId
    "javascript error" -> r JavascriptError
    "move target out of bounds" -> r MoveTargetOutOfBounds
    "no such alert" -> r NoSuchAlert
    "no such cookie" -> r NoSuchCookie
    "no such element" -> r NoSuchElement
    "no such frame" -> r NoSuchFrame
    "no such window" -> r NoSuchWindow
    "no such shadow root" -> r NoSuchShadowRoot
    "script timeout" -> r ScriptTimeoutError
    "session not created" -> r SessionNotCreated
    "stale element reference" -> r StaleElementReference
    "detached shadow root" -> r DetachedShadowRoot
    "timeout" -> r Timeout
    "unable to set cookie" -> r UnableToSetCookie
    "unable to capture screen" -> r UnableToCaptureScreen
    "unexpected alert open" -> r UnexpectedAlertOpen
    "unknown command" -> r UnknownCommand
    "unknown error" -> r UnknownError
    "unknown method" -> r UnknownMethod
    "unsupported operation" -> r UnsupportedOperation
    er -> Left er
  where
    r = Right

errorTypeToErrorCode :: WebDriverErrorType -> Text
errorTypeToErrorCode = \case
  ElementClickIntercepted -> "element click intercepted"
  ElementNotInteractable -> "element not interactable"
  InsecureCertificate -> "insecure certificate"
  InvalidArgument -> "invalid argument"
  InvalidCookieDomain -> "invalid cookie domain"
  InvalidElementState -> "invalid element state"
  InvalidSelector -> "invalid selector"
  InvalidSessionId -> "invalid session id"
  JavascriptError -> "javascript error"
  MoveTargetOutOfBounds -> "move target out of bounds"
  NoSuchAlert -> "no such alert"
  NoSuchCookie -> "no such cookie"
  NoSuchElement -> "no such element"
  NoSuchFrame -> "no such frame"
  NoSuchWindow -> "no such window"
  NoSuchShadowRoot -> "no such shadow root"
  ScriptTimeoutError -> "script timeout"
  SessionNotCreated -> "session not created"
  StaleElementReference -> "stale element reference"
  DetachedShadowRoot -> "detached shadow root"
  Timeout -> "timeout"
  UnableToSetCookie -> "unable to set cookie"
  UnableToCaptureScreen -> "unable to capture screen"
  UnexpectedAlertOpen -> "unexpected alert open"
  UnknownCommand -> "unknown command"
  UnknownError -> "unknown error"
  UnknownMethod -> "unknown method"
  UnsupportedOperation -> "unsupported operation"

errorDescription :: WebDriverErrorType -> Text
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
  ScriptTimeoutError -> "A script did not complete before its timeout expired"
  SessionNotCreated -> "A new session could not be created"
  StaleElementReference -> "A command failed because the referenced element is no longer attached to the DOM"
  DetachedShadowRoot -> "A command failed because the referenced shadow root is no longer attached to the DOM"
  Timeout -> "An operation did not complete before its timeout expired"
  UnableToSetCookie -> "A command to set a cookie's value could not be satisfied"
  UnableToCaptureScreen -> "A screen capture was made impossible"
  UnexpectedAlertOpen -> "A modal dialog was open, blocking this operation"
  UnknownCommand -> "A command could not be executed because the remote end is not aware of it"
  UnknownError -> "An unknown error occurred in the remote end while processing the command"
  UnknownMethod -> "The requested command matched a known URL but did not match any method for that URL"
  UnsupportedOperation -> "Indicates that a command that should have executed properly cannot be supported for some reason"

data ErrorClassification
  = ResponeParseError {httpResponse :: Value}
  | UnrecognisedError {httpResponse :: Value}
  | WebDriverError
      { error :: WebDriverErrorType,
        description :: Text,
        message :: Text,
        stacktrace :: Maybe Text,
        errorData :: Maybe Value,
        httpResponse :: Value
      }
  deriving (Eq, Show, Ord)

parseWebDriverError :: Value -> ErrorClassification
parseWebDriverError body =
  parseMaybe parseErrorCode body & maybe
    parserErr
    \err ->
      case errorCodeToErrorType err of
        Left _ -> UnrecognisedError body
        Right et -> mkWebDriverError et
  where
    parserErr = ResponeParseError body
    mkWebDriverError :: WebDriverErrorType -> ErrorClassification
    mkWebDriverError et =
      parseMaybe (getValue >=> parseJSON) body
        & maybe
          parserErr
          \MkWebDriverErrorRaw {..} ->
            WebDriverError {error = et, description = errorDescription et, httpResponse = body, ..}

-- getBody :: Value -> Parser Value
-- getBody =
--   withObject "body" (.: "body")

getValue :: Value -> Parser Value
getValue =
  withObject "value" (.: "value")

parseErrorCode :: Value -> Parser Text
parseErrorCode =
  getValue >=> withObject "error" (.: "error")

parseWebDriverErrorType :: Value -> Maybe WebDriverErrorType
parseWebDriverErrorType resp =
  case parseWebDriverError resp of
    WebDriverError {error} -> Just error
    ResponeParseError {} -> Nothing
    UnrecognisedError {} -> Nothing

data WebDriverErrorRaw = MkWebDriverErrorRaw
  { error :: Text,
    message :: Text,
    stacktrace :: Maybe Text,
    errorData :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON WebDriverErrorRaw where
  parseJSON :: Value -> Parser WebDriverErrorRaw
  parseJSON = genericParseJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = \case
      "errorData" -> "data"
      other -> other
    }
