module WebDriverPreCore.Error where

import Data.Aeson (Value, FromJSON (parseJSON))
import Data.Aeson.Types ((.:), parseMaybe)
import Data.Text (Text)
import Data.Map.Strict (Map, fromList, lookup)
import Data.Eq (Eq)
import GHC.Show (Show)
import Data.Ord (Ord)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Function (($))

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


data WebDriverError = 
  UnrecognisedError Text |
  ElementClickIntercepted |
  ElementNotInteractable |
  InsecureCertificate |
  InvalidArgument |
  InvalidCookieDomain |
  InvalidElementState |
  InvalidSelector |
  InvalidSessionId |
  JavascriptError |
  MoveTargetOutOfBounds |
  NoSuchAlert |
  NoSuchCookie |
  NoSuchElement |
  NoSuchFrame |
  NoSuchWindow |
  NoSuchShadowRoot |
  ScriptTimeoutError |
  SessionNotCreated |
  StaleElementReference |
  DetachedShadowRoot |
  Timeout |
  UnableToSetCookie |
  UnableToCaptureScreen |
  UnexpectedAlertOpen |
  UnknownCommand |
  UnknownError |
  UnknownMethod |
  UnsupportedOperation
  deriving (Eq, Show, Ord)


classifyError :: Value -> Maybe WebDriverError
classifyError response = do
  o <- parseMaybe parseJSON response
  txt <- parseMaybe (.: "error") o
  Just $ fromMaybe (UnrecognisedError txt) (lookup txt errorCodeMap) 

errorCodeMap :: Map Text WebDriverError
errorCodeMap = fromList
  [ ("element click intercepted", ElementClickIntercepted)
  , ("element not interactable", ElementNotInteractable)
  , ("insecure certificate", InsecureCertificate)
  , ("invalid argument", InvalidArgument)
  , ("invalid cookie domain", InvalidCookieDomain)
  , ("invalid element state", InvalidElementState)
  , ("invalid selector", InvalidSelector)
  , ("invalid session id", InvalidSessionId)
  , ("javascript error", JavascriptError)
  , ("move target out of bounds", MoveTargetOutOfBounds)
  , ("no such alert", NoSuchAlert)
  , ("no such cookie", NoSuchCookie)
  , ("no such element", NoSuchElement)
  , ("no such frame", NoSuchFrame)
  , ("no such window", NoSuchWindow)
  , ("no such shadow root", NoSuchShadowRoot)
  , ("script timeout", ScriptTimeoutError)
  , ("session not created", SessionNotCreated)
  , ("stale element reference", StaleElementReference)
  , ("detached shadow root", DetachedShadowRoot)
  , ("timeout", Timeout)
  , ("unable to set cookie", UnableToSetCookie)
  , ("unable to capture screen", UnableToCaptureScreen)
  , ("unexpected alert open", UnexpectedAlertOpen)
  , ("unknown command", UnknownCommand)
  , ("unknown error", UnknownError)
  , ("unknown method", UnknownMethod)
  , ("unsupported operation", UnsupportedOperation)
  ]