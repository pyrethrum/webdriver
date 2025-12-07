module ErrorCoverageTest where

import Data.Set as S (Set, difference, fromList, null, union)
import Data.Text as T (Text, lines, null, pack, strip, split)
import GHC.Utils.Misc (filterOut)
import Test.Tasty.HUnit as HUnit ( assertBool, Assertion, (@=?) )
import Text.RawString.QQ (r)
import WebDriverPreCore.BiDi.Protocol 
    ( toErrorCode,
      errorDescription,
      toErrorType )
import Data.Function ((&))
import Utils(enumerate)
import Data.Foldable (traverse_)
import WebDriverPreCore.Http.Protocol (ErrorType)

{-
!! Replace this the endepoints from the spec with every release
!! remove full stops and replace tabs with " | "
https://www.w3.org/TR/2025/WD-webdriver2-20251028 - W3C Editor's Draft 10 February 2025
61 endpoints
Error Code 	HTTP Status 	JSON Error Code 	Description 
NOTE: This entry is moved out because it is duplicated, with a slightly different description in the BiDi spec
unable to set cookie  | 500  | unable to set cookie  | A command to set a cookie's value could not be satisfied
-}
httpErrorsFromSpec :: Text
httpErrorsFromSpec = pack
  [r|element click intercepted  | 400  | element click intercepted  | The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked
element not interactable  | 400  | element not interactable  | A command could not be completed because the element is not pointer- or keyboard interactable
insecure certificate  | 400  | insecure certificate  | Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate
invalid argument  | 400  | invalid argument  | The arguments passed to a command are either invalid or malformed
invalid cookie domain  | 400  | invalid cookie domain  | An illegal attempt was made to set a cookie under a different domain than the current page
invalid element state  | 400  | invalid element state  | A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable
invalid selector  | 400  | invalid selector  | Argument was an invalid selector
invalid session id  | 404  | invalid session id  | Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active
javascript error  | 500  | javascript error  | An error occurred while executing JavaScript supplied by the user
move target out of bounds  | 500  | move target out of bounds  | The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport
no such alert  | 404  | no such alert  | An attempt was made to operate on a modal dialog when one was not open
no such cookie  | 404  | no such cookie  | No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document
no such element  | 404  | no such element  | An element could not be located on the page using the given search parameters
no such frame  | 404  | no such frame  | A command to switch to a frame could not be satisfied because the frame could not be found
no such window  | 404  | no such window  | A command to switch to a window could not be satisfied because the window could not be found
no such shadow root  | 404  | no such shadow root  | The element does not have a shadow root
script timeout error  | 500  | script timeout  | A script did not complete before its timeout expired
session not created  | 500  | session not created  | A new session could not be created
stale element reference  | 404  | stale element reference  | A command failed because the referenced element is no longer attached to the DOM
detached shadow root  | 404  | detached shadow root  | A command failed because the referenced shadow root is no longer attached to the DOM
timeout  | 500  | timeout  | An operation did not complete before its timeout expired
unable to capture screen  | 500  | unable to capture screen  | A screen capture was made impossible
unexpected alert open  | 500  | unexpected alert open  | A modal dialog was open, blocking this operation
unknown command  | 404  | unknown command  | A command could not be executed because the remote end is not aware of it
unknown error  | 500  | unknown error  | An unknown error occurred in the remote end while processing the command
unknown method  | 405  | unknown method  | The requested command matched a known URL but did not match any method for that URL
unsupported operation  | 500  | unsupported operation  | Indicates that a command that should have executed properly cannot be supported for some reason 
|]

{-
!! Replace this with the error codes from the BiDi spec with every release
WebDriver BiDi Spec
-}
bidiErrorsFromSpec :: Text
bidiErrorsFromSpec = pack
  [r|invalid web extension | Tried to install an invalid web extension 
no such client window | Tried to interact with an unknown client window 
no such handle | Tried to deserialize an unknown RemoteObjectReference 
no such history entry | Tried to navigate to an unknown session history entry 
no such network collector | Tried to remove an unknown collector 
no such intercept | Tried to remove an unknown network intercept 
no such network data | Tried to reference an unknown network data 
no such node | Tried to deserialize an unknown SharedReference 
no such request | Tried to continue an unknown request 
no such script | Tried to remove an unknown preload script 
no such storage partition | Tried to access data in a non-existent storage partition 
no such user context | Tried to reference an unknown user context 
no such web extension | Tried to reference an unknown web extension 
unable to close browser | Tried to close the browser, but failed to do so 
unable to set cookie | Tried to create a cookie, but the user agent rejected it 
underspecified storage partition | Tried to interact with data in a storage partition which was not adequately specified 
unable to set file input | Tried to set a file input, but failed to do so 
unavailable network data | Tried to get network data which was not collected or already evicted|]


data ErrorLine = MkErrorLine
  { 
    jsonErrorCode :: Text,
    description :: Text
  }
  deriving (Show, Eq, Ord)

-- >>> allHttpErrorsFromSpec
-- fromList [MkErrorLine {jsonErrorCode = "detached shadow root", description = "A command failed because the referenced shadow root is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "element click intercepted", description = "The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked"},MkErrorLine {jsonErrorCode = "element not interactable", description = "A command could not be completed because the element is not pointer- or keyboard interactable"},MkErrorLine {jsonErrorCode = "insecure certificate", description = "Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate"},MkErrorLine {jsonErrorCode = "invalid argument", description = "The arguments passed to a command are either invalid or malformed"},MkErrorLine {jsonErrorCode = "invalid cookie domain", description = "An illegal attempt was made to set a cookie under a different domain than the current page"},MkErrorLine {jsonErrorCode = "invalid element state", description = "A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable"},MkErrorLine {jsonErrorCode = "invalid selector", description = "Argument was an invalid selector"},MkErrorLine {jsonErrorCode = "invalid session id", description = "Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active"},MkErrorLine {jsonErrorCode = "javascript error", description = "An error occurred while executing JavaScript supplied by the user"},MkErrorLine {jsonErrorCode = "move target out of bounds", description = "The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport"},MkErrorLine {jsonErrorCode = "no such alert", description = "An attempt was made to operate on a modal dialog when one was not open"},MkErrorLine {jsonErrorCode = "no such cookie", description = "No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document"},MkErrorLine {jsonErrorCode = "no such element", description = "An element could not be located on the page using the given search parameters"},MkErrorLine {jsonErrorCode = "no such frame", description = "A command to switch to a frame could not be satisfied because the frame could not be found"},MkErrorLine {jsonErrorCode = "no such shadow root", description = "The element does not have a shadow root"},MkErrorLine {jsonErrorCode = "no such window", description = "A command to switch to a window could not be satisfied because the window could not be found"},MkErrorLine {jsonErrorCode = "script timeout", description = "A script did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "session not created", description = "A new session could not be created"},MkErrorLine {jsonErrorCode = "stale element reference", description = "A command failed because the referenced element is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "timeout", description = "An operation did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "unable to capture screen", description = "A screen capture was made impossible"},MkErrorLine {jsonErrorCode = "unable to set cookie", description = "A command to set a cookie's value could not be satisfied"},MkErrorLine {jsonErrorCode = "unexpected alert open", description = "A modal dialog was open, blocking this operation"},MkErrorLine {jsonErrorCode = "unknown command", description = "A command could not be executed because the remote end is not aware of it"},MkErrorLine {jsonErrorCode = "unknown error", description = "An unknown error occurred in the remote end while processing the command"},MkErrorLine {jsonErrorCode = "unknown method", description = "The requested command matched a known URL but did not match any method for that URL"},MkErrorLine {jsonErrorCode = "unsupported operation", description = "Indicates that a command that should have executed properly cannot be supported for some reason"}]
allHttpErrorsFromSpec :: Set ErrorLine
allHttpErrorsFromSpec = fromList $ parseErrorLine <$> filterOut T.null (T.lines httpErrorsFromSpec)
 where
  parseErrorLine :: Text -> ErrorLine
  parseErrorLine line = 
    T.split (== '|') line & \case
      [_errrorCode, _httpStatus , jsonErrorCode, description] -> MkErrorLine (strip jsonErrorCode) $ strip description
      _ -> error $ "Error parsing line: " <> show line

-- >>> allBiDiErrorsFromSpec
-- fromList [MkErrorLine {jsonErrorCode = "invalid web extension", description = "Tried to install an invalid web extension"},MkErrorLine {jsonErrorCode = "no such client window", description = "Tried to interact with an unknown client window"},MkErrorLine {jsonErrorCode = "no such handle", description = "Tried to deserialize an unknown RemoteObjectReference"},MkErrorLine {jsonErrorCode = "no such history entry", description = "Tried to havigate to an unknown session history entry"},MkErrorLine {jsonErrorCode = "no such intercept", description = "Tried to remove an unknown network intercept"},MkErrorLine {jsonErrorCode = "no such network collector", description = "Tried to remove an unknown collector"},MkErrorLine {jsonErrorCode = "no such network data", description = "Tried to reference an unknown network data"},MkErrorLine {jsonErrorCode = "no such node", description = "Tried to deserialize an unknown SharedReference"},MkErrorLine {jsonErrorCode = "no such request", description = "Tried to continue an unknown request"},MkErrorLine {jsonErrorCode = "no such script", description = "Tried to remove an unknown preload script"},MkErrorLine {jsonErrorCode = "no such storage partition", description = "Tried to access data in a non-existent storage partition"},MkErrorLine {jsonErrorCode = "no such user context", description = "Tried to reference an unknown user context"},MkErrorLine {jsonErrorCode = "no such web extension", description = "Tried to reference an unknown web extension"},MkErrorLine {jsonErrorCode = "unable to close browser", description = "Tried to close the browser, but failed to do so"},MkErrorLine {jsonErrorCode = "unable to set cookie", description = "Tried to create a cookie, but the user agent rejected it"},MkErrorLine {jsonErrorCode = "unable to set file input", description = "Tried to set a file input, but failed to do so"},MkErrorLine {jsonErrorCode = "unavailable network data", description = "Tried to get network data which was not collected or already evicted"},MkErrorLine {jsonErrorCode = "underspecified storage partition", description = "Tried to interact with data in a storage partition which was not adequately specified"}]
allBiDiErrorsFromSpec :: Set ErrorLine
allBiDiErrorsFromSpec = fromList $ parseErrorLine <$> filterOut T.null (T.lines bidiErrorsFromSpec)
 where
  parseErrorLine :: Text -> ErrorLine
  parseErrorLine line = 
    T.split (== '|') line & \case
      [jsonErrorCode, description] -> MkErrorLine (strip jsonErrorCode) $ strip description
      _ -> error $ "Error parsing line: " <> show line


allErrorsFromSpec :: Set ErrorLine
allErrorsFromSpec = allHttpErrorsFromSpec `union` allBiDiErrorsFromSpec

-- >>> allErrors
-- fromList [MkErrorLine {jsonErrorCode = "detached shadow root", description = "A command failed because the referenced shadow root is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "element click intercepted", description = "The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked"},MkErrorLine {jsonErrorCode = "element not interactable", description = "A command could not be completed because the element is not pointer- or keyboard interactable"},MkErrorLine {jsonErrorCode = "insecure certificate", description = "Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate"},MkErrorLine {jsonErrorCode = "invalid argument", description = "The arguments passed to a command are either invalid or malformed"},MkErrorLine {jsonErrorCode = "invalid cookie domain", description = "An illegal attempt was made to set a cookie under a different domain than the current page"},MkErrorLine {jsonErrorCode = "invalid element state", description = "A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable"},MkErrorLine {jsonErrorCode = "invalid selector", description = "Argument was an invalid selector"},MkErrorLine {jsonErrorCode = "invalid session id", description = "Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it's not active"},MkErrorLine {jsonErrorCode = "javascript error", description = "An error occurred while executing JavaScript supplied by the user"},MkErrorLine {jsonErrorCode = "move target out of bounds", description = "The target for mouse interaction is not in the browser's viewport and cannot be brought into that viewport"},MkErrorLine {jsonErrorCode = "no such alert", description = "An attempt was made to operate on a modal dialog when one was not open"},MkErrorLine {jsonErrorCode = "no such cookie", description = "No cookie matching the given path name was found amongst the associated cookies of session's current browsing context's active document"},MkErrorLine {jsonErrorCode = "no such element", description = "An element could not be located on the page using the given search parameters"},MkErrorLine {jsonErrorCode = "no such frame", description = "A command to switch to a frame could not be satisfied because the frame could not be found"},MkErrorLine {jsonErrorCode = "no such shadow root", description = "The element does not have a shadow root"},MkErrorLine {jsonErrorCode = "no such window", description = "A command to switch to a window could not be satisfied because the window could not be found"},MkErrorLine {jsonErrorCode = "script timeout", description = "A script did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "session not created", description = "A new session could not be created"},MkErrorLine {jsonErrorCode = "stale element reference", description = "A command failed because the referenced element is no longer attached to the DOM"},MkErrorLine {jsonErrorCode = "timeout", description = "An operation did not complete before its timeout expired"},MkErrorLine {jsonErrorCode = "unable to capture screen", description = "A screen capture was made impossible"},MkErrorLine {jsonErrorCode = "unable to set cookie", description = "A command to set a cookie's value could not be satisfied"},MkErrorLine {jsonErrorCode = "unexpected alert open", description = "A modal dialog was open, blocking this operation"},MkErrorLine {jsonErrorCode = "unknown command", description = "A command could not be executed because the remote end is not aware of it"},MkErrorLine {jsonErrorCode = "unknown error", description = "An unknown error occurred in the remote end while processing the command"},MkErrorLine {jsonErrorCode = "unknown method", description = "The requested command matched a known URL but did not match any method for that URL"},MkErrorLine {jsonErrorCode = "unsupported operation", description = "Indicates that a command that should have executed properly cannot be supported for some reason"}]
allErrorsFromType :: Set ErrorLine
allErrorsFromType = fromList $
 (\et -> MkErrorLine { 
    jsonErrorCode  = toErrorCode et,
    description = errorDescription et
  })  <$> enumerate @ErrorType

-- >>> unit_test_all_errors_covered
unit_test_all_errors_covered :: Assertion
unit_test_all_errors_covered = do
  -- print allErrors
  -- putStrLn ""
  assertBool ("!!!! Missing ErrorTypes ~ in spec but not in WebDriverErrorType) !!!!:\n " <> show missing) (S.null missing)
  assertBool ("!!!! Extra errors ~ in WebDriverErrorType but not in spec !!!!:\n " <> show extra) (S.null extra)
  where
    missing = allErrorsFromSpec `difference` allErrorsFromType
    extra = allErrorsFromType `difference` allErrorsFromSpec


-- >>> unit_round_trip_error_codes
unit_round_trip_error_codes :: Assertion
unit_round_trip_error_codes = do
  traverse_ checkRoundTripErrorCodes enumerate
  where
    checkRoundTripErrorCodes errorType = do
      let errorCode = toErrorCode errorType
          errorType' = toErrorType errorCode
      errorType' & either (\e -> error $ "Failed to parse error code: " <> show e <> " for error code: " <> show errorCode) (errorType @=?)




