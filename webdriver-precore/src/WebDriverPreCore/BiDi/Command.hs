module WebDriverPreCore.BiDi.Command where

import Data.Aeson
  ( Object,
    ToJSON,
    Value (..),
    object,
    (.=),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (..))
import Data.Text as T (Text, unlines, unpack)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import WebDriverPreCore.Internal.AesonUtils (objectOrThrow)
import WebDriverPreCore.Internal.Utils (enumerate, txt)
import Prelude

data CommandMethodType
  = BrowserClose
  | BrowserCreateUserContext
  | BrowserGetClientWindows
  | BrowserGetUserContexts
  | BrowserRemoveUserContext
  | BrowserSetClientWindowState
  | BrowsingContextActivate
  | BrowsingContextCaptureScreenshot
  | BrowsingContextClose
  | BrowsingContextCreate
  | BrowsingContextGetTree
  | BrowsingContextHandleUserPrompt
  | BrowsingContextLocateNodes
  | BrowsingContextNavigate
  | BrowsingContextPrint
  | BrowsingContextReload
  | BrowsingContextSetViewport
  | BrowsingContextTraverseHistory
  | InputPerformActions
  | InputReleaseActions
  | InputSetFiles
  | NetworkAddIntercept
  | NetworkContinueRequest
  | NetworkContinueResponse
  | NetworkContinueWithAuth
  | NetworkFailRequest
  | NetworkProvideResponse
  | NetworkRemoveIntercept
  | NetworkSetCacheBehavior
  | PermissionsSetPermission
  | ScriptAddPreloadScript
  | ScriptCallFunction
  | ScriptDisown
  | ScriptEvaluate
  | ScriptGetRealms
  | ScriptRemovePreloadScript
  | SessionEnd
  | SessionNew
  | SessionStatus
  | SessionSubscribe
  | SessionUnsubscribe
  | StorageDeleteCookies
  | StorageGetCookies
  | StorageSetCookie
  deriving (Show, Eq, Enum, Bounded)

instance ToJSON CommandMethodType where
  toJSON :: CommandMethodType -> Value
  toJSON = \case
    BrowserClose -> "browser.close"
    BrowserCreateUserContext -> "browser.createUserContext"
    BrowserGetClientWindows -> "browser.getClientWindows"
    BrowserGetUserContexts -> "browser.getUserContexts"
    BrowserRemoveUserContext -> "browser.removeUserContext"
    BrowserSetClientWindowState -> "browser.setClientWindowState"
    BrowsingContextActivate -> "browsingContext.activate"
    BrowsingContextCaptureScreenshot -> "browsingContext.captureScreenshot"
    BrowsingContextClose -> "browsingContext.close"
    BrowsingContextCreate -> "browsingContext.create"
    BrowsingContextGetTree -> "browsingContext.getTree"
    BrowsingContextHandleUserPrompt -> "browsingContext.handleUserPrompt"
    BrowsingContextLocateNodes -> "browsingContext.locateNodes"
    BrowsingContextNavigate -> "browsingContext.navigate"
    BrowsingContextPrint -> "browsingContext.print"
    BrowsingContextReload -> "browsingContext.reload"
    BrowsingContextSetViewport -> "browsingContext.setViewport"
    BrowsingContextTraverseHistory -> "browsingContext.traverseHistory"
    InputPerformActions -> "input.performActions"
    InputReleaseActions -> "input.releaseActions"
    InputSetFiles -> "input.setFiles"
    NetworkAddIntercept -> "network.addIntercept"
    NetworkContinueRequest -> "network.continueRequest"
    NetworkContinueResponse -> "network.continueResponse"
    NetworkContinueWithAuth -> "network.continueWithAuth"
    NetworkFailRequest -> "network.failRequest"
    NetworkProvideResponse -> "network.provideResponse"
    NetworkRemoveIntercept -> "network.removeIntercept"
    NetworkSetCacheBehavior -> "network.setCacheBehavior"
    PermissionsSetPermission -> "permissions.setPermission"
    ScriptAddPreloadScript -> "script.addPreloadScript"
    ScriptCallFunction -> "script.callFunction"
    ScriptDisown -> "script.disown"
    ScriptEvaluate -> "script.evaluate"
    ScriptGetRealms -> "script.getRealms"
    ScriptRemovePreloadScript -> "script.removePreloadScript"
    SessionEnd -> "session.end"
    SessionNew -> "session.new"
    SessionStatus -> "session.status"
    SessionSubscribe -> "session.subscribe"
    SessionUnsubscribe -> "session.unsubscribe"
    StorageDeleteCookies -> "storage.deleteCookies"
    StorageGetCookies -> "storage.getCookies"
    StorageSetCookie -> "storage.setCookie"

instance FromJSON CommandMethodType where
  parseJSON :: Value -> Parser CommandMethodType
  parseJSON = \case
    String "browser.close" -> pure BrowserClose
    String "browser.createUserContext" -> pure BrowserCreateUserContext
    String "browser.getClientWindows" -> pure BrowserGetClientWindows
    String "browser.getUserContexts" -> pure BrowserGetUserContexts
    String "browser.removeUserContext" -> pure BrowserRemoveUserContext
    String "browser.setClientWindowState" -> pure BrowserSetClientWindowState
    String "browsingContext.activate" -> pure BrowsingContextActivate
    String "browsingContext.captureScreenshot" -> pure BrowsingContextCaptureScreenshot
    String "browsingContext.close" -> pure BrowsingContextClose
    String "browsingContext.create" -> pure BrowsingContextCreate
    String "browsingContext.getTree" -> pure BrowsingContextGetTree
    String "browsingContext.handleUserPrompt" -> pure BrowsingContextHandleUserPrompt
    String "browsingContext.locateNodes" -> pure BrowsingContextLocateNodes
    String "browsingContext.navigate" -> pure BrowsingContextNavigate
    String "browsingContext.print" -> pure BrowsingContextPrint
    String "browsingContext.reload" -> pure BrowsingContextReload
    String "browsingContext.setViewport" -> pure BrowsingContextSetViewport
    String "browsingContext.traverseHistory" -> pure BrowsingContextTraverseHistory
    String "input.performActions" -> pure InputPerformActions
    String "input.releaseActions" -> pure InputReleaseActions
    String "input.setFiles" -> pure InputSetFiles
    String "network.addIntercept" -> pure NetworkAddIntercept
    String "network.continueRequest" -> pure NetworkContinueRequest
    String "network.continueResponse" -> pure NetworkContinueResponse
    String "network.continueWithAuth" -> pure NetworkContinueWithAuth
    String "network.failRequest" -> pure NetworkFailRequest
    String "network.provideResponse" -> pure NetworkProvideResponse
    String "network.removeIntercept" -> pure NetworkRemoveIntercept
    String "network.setCacheBehavior" -> pure NetworkSetCacheBehavior
    String "permissions.setPermission" -> pure PermissionsSetPermission
    String "script.addPreloadScript" -> pure ScriptAddPreloadScript
    String "script.callFunction" -> pure ScriptCallFunction
    String "script.disown" -> pure ScriptDisown
    String "script.evaluate" -> pure ScriptEvaluate
    String "script.getRealms" -> pure ScriptGetRealms
    String "script.removePreloadScript" -> pure ScriptRemovePreloadScript
    String "session.end" -> pure SessionEnd
    String "session.new" -> pure SessionNew
    String "session.status" -> pure SessionStatus
    String "session.subscribe" -> pure SessionSubscribe
    String "session.unsubscribe" -> pure SessionUnsubscribe
    String "storage.deleteCookies" -> pure StorageDeleteCookies
    String "storage.getCookies" -> pure StorageGetCookies
    String "storage.setCookie" -> pure StorageSetCookie
    c ->
      fail . unpack $
        "Unrecognised CommandMethodType: "
          <> txt c
          <> "\n"
          <> ". Expected one of: "
          <> "\n"
          <> (T.unlines $ txt <$> enumerate @CommandMethodType)

-- uses txt for method rather than CommandType to allow for workarounds
-- for unexpected method names
data Command c r = MkCommand
  { method :: Text,
    params :: c,
    extended :: Maybe Object
  }
  deriving (Show, Eq)

mkCommandTxt :: forall c r. Text -> c -> Command c r
mkCommandTxt method params = MkCommand {method, params, extended = Nothing}

emptyCommandTxt :: forall r. Text -> Command Object r
emptyCommandTxt method = mkCommandTxt method KM.empty

mkCommand :: forall c r. CommandMethodType -> c -> Command c r
mkCommand method params = MkCommand {method = txt method, params, extended = Nothing}

emptyCommand :: forall r. CommandMethodType -> Command Object r
emptyCommand method = mkCommand method KM.empty

extendParams :: Command c r -> Object -> Command c r
extendParams MkCommand {method, params} extended = MkCommand {method, params, extended = Just extended}

commandValue :: (ToJSON c) => Command c r -> JSUInt -> Value
commandValue MkCommand {method, params, extended} id' =
  object
    [ "id" .= id',
      "method" .= method,
      "params" .= maybe cmdObj (cmdObj <>) extended
    ]
  where
    cmdObj = objectOrThrow ("Failed setting command parameters for method: " <> method) params