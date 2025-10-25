module WebDriverPreCore.BiDi.Command where

import Data.Aeson
  ( Object,
    Result (..),
    ToJSON,
    Value (..),
    object,
    (.=),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (..))
import Data.Text as T (Text, unlines, unpack, intercalate)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import WebDriverPreCore.Internal.AesonUtils (objectOrThrow, jsonToText)
import WebDriverPreCore.Internal.Utils (enumerate, txt)
import Prelude

data CommandMethod
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

instance ToJSON CommandMethod where
  toJSON :: CommandMethod -> Value
  toJSON = String . toMethodText

toMethodText :: CommandMethod -> Text
toMethodText = \case
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

instance FromJSON CommandMethod where
  parseJSON :: Value -> Parser CommandMethod
  parseJSON = \case
    String c -> case c of
      "browser.close" -> pure BrowserClose
      "browser.createUserContext" -> pure BrowserCreateUserContext
      "browser.getClientWindows" -> pure BrowserGetClientWindows
      "browser.getUserContexts" -> pure BrowserGetUserContexts
      "browser.removeUserContext" -> pure BrowserRemoveUserContext
      "browser.setClientWindowState" -> pure BrowserSetClientWindowState
      "browsingContext.activate" -> pure BrowsingContextActivate
      "browsingContext.captureScreenshot" -> pure BrowsingContextCaptureScreenshot
      "browsingContext.close" -> pure BrowsingContextClose
      "browsingContext.create" -> pure BrowsingContextCreate
      "browsingContext.getTree" -> pure BrowsingContextGetTree
      "browsingContext.handleUserPrompt" -> pure BrowsingContextHandleUserPrompt
      "browsingContext.locateNodes" -> pure BrowsingContextLocateNodes
      "browsingContext.navigate" -> pure BrowsingContextNavigate
      "browsingContext.print" -> pure BrowsingContextPrint
      "browsingContext.reload" -> pure BrowsingContextReload
      "browsingContext.setViewport" -> pure BrowsingContextSetViewport
      "browsingContext.traverseHistory" -> pure BrowsingContextTraverseHistory
      "input.performActions" -> pure InputPerformActions
      "input.releaseActions" -> pure InputReleaseActions
      "input.setFiles" -> pure InputSetFiles
      "network.addIntercept" -> pure NetworkAddIntercept
      "network.continueRequest" -> pure NetworkContinueRequest
      "network.continueResponse" -> pure NetworkContinueResponse
      "network.continueWithAuth" -> pure NetworkContinueWithAuth
      "network.failRequest" -> pure NetworkFailRequest
      "network.provideResponse" -> pure NetworkProvideResponse
      "network.removeIntercept" -> pure NetworkRemoveIntercept
      "network.setCacheBehavior" -> pure NetworkSetCacheBehavior
      "permissions.setPermission" -> pure PermissionsSetPermission
      "script.addPreloadScript" -> pure ScriptAddPreloadScript
      "script.callFunction" -> pure ScriptCallFunction
      "script.disown" -> pure ScriptDisown
      "script.evaluate" -> pure ScriptEvaluate
      "script.getRealms" -> pure ScriptGetRealms
      "script.removePreloadScript" -> pure ScriptRemovePreloadScript
      "session.end" -> pure SessionEnd
      "session.new" -> pure SessionNew
      "session.status" -> pure SessionStatus
      "session.subscribe" -> pure SessionSubscribe
      "session.unsubscribe" -> pure SessionUnsubscribe
      "storage.deleteCookies" -> pure StorageDeleteCookies
      "storage.getCookies" -> pure StorageGetCookies
      "storage.setCookie" -> pure StorageSetCookie
      _ -> failConversion c
    c -> failConversion $ jsonToText c
    where
      failConversion c =
        fail . unpack $
          "Unrecognised CommandMethodType: "
            <> c
            <> " - "
            <> "Expected one of: "
            <> " "
            <> (T.intercalate ", " $ toMethodText <$> enumerate @CommandMethod)

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

mkCommand :: forall c r. CommandMethod -> c -> Command c r
mkCommand method params = MkCommand {method = toMethodText method, params, extended = Nothing}

emptyCommand :: forall r. CommandMethod -> Command Object r
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