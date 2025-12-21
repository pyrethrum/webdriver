module WebDriverPreCore.BiDi.Command
  ( Command (..),
    CommandMethod (..), 
    KnownCommand (..), 
    OffSpecCommand (..), 
    mkCommand,
    emptyCommand,
    mkOffSpecCommand,
    extendLoosenCommand,
    extendCommand,
    extendCoerceCommand,
    loosenCommand,
    coerceCommand,
    knownCommandToText,
    toCommandText,
  )
where

import AesonUtils (jsonToText, objectOrThrow)
import Control.Applicative (Alternative (..))
import Data.Aeson
  ( Object,
    ToJSON,
    Value (..),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (..))
import Data.Text as T (Text, intercalate, unpack)
import Utils (enumerate)

-- |  A BiDi Command to be sent over the WebSocket connection.
--
-- All functions in "WebDriverPreCore.BiDi.API" return values of this type.
--
-- The command contains the method name and parameters to be serialized as JSON and a phantom response type @r@, which is always an instance of 'FromJSON'.
data Command r = MkCommand
  { -- | An identifier for the command to be invoked
    method :: CommandMethod,
    -- | The JSON payload to be sent
    params :: Object
  }
  deriving (Show, Eq)

-- | The method name of a BiDi command. This library provides known commands as 'KnownCommand' values, but users can also create off-spec commands using 'OffSpecCommand' as a fallback, when the driver supports commands not yet implemented in this library.
data CommandMethod
  = KnownCommand KnownCommand
  | OffSpecCommand OffSpecCommand
  deriving (Show, Eq)

instance ToJSON CommandMethod where
  toJSON :: CommandMethod -> Value
  toJSON = String . toCommandText

-- | The method name of a BiDi command known to this library.
data KnownCommand
  = BrowserClose
  | BrowserCreateUserContext
  | BrowserGetClientWindows
  | BrowserGetUserContexts
  | BrowserRemoveUserContext
  | BrowserSetClientWindowState
  | BrowserSetDownloadBehavior
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
  | EmulationSetForcedColorsModeThemeOverride
  | EmulationSetGeolocationOverride
  | EmulationSetLocaleOverride
  | EmulationSetNetworkConditions
  | EmulationSetScreenOrientationOverride
  | EmulationSetScreenSettingsOverride
  | EmulationSetScriptingEnabled
  | EmulationSetTimezoneOverride
  | EmulationSetUserAgentOverride
  | InputPerformActions
  | InputReleaseActions
  | InputSetFiles
  | NetworkAddDataCollector
  | NetworkAddIntercept
  | NetworkContinueRequest
  | NetworkContinueResponse
  | NetworkContinueWithAuth
  | NetworkDisownData
  | NetworkFailRequest
  | NetworkGetData
  | NetworkProvideResponse
  | NetworkRemoveDataCollector
  | NetworkRemoveIntercept
  | NetworkSetCacheBehavior
  | NetworkSetExtraHeaders
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
  | WebExtensionInstall
  | WebExtensionUninstall
  deriving (Show, Eq, Enum, Bounded)


-- | The method name of a BiDi command not yet implemented in this library
newtype OffSpecCommand = MkOffSpecCommand {command :: Text}
  deriving (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

-- constructors

mkCommand :: forall c r. (ToJSON c) => KnownCommand -> c -> Command r
mkCommand method params = MkCommand {method = KnownCommand method, params = objectOrThrow ("mkCommand - " <> toCommandText (KnownCommand method)) params}

emptyCommand :: forall r. KnownCommand -> Command r
emptyCommand method = MkCommand {method = KnownCommand method, params = KM.empty}

mkOffSpecCommand :: Text -> Object -> Command Object
mkOffSpecCommand method = MkCommand (OffSpecCommand $ MkOffSpecCommand method)

-- fallback modifiers

extendLoosenCommand :: forall r. Object -> Command r -> Command Value
extendLoosenCommand = extendCoerceCommand

extendCommand :: forall r. Object -> Command r -> Command r
extendCommand = extendCoerceCommand

extendCoerceCommand :: forall r r2. Object -> Command r -> Command r2
extendCoerceCommand extended MkCommand {method, params} =
  MkCommand
    { method,
      params = params <> extended
    }

loosenCommand :: forall r. Command r -> Command Value
loosenCommand = coerceCommand

coerceCommand :: forall r r'. Command r -> Command r'
coerceCommand MkCommand {method, params} = MkCommand {method, params}

instance ToJSON KnownCommand where
  toJSON :: KnownCommand -> Value
  toJSON = String . knownCommandToText

instance FromJSON KnownCommand where
  parseJSON :: Value -> Parser KnownCommand
  parseJSON =
    \case
      String c -> case c of
        "browser.close" -> p BrowserClose
        "browser.createUserContext" -> p BrowserCreateUserContext
        "browser.getClientWindows" -> p BrowserGetClientWindows
        "browser.getUserContexts" -> p BrowserGetUserContexts
        "browser.removeUserContext" -> p BrowserRemoveUserContext
        "browser.setClientWindowState" -> p BrowserSetClientWindowState
        "browser.setDownloadBehavior" -> p BrowserSetDownloadBehavior
        "browsingContext.activate" -> p BrowsingContextActivate
        "browsingContext.captureScreenshot" -> p BrowsingContextCaptureScreenshot
        "browsingContext.close" -> p BrowsingContextClose
        "browsingContext.create" -> p BrowsingContextCreate
        "browsingContext.getTree" -> p BrowsingContextGetTree
        "browsingContext.handleUserPrompt" -> p BrowsingContextHandleUserPrompt
        "browsingContext.locateNodes" -> p BrowsingContextLocateNodes
        "browsingContext.navigate" -> p BrowsingContextNavigate
        "browsingContext.print" -> p BrowsingContextPrint
        "browsingContext.reload" -> p BrowsingContextReload
        "browsingContext.setViewport" -> p BrowsingContextSetViewport
        "browsingContext.traverseHistory" -> p BrowsingContextTraverseHistory
        "emulation.setForcedColorsModeThemeOverride" -> p EmulationSetForcedColorsModeThemeOverride
        "emulation.setGeolocationOverride" -> p EmulationSetGeolocationOverride
        "emulation.setLocaleOverride" -> p EmulationSetLocaleOverride
        "emulation.setNetworkConditions" -> p EmulationSetNetworkConditions
        "emulation.setScreenOrientationOverride" -> p EmulationSetScreenOrientationOverride
        "emulation.setScreenSettingsOverride" -> p EmulationSetScreenSettingsOverride
        "emulation.setScriptingEnabled" -> p EmulationSetScriptingEnabled
        "emulation.setTimezoneOverride" -> p EmulationSetTimezoneOverride
        "emulation.setUserAgentOverride" -> p EmulationSetUserAgentOverride
        "input.performActions" -> p InputPerformActions
        "input.releaseActions" -> p InputReleaseActions
        "input.setFiles" -> p InputSetFiles
        "network.addDataCollector" -> p NetworkAddDataCollector
        "network.addIntercept" -> p NetworkAddIntercept
        "network.continueRequest" -> p NetworkContinueRequest
        "network.continueResponse" -> p NetworkContinueResponse
        "network.continueWithAuth" -> p NetworkContinueWithAuth
        "network.disownData" -> p NetworkDisownData
        "network.failRequest" -> p NetworkFailRequest
        "network.getData" -> p NetworkGetData
        "network.provideResponse" -> p NetworkProvideResponse
        "network.removeDataCollector" -> p NetworkRemoveDataCollector
        "network.removeIntercept" -> p NetworkRemoveIntercept
        "network.setCacheBehavior" -> p NetworkSetCacheBehavior
        "network.setExtraHeaders" -> p NetworkSetExtraHeaders
        "script.addPreloadScript" -> p ScriptAddPreloadScript
        "script.callFunction" -> p ScriptCallFunction
        "script.disown" -> p ScriptDisown
        "script.evaluate" -> p ScriptEvaluate
        "script.getRealms" -> p ScriptGetRealms
        "script.removePreloadScript" -> p ScriptRemovePreloadScript
        "session.end" -> p SessionEnd
        "session.new" -> p SessionNew
        "session.status" -> p SessionStatus
        "session.subscribe" -> p SessionSubscribe
        "session.unsubscribe" -> p SessionUnsubscribe
        "storage.deleteCookies" -> p StorageDeleteCookies
        "storage.getCookies" -> p StorageGetCookies
        "storage.setCookie" -> p StorageSetCookie
        "webExtension.install" -> p WebExtensionInstall
        "webExtension.uninstall" -> p WebExtensionUninstall
        u -> failConversion u
      c -> failConversion $ jsonToText c
    where
      p = pure
      failConversion c =
        fail . unpack $
          "Unrecognised CommandMethodType: "
            <> c
            <> " - "
            <> "Expected one of: "
            <> " "
            <> (T.intercalate ", " $ knownCommandToText <$> enumerate @KnownCommand)

knownCommandToText :: KnownCommand -> Text
knownCommandToText = \case
  BrowserClose -> "browser.close"
  BrowserCreateUserContext -> "browser.createUserContext"
  BrowserGetClientWindows -> "browser.getClientWindows"
  BrowserGetUserContexts -> "browser.getUserContexts"
  BrowserRemoveUserContext -> "browser.removeUserContext"
  BrowserSetClientWindowState -> "browser.setClientWindowState"
  BrowserSetDownloadBehavior -> "browser.setDownloadBehavior"
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
  EmulationSetForcedColorsModeThemeOverride -> "emulation.setForcedColorsModeThemeOverride"
  EmulationSetGeolocationOverride -> "emulation.setGeolocationOverride"
  EmulationSetLocaleOverride -> "emulation.setLocaleOverride"
  EmulationSetNetworkConditions -> "emulation.setNetworkConditions"
  EmulationSetScreenOrientationOverride -> "emulation.setScreenOrientationOverride"
  EmulationSetScreenSettingsOverride -> "emulation.setScreenSettingsOverride"
  EmulationSetScriptingEnabled -> "emulation.setScriptingEnabled"
  EmulationSetTimezoneOverride -> "emulation.setTimezoneOverride"
  EmulationSetUserAgentOverride -> "emulation.setUserAgentOverride"
  InputPerformActions -> "input.performActions"
  InputReleaseActions -> "input.releaseActions"
  InputSetFiles -> "input.setFiles"
  NetworkAddDataCollector -> "network.addDataCollector"
  NetworkAddIntercept -> "network.addIntercept"
  NetworkContinueRequest -> "network.continueRequest"
  NetworkContinueResponse -> "network.continueResponse"
  NetworkContinueWithAuth -> "network.continueWithAuth"
  NetworkDisownData -> "network.disownData"
  NetworkFailRequest -> "network.failRequest"
  NetworkGetData -> "network.getData"
  NetworkProvideResponse -> "network.provideResponse"
  NetworkRemoveDataCollector -> "network.removeDataCollector"
  NetworkRemoveIntercept -> "network.removeIntercept"
  NetworkSetCacheBehavior -> "network.setCacheBehavior"
  NetworkSetExtraHeaders -> "network.setExtraHeaders"
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
  WebExtensionInstall -> "webExtension.install"
  WebExtensionUninstall -> "webExtension.uninstall"

toCommandText :: CommandMethod -> Text
toCommandText = \case
  KnownCommand k -> knownCommandToText k
  OffSpecCommand (MkOffSpecCommand t) -> t

instance FromJSON CommandMethod where
  parseJSON :: Value -> Parser CommandMethod
  parseJSON val =
    (KnownCommand <$> parseJSON @KnownCommand val)
      <|> (OffSpecCommand <$> parseJSON @OffSpecCommand val)
      <|> failConversion
    where
      failConversion =
        fail . unpack $
          "Unrecognised CommandMethodType: "
            <> jsonToText val
            <> " - "
            <> "Expected a string, the standard command types of whcih are: "
            <> " "
            <> (T.intercalate ", " $ knownCommandToText <$> enumerate @KnownCommand)
