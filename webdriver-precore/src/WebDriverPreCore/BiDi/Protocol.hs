module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Object,
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Text
import WebDriverPreCore.BiDi.BrowsingContext
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command (Command (..))
import WebDriverPreCore.BiDi.Session
import Prelude

unExtended :: forall c r. Text -> c -> Command c r
unExtended method params = MkCommand {method, params, extended = Nothing}

emptyCommand :: forall r. Text -> Command Object r
emptyCommand method = unExtended method KM.empty

setExtension :: Command c r -> Object -> Command c r
setExtension MkCommand {method, params} extended = MkCommand {method, params, extended = Just extended}

---- Session ----

sessionNew :: Capabilities -> Command Capabilities SessionNewResult
sessionNew = unExtended "session.new"

sessionStatus :: Command Object SessionStatusResult
sessionStatus = emptyCommand "session.status"

sessionEnd :: Command Object Object
sessionEnd = emptyCommand "session.end"

sessionSubScribe :: SessionSubscriptionRequest -> Command SessionSubscriptionRequest SessionSubscribeResult
sessionSubScribe = unExtended "session.subscribe"

sessionUnsubscribe :: SessionUnsubscribeParameters -> Command SessionUnsubscribeParameters Object
sessionUnsubscribe = unExtended "session.unsubscribe"

---- Browsing Context ----

browsingContextActivate :: Activate -> Command Activate Object
browsingContextActivate = unExtended "browsingContext.activate"

browsingContextCaptureScreenshot :: CaptureScreenshot -> Command CaptureScreenshot CaptureScreenshotResult
browsingContextCaptureScreenshot = unExtended "browsingContext.captureScreenshot"

browsingContextClose :: Close -> Command Close Object
browsingContextClose = unExtended "browsingContext.close"

browsingContextCreate :: Create -> Command Create CreateResult
browsingContextCreate = unExtended "browsingContext.create"

browsingContextGetTree :: GetTree -> Command GetTree GetTreeResult
browsingContextGetTree = unExtended "browsingContext.getTree"

browsingContextHandleUserPrompt :: HandleUserPrompt -> Command HandleUserPrompt Object
browsingContextHandleUserPrompt = unExtended "browsingContext.handleUserPrompt"

browsingContextLocateNodes :: LocateNodes -> Command LocateNodes LocateNodesResult
browsingContextLocateNodes = unExtended "browsingContext.locateNodes"

browsingContextNavigate :: Navigate -> Command Navigate NavigateResult
browsingContextNavigate = unExtended "browsingContext.navigate"

browsingContextPrint :: Print -> Command Print PrintResult
browsingContextPrint = unExtended "browsingContext.print"

browsingContextReload :: Reload -> Command Reload Object
browsingContextReload = unExtended "browsingContext.reload"

browsingContextSetViewport :: SetViewport -> Command SetViewport Object
browsingContextSetViewport = unExtended "browsingContext.setViewport"

browsingContextTraverseHistory :: TraverseHistory -> Command TraverseHistory TraverseHistoryResult
browsingContextTraverseHistory = unExtended "browsingContext.traverseHistory"
