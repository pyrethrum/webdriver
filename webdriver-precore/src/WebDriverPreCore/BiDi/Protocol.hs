module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Object,
  )
import Data.Text
import WebDriverPreCore.BiDi.BrowsingContext
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Command 
import WebDriverPreCore.BiDi.Session
import Prelude



---- Session ----

sessionNew :: Capabilities -> Command Capabilities SessionNewResult
sessionNew = mkCommand "session.new"

sessionStatus :: Command Object SessionStatusResult
sessionStatus = emptyCommand "session.status"

sessionEnd :: Command Object Object
sessionEnd = emptyCommand "session.end"

sessionSubScribe :: SessionSubscriptionRequest -> Command SessionSubscriptionRequest SessionSubscribeResult
sessionSubScribe = mkCommand "session.subscribe"

sessionUnsubscribe :: SessionUnsubscribeParameters -> Command SessionUnsubscribeParameters Object
sessionUnsubscribe = mkCommand "session.unsubscribe"

---- Browsing Context ----

browsingContextActivate :: Activate -> Command Activate Object
browsingContextActivate = mkCommand "browsingContext.activate"

browsingContextCaptureScreenshot :: CaptureScreenshot -> Command CaptureScreenshot CaptureScreenshotResult
browsingContextCaptureScreenshot = mkCommand "browsingContext.captureScreenshot"

browsingContextClose :: Close -> Command Close Object
browsingContextClose = mkCommand "browsingContext.close"

browsingContextCreate :: Create -> Command Create CreateResult
browsingContextCreate = mkCommand "browsingContext.create"

browsingContextGetTree :: GetTree -> Command GetTree GetTreeResult
browsingContextGetTree = mkCommand "browsingContext.getTree"

browsingContextHandleUserPrompt :: HandleUserPrompt -> Command HandleUserPrompt Object
browsingContextHandleUserPrompt = mkCommand "browsingContext.handleUserPrompt"

browsingContextLocateNodes :: LocateNodes -> Command LocateNodes LocateNodesResult
browsingContextLocateNodes = mkCommand "browsingContext.locateNodes"

browsingContextNavigate :: Navigate -> Command Navigate NavigateResult
browsingContextNavigate = mkCommand "browsingContext.navigate"

browsingContextPrint :: Print -> Command Print PrintResult
browsingContextPrint = mkCommand "browsingContext.print"

browsingContextReload :: Reload -> Command Reload Object
browsingContextReload = mkCommand "browsingContext.reload"

browsingContextSetViewport :: SetViewport -> Command SetViewport Object
browsingContextSetViewport = mkCommand "browsingContext.setViewport"

browsingContextTraverseHistory :: TraverseHistory -> Command TraverseHistory TraverseHistoryResult
browsingContextTraverseHistory = mkCommand "browsingContext.traverseHistory"


