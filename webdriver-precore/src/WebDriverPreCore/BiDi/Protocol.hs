module WebDriverPreCore.BiDi.Protocol where

import Prelude
import Data.Text


import Data.Aeson
  ( Object,
    Value (..), ToJSON,
  )

import WebDriverPreCore.BiDi.CoreTypes (JSUInt, BiDiMethod)
import WebDriverPreCore.BiDi.Command (Command(..))
import WebDriverPreCore.BiDi.Capabilities (Capabilities)
import WebDriverPreCore.BiDi.Session 
import Data.Aeson.KeyMap qualified as KM

unExtended :: forall c r. Text -> c -> Command c r
unExtended method params = MkCommand {method, params, extended = Nothing}

emptyCommand :: forall r. Text -> Command Object r 
emptyCommand method = unExtended method KM.empty

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


