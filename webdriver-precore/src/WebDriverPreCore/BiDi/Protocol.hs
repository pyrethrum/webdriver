module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Object,
    Value (..), ToJSON,
  )
import WebDriverPreCore.BiDi.Command as C (command, extendedCommand)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt, BiDiMethod)

data Command c r = MkCommand
  { command :: c -> JSUInt -> Value,
    extended :: c -> Object -> JSUInt -> Value
  }

command :: (BiDiMethod c, ToJSON c) => Command c r
command = MkCommand {command = C.command, extended = extendedCommand}
