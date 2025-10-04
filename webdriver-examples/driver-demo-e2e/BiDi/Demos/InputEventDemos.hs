module BiDi.Demos.InputEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Input (FileDialogOpened (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)


{- 
Input Events TODO:

1. input.fileDialogOpened :: TODO
-}
