module BiDi.Demos.ScriptEventDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Script (ScriptEvent (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)


{- 
Script Events TODO:

1. script.message :: TODO
2. script.realmCreated :: TODO
3. script.realmDestroyed :: TODO
-}
