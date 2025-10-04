module BiDi.Demos.LogEventDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Log (LogEntry (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)


{- 
Log Events TODO:

1. log.entryAdded (generic) :: TODO
2. log.entryAdded (console) :: TODO
3. log.entryAdded (javascript) :: TODO
-}
