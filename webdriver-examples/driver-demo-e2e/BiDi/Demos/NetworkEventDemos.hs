module BiDi.Demos.NetworkEventDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Network (NetworkEvent (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)


{- 
Network Events TODO:

1. network.authRequired :: TODO
2. network.beforeRequestSent :: TODO
3. network.fetchError :: TODO
4. network.responseCompleted :: TODO
5. network.responseStarted :: TODO
-}
