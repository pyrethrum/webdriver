module BiDi.Demos.NetworkDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Data.Aeson (ToJSON (..))
import Data.Maybe (catMaybes)
import Data.Text (isInfixOf, pack)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import qualified WebDriverPreCore.BiDi.Network as Network
import WebDriverPreCore.Internal.AesonUtils (jsonToText)
import Prelude hiding (log, putStrLn)

{-

##### Network #####
1. networkAddDataCollector :: TODO
2. networkAddIntercept :: TODO
3. networkContinueRequest :: TODO
4. networkContinueResponse :: TODO
5. networkContinueWithAuth :: TODO
6. networkDisownData :: TODO
7. networkFailRequest :: TODO
8. networkGetData :: TODO
9. networkProvideResponse :: TODO
10. networkRemoveDataCollector :: TODO
11. networkRemoveIntercept :: TODO
12. networkSetCacheBehavior :: TODO

-}

-- Placeholder for future network demo implementations
-- Following the same structure as ScriptDemos and BrowsingContextDemos