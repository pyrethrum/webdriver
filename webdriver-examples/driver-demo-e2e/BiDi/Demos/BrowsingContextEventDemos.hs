module BiDi.Demos.BrowsingContextEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent (..), Locator (..), PrintMargin (..), PrintPage (..), Viewport (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..), NodeRemoteValue (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)


{- 
BrowsingContext Events TODO:

1. browsingContext.contextCreated :: TODO
2. browsingContext.contextDestroyed :: TODO
3. browsingContext.domContentLoaded :: TODO
4. browsingContext.downloadEnd :: TODO
5. browsingContext.downloadWillBegin :: TODO
6. browsingContext.fragmentNavigated :: TODO
7. browsingContext.historyUpdated :: TODO
8. browsingContext.load :: TODO
9. browsingContext.navigationAborted :: TODO
10. browsingContext.navigationCommitted :: TODO
11. browsingContext.navigationFailed :: TODO
12. browsingContext.navigationStarted :: TODO
13. browsingContext.userPromptClosed :: TODO
14. browsingContext.userPromptOpened :: TODO
-}


-- >>> runDemo browsingContextEventDemo
browsingContextEventDemo :: BiDiDemo
browsingContextEventDemo =
  demo "Browsing Context Events" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      undefined
