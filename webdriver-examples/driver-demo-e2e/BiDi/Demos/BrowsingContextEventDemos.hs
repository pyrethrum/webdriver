module BiDi.Demos.BrowsingContextEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import Prelude hiding (log, putStrLn)
import WebDriverPreCore.BiDi.Protocol (Create(..))
import WebDriverPreCore.BiDi.Protocol (CreateType(..))
import Const (seconds)


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


--  TODO aake sure all demos only use Protocol, not sub modules

-- >>> runDemo browsingContextEventDemo
browsingContextEventDemo :: BiDiDemo
browsingContextEventDemo =
  demo "Browsing Context Events" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      subId <- subscribeBrowsingContextCreated (logShow "Event: browsingContext.contextCreated")
      logShow "Subscription id:" subId

      logTxt "New browsing context - Tab"
      let bcParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      bc <- browsingContextCreate bcParams
      logShow "Browsing context - Tab" bc
      pauseMin $ 5 * seconds
