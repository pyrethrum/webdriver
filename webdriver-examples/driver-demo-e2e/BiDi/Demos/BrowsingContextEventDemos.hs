module BiDi.Demos.BrowsingContextEventDemos where

import BiDi.BiDiRunner (BiDiActions (..), BiDiMethods (unsubscribe))
import BiDi.DemoUtils
import Const (second, seconds)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BrowsingContext (Close (..))
import WebDriverPreCore.BiDi.Protocol (BrowsingContext (context), Create (..), CreateType (..), SubscriptionType (BrowsingContextContextCreated, BrowsingContextContextDestroyed))
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

--  TODO aake sure all demos only use Protocol, not sub modules

-- >>> runDemo browsingContextEventDemo
browsingContextEventDemo :: BiDiDemo
browsingContextEventDemo =
  demo "Browsing Context Events" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      subId <- subscribeBrowsingContextCreated (logShow "Event Subscription Fired: browsingContext.contextCreated")
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
      logShow "Browsing context - Tab II" bc

      logShow "Unsubscribing from browsingContext.contextCreated event" subId
      unsubscribe subId

      bc2 <- browsingContextCreate bcParams
      logShow "Browsing context - Tab III (No Event Triggered)" bc2
      pause

-- >>> runDemo browsingContextEventDemoMulti
browsingContextEventDemoMulti :: BiDiDemo
browsingContextEventDemoMulti =
  demo "Browsing Context Events" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      subId <- subscribeMany [BrowsingContextContextCreated, BrowsingContextContextDestroyed] (logShow "Event Subscription Fired: browsingContext.contextCreated or contextDestroyed")
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
      logShow "Browsing context - Tab II" bc

      logShow "Closing browsing context - Tab II" bc
      browsingContextClose
        MkClose
          { context = bc,
            promptUnload = Nothing
          }

      logShow "Unsubscribing from events" subId
      unsubscribe subId

      bc2 <- browsingContextCreate bcParams
      logShow "Browsing context created - Tab III (No Event Triggered)" bc2

      browsingContextClose $ MkClose bc2 Nothing
      logShow "Browsing context destroyed - Tab III (No Event Triggered)" bc2
  
