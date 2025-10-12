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
  demo "Browsing Context Create - Subscribe Unsubscribe" action
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
  demo "Browsing Context Events - Subscribe Unsubscribe Using subscribeMany" action
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

-- >>> runDemo browsingContextEventDemoFilteredSubscriptions
browsingContextEventDemoFilteredSubscriptions :: BiDiDemo
browsingContextEventDemoFilteredSubscriptions =
  demo "Browsing Context Events - Filtered Subscriptions per Context" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Creating two initial browsing contexts"
      
      let createParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      
      -- Create first parent context
      parentContext1 <- browsingContextCreate createParams
      logShow "Created parent context 1:" parentContext1
      
      -- Create second parent context
      parentContext2 <- browsingContextCreate createParams
      logShow "Created parent context 2:" parentContext2
      
      logTxt "Setting up filtered subscriptions"
      
      -- Subscribe to contextCreated events only for parentContext1
      subIdCreated1 <- subscribeBrowsingContextCreated' [parentContext1] [] $ logShow "Created Event Fired" 
      logShow "Subscribed to contextCreated for parent context 1:" subIdCreated1
      
      -- Subscribe to contextDestroyed events only for parentContext2
      subIdDestroyed2 <- subscribeBrowsingContextDestroyed' [parentContext2] [] $ logShow "Destroyed Event Fired"
      logShow "Subscribed to contextDestroyed for parent context 2:" subIdDestroyed2
      pause
      
      logTxt "Creating child contexts"
      
      -- Create a child context in parentContext1
      logTxt "Creating child in parent context 1 (should trigger PARENT 1 - Created Event)"
      let childParams1 = createParams {referenceContext = Just parentContext1}
      childContext1 <- browsingContextCreate childParams1
      logShow "Created child context 1:" childContext1
      pause
      
      -- Create a child context in parentContext2
      logTxt "Creating child in parent context 2 - no subscription"
      let childParams2 = createParams {referenceContext = Just parentContext2}
      childContext2 <- browsingContextCreate childParams2
      logShow "Created child context 2:" childContext2
      pause
      
      logTxt "Closing child contexts"
      
      -- Close child context 1 (should trigger PARENT 1 - Destroyed Event)
      logTxt "Closing child context 1 - no subscription"
      browsingContextClose $ MkClose childContext1 Nothing
      pause
      
      -- Close child context 2 (should trigger PARENT 2 - Destroyed Event)
      logTxt "Closing child context 2 (should trigger PARENT 2 - Destroyed Event)"
      browsingContextClose $ MkClose childContext2 Nothing
      pause

      pauseAtLeast $ 2 * seconds
      

  
