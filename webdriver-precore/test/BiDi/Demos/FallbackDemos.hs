module BiDi.Demos.FallbackDemos where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import Const (seconds)
import Data.Aeson (Value (..), FromJSON)
import Data.Aeson.KeyMap qualified as KM
import IOUtils (DemoActions (..))
import TestData (contentPageUrl)
import WebDriverPreCore.BiDi.API qualified as API
import WebDriverPreCore.BiDi.Protocol
    ( JSUInt(MkJSUInt),
      BrowsingContext(MkBrowsingContext),
      UnknownSubscriptionType(MkUnknownSubscriptionType),
      URL(..),
      GetTree(..), Navigate(..),
      coerceCommand,
      extendLoosenCommand,
      loosenCommand,
      mkOffSpecCommand )
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)
import GHC.Generics (Generic)

-- >>> runDemo fallbackExtendCommandDemo
fallbackExtendCommandDemo :: BiDiDemo
fallbackExtendCommandDemo =
  demo "Fallback - Extend Navigate Command with Extra Property" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      url <- contentPageUrl

      logTxt "Demo 1: Navigate using typed command with extension"
      logTxt "Creating Navigate command with extra 'customProperty' field"
      
      let navigateCmd = API.browsingContextNavigate $ MkNavigate
            { context = bc,
              url,
              wait = Nothing
            }
      
          extraProps = KM.fromList [("customProperty", String "this will be ignored by the driver")]
          extendedCmd = extendLoosenCommand extraProps navigateCmd
      
      logShow "Extended command (with extra property)" extendedCmd
      pause

      logTxt "Sending extended navigate command..."
      result <- sendCommand' (MkJSUInt 100) extendedCmd
      logShow "Navigation result" result
      pause


-- >>> runDemo fallbackOffSpecCommandDemo
fallbackOffSpecCommandDemo :: BiDiDemo
fallbackOffSpecCommandDemo =
  demo "Fallback - Navigate Using mkAnyCommand with Raw Object" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      url <- contentPageUrl

      logTxt "Demo 2: Navigate using mkAnyCommand with raw Aeson Object"
      logTxt "Constructing navigation parameters manually as JSON object"
      
      let MkBrowsingContext contextId = bc
          navParams = KM.fromList
            [ ("context", String contextId),
              ("url", String url.url),
              ("customProperty", String "this will also be ignored by the driver")
            ]
      
          unknownNav = mkOffSpecCommand "browsingContext.navigate" navParams
      
      logShow "Any command (raw object)" unknownNav
      logShowM "Unknown navigate result" $ sendCommand unknownNav
      pause

      logTxt "Sending command via sendAnyCommand'..."
      resultObj <- sendOffSpecCommand' (MkJSUInt 101) "browsingContext.navigate" navParams 
      
      logShow "Navigation result object" resultObj
      pause

newtype GetTreeResultContextOnly = MkGetTreeResultContextOnly
  { contexts :: [ContextOnly]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetTreeResultContextOnly

data ContextOnly = MkContextOnly
  { 
    context :: BrowsingContext
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContextOnly

-- >>> runDemo fallbackCommandCoercionsDemo
fallbackCommandCoercionsDemo :: BiDiDemo
fallbackCommandCoercionsDemo =
  demo "Fallback - Command Coercions" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      url <- contentPageUrl

      logTxt "Navigate using typed command"
      browsingContextNavigate $ MkNavigate {context = bc, url, wait = Nothing}
      pause

      logTxt "Return a different type with compatible JSON (coerceCommand)"
      logShowM 
        "Get tree result - coerced to JSON compatible GetTreeResultContextOnly" 
          $ sendCommand . coerceCommand @_ @GetTreeResultContextOnly $ API.browsingContextGetTree (MkGetTree Nothing Nothing)
      pause

      logTxt "Return Value rather than GetTreeResult (loosenCommand)"
      treeVal <- sendCommand . loosenCommand $ API.browsingContextGetTree (MkGetTree Nothing Nothing)
      logShow "Get tree result - as Value" treeVal
      pause

-- >>> runDemo fallbackSubscribeUnknownEventDemo
fallbackSubscribeUnknownEventDemo :: BiDiDemo
fallbackSubscribeUnknownEventDemo =
  demo "Fallback - Subscribe to Navigation Event Using Unknown Subscription" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      url <- contentPageUrl

      logTxt "Demo 3: Subscribe to browsingContext.navigationStarted using subscribeUnknownMany"
      logTxt "This demonstrates subscribing to events as unknown types"
      
      let unknownEventType = MkUnknownSubscriptionType "browsingContext.navigationStarted"
      
      (unknownEventFired, waitUnknownEventFired) <- timeLimitLog' "navigationStarted (unknown subscription)" (10 * seconds) unknownEventType
      
      subId <- subscribeUnknownMany
        [unknownEventType]
        unknownEventFired
      
      logShow "Subscription ID" subId
      pause

      logTxt "Navigating to trigger the event..."
      navResult <- browsingContextNavigate $ MkNavigate
        { context = bc,
          url = url,
          wait = Nothing
        }
      logShow "Navigation result" navResult
      pause

      logTxt "Waiting for event to be received..."
      waitUnknownEventFired

      logTxt "Unsubscribing from unknown event"
      unsubscribe subId
      pause

-- >>> runDemo fallbackSubscribeUnknownEventFilteredDemo
fallbackSubscribeUnknownEventFilteredDemo :: BiDiDemo
fallbackSubscribeUnknownEventFilteredDemo =
  demo "Fallback - Subscribe to Navigation Event with Context Filter" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      url <- contentPageUrl

      logTxt "Demo 4: Subscribe using subscribeUnknownMany' with context filtering"
      logTxt "Creating a second browsing context for comparison"
      
      bc2 <- newWindowContext utils bidi
      pause

      logTxt $ "Subscribing to navigationStarted events ONLY for first context: " <> txt bc
      let unknownEventType = MkUnknownSubscriptionType "browsingContext.navigationStarted"
      
      (unknownEventFired, waitUnknownEventFired) <- timeLimitLog' "navigationStarted (filtered unknown subscription)" (10 * seconds) unknownEventType
      
      subId <- subscribeUnknownMany'
        [bc]  -- Only subscribe for first context
        []    -- No user context filter
        [unknownEventType]
        unknownEventFired
      
      logShow "Subscription ID" subId
      pause

      logTxt $ "Navigating in FIRST context (should trigger event): " <> txt bc
      navResult1 <- browsingContextNavigate $ MkNavigate
        { context = bc,
          url = url,
          wait = Nothing
        }
      logShow "Navigation result (first context)" navResult1
      
      logTxt "Waiting for event..."
      waitUnknownEventFired

      logTxt $ "Navigating in SECOND context (should NOT trigger event): " <> txt bc2
      navResult2 <- browsingContextNavigate $ MkNavigate
        { context = bc2,
          url = url,
          wait = Nothing
        }
      logShow "Navigation result (second context)" navResult2
      pause

      logTxt "Unsubscribing from filtered unknown event"
      unsubscribe subId
      
