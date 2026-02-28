{-# LANGUAGE DataKinds #-}

-- |
-- Test suite for webdriver-bluefin-poc library
module Main where

import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Bluefin.Eff (runEff_)
import Bluefin.IO (effIO)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import UnliftIO (throwIO)
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (atomically, newEmptyTMVarIO, readTMVar, tryPutTMVar)
import Utils (txt)
import WebDriver.Bluefin hiding (log)
import WebDriver.Bluefin qualified as B
import WebDriver.Bluefin.BiDi.Base.Actions
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    GetTree (..),
    GetTreeResult (..),
    Info (..),
    KeySourceAction (..),
    KeySourceActions (..),
    KnownSubscriptionType (..),
    LocateNodes (..),
    LocateNodesResult (..),
    Locator (..),
    Navigate (..),
    NodeRemoteValue (..),
    Origin (..),
    PerformActions (..),
    Pointer (..),
    PointerCommonProperties (..),
    PointerSourceAction (..),
    PointerSourceActions (..),
    PointerType (..),
    SharedId,
    SharedReference (..),
    SourceActions (..),
  )
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Test.TestData (loginUrl)
import WebDriverPreCore.Utils.Timeout (milliseconds)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Bluefin Tests"
    [ testCase "BiDi login demo" bidi_login_demo
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkInteractBehaviour :: Config -> InteractBehaviour
mkInteractBehaviour config =
  MkInteractBehaviour
    { pauseDuration = fromIntegral config.pauseMS * milliseconds,
      driverLogging = config.logging
    }

mkBiDiCaps :: Config -> HttpCapabilities
mkBiDiCaps config =
  MkFullCapabilities
    { alwaysMatch = Just cap {httpWebSocketUrl = Just True},
      firstMatch = []
    }
  where
    cap = fromHttpCapability $ httpCapabilities config

-- | Minimal pointer properties with all optional fields set to 'Nothing'.
defaultPointerProps :: PointerCommonProperties
defaultPointerProps =
  MkPointerCommonProperties
    { width = Nothing,
      height = Nothing,
      pressure = Nothing,
      tangentialPressure = Nothing,
      twist = Nothing,
      altitudeAngle = Nothing,
      azimuthAngle = Nothing
    }

-- | Convert a 'Char' to a pair of keyDown/keyUp 'KeySourceAction's.
charToKeys :: Char -> [KeySourceAction]
charToKeys c = [KeyDown {value = T.singleton c}, KeyUp {value = T.singleton c}]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

-- | BiDi version of the login demo:
--   - Subscribes to browsingContext.domContentLoaded events with a timed wait
--   - Navigates to the login page
--   - Locates the #username field via BiDi locateNodes
--   - Types 'bluefinUser' into the field via BiDi key actions

-- >>> bidi_login_demo
bidi_login_demo :: IO ()
bidi_login_demo = runEff_ $ \io -> do
  config <- effIO io loadConfig
  let behaviour = mkInteractBehaviour config
      caps = mkBiDiCaps config
      driverInfo =
        MkHttpDriverInfo
          { httpEndpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort},
            driverLogging = behaviour.driverLogging
          }
      http = MkHttpEnv driverInfo io

  B.withBiDiSession http behaviour caps $ \bidi -> do
    B.log io "=== Get root browsing context ==="
    tree <- browsingContextGetTree bidi (MkGetTree Nothing Nothing)
    bc <- case tree of
      MkGetTreeResult (info : _) -> do
        let MkBrowsingContext ctxId = info.context
        B.log io $ "Root context: " <> ctxId
        pure info.context
      _ -> effIO io $ throwIO $ userError "No browsing contexts found"

    B.log io "=== Subscribe to browsingContext.domContentLoaded ==="
    loadedVar <- effIO io newEmptyTMVarIO
    let onLoadedEvent evt =
          void $ atomically $ tryPutTMVar loadedVar evt
    void $ subscribeBrowsingContextDomContentLoaded bidi onLoadedEvent

    B.log io "=== Subscribe to browsingContext.load (many-style) ==="
    navVar <- effIO io newEmptyTMVarIO
    void $ subscribeMany bidi [BrowsingContextLoad] $ \evt -> do
      TIO.putStrLn $ "!!! browsingContext.load event (many-style): " <> txt evt
      void $ atomically $ tryPutTMVar navVar ()

    B.log io "=== Navigate to login page ==="
    loginPage <- effIO io loginUrl
    void $ browsingContextNavigate bidi $ MkNavigate {context = bc, url = loginPage, wait = Nothing}
    pauseBiDi bidi

    B.log io "=== Waiting for domContentLoaded event ==="
    effIO io $
      race_
        ( atomically (readTMVar loadedVar) >>= \evt ->
            TIO.putStrLn $ "!!! domContentLoaded fired: " <> txt evt
        )
        ( threadDelay (10 * 1_000_000)
            >> throwIO (userError "Timeout: domContentLoaded did not fire within 10 s")
        )
    pauseBiDi bidi

    B.log io "=== Locate #username field ==="
    nodesResult <-
      browsingContextLocateNodes bidi $
        MkLocateNodes
          { context = bc,
            locator = CSS {value = "#username"},
            maxNodeCount = Nothing,
            serializationOptions = Nothing,
            startNodes = Nothing
          }
    B.log io $ "Located nodes: " <> txt nodesResult
    pauseBiDi bidi

    let MkLocateNodesResult nodes = nodesResult
        usernameSharedId :: SharedId
        usernameSharedId = case nodes of
          [node] -> fromJust node.sharedId
          _ -> error "Expected exactly one #username element"

    B.log io "=== Type 'bluefinUser' into #username via BiDi key actions ==="
    inputPerformActions bidi $
      MkPerformActions
        { context = bc,
          actions =
            [ PointerSourceActions
                $ MkPointerSourceActions
                  { pointerId = "mouse1",
                    pointer = Just $ MkPointer {pointerType = Just MousePointer},
                    pointerActions =
                      [ PointerMove
                          { x = 0,
                            y = 0,
                            duration = Nothing,
                            origin =
                              Just
                                $ ElementOrigin
                                $ MkSharedReference
                                  { sharedId = usernameSharedId,
                                    handle = Nothing,
                                    extensions = Nothing
                                  },
                            pointerCommonProperties = defaultPointerProps
                          },
                        PointerDown {button = 0, pointerCommonProperties = defaultPointerProps},
                        PointerUp {button = 0}
                      ]
                  },
              KeySourceActions
                $ MkKeySourceActions
                  { keyId = "keyboard1",
                    keyActions = concatMap charToKeys (T.unpack "bluefinUser")
                  }
            ]
        }
    pauseBiDi bidi
