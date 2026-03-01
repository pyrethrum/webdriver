-- |
-- Test suite for webdriver-bluefin-poc library
module Main where

import Prelude hiding (log)
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
import UnliftIO.STM (atomically, newEmptyTMVarIO, readTMVar, tryPutTMVar, putTMVar)
import Utils (txt)
import WebDriver.Bluefin 
import WebDriver.Bluefin.BiDi.Base.Actions
import WebDriver.Bluefin.HTTP.Base.Actions qualified as HTTP
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
import WebDriverPreCore.Test.TestData (contentPageUrl, loginUrl)
import WebDriverPreCore.Utils.Timeout (milliseconds)
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as P

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Bluefin Tests"
    [ testCase "HTTP login and navigation demo" http_login_navigation_demo,
      testCase "BiDi login demo" bidi_login_demo
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

mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch = []
    }

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

-- | HTTP-only demo:
--   - Creates an HTTP session
--   - Navigates to the login page
--   - Fills in username and password via elementSendKeys
--   - Navigates to the colourful content page
--   - Gets and logs the page title

-- >>> http_login_navigation_demo
http_login_navigation_demo :: IO ()
http_login_navigation_demo = runEff_ $ \io -> do
  config <- effIO io loadConfig
  let behaviour = mkInteractBehaviour config
      caps = mkHttpCaps config
      driverInfo =
        MkHttpDriverInfo
          { httpEndpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort},
            driverLogging = behaviour.driverLogging
          }
      http = MkHttpEnv driverInfo io

  withHttpSession http behaviour caps $ \sess ->
    withLogPause io behaviour.pauseDuration $ \lp -> do
      log lp "=== Navigate to login form ==="
      loginPage <- effIO io loginUrl
      HTTP.navigateTo sess loginPage
      _ <- HTTP.maximizeWindow sess
      pause lp

      log lp "=== Fill in username ==="
      usernameField <- HTTP.findElement sess $ P.CSS "#username"
      HTTP.elementSendKeys sess usernameField "demoUser"
      pause lp

      log lp "=== Fill in password ==="
      passwordField <- HTTP.findElement sess $ P.CSS "#password"
      HTTP.elementSendKeys sess passwordField "s3cr3tP4ssw0rd"
      pause lp

      log lp "=== Navigate to colourful content page ==="
      contentPage <- effIO io contentPageUrl
      HTTP.navigateTo sess contentPage
      pause lp

      title <- HTTP.getTitle sess
      log lp $ "Landed on: " <> title

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

  withBiDiSession http behaviour caps $ \bidi ->
    withLogPause io behaviour.pauseDuration $ \lp -> do
      log lp "=== Get root browsing context ==="
      tree <- browsingContextGetTree bidi (MkGetTree Nothing Nothing)
      bc <- case tree of
        MkGetTreeResult (info : _) -> do
          let MkBrowsingContext ctxId = info.context
          log lp $ "Root context: " <> ctxId
          pure info.context
        _ -> effIO io $ throwIO $ userError "No browsing contexts found"

      log lp "=== Subscribe to browsingContext.domContentLoaded ==="
      loadedVar <- effIO io newEmptyTMVarIO
      let onLoadedEvent evt =
            void $ atomically $ tryPutTMVar loadedVar evt
      subscribeBrowsingContextDomContentLoaded bidi onLoadedEvent

      log lp "=== Subscribe to browsingContext.load (many-style) ==="
      navVar <- effIO io newEmptyTMVarIO
      subscribeMany bidi [BrowsingContextLoad] $ \evt -> do
        TIO.putStrLn $ "!!! browsingContext.load event (many-style): " <> txt evt
        atomically $ putTMVar navVar ()

      log lp "=== Navigate to login page ==="
      loginPage <- effIO io loginUrl
      browsingContextNavigate bidi $ MkNavigate {context = bc, url = loginPage, wait = Nothing}
      pause lp

      log lp "=== Waiting for domContentLoaded event ==="
      effIO io $
        race_
          ( atomically (readTMVar loadedVar) >>= \evt ->
              TIO.putStrLn $ "!!! domContentLoaded fired: " <> txt evt
          )
          ( threadDelay (10 * 1_000_000)
              >> throwIO (userError "Timeout: domContentLoaded did not fire within 10 s")
          )
      pause lp

      log lp "=== Locate #username field ==="
      nodesResult <-
        browsingContextLocateNodes bidi $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#username"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      log lp $ "Located nodes: " <> txt nodesResult
      pause lp

      let MkLocateNodesResult nodes = nodesResult
          usernameSharedId :: SharedId
          usernameSharedId = case nodes of
            [node] -> fromJust node.sharedId
            _ -> error "Expected exactly one #username element"

      log lp "=== Type 'bluefinUser' into #username via BiDi key actions ==="
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
      pause lp
