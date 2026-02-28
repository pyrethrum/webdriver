{-# LANGUAGE DataKinds #-}

-- |
-- Test suite for webdriver-rio-poc library
module Main where

import Data.Maybe (fromJust)
import RIO hiding (log)
import RIO.Text qualified as T
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Utils (txt)
import WebDriver.RIO hiding (runHttp, withHttpSession)
import WebDriver.RIO qualified as R
import WebDriver.RIO.BiDi.Base.Actions
import WebDriver.RIO.HTTP.Base.Actions
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
import WebDriverPreCore.Extended.Capabilities ()
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as HTTP
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Test.TestData (contentPageUrl, loginUrl)
import WebDriverPreCore.Utils.Timeout (Timeout (..))
import Prelude (userError)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "RIO Tests"
    [ testCase "BiDi login demo" bidi_login_demo
    ]

-- testCase "Basic Demo" basic_demo

-- challenge
-- basic bidi
-- basic http
-- bidi events
-- bidi and http mixed on the same instance
-- hooks to start runner and close connection
--
-- concurrency - 2 browsers

runHttp' :: (Config -> RIO HttpEnv a) -> IO a
runHttp' httpAction = do
  config@MkConfig {httpPort, httpUrl, logging} <- loadConfig
  let logConfig = ConsoleAndFile "eval.log"
      driverInfo =
        MkHttpDriverInfo
          { httpEndpoint = MkHttpEndpoint {host = httpUrl, port = httpPort},
            driverLogging = logging
          }
  R.runHttp mkHttpEnv logConfig driverInfo (httpAction config)

runHttp :: RIO HttpEnv a -> IO a
runHttp httpAction = runHttp' (const httpAction)

withSession :: RIO HttpSessionEnv a -> IO a
withSession sessionAction = runHttp' $ \config -> do
  let caps = mkHttpCaps config
      pauseDuration = MkTimeout $ (* 1000) $ fromIntegral config.pauseMS
  R.withHttpSessionEnv pauseDuration caps sessionAction

mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch = []
    }

mkBiDiCaps :: Config -> HttpCapabilities
mkBiDiCaps config =
  MkFullCapabilities
    { alwaysMatch = Just cap {httpWebSocketUrl = Just True},
      firstMatch = []
    }
  where
    cap = fromHttpCapability $ httpCapabilities config

loadCapabilities :: IO HttpCapabilities
loadCapabilities = do
  config <- loadConfig
  pure
    $ MkFullCapabilities
      { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
        firstMatch = []
      }

-- >>> basic_demo
basic_demo :: IO ()
basic_demo =
  runHttp $ log "Loaded eval config"

-- | Example showing how to use withHttpSession to set and get timeouts

--- >>> session_demo
session_demo :: IO ()
session_demo = withSession $ do
  log "Session created, setting timeouts"

  -- Set new timeout values
  let newTimeouts =
        MkTimeouts
          { implicit = Just 5000, -- 5 seconds
            pageLoad = Just 60000, -- 60 seconds
            script = Just 30000 -- 30 seconds
          }
  setTimeouts newTimeouts
  navigateTo $ HTTP.MkUrl "https://www.example.com"
  pause

  -- Get and log the current timeouts
  currentTimeouts <- getTimeouts
  log $ "Current timeouts: " <> txt currentTimeouts

-- | Example showing how to use withHttpSession to set and get timeouts

--- >>> input_navigation_base_demo
input_navigation_base_demo :: IO ()
input_navigation_base_demo = withSession $ do
  log "=== Navigate to login form ==="
  loginPage <- loginUrl
  navigateTo loginPage
  maximizeWindow
  pause

  log "=== Fill in username ==="
  usernameField <- findElement $ HTTP.CSS "#username"
  elementSendKeys usernameField "demoUser"
  pause

  log "=== Fill in password ==="
  passwordField <- findElement $ HTTP.CSS "#password"
  elementSendKeys passwordField "s3cr3tP4ssw0rd"
  pause

  log "=== Navigate to colourful content page ==="
  contentPage <- contentPageUrl
  navigateTo contentPage
  pause

  title <- getTitle
  log $ "Landed on: " <> title

-- ---------------------------------------------------------------------------
-- BiDi demo
-- ---------------------------------------------------------------------------

-- | BiDi version of the login demo:
--   - Subscribes to browsingContext.domContentLoaded events with a timed wait
--   - Navigates to the login page
--   - Fills in the username field via BiDi input actions
--   - Waits for and logs the received event

-- >>> bidi_login_demo
bidi_login_demo :: IO ()
bidi_login_demo = runHttp' $ \config -> do
  let caps = mkBiDiCaps config

  R.withBiDiSession False caps $ do
    log "=== Get root browsing context ==="
    tree <- browsingContextGetTree $ MkGetTree Nothing Nothing
    bc <- case tree of
      MkGetTreeResult (info : _) -> do
        let MkBrowsingContext ctxId = info.context
        log $ "Root context: " <> ctxId
        pure info.context
      _ -> throwIO $ userError "No browsing contexts found"

    log "=== Subscribe to browsingContext.domContentLoaded ==="
    loadedVar <- newEmptyTMVarIO
    let onLoadedEvent evt = do
          void $ atomically $ tryPutTMVar loadedVar evt

    subscribeBrowsingContextDomContentLoaded onLoadedEvent

    log "=== Subscribe to browsingContext.domContentLoaded Many-style ==="
    navVar <- newEmptyTMVarIO
    subscribeMany [BrowsingContextLoad] $ \evt -> do
      log $ "!!! browsingContext.load event (many-style): " <> txt evt
      void $ atomically $ tryPutTMVar navVar ()

    log "=== Navigate to login page ==="
    loginPage <- loginUrl
    browsingContextNavigate $ MkNavigate bc loginPage Nothing
    pause

    log "=== Waiting for domContentLoaded event ==="
    let waitLoaded =
          atomically (readTMVar loadedVar) >>= \evt ->
            log $ "!!! domContentLoaded fired: " <> txt evt
        waitTimeout =
          threadDelay (10 * 1_000_000)
            >> throwM (userError "Timeout: domContentLoaded did not fire within 10 s")
    race_ waitTimeout waitLoaded
    pause

    log "=== Locate #username field ==="
    nodesResult <-
      browsingContextLocateNodes
        $ MkLocateNodes
          { context = bc,
            locator = CSS {value = "#username"},
            maxNodeCount = Nothing,
            serializationOptions = Nothing,
            startNodes = Nothing
          }
    log $ "Located nodes: " <> txt nodesResult
    pause

    let MkLocateNodesResult nodes = nodesResult
        usernameSharedId :: SharedId
        usernameSharedId = case nodes of
          [node] -> fromJust node.sharedId
          _ -> error "Expected exactly one #username element"

    log "=== Type 'rioUser' into #username via BiDi key actions ==="
    inputPerformActions
      $ MkPerformActions
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
                    keyActions = concatMap charToKeys (T.unpack "rioUser")
                  }
            ]
        }
    pause

-- | Minimal pointer properties (all optional fields set to Nothing).
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
