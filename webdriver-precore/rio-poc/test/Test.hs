{-# LANGUAGE DataKinds #-}

-- |
-- Test suite for webdriver-rio-poc library
module Main where

import RIO
import RIO.Text qualified as T
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import WebDriver.RIO hiding (runHttp, withHttpSession)
import WebDriver.RIO qualified as R
import WebDriver.RIO.HTTP.Base.Actions
import WebDriver.RIO.BiDi.Base.Actions
import WebDriverPreCore.Extended.Capabilities ()
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as HTTP
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Test.TestData (contentPageUrl, loginUrl)
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    GetTree (..),
    GetTreeResult (..),
    Info (..),
    KnownSubscriptionType (..),
    Navigate (..),
    NavigationInfo (..),
    LocateNodes (..),
    LocateNodesResult (..),
    NodeRemoteValue (..),
    Locator (..),
    PerformActions (..),
    PointerSourceActions (..),
    PointerSourceAction (..),
    Pointer (..),
    PointerType (..),
    PointerCommonProperties (..),
    Origin (..),
    SharedReference (..),
    SharedId,
    KeySourceActions (..),
    KeySourceAction (..),
    SourceActions (..),
  )
import WebDriverPreCore.Utils.Timeout (Timeout (..))
import Data.Maybe (fromJust)

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
      driverInfo = MkHttpDriverInfo
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
    { alwaysMatch = Just cap { httpWebSocketUrl = Just True },
      firstMatch = []
    }
  where
    cap = fromHttpCapability $ httpCapabilities config

loadCapabilities :: IO HttpCapabilities
loadCapabilities = do
  config <- loadConfig
  pure $
    MkFullCapabilities
      { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
        firstMatch = []
      }

-- >>> basic_demo
basic_demo :: IO ()
basic_demo =
  runHttp $ logInfo "Loaded eval config"

-- | Example showing how to use withHttpSession to set and get timeouts

--- >>> session_demo
session_demo :: IO ()
session_demo = withSession $ do
  logInfo "Session created, setting timeouts"

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
  logInfo $ "Current timeouts: " <> displayShow currentTimeouts

-- | Example showing how to use withHttpSession to set and get timeouts

--- >>> input_navigation_base_demo
input_navigation_base_demo :: IO ()
input_navigation_base_demo = withSession $ do
  logInfo "=== Navigate to login form ==="
  loginPage <- loginUrl
  navigateTo loginPage
  maximizeWindow
  pause

  logInfo "=== Fill in username ==="
  usernameField <- findElement $ HTTP.CSS "#username"
  elementSendKeys usernameField "demoUser"
  pause

  logInfo "=== Fill in password ==="
  passwordField <- findElement $ HTTP.CSS "#password"
  elementSendKeys passwordField "s3cr3tP4ssw0rd"
  pause

  logInfo "=== Navigate to colourful content page ==="
  contentPage <- contentPageUrl
  navigateTo contentPage
  pause

  title <- getTitle
  logInfo $ "Landed on: " <> display title

-- ---------------------------------------------------------------------------
-- BiDi demo
-- ---------------------------------------------------------------------------

-- | BiDi version of the login demo:
--   - Subscribes to browsingContext.domContentLoaded events with a timed wait
--   - Navigates to the login page
--   - Fills in the username field via BiDi input actions
--   - Waits for and logs the received event
--
-- Run with: >>> bidi_login_demo
bidi_login_demo :: IO ()
bidi_login_demo = runHttp' $ \config -> do
  let caps = mkBiDiCaps config

  R.withBiDiSession False caps $ do

    -- ── Get the root browsing context ────────────────────────────────────
    logInfo "=== Get root browsing context ==="
    tree <- browsingContextGetTree $ MkGetTree Nothing Nothing
    bc <- case tree of
      MkGetTreeResult (info : _) -> do
        let MkBrowsingContext ctxId = info.context
        logInfo $ "Root context: " <> display ctxId
        pure info.context
      _ -> throwIO $ userError "No browsing contexts found"

    -- ── Subscribe to domContentLoaded with a timed wait ──────────────────
    logInfo "=== Subscribe to browsingContext.domContentLoaded ==="
    loadedVar <- newEmptyTMVarIO
    let onLoadedEvent :: NavigationInfo -> IO ()
        onLoadedEvent evt = do
          void $ atomically $ tryPutTMVar loadedVar evt

    _ <- subscribeBrowsingContextDomContentLoaded onLoadedEvent

    -- Also subscribe to the multi (many) style for demonstration
    navVar <- newEmptyTMVarIO
    _ <- subscribeMany [BrowsingContextLoad] $ \evt -> do
      putStrLn $ "!!! browsingContext.load event (many-style): " <> show evt
      void $ atomically $ tryPutTMVar navVar ()

    -- ── Navigate to the login page ───────────────────────────────────────
    logInfo "=== Navigate to login page ==="
    loginPage <- loginUrl
    _ <- browsingContextNavigate $ MkNavigate bc loginPage Nothing

    -- ── Wait for domContentLoaded (10 s timeout) ─────────────────────────
    logInfo "=== Waiting for domContentLoaded event ==="
    let waitLoaded =
          atomically (readTMVar loadedVar) >>= \evt ->
            logInfo $ "!!! domContentLoaded fired: " <> displayShow evt
        waitTimeout =
          threadDelay (10 * 1_000_000)
            >> throwIO (userError "Timeout: domContentLoaded did not fire within 10 s")
    race_ waitTimeout waitLoaded

    -- ── Locate the username field and type into it ───────────────────────
    logInfo "=== Locate #username field ==="
    nodesResult <- browsingContextLocateNodes $
      MkLocateNodes
        { context = bc,
          locator = CSS {value = "#username"},
          maxNodeCount = Nothing,
          serializationOptions = Nothing,
          startNodes = Nothing
        }
    logInfo $ "Located nodes: " <> displayShow nodesResult

    let MkLocateNodesResult nodes = nodesResult
        usernameSharedId :: SharedId
        usernameSharedId = case nodes of
          [node] -> fromJust node.sharedId
          _      -> error "Expected exactly one #username element"

    logInfo "=== Type 'rioUser' into #username via BiDi key actions ==="
    inputPerformActions $
      MkPerformActions
        { context = bc,
          actions =
            [ PointerSourceActions $
                MkPointerSourceActions
                  { pointerId = "mouse1",
                    pointer = Just $ MkPointer {pointerType = Just MousePointer},
                    pointerActions =
                      [ PointerMove
                          { x = 0,
                            y = 0,
                            duration = Nothing,
                            origin =
                              Just $
                                ElementOrigin $
                                  MkSharedReference
                                    { sharedId = usernameSharedId,
                                      handle = Nothing,
                                      extensions = Nothing
                                    },
                            pointerCommonProperties = defaultPointerProps
                          },
                        PointerDown {button = 0, pointerCommonProperties = defaultPointerProps},
                        PointerUp   {button = 0}
                      ]
                  },
              KeySourceActions $
                MkKeySourceActions
                  { keyId = "keyboard1",
                    keyActions = concatMap charToKeys (T.unpack "rioUser")
                  }
            ]
        }

    logInfo "=== Typed into #username successfully ==="

    -- ── Wait 1 s so the browser has time to dispatch any remaining events ─
    threadDelay 1_000_000
    logInfo "=== BiDi login demo complete ==="

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
