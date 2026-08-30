-- |
-- Test suite for webdriver-bluefin-poc library
module Main where

import Bluefin.Eff (Eff, Effects, runEff_, (:&))
import Bluefin.Exception (Exception, catch, throw)
import Bluefin.IO (IOE, effIO)
import Data.Functor (void)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import UnliftIO (throwIO)
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (atomically, newEmptyTMVarIO, putTMVar, readTMVar, tryPutTMVar)
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
    SharedReference (..),
    SourceActions (..),
  )
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as P
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Test.TestData (contentPageUrl, loginUrl)
import WebDriverPreCore.Utils.Timeout (milliseconds)
import Prelude hiding (log)

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
-- Setup runners
-- ---------------------------------------------------------------------------

-- | Load config and build an 'HttpEnv', then run an action inside 'runEff_'.
--
-- Both 'runHttpTest' and 'runBiDiTest' delegate to this so config loading and
-- environment construction happen in exactly one place.
runSetup ::
  (forall e. IOE e -> HttpEnv e -> InteractOpts -> Config -> Eff e a) ->
  IO a
runSetup action = runEff_ $ \io -> do
  config <- effIO io loadConfig
  let behaviour = mkInteractOpts config
      endpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort}
  action io (MkHttpEnv endpoint (const $ pure ()) io) behaviour config

-- | Full HTTP test harness: loads config, opens a session, provides a
-- 'Logger' and 'LogPause' handle, and runs the supplied action.
runHttpTest ::
  (forall e. IOE e -> HttpSessionEnv e -> Logger e -> LogPause e -> Eff e ()) ->
  IO ()
runHttpTest action =
  runSetup $ \io http behaviour config ->
    withLogger io $ \logger ->
      withHttpSession http behaviour logger (mkHttpCaps config) $ \sess ->
        withLogPause io behaviour.pauseDuration $ \lp ->
          action io sess logger lp

-- | Full BiDi test harness: loads config, opens a BiDi session, provides a
-- 'Logger' and 'LogPause' handle, and runs the supplied action.
--
-- The action receives an 'Exception' handle so it can use 'throw' rather than
-- 'error' or 'throwIO' for test-level failures.  Any caught exception is
-- re-thrown as an 'IOError' so Tasty reports it as a test failure.
runBiDiTest ::
  (forall (es :: Effects) (e :: Effects). Exception String e -> IOE es -> BiDiEnv es -> Logger es -> LogPause es -> Eff (e :& es) ()) ->
  IO ()
runBiDiTest action =
  runSetup $ \io http behaviour config ->
    withLogger io $ \logger ->
      withBiDiSession http behaviour logger (mkBiDiCaps config) $ \bidi ->
        withLogPause io behaviour.pauseDuration $ \lp ->
          catch
            (\ex -> action ex io bidi logger lp)
            (\err -> effIO io $ throwIO $ userError err)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkInteractOpts :: Config -> InteractOpts
mkInteractOpts config =
  MkInteractOpts
    { pauseDuration = fromIntegral config.pauseMS * milliseconds,
      driverLogging = config.logging
    }

mkBiDiCaps :: Config -> HttpCapabilities
mkBiDiCaps config =
  MkFullCapabilities
    { alwaysMatch = Just cap {httpWebSocketUrl = True},
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
http_login_navigation_demo = runHttpTest $ \io sess logger lp -> do
  log logger "=== Navigate to login form ==="
  loginPage <- effIO io loginUrl
  HTTP.navigateTo sess loginPage
  _ <- HTTP.maximizeWindow sess
  pause lp

  log logger "=== Fill in username ==="
  usernameField <- HTTP.findElement sess $ P.CSS "#username"
  HTTP.elementSendKeys sess usernameField "demoUser"
  pause lp

  log logger "=== Fill in password ==="
  passwordField <- HTTP.findElement sess $ P.CSS "#password"
  HTTP.elementSendKeys sess passwordField "s3cr3tP4ssw0rd"
  pause lp

  log logger "=== Navigate to colourful content page ==="
  contentPage <- effIO io contentPageUrl
  HTTP.navigateTo sess contentPage
  pause lp

  title <- HTTP.getTitle sess
  log logger $ "Landed on: " <> title

-- | BiDi version of the login demo:
--   - Subscribes to browsingContext.domContentLoaded events with a timed wait
--   - Navigates to the login page>
--   - Locates the #username field via BiDi locateNodes
--   - Types 'bluefin-user' into the field via BiDi key actions

-- >>> bidi_login_demo
bidi_login_demo :: IO ()
bidi_login_demo = runBiDiTest $ \ex io bidi logger lp -> do
  log logger "=== Get root browsing context ==="
  tree <- browsingContextGetTree bidi (MkGetTree Nothing Nothing)
  bc <- case tree of
    MkGetTreeResult (info : _) -> do
      let MkBrowsingContext ctxId = info.context
      log logger $ "Root context: " <> ctxId
      pure info.context
    _ -> throw ex "No browsing contexts found"

  log logger "=== Subscribe to browsingContext.domContentLoaded ==="
  loadedVar <- effIO io newEmptyTMVarIO
  let onLoadedEvent evt =
        effIO io $ void $ atomically $ tryPutTMVar loadedVar evt
  subscribeBrowsingContextDomContentLoaded bidi onLoadedEvent

  log logger "=== Subscribe to browsingContext.load (many-style) ==="
  navVar <- effIO io newEmptyTMVarIO
  subscribeMany bidi [BrowsingContextLoad] $ \evt -> do
    effIO io $ TIO.putStrLn $ "!!! browsingContext.load event (many-style): " <> txt evt
    effIO io $ atomically $ putTMVar navVar ()

  log logger "=== Navigate to login page ==="
  loginPage <- effIO io loginUrl
  browsingContextNavigate bidi $ MkNavigate {context = bc, url = loginPage, wait = Nothing}
  pause lp

  log logger "=== Waiting for domContentLoaded event ==="
  -- Note in full example this should be moved into its own effect to support propper logging
  effIO io $
    race_
      ( atomically (readTMVar loadedVar) >>= \evt ->
          TIO.putStrLn $ "!!! domContentLoaded fired: " <> txt evt
      )
      ( threadDelay (10 * 1_000_000)
          >> throwIO (userError "Timeout: domContentLoaded did not fire within 10 s")
      )
  pause lp

  log logger "=== Locate #username field ==="
  nodesResult <-
    browsingContextLocateNodes bidi $
      MkLocateNodes
        { context = bc,
          locator = CSS {value = "#username"},
          maxNodeCount = Nothing,
          serializationOptions = Nothing,
          startNodes = Nothing
        }
  log logger $ "Located nodes: " <> txt nodesResult
  pause lp

  let MkLocateNodesResult nodes = nodesResult
  usernameSharedId <- case nodes of
    [node] -> maybe (throw ex "sharedId is missing") pure node.sharedId
    _ -> throw ex "Expected exactly one #username element"

  log logger "=== Type 'bluefin-user' into #username via BiDi key actions ==="
  inputPerformActions bidi $
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
                      PointerUp {button = 0}
                    ]
                }
          ]
      }
  inputPerformActions bidi $
    MkPerformActions
      { context = bc,
        actions =
          [ KeySourceActions $
              MkKeySourceActions
                { keyId = "keyboard1",
                  keyActions = concatMap charToKeys (T.unpack "bluefin-user")
                }
          ]
      }
  pause lp
