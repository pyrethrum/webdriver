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
  (forall e. IOE e -> HttpEnv e -> InteractBehaviour -> Config -> Eff e a) ->
  IO a
runSetup action = runEff_ $ \io -> do
  config <- effIO io loadConfig
  let behaviour = mkInteractBehaviour config
      driverInfo =
        MkHttpDriverInfo
          { httpEndpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort},
            driverLogging = behaviour.driverLogging
          }
  action io (MkHttpEnv driverInfo io) behaviour config

-- | Full HTTP test harness: loads config, opens a session, provides a
-- 'LogPause' handle, and runs the supplied action.
runHttpTest ::
  (forall e. IOE e -> HttpSessionEnv e -> LogPause e -> Eff e ()) ->
  IO ()
runHttpTest action =
  runSetup $ \io http behaviour config ->
    withHttpSession http behaviour (mkHttpCaps config) $ \sess ->
      withLogPause io behaviour.pauseDuration $ \lp ->
        action io sess lp

-- | Full BiDi test harness: loads config, opens a BiDi session, provides a
-- 'LogPause' handle, and runs the supplied action.
--
-- The action receives an 'Exception' handle so it can use 'throw' rather than
-- 'error' or 'throwIO' for test-level failures.  Any caught exception is
-- re-thrown as an 'IOError' so Tasty reports it as a test failure.
runBiDiTest ::
  (forall (es :: Effects) (e :: Effects). Exception String e -> IOE es -> BiDiEnv es -> LogPause es -> Eff (e :& es) ()) ->
  IO ()
runBiDiTest action =
  runSetup $ \io http behaviour config ->
    withBiDiSession http behaviour (mkBiDiCaps config) $ \bidi ->
      withLogPause io behaviour.pauseDuration $ \lp ->
        catch
          (\ex -> action ex io bidi lp)
          (\err -> effIO io $ throwIO $ userError err)

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
http_login_navigation_demo = runHttpTest $ \io sess lp -> do
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
--   - Navigates to the login page>
--   - Locates the #username field via BiDi locateNodes
--   - Types 'bluefinUser' into the field via BiDi key actions

-- >>> bidi_login_demo
bidi_login_demo :: IO ()
bidi_login_demo = runBiDiTest $ \ex io bidi lp -> do
  log lp "=== Get root browsing context ==="
  tree <- browsingContextGetTree bidi (MkGetTree Nothing Nothing)
  bc <- case tree of
    MkGetTreeResult (info : _) -> do
      let MkBrowsingContext ctxId = info.context
      log lp $ "Root context: " <> ctxId
      pure info.context
    _ -> throw ex "No browsing contexts found"

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
  usernameSharedId <- case nodes of
    [node] -> maybe (throw ex "sharedId is missing") pure node.sharedId
    _ -> throw ex "Expected exactly one #username element"

  log lp "=== Type 'bluefinUser' into #username via BiDi key actions ==="
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
                },
            KeySourceActions $
              MkKeySourceActions
                { keyId = "keyboard1",
                  keyActions = concatMap charToKeys (T.unpack "bluefin-user")
                }
          ]
      }
  pause lp
