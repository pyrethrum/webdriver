-- |
-- Module: Main
-- Description: Test suite for webdriver-effectful library
--
-- Mirrors the Bluefin and RIO POC tests but uses the Effectful algebraic
-- effect style:
--
-- * @WebDriverHttp@ and @WebDriverBiDi@ are Dynamic effects — call-site code
--   uses 'send'-wrapper functions and the interpreter is swapped at the top.
-- * @Logger@ and @LogPause@ are Static effects — threaded implicitly through
--   the @es@ constraint (@:>@).
-- * STM operations use 'liftIO' (since @IOE :> es@); a future enhancement
--   could layer 'Effectful.Concurrent.STM' for a fully effect-saturated stack.
module Main where

import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    tryPutTMVar,
  )
import Data.Functor (void)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (Eff, IOE, liftIO, (:>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import UnliftIO (throwIO)
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.BiDi.Base.Actions
import WebDriver.Effectful.HTTP.Base.Actions qualified as HTTP
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
    "Effectful Tests"
    [ testCase "HTTP login and navigation demo" http_login_navigation_demo,
      testCase "BiDi login demo" bidi_login_demo
    ]

-- ---------------------------------------------------------------------------
-- Setup runners
-- ---------------------------------------------------------------------------

-- | Load config and set up the common driver info and behaviour, then run
-- the supplied action inside 'runHttp'.
runSetup
  :: (forall es. (IOE :> es) => HttpDriverInfo -> InteractBehaviour -> Config -> Eff es a)
  -> IO a
runSetup action = runHttp $ do
  config <- liftIO loadConfig
  let behaviour = mkInteractBehaviour config
      driverInfo =
        MkHttpDriverInfo
          { httpEndpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort},
            driverLogFn  = Nothing
          }
  action driverInfo behaviour config

-- | Full HTTP test harness: loads config, opens a session, and provides
-- 'Logger', 'LogPause', and 'WebDriverHttp' effects for the action.
runHttpTest
  :: ( forall es
      . ( IOE :> es
        , Logger :> es
        , LogPause :> es
        , WebDriverHttp :> es
        )
     => Eff es ()
     )
  -> IO ()
runHttpTest action =
  runSetup $ \driverInfo behaviour config ->
    withLogger "eval.log" $
      withHttpSession driverInfo behaviour (mkHttpCaps config) $
        withLogPause behaviour.pauseDuration action

-- | Full BiDi test harness: loads config, opens a BiDi-enabled session, and
--
-- Any exception thrown by the action propagates as an 'IOError' so Tasty
-- reports it as a test failure.
runBiDiTest
  :: ( forall es
      . ( IOE :> es
        , Logger :> es
        , LogPause :> es
        , WebDriverBiDi :> es
        )
     => Eff es ()
     )
  -> IO ()
runBiDiTest action =
  runSetup $ \driverInfo behaviour config ->
    withLogger "eval.log" $
      withBiDiSession driverInfo behaviour (mkBiDiCaps config) $
        withLogPause behaviour.pauseDuration action

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
      firstMatch  = []
    }
  where
    cap = fromHttpCapability $ httpCapabilities config

mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch  = []
    }

-- | Minimal pointer properties with all optional fields set to 'Nothing'.
defaultPointerProps :: PointerCommonProperties
defaultPointerProps =
  MkPointerCommonProperties
    { width              = Nothing,
      height             = Nothing,
      pressure           = Nothing,
      tangentialPressure = Nothing,
      twist              = Nothing,
      altitudeAngle      = Nothing,
      azimuthAngle       = Nothing
    }

-- | Convert a 'Char' to a pair of keyDown\/keyUp 'KeySourceAction's.
charToKeys :: Char -> [KeySourceAction]
charToKeys c = [KeyDown {value = T.singleton c}, KeyUp {value = T.singleton c}]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

-- | HTTP-only demo:
--
--   * Navigates to the login page
--   * Fills in username and password via 'HTTP.elementSendKeys'
--   * Navigates to the colourful content page
--   * Gets and logs the page title
--
-- >>> http_login_navigation_demo
http_login_navigation_demo :: IO ()
http_login_navigation_demo = runHttpTest $ do
  log "=== Navigate to login form ==="
  loginPage <- liftIO loginUrl
  HTTP.navigateTo loginPage
  _ <- HTTP.maximizeWindow
  pause

  log "=== Fill in username ==="
  usernameField <- HTTP.findElement $ P.CSS "#username"
  HTTP.elementSendKeys usernameField "demoUser"
  pause

  log "=== Fill in password ==="
  passwordField <- HTTP.findElement $ P.CSS "#password"
  HTTP.elementSendKeys passwordField "s3cr3tP4ssw0rd"
  pause

  log "=== Navigate to colourful content page ==="
  contentPage <- liftIO contentPageUrl
  HTTP.navigateTo contentPage
  pause

  title <- HTTP.getTitle
  log $ "Landed on: " <> title

-- | BiDi version of the login demo:
--
--   * Subscribes to @browsingContext.domContentLoaded@ with a 'TMVar' callback
--   * Navigates to the login page
--   * Waits for the @domContentLoaded@ event (with a 10-second timeout)
--   * Locates the @#username@ field via BiDi @locateNodes@
--   * Types @effectful-user@ into the field via BiDi key actions
--

-- >>> bidi_login_demo
bidi_login_demo :: IO ()
bidi_login_demo = runBiDiTest $ do
  log "=== Get root browsing context ==="
  tree <- browsingContextGetTree (MkGetTree Nothing Nothing)
  bc <- case tree of
    MkGetTreeResult (info : _) -> do
      let MkBrowsingContext ctxId = info.context
      log $ "Root context: " <> ctxId
      pure info.context
    _ -> liftIO . throwIO . userError $ "No browsing contexts found"

  log "=== Subscribe to browsingContext.domContentLoaded ==="
  loadedVar <- liftIO newEmptyTMVarIO
  let onLoadedEvent evt =
        atomically $ putTMVar loadedVar evt
  subscribeBrowsingContextDomContentLoaded onLoadedEvent

  log "=== Subscribe to browsingContext.load (many-style) ==="
  navVar <- liftIO newEmptyTMVarIO
  subscribeMany [BrowsingContextLoad] $ \evt -> do
    TIO.putStrLn $ "!!! browsingContext.load event (many-style): " <> txt evt
    atomically $ putTMVar navVar ()

  log "=== Navigate to login page ==="
  loginPage <- liftIO loginUrl
  browsingContextNavigate $ MkNavigate {context = bc, url = loginPage, wait = Nothing}
  pause

  log "=== Waiting for domContentLoaded event ==="
  -- STM is used via 'liftIO'. For a fully effect-saturated approach, one could
  -- add 'Effectful.Concurrent.STM' to the stack and call 'atomically' directly.
  liftIO $
    race_
      ( atomically (readTMVar loadedVar) >>= \evt ->
          TIO.putStrLn $ "!!! domContentLoaded fired: " <> txt evt
      )
      ( threadDelay (10 * 1_000_000)
          >> throwIO (userError "Timeout: domContentLoaded did not fire within 10 s")
      )
  pause

  log "=== Locate #username field ==="
  nodesResult <-
    browsingContextLocateNodes $
      MkLocateNodes
        { context            = bc,
          locator            = CSS {value = "#username"},
          maxNodeCount       = Nothing,
          serializationOptions = Nothing,
          startNodes         = Nothing
        }
  log $ "Located nodes: " <> txt nodesResult
  pause

  let MkLocateNodesResult nodes = nodesResult
  usernameSharedId <- case nodes of
    [node] -> maybe (liftIO . throwIO . userError $ "sharedId is missing") pure node.sharedId
    _      -> liftIO . throwIO . userError $ "Expected exactly one #username element"

  log "=== Type 'effectful-user' into #username via BiDi key actions ==="
  inputPerformActions $
    MkPerformActions
      { context = bc,
        actions =
          [ PointerSourceActions $
              MkPointerSourceActions
                { pointerId = "mouse1",
                  pointer   = Just $ MkPointer {pointerType = Just MousePointer},
                  pointerActions =
                    [ PointerMove
                        { x      = 0,
                          y      = 0,
                          duration = Nothing,
                          origin   =
                            Just $
                              ElementOrigin $
                                MkSharedReference
                                  { sharedId  = usernameSharedId,
                                    handle     = Nothing,
                                    extensions = Nothing
                                  },
                          pointerCommonProperties = defaultPointerProps
                        },
                      PointerDown {button = 0, pointerCommonProperties = defaultPointerProps},
                      PointerUp   {button = 0}
                    ]
                }
          ]
      }
  inputPerformActions $
    MkPerformActions
      { context = bc,
        actions =
          [ KeySourceActions $
              MkKeySourceActions
                { keyId      = "keyboard1",
                  keyActions = concatMap charToKeys (T.unpack "effectful-user")
                }
          ]
      }
  pause
