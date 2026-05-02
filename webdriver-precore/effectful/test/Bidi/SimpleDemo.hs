module Bidi.SimpleDemo where

import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar
  )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (Eff, IOE, liftIO, (:>))
import UnliftIO (throwIO)
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.BiDi.Base.Actions
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
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Test.TestData (loginUrl)
import WebDriverPreCore.Utils.Timeout (milliseconds)
import Prelude hiding (log)

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

runBiDiTest
  :: ( forall es
      . ( IOE :> es
        , Logger :> es
        , Pause :> es
        , WebDriverBiDi :> es
        )
     => Eff es ()
     )
  -> IO ()
runBiDiTest action =
  runSetup $ \driverInfo behaviour config ->
    withLogger "eval.log" $
      withBiDiSession driverInfo behaviour (mkBiDiCaps config) $
        withPause behaviour.pauseDuration action

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
        liftIO $ atomically $ putTMVar loadedVar evt
  subscribeBrowsingContextDomContentLoaded onLoadedEvent

  log "=== Subscribe to browsingContext.load (many-style) ==="
  navVar <- liftIO newEmptyTMVarIO
  subscribeMany [BrowsingContextLoad] $ \evt -> do
    liftIO $ TIO.putStrLn $ "!!! browsingContext.load event (many-style): " <> txt evt
    liftIO $ atomically $ putTMVar navVar ()

  log "=== Navigate to login page ==="
  loginPage <- liftIO loginUrl
  browsingContextNavigate $ MkNavigate {context = bc, url = loginPage, wait = Nothing}
  pause

  log "=== Waiting for domContentLoaded event ==="
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
