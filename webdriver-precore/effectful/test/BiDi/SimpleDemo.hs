module BiDi.SimpleDemo where

import BiDi.Runner
  ( charToKeys,
    defaultPointerProps
  )
import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
  )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (Eff, IOE, liftIO, (:>))
import UnliftIO (throwIO)
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import Utils (txt)
import WebDriver.Effectful
  (
    Pause,
    WebDriverBiDi,
    pause,
  )
import WebDriver.Effectful.Logger (Logger, log)
import WebDriver.Effectful.BiDi.Base.Actions
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    GetTree (..),
    GetTreeResult (..),
    Info (..),
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
    PointerSourceAction (..),
    PointerSourceActions (..),
    PointerType (..),
    SharedReference (..),
    SourceActions (..),
  )
import WebDriverPreCore.Test.TestData (loginUrl)
import Prelude hiding (log)

-- >>> runBiDiTest bidi_login_demo
bidi_login_demo :: (Logger :> es, WebDriverBiDi :> es, IOE :> es, Pause :> es) => Eff es ()
bidi_login_demo = do
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
    log $ "!!! browsingContext.load event (many-style): " <> txt evt
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
