module BiDi.Runner where

import Common.SessionInit (WDSession (..), closeWDSession, getWDSession)
import Common.Utils (autoId)
import Common.WebDriver.Effect (WebDriver)
import Common.WebDriver.InterpreterBiDi qualified as Interp
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Error.Dynamic (Error, runErrorNoCallStack)
import Test.Tasty (TestTree, withResource)
import Test.Tasty.HUnit (testCase)
import UnliftIO (SomeException, catchAny, finally, throwIO, try)
import WebDriver.Effectful
import WebDriver.Effectful.BiDi.Base.Actions (browsingContextGetTree)
import WebDriver.Effectful.Logger (Logger, logError, runLogger)
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext,
    GetTree (..),
    GetTreeResult (..),
    Info (..),
    KeySourceAction (..),
    NodeRemoteValue,
    PointerCommonProperties (..),
  )
import WebDriverPreCore.Extended.BiDi.Locate qualified as BL
import WebDriverPreCore.Utils.Utils (ioThrow)

-- | Acquire a BiDi-enabled session, run the action, then release resources.
withBidi :: (forall es. (IOE :> es, Logger :> es, WaitPrimative :> es, WebDriverBiDi :> es) => Eff es ()) -> IO ()
withBidi action = do
  sess <- getWDSession True
  runBiDi sess action `finally` closeWDSession sess

-- | Run a 'BiDiTestEff' action as a Tasty test using a session supplied by the
-- caller (typically via 'Test.Tasty.withResource').
runBiDiTest :: IO WDSession -> Text -> BiDiTestEff () -> TestTree
runBiDiTest sessPrms name action =
  testCase (unpack name) $
    sessPrms >>= flip runBiDi action

-- | Run a 'WebDriverBiDi' action against an existing 'WDSession'.
--
-- The session must have been acquired with 'getWDSession True' so its
-- 'websocketUrl' is populated. This is the resource-free counterpart used by
-- 'runBiDiTest' inside a Tasty 'withResource' group.
runBiDi :: forall a. WDSession -> BiDiTestEff a -> IO a
runBiDi MkWDSession {loggerEnv, websocketUrl} action = do
  bidiUrl <- ioThrow (parseBiDiUrlProperty websocketUrl)
  runEff
    $ runWaitPrimative
    $ runLogger loggerEnv
    $ withBiDiSession bidiUrl
    $ action

type BiDiTestEff = Eff '[WebDriverBiDi, Logger, WaitPrimative, IOE]

-- | Effect stack over which the shared BiDi WebSocket is established.
type BiDiRunnerEffs = '[Logger, WaitPrimative, IOE]

-- | Effect stack for tests using the extended 'WebDriver' locate abstraction
-- over BiDi, with the dynamic 'Error' effect first.
type BiDiLocateEffs = '[Error Text, WebDriverBiDi, Logger, WaitPrimative, IOE]

-- | The HTTP session together with its single BiDi WebSocket, shared by all tests.
data BiDiResources = MkBiDiResources
  { wdSession :: WDSession,
    biDiHandle :: BiDiRunClose (Eff BiDiRunnerEffs)
  }

-- | Acquire the HTTP session and its BiDi WebSocket once for the whole group.
acquireBiDiResources :: IO BiDiResources
acquireBiDiResources = do
  sess@MkWDSession {loggerEnv, websocketUrl} <- getWDSession True
  acquired <- try $ do
    bidiUrl <- ioThrow (parseBiDiUrlProperty websocketUrl)
    handle <-
      runEff
        $ runWaitPrimative
        $ runLogger loggerEnv
        $ acquireBiDiSession bidiUrl
    pure (MkBiDiResources sess handle)
  case acquired of
    Right resources -> pure resources
    Left (e :: SomeException) -> do
      -- Close the leaked HTTP session, but don't let a cleanup failure mask
      -- the original exception.
      closeWDSession sess `catchAny` \_ -> pure ()
      throwIO e

-- | Close the BiDi WebSocket and then the HTTP session.
--
-- The HTTP session is deleted in a 'finally' so a failure while closing the
-- WebSocket cannot leak a live session in geckodriver (which then rejects the
-- next run with @\"Session is already started\"@).
releaseBiDiResources :: BiDiResources -> IO ()
releaseBiDiResources MkBiDiResources {wdSession = ws@MkWDSession {loggerEnv}, biDiHandle} =
  ( runEff
      $ runWaitPrimative
      $ runLogger loggerEnv
      $ releaseBiDiSession biDiHandle
    )
    `finally` closeWDSession ws

-- | Run a group of tests sharing one HTTP session and BiDi WebSocket.
withBiDiResources :: (IO BiDiResources -> TestTree) -> TestTree
withBiDiResources = withResource acquireBiDiResources releaseBiDiResources

-- | Default BiDi locate options.
--
-- Note: BiDi locate has no equivalent of the HTTP-only extended role matching
-- or displayed-check modes, so every test group shares these defaults.
defBiDiOpts :: BL.BiDiLocateOpts
defBiDiOpts =
  BL.MkBiDiLocateOpts
    { singletonCardinality = BL.Unique,
      mkDefaultLoc = autoId
    }

-- | Fetch the root (top-level) browsing context of the current session.
getRootBrowsingContext :: (WebDriverBiDi :> es, IOE :> es) => Eff es BrowsingContext
getRootBrowsingContext = do
  tree <- browsingContextGetTree (MkGetTree Nothing Nothing)
  case tree of
    MkGetTreeResult (info : _) -> pure info.context
    _ -> liftIO (fail "No browsing contexts found")

-- | Interpret the extended 'WebDriver' locate effect over the BiDi effect stack.
interpBiDiLocate :: (WebDriverBiDi :> es, IOE :> es) => Eff (WebDriver NodeRemoteValue : es) a -> Eff es a
interpBiDiLocate act = do
  ctx <- getRootBrowsingContext
  Interp.runWebDriverBiDi defBiDiOpts ctx act

-- | Run a 'BiDiLocateEffs' action against the shared resources, logging any
-- dynamic 'Error' via the katip 'Logger' before converting it into an 'IO'
-- exception.
runBiDiLocate :: forall a. IO BiDiResources -> Eff '[Error Text, WebDriverBiDi, Logger, WaitPrimative, IOE] a -> IO a
runBiDiLocate res act = do
  MkBiDiResources {wdSession = MkWDSession {loggerEnv}, biDiHandle = MkBiDiRunClose {run}} <- res
  runEff
    $ runWaitPrimative
    $ runLogger loggerEnv
    $ runWebDriverBiDi run
    $ runErrorNoCallStack @Text act
      >>= either logAndThrowIO pure
  where
    logAndThrowIO :: (Logger :> es, IOE :> es) => Text -> Eff es a
    logAndThrowIO err = logError err >> liftIO (throwIO $ userError $ unpack err)

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
