module IOUtils
  ( sleep,
    encodeFileToBase64,
    exceptionTextIncludes,
    logNothingLogger,
    lwrTxtIncludes,
    DemoActions (..),
    noOpUtils,
    mkDemoActions,
    (===),
    findWebDriverRoot,
    Logger (..),
    loopForever,
    catchLog,
    defToLogNothing,
  )
where

import Const (Timeout (..), seconds)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), Handler (..), SomeException, catches)
import Control.Monad (void, when)
import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Maybe (fromMaybe)
import Data.Text (Text, isInfixOf, pack, toLower, unpack)
import GHC.Base (coerce)
import System.FilePath (joinPath, splitDirectories, (</>))
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, (@=?))
import UnliftIO (AsyncCancelled, async, atomically, race_, readTMVar, throwIO, tryPutTMVar)
import UnliftIO.Async (Async)
import UnliftIO.STM (newEmptyTMVarIO)
import Prelude hiding (log)
import Data.Aeson (Value)
import WebDriverPreCore.Internal.AesonUtils (prettyJSON)
import WebDriverPreCore.Internal.Utils (txt)

findWebDriverRoot :: FilePath -> Maybe FilePath
findWebDriverRoot path =
  if rootDir `elem` dirs
    then Just webDriverPath
    else Nothing
  where
    rootDir = "webdriver"
    dirs = splitDirectories path
    webDriverPath = (joinPath $ takeWhile (/= rootDir) dirs) </> rootDir


newtype Logger = MkLogger
  { log :: Text -> IO ()
  }

logNothingLogger :: Logger
logNothingLogger = MkLogger . const $ pure ()

-- default Nothing to Logger that does nothing
defToLogNothing :: Maybe Logger -> Logger
defToLogNothing = fromMaybe logNothingLogger

mkDemoActions :: Logger -> Timeout -> DemoActions
mkDemoActions qLog pause =
  MkDemoActions
    { sleep,
      log = log',
      logShow = logShow',
      logJSON,
      logM = \l mt -> mt >>= log' l,
      logShowM = \l t -> t >>= logShow' l,
      logTxt = logTxt',
      pause = sleep pause,
      pauseAtLeast = pauseAtLeast pause,
      timeLimitLog = timeLimitLog hasEventFired,
      timeLimitLogMany = timeLimitLogMany hasEventFired,
      timeLimitLog' = \msg tmo -> timeLimitLog' msg hasEventFired tmo
    }
  where
    logTxt' :: Text -> IO ()
    logTxt' = qLog.log
    log' l t = logTxt' $ l <> ": " <> t
    logJSON msg val = prettyJSON msg val >>= logTxt'
    logShow' :: forall a. (Show a) => Text -> a -> IO ()
    logShow' l = log' l . txt
    hasEventFired :: forall a b. (Show a, Show b) => b -> a -> IO Bool
    hasEventFired b a = logShow' ("!!!!! " <> txt b <> " Event Fired !!!!!\n") a >> pure True

exceptionTextIncludes :: Text -> SomeException -> Bool
exceptionTextIncludes expected = lwrTxtIncludes expected . pack . displayException

lwrTxtIncludes :: Text -> Text -> Bool
lwrTxtIncludes expected = (toLower expected `isInfixOf`) . toLower

data DemoActions = MkDemoActions
  { sleep :: Timeout -> IO (),
    pause :: IO (),
    pauseAtLeast :: Timeout -> IO (),
    logTxt :: Text -> IO (),
    log :: Text -> Text -> IO (),
    logShow :: forall a. (Show a) => Text -> a -> IO (),
    logJSON :: Text -> Value -> IO (),
    logM :: Text -> IO Text -> IO (),
    logShowM :: forall a. (Show a) => Text -> IO a -> IO (),
    -- timeout functions for testing events TODO: rename
    timeLimitLog :: forall a b. (Show a, Show b) => b -> IO (a -> IO (), IO ()),
    timeLimitLogMany :: forall a b. (Show a, Show b) => b -> IO (a -> IO (), IO ()),
    timeLimitLog' :: forall a b. (Show a, Show b) => Text -> Timeout -> b -> IO (a -> IO (), IO ())
  }

noOpUtils :: DemoActions
noOpUtils =
  MkDemoActions
    { sleep = noOp1,
      logTxt = noOp1,
      log = noOp2,
      logShow = noOp2,
      logJSON = noOp2,
      logM = noOp2,
      logShowM = noOp2,
      pause = pure (),
      -- even for no-op, ensure sleep for passed in ms
      pauseAtLeast = sleep,
      timeLimitLog = timeLimitLog hasEventFired,
      timeLimitLogMany = timeLimitLogMany hasEventFired,
      timeLimitLog' = \msg tmo -> timeLimitLog' msg hasEventFired tmo
    }
  where
    noOp1 = const $ pure ()
    noOp2 = const . const $ pure ()

    hasEventFired :: b -> a -> IO Bool
    hasEventFired _b _a = pure True

sleep :: Timeout -> IO ()
sleep = threadDelay . coerce

pauseAtLeast :: Timeout -> Timeout -> IO ()
pauseAtLeast defaultSleep = sleep . MkTimeout . max (coerce defaultSleep) . coerce

timeLimit :: forall a b. (Show b) => Text -> Timeout -> b -> (a -> IO Bool) -> IO (a -> IO (), IO ())
timeLimit timeoutMsg (MkTimeout mu) eventDesc action = do
  triggered <- newEmptyTMVarIO
  let waitTriggered = atomically $ readTMVar triggered
      waitLimit = threadDelay mu >> (fail . unpack $ "Timeout - " <> timeoutMsg <> ": " <> txt eventDesc <> " after " <> txt (mu `div` 1000) <> " milliseconds")
      interceptedAction = \a -> do
        result <- action a
        when result $
          void $
            atomically $
              tryPutTMVar triggered ()
        pure ()
  pure $ (interceptedAction, race_ waitLimit waitTriggered)

timeLimitLog' :: forall a b. (Show b) => Text -> (b -> a -> IO Bool) -> Timeout -> b -> IO (a -> IO (), IO ())
timeLimitLog' timeoutMsg prd timeout b =
  timeLimit timeoutMsg timeout b (\a -> prd b a >> pure True)

timeLimitLog :: forall a b. (Show b) => (b -> a -> IO Bool) -> b -> IO (a -> IO (), IO ())
timeLimitLog prd = timeLimitLog' "Expected event did not fire" prd (10 * seconds)

timeLimitLogMany :: forall a b. (Show b) => (b -> a -> IO Bool) -> b -> IO (a -> IO (), IO ())
timeLimitLogMany prd = timeLimitLog' "Expected event(s) did not fire (Many Subscription)" prd (10 * seconds)

encodeFileToBase64 :: FilePath -> IO Text
encodeFileToBase64 filePath =
  B64T.extractBase64 . B64.encodeBase64 <$> BS.readFile filePath


(===) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  Assertion
(===) = (@=?)

catchLog :: Text -> (Text -> IO ()) -> IO () -> IO ()
catchLog name logAction action =
  action
    `catches` [ Handler $ \(e :: AsyncCancelled) -> do
                  logAction $ name <> " thread cancelled"
                  throwIO e,
                Handler $ \(e :: SomeException) -> do
                  logAction $ "Exception thrown in " <> name <> " thread" <> ": " <> (pack $ displayException e)
                  throwIO e
              ]

-- | like forever but, unlike forever, it fails if an exception is thrown
loopForever :: (Text -> IO ()) -> Text -> IO () -> IO (Async ())
loopForever logger name action = async $ do
  logger $ "Starting " <> name <> " thread"
  loop
  where
    loop = catchLog name logger action >> loop
