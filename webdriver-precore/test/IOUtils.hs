module IOUtils
  ( sleep,
    encodeFileToBase64,
    exceptionTextIncludes,
    lwrTxtIncludes,
    logTxt,
    log,
    logShow,
    logM,
    logShowM,
    DemoUtils (..),
    Logger (..),
    mkLogger,
    demoUtils,
    noOpUtils,
    -- withAsyncLogger,
    bidiDemoUtils,
    sleep1,
    sleep2,
    (===),
    findWebDriverRoot,
  )
where

import Const (Timeout (..), second, seconds)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), SomeException)
import Control.Monad (unless, void, when)
import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text, isInfixOf, pack, toLower, unpack)
import Data.Text.IO qualified as TIO
import GHC.Base (coerce)
import System.FilePath (joinPath, splitDirectories, (</>))
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, (@=?))
import UnliftIO (TChan, atomically, isEmptyTChan, race_, readTMVar, tryPutTMVar, writeTChan)
import UnliftIO.STM (newEmptyTMVarIO)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log)

data Logger = MkLogger
  { log :: Text -> IO (),
    waitEmpty :: IO ()
  }

findWebDriverRoot :: FilePath -> Maybe FilePath
findWebDriverRoot path =
  if rootDir `elem` dirs
    then Just webDriverPath
    else Nothing
  where
    rootDir = "webdriver"
    dirs = splitDirectories path
    webDriverPath = (joinPath $ takeWhile (/= rootDir) dirs) </> rootDir

--  TODO : deprectate static logTxt et al

bidiDemoUtils :: (Text -> IO ()) -> Timeout -> DemoUtils
bidiDemoUtils baseLog pause =
  MkDemoUtils
    { sleep = sleep,
      log = log',
      logShow = logShow',
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
    logTxt' = baseLog
    log' l t = logTxt' $ l <> ": " <> t
    logShow' :: forall a. (Show a) => Text -> a -> IO ()
    logShow' l = log' l . txt
    hasEventFired :: forall a b. (Show a, Show b) => b -> a -> IO Bool
    hasEventFired b a = logShow' ("!!!!! " <> txt b <> " Event Fired !!!!!\n") a >> pure True

mkLogger :: TChan Text -> IO Logger
mkLogger logChan =
  pure $
    MkLogger
      { log = atomically . writeTChan logChan,
        waitEmpty = waitEmpty' 0
      }
  where
    waitEmpty' :: Int -> IO ()
    waitEmpty' attempt = do
      empty <- atomically $ isEmptyTChan logChan
      unless (empty || attempt > 500) $ do
        threadDelay 10_000
        waitEmpty' $ succ attempt

exceptionTextIncludes :: Text -> SomeException -> Bool
exceptionTextIncludes expected = lwrTxtIncludes expected . pack . displayException

lwrTxtIncludes :: Text -> Text -> Bool
lwrTxtIncludes expected = (toLower expected `isInfixOf`) . toLower

data DemoUtils = MkDemoUtils
  { sleep :: Timeout -> IO (),
    pause :: IO (),
    pauseAtLeast :: Timeout -> IO (),
    logTxt :: Text -> IO (),
    log :: Text -> Text -> IO (),
    logShow :: forall a. (Show a) => Text -> a -> IO (),
    logM :: Text -> IO Text -> IO (),
    logShowM :: forall a. (Show a) => Text -> IO a -> IO (),
    -- timeout functions for testing events TODO: rename
    timeLimitLog :: forall a b. (Show a, Show b) => b -> IO (a -> IO (), IO ()),
    timeLimitLogMany :: forall a b. (Show a, Show b) => b -> IO (a -> IO (), IO ()),
    timeLimitLog' :: forall a b. (Show a, Show b) => Text -> Timeout -> b -> IO (a -> IO (), IO ())
  }

-- TODO :: DELETE THIS
demoUtils :: Timeout -> DemoUtils
demoUtils pause =
  MkDemoUtils
    { sleep = sleep,
      logTxt,
      log,
      logShow,
      logM,
      logShowM,
      pause = sleep pause,
      pauseAtLeast = pauseAtLeast pause,
      timeLimitLog = timeLimitLog hasEventFired,
      timeLimitLogMany = timeLimitLogMany hasEventFired,
      timeLimitLog' = \msg tmo -> timeLimitLog' msg hasEventFired tmo 
    }
  where
    hasEventFired :: forall a b. (Show a, Show b) => b -> a -> IO Bool
    hasEventFired b a = logShow (txt b) a >> pure True

noOpUtils :: DemoUtils
noOpUtils =
  MkDemoUtils
    { sleep = noOp1,
      logTxt = noOp1,
      log = noOp2,
      logShow = noOp2,
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
      waitLimit = threadDelay mu >> (fail . unpack $ "Timeout - " <> timeoutMsg <> ": " <> txt eventDesc)
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

logTxt :: Text -> IO ()
logTxt = TIO.putStrLn

log :: Text -> Text -> IO ()
log l t = logTxt $ l <> ": " <> t

logShow :: (Show a) => Text -> a -> IO ()
logShow l = log l . txt

logM :: Text -> IO Text -> IO ()
logM l t = t >>= log l

logShowM :: (Show a) => Text -> IO a -> IO ()
logShowM l t = t >>= logShow l

sleep1 :: IO ()
sleep1 = sleep $ 1 * second

sleep2 :: IO ()
sleep2 = sleep $ 2 * seconds

-- todo: test extras - split off

(===) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  Assertion
(===) = (@=?)
