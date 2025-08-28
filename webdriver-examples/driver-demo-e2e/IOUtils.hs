module IOUtils
  ( sleepMs,
    encodeFileToBase64,
    logTxt,
    log,
    logShow,
    logM,
    logShowM,
    DemoUtils (..),
    Logger (..),
    demoUtils,
    doNothingUtils,
    withAsyncLogger,
    sleep1,
    sleep2,
    (===),
  )
where

import Const (second, seconds)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, (@=?))
import UnliftIO (Async, TChan, atomically, cancel, isEmptyTChan, newTChanIO, readTChan, wait, withAsync, writeTChan, finally)
import UnliftIO.Async (async)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log)

data Logger = MkLogger
  { log :: Text -> IO (),
    stop :: IO ()
  }

loggerAsync :: TChan Text -> IO (Async ())
loggerAsync logChan = async . forever $ do
  msg <- atomically $ readTChan logChan
  TIO.putStrLn $ "[LOG] " <> msg

mkLogger :: TChan Text -> Async () -> IO Logger
mkLogger logChan loggerAsync' =
  pure $
    MkLogger
      { log = atomically . writeTChan logChan,
        stop = stop' 0
      }
  where
    stop' :: Int -> IO ()
    stop' attempt = do
      empty <- atomically $ isEmptyTChan logChan
      if empty || attempt > 1000
        then
          cancel loggerAsync'
        else do
          threadDelay 10_000
          stop' $ succ attempt

withAsyncLogger :: (Logger -> IO ()) -> IO ()
withAsyncLogger action = do
  logChan <- newTChanIO
  asyncLogger <- loggerAsync logChan
  logger <- mkLogger logChan asyncLogger
  finally 
    (action logger)
    (wait asyncLogger)

data DemoUtils = MkDemoUtils
  { sleep :: Int -> IO (),
    pause :: IO (),
    logTxt :: Text -> IO (),
    log :: Text -> Text -> IO (),
    logShow :: forall a. (Show a) => Text -> a -> IO (),
    logM :: Text -> IO Text -> IO (),
    logShowM :: forall a. (Show a) => Text -> IO a -> IO (),
    stopLogger :: IO ()
  }

demoUtils :: DemoUtils
demoUtils =
  MkDemoUtils
    { sleep = sleepMs,
      logTxt,
      log,
      logShow,
      logM,
      logShowM,
      pause = sleepMs 3_000,
      stopLogger = pure ()
    }

doNothingUtils :: DemoUtils
doNothingUtils =
  MkDemoUtils
    { sleep = \_ -> pure (),
      logTxt = \_ -> pure (),
      log = \_ _ -> pure (),
      logShow = \_ _ -> pure (),
      logM = \_ _ -> pure (),
      logShowM = \_ _ -> pure (),
      pause = pure (),
      stopLogger = pure ()
    }

sleepMs :: Int -> IO ()
sleepMs = threadDelay . (*) 1_000

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
sleep1 = sleepMs $ 1 * second

sleep2 :: IO ()
sleep2 = sleepMs $ 2 * seconds

-- todo: test extras - split off

(===) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  Assertion
(===) = (@=?)