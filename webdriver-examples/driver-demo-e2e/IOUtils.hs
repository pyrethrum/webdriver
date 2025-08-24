module IOUtils
  ( sleepMs,
    encodeFileToBase64,
    logTxt,
    log,
    logShow,
    logM,
    logShowM,
    DemoUtils(..),
    Logger(..),
    demoUtils,
    doNothingUtils,
    getLogger,
    sleep1,
    sleep2,
    (===),
  )
where

import Const (second, seconds)
import Control.Concurrent (threadDelay)
import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, (@=?))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log)
import UnliftIO (newTChanIO, atomically, readTChan, writeTChan, isEmptyTChan, cancel)
import UnliftIO.Async (async)
import Control.Monad (forever)


data Logger = MkLogger
  { log :: Text -> IO (),
    stop :: IO ()
  }

getLogger :: IO Logger
getLogger = do
  logChan <- newTChanIO
  loggerAsync <- async . forever $ do
    msg <- atomically $ readTChan logChan
    TIO.putStrLn $ "[LOG] " <> msg
  let log' = atomically . writeTChan logChan
      stop' :: Int -> IO ()
      stop' attempt = do
        empty <- atomically $ isEmptyTChan logChan
        if empty || attempt > 1000
          then
            cancel loggerAsync
          else do
            threadDelay 10_000
            stop' $ succ attempt
  pure $ MkLogger {log = log', stop = stop' 0}

data DemoUtils = MkDemoUtils
  { sleep :: Int -> IO (),
    logTxt :: Text -> IO (),
    log :: Text -> Text -> IO (),
    logShow :: forall a. (Show a) => Text -> a -> IO (),
    logM :: Text -> IO Text -> IO (),
    logShowM :: forall a. (Show a) => Text -> IO a -> IO (),
    stopLogger :: IO ()
  }

demoUtils :: DemoUtils
demoUtils = MkDemoUtils
  { sleep = sleepMs,
    logTxt,
    log,
    logShow,
    logM,
    logShowM,
    stopLogger = pure ()
  }

doNothingUtils :: DemoUtils
doNothingUtils = MkDemoUtils
  { sleep = \_ -> pure (),
    logTxt = \_ -> pure (),
    log = \_ _ -> pure (),
    logShow = \_ _ -> pure (),
    logM = \_ _ -> pure (),
    logShowM = \_ _ -> pure (),
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