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
    mkLogger,
    demoUtils,
    noOpUtils,
    -- withAsyncLogger,
    bidiDemoUtils,
    sleep1,
    sleep2,
    (===),
  )
where

import Const (second, seconds)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, (@=?))
import UnliftIO (TChan, atomically, isEmptyTChan, writeTChan)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log)

data Logger = MkLogger
  { log :: Text -> IO (),
    waitEmpty :: IO ()
  }

--  TODO : deprectate static logTxt et al

bidiDemoUtils :: (Text -> IO ()) -> Int -> DemoUtils
bidiDemoUtils baseLog pauseMs =
  let logTxt' = baseLog
      log' l t = logTxt' $ l <> ": " <> t
      logShow' :: forall a. (Show a) => Text -> a -> IO ()
      logShow' l = log' l . txt
   in MkDemoUtils
        { sleep = sleepMs,
          log = log',
          logShow = logShow',
          logM = \l mt -> mt >>= log' l,
          logShowM = \l t -> t >>= logShow l,
          logTxt = logTxt',
          pause = sleepMs pauseMs,
          pauseMinMs = sleepMs . max pauseMs
        }

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

-- withAsyncLogger :: (Logger -> IO ()) -> IO ()
-- withAsyncLogger action = do
--   logChan <- newTChanIO
--   withAsync (printLoop logChan) $ \_ -> do
--     logger <- mkLogger logChan
--     finally
--       ( action logger `catch` \(e :: SomeException) -> do
--           logger.log $ "Exception encountered: " <> txt e
--           throw e
--       )
--       logger.waitEmpty

data DemoUtils = MkDemoUtils
  { sleep :: Int -> IO (),
    pause :: IO (),
    pauseMinMs :: Int -> IO (),
    logTxt :: Text -> IO (),
    log :: Text -> Text -> IO (),
    logShow :: forall a. (Show a) => Text -> a -> IO (),
    logM :: Text -> IO Text -> IO (),
    logShowM :: forall a. (Show a) => Text -> IO a -> IO ()
  }

demoUtils :: Int -> DemoUtils
demoUtils pauseMs =
  MkDemoUtils
    { sleep = sleepMs,
      logTxt,
      log,
      logShow,
      logM,
      logShowM,
      pause = sleepMs pauseMs,
      pauseMinMs = sleepMs . max pauseMs
    }

noOpUtils :: DemoUtils
noOpUtils =
  MkDemoUtils
    { sleep = const $ pure (),
      logTxt = const $ pure (),
      log = const2 $ pure (),
      logShow = const2 $ pure (),
      logM = const2 $ pure (),
      logShowM = const2 $ pure (),
      pause = pure (),
      -- even for no-op, ensure sleep for passed in ms
      pauseMinMs = sleepMs
    }
  where
    const2 = const . const

sleepMs :: Int -> IO ()
sleepMs = threadDelay . (*) 1_000

encodeFileToBase64 :: FilePath -> IO Text
encodeFileToBase64 filePath =
  B64T.extractBase64 . B64.encodeBase64 <$> BS.readFile filePath

logTxt :: Text -> IO ()
logTxt = TIO.putStrLn

log :: Text -> Text -> IO ()
log l t = logTxt $ l <> ": " <> t

ppTxt :: Show a => a -> Text
ppTxt = pack . P.ppShow

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