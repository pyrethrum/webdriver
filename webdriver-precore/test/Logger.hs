module Logger
  ( Printer (..),
    LogQueue (..),
    withLogger,
    withLogFileLogger,
    withChannelFileLogger,
    printToFileAndLog,
  )
where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import IOUtils (QueLog (..), findWebDriverRoot, loopForever)
import System.FilePath ((</>))
import UnliftIO
  ( IOMode (..),
    atomically,
    bracket,
    cancel,
    isEmptyTChan,
    newTChanIO,
    readTChan,
    withFile,
    writeTChan,
    atomically
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (getCurrentDirectory)
import Data.Coerce (coerce)
import Prelude


-- | Printer abstraction for logging output
newtype Printer = MkPrinter
  { printLog :: Text -> IO ()
  }

-- | LogQueue provides queueing and dequeuing for log messages
data LogQueue = MkLogQueue
  { queueLog :: QueLog,
    deQueueLog :: IO Text,
    waitEmpty :: IO ()
  }

-- | Creates a logger with a channel and async loop that processes messages using the provided Printer
withLogger :: Printer -> (QueLog -> IO ()) -> IO ()
withLogger printer action = do
  logChan <- newTChanIO
  let waitEmpty' :: Int -> IO ()
      waitEmpty' attempt = do
        empty <- atomically $ isEmptyTChan logChan
        unless (empty || attempt > 500) $ do
          threadDelay 10_000
          waitEmpty' $ succ attempt

      logQ :: LogQueue
      logQ =
        MkLogQueue
          { queueLog = MkQueLog $ atomically . writeTChan logChan,
            deQueueLog = atomically $ readTChan logChan,
            waitEmpty = waitEmpty' 0
          }

      printAction = logQ.deQueueLog >>= printer.printLog

  bracket
    (loopForever (coerce logQ.queueLog) "Logger" printAction)
    ( \printLoop -> do
        logQ.waitEmpty
        cancel printLoop
    )
    (const $ action logQ.queueLog)

-- | Opens a log file and provides a function to write to it
withLogFileLogger :: ((Text -> IO ()) -> IO ()) -> IO ()
withLogFileLogger action = do
  lgPath <- getLogPath <$> getCurrentDirectory
  withFile lgPath WriteMode
    $ action
    . TIO.hPutStrLn
  where
    lgName = "eval.log"
    getLogPath = maybe lgName (</> lgName) . findWebDriverRoot

-- | Combines withLogger and withLogFileLogger to provide channel-based file logging
withChannelFileLogger :: (QueLog -> IO ()) -> IO ()
withChannelFileLogger action =
  withLogFileLogger $ \printToFile ->
    withLogger (printToFileAndLog printToFile) action

-- | Creates a Printer that writes to both a file and stdout
printToFileAndLog :: (Text -> IO ()) -> Printer
printToFileAndLog printToFile =
  MkPrinter
    { printLog = \msg -> do
        let logMsg = "[LOG] " <> msg
        printToFile logMsg
        TIO.putStrLn logMsg
    }
