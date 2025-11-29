module Logger
  ( Printer (..),
    withLogger,
    withLogFileLogger,
    withChannelFileLogger,
    printToFileAndLog,
  )
where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import IOUtils (Logger (..), findWebDriverRoot, loopForever)
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
import Prelude
import UnliftIO.IO (hSetBuffering)
import UnliftIO.IO (BufferMode(..))


-- | Printer abstraction for logging output
newtype Printer = MkPrinter
  { print :: Text -> IO ()
  }

-- | Creates a logger with a channel and async loop that processes messages using the provided Printer
withLogger :: Printer -> (Logger -> IO ()) -> IO ()
withLogger p loggingAction = do
  logChan <- newTChanIO
  let waitEmpty :: Int -> IO ()
      waitEmpty attempt = do
        empty <- atomically $ isEmptyTChan logChan
        unless (empty || attempt > 500) $ do
          threadDelay 10_000
          waitEmpty $ succ attempt

      writeToChan = atomically . writeTChan @Text logChan
      readAndPrint = atomically (readTChan logChan) >>= p.print

  bracket
    -- initialise printloop
    (loopForever writeToChan "Logger" readAndPrint)
    -- empty and cancel print loop
    ( \printLoop -> do
        waitEmpty 0
        cancel printLoop
    )
    -- run the loggingAction with a logger that writes messages to the print channel
    (const . loggingAction $ MkLogger  writeToChan)

-- | Opens a log file and provides a function to write to it
withLogFileLogger :: ((Text -> IO ()) -> IO ()) -> IO ()
withLogFileLogger action = do
  lgPath <- getLogPath <$> getCurrentDirectory
  withFile lgPath WriteMode $ \h -> do
     hSetBuffering h LineBuffering
     action $ TIO.hPutStrLn h
  where
    lgName = "eval.log"
    getLogPath = maybe lgName (</> lgName) . findWebDriverRoot

-- | Combines withLogger and withLogFileLogger to provide channel-based file logging
withChannelFileLogger :: (Logger -> IO ()) -> IO ()
withChannelFileLogger loggingAction =
  withLogFileLogger $ \printToFile ->
    withLogger (printToFileAndLog printToFile) loggingAction

-- | Creates a Printer that writes to both a file and stdout
printToFileAndLog :: (Text -> IO ()) -> Printer
printToFileAndLog printToFile =
  MkPrinter
    { print = \msg -> do
        let logMsg = "[LOG] " <> msg
        TIO.putStrLn logMsg
        printToFile logMsg
    }
