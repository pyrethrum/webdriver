module WebDriverPreCore.Test.Logger
  ( 
    withLogger,
    withLogFileLogger,
    withChannelFileLogger,
    printToFileAndLog,
  )
where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.FilePath ((</>))
import UnliftIO
  ( IOMode (..),
    async,
    atomically,
    bracket,
    newTChanIO,
    readTChan,
    wait,
    withFile,
    writeTChan,
  )
import UnliftIO.Directory (getCurrentDirectory)
import UnliftIO.IO (BufferMode (..), hSetBuffering)
import WebDriverPreCore.Test.IOUtils (findWebDriverRoot)
import WebDriverPreCore.Types.BaseTypes (IOLogger)

-- given an IOLogger, perform IO action with logging
type IOActionWithLogging = IOLogger -> IO ()

-- | Creates a logger with a channel and async loop that processes messages using the provided IOLogger
withLogger :: IOLogger -> IOActionWithLogging -> IO ()
withLogger print' loggingAction = do
  logChan <- newTChanIO
  let writeToChan = atomically . writeTChan logChan . Just
      drainLoop = atomically (readTChan logChan) >>= maybe (pure ()) (\m -> print' m >> drainLoop)
  bracket
    (async drainLoop)
    (\printLoop -> atomically (writeTChan logChan Nothing) >> wait printLoop)
    (const $ loggingAction writeToChan)
    
-- | Opens a log file and provides a function to write to it
withLogFileLogger :: IOActionWithLogging -> IO ()
withLogFileLogger action = do
  lgPath <- getLogPath <$> getCurrentDirectory
  withFile lgPath WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    action $ TIO.hPutStrLn h
  where
    lgName = "eval.log"
    getLogPath = maybe lgName (</> lgName) . findWebDriverRoot

-- | Combines withLogger and withLogFileLogger to provide channel-based file logging
withChannelFileLogger :: IOActionWithLogging -> IO ()
withChannelFileLogger loggingAction =
  withLogFileLogger $ \printToFile ->
    withLogger (printToFileAndLog printToFile) loggingAction

-- | Creates a IOLogger that writes to both a file and stdout
printToFileAndLog :: IOLogger -> Text -> IO ()
printToFileAndLog printToFile msg =
  TIO.putStrLn logMsg >> printToFile logMsg
  where
    logMsg = "[LOG] " <> msg
