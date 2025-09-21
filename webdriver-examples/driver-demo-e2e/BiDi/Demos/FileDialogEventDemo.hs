module BiDi.Demos.FileDialogEventDemo where

import BiDi.BiDiRunner (BiDiMethods, EventSubscription)
import Control.Monad (forever)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import UnliftIO (async, cancel)
import UnliftIO.STM (atomically, readTChan)

-- | Demo showing how to subscribe to file dialog events using the broadcast channel pattern
fileDialogEventDemo :: BiDiMethods -> IO ()
fileDialogEventDemo methods = do
  TIO.putStrLn "Starting file dialog event monitoring..."
  
  -- Subscribe to file dialog events
  subscription <- methods.subscribeToEvents isFileDialogEvent
  
  -- Start monitoring in background
  monitorAsync <- async $ monitorFileDialogEvents subscription
  
  TIO.putStrLn "File dialog event monitoring started. Press Enter to stop..."
  _ <- getLine
  
  -- Clean up
  cancel monitorAsync
  methods.unsubscribeFromEvents subscription
  TIO.putStrLn "File dialog event monitoring stopped."

-- | Filter function to detect file dialog events
isFileDialogEvent :: (Text, Value) -> Bool
isFileDialogEvent (eventType, _value) = 
  eventType == "input.fileDialogOpened"

-- | Monitor and log file dialog events
monitorFileDialogEvents :: EventSubscription -> IO ()
monitorFileDialogEvents subscription = forever $ do
  -- Read events from the subscription queue
  (eventType, eventValue) <- atomically $ readTChan subscription.eventQueue
  
  -- Log the file dialog event
  TIO.putStrLn $ "🎯 File Dialog Event Detected!"
  TIO.putStrLn $ "   Event Type: " <> eventType
  TIO.putStrLn $ "   Event Data: " <> T.pack (show eventValue)
  TIO.putStrLn $ "   " <> T.replicate 50 "-"

-- | Example of more sophisticated event filtering
advancedFileDialogDemo :: BiDiMethods -> IO ()
advancedFileDialogDemo methods = do
  TIO.putStrLn "Starting advanced file dialog monitoring..."
  
  -- Subscribe to multiple input-related events
  subscription <- methods.subscribeToEvents isInputEvent
  
  -- Monitor with more detailed logging
  monitorAsync <- async $ advancedEventMonitor subscription
  
  TIO.putStrLn "Advanced monitoring started. Press Enter to stop..."
  _ <- getLine
  
  -- Clean up
  cancel monitorAsync
  methods.unsubscribeFromEvents subscription
  TIO.putStrLn "Advanced monitoring stopped."

-- | Filter for various input events (including file dialogs)
isInputEvent :: (Text, Value) -> Bool
isInputEvent (eventType, _value) = 
  "input." `T.isPrefixOf` eventType

-- | Advanced event monitor with detailed logging
advancedEventMonitor :: EventSubscription -> IO ()
advancedEventMonitor subscription = forever $ do
  (eventType, eventValue) <- atomically $ readTChan subscription.eventQueue
  
  case eventType of
    "input.fileDialogOpened" -> do
      TIO.putStrLn "📁 FILE DIALOG OPENED!"
      logEventDetails eventType eventValue
    
    "input.fileDialogClosed" -> do
      TIO.putStrLn "📁 FILE DIALOG CLOSED!"
      logEventDetails eventType eventValue
    
    _ | "input." `T.isPrefixOf` eventType -> do
      TIO.putStrLn $ "⌨️  INPUT EVENT: " <> eventType
      logEventDetails eventType eventValue
    
    _ -> do
      TIO.putStrLn $ "❓ UNKNOWN EVENT: " <> eventType

-- | Helper function to log event details
logEventDetails :: Text -> Value -> IO ()
logEventDetails eventType eventValue = do
  TIO.putStrLn $ "   Type: " <> eventType
  TIO.putStrLn $ "   Data: " <> T.pack (show eventValue)
  TIO.putStrLn $ "   " <> T.replicate 40 "="