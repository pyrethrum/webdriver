-- filepath: /workspaces/webdriver/webdriver-examples/driver-demo-e2e/BiDi/IORunner.hs
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BiDi.IORunner where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever, void, unless)
import Control.Applicative ((<*>))
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<$>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Wuss (runSecureClient)
import WebDriverPreCore.BiDi.Session

-- Example from Wuss
wussDemo :: IO ()
wussDemo = runSecureClient "echo.websocket.org" 443 "/" ws

ws :: ClientApp ()
ws connection = do
  putStrLn "Connected!"

  void . forkIO . forever $ do
    message <- receiveData connection
    print (message :: Text)

  let loop = do
        line <- getLine
        unless (null line) $ do
          sendTextData connection (pack line)
          loop
  loop

  sendClose connection (pack "Bye!")

-- | WebDriver BiDi client configuration
data WebDriverBiDiConfig = WebDriverBiDiConfig
  { host :: String
  , port :: Int
  , path :: String
  } deriving (Show)

-- | Default WebDriver BiDi configuration for GeckoDriver
defaultGeckoDriverConfig :: WebDriverBiDiConfig
defaultGeckoDriverConfig = WebDriverBiDiConfig
  { host = "localhost"
  , port = 4444
  , path = "/session"
  }

-- | WebDriver BiDi message
data WebDriverBiDiMessage a = WebDriverBiDiMessage
  { msgId :: Int
  , msgMethod :: Maybe Text
  , msgParams :: Maybe a
  , msgResult :: Maybe Aeson.Value
  , msgError :: Maybe Aeson.Value
  } deriving (Show)

instance (FromJSON a) => FromJSON (WebDriverBiDiMessage a) where
  parseJSON = Aeson.withObject "WebDriverBiDiMessage" $ \v -> WebDriverBiDiMessage
    <$> v .: "id"
    <*> v .:? "method"
    <*> v .:? "params"
    <*> v .:? "result"
    <*> v .:? "error"

instance (ToJSON a) => ToJSON (WebDriverBiDiMessage a) where
  toJSON (WebDriverBiDiMessage id' method params _ _) =
    Aeson.object $ 
      [ "id" .= id' ] ++
      maybe [] (\m -> ["method" .= m]) method ++
      maybe [] (\p -> ["params" .= p]) params

-- | Create a BiDi message for session.new command
createSessionNewMessage :: Int -> Capabilities -> WebDriverBiDiMessage Capabilities
createSessionNewMessage msgId caps = WebDriverBiDiMessage
  { msgId = msgId
  , msgMethod = Just "session.new"
  , msgParams = Just caps
  , msgResult = Nothing
  , msgError = Nothing
  }

-- | Create a BiDi message for session.end command
createSessionEndMessage :: Int -> WebDriverBiDiMessage ()
createSessionEndMessage msgId = WebDriverBiDiMessage
  { msgId = msgId
  , msgMethod = Just "session.end"
  , msgParams = Nothing
  , msgResult = Nothing
  , msgError = Nothing
  }

-- | Run WebDriver BiDi client
runWebDriverBiDi :: WebDriverBiDiConfig -> IO ()
runWebDriverBiDi WebDriverBiDiConfig{..} = do
  putStrLn $ "Connecting to WebDriver at " <> pack host <> ":" <> pack (show port) <> pack path
  runSecureClient host port path webDriverBiDiClient

-- | WebDriver BiDi client application
webDriverBiDiClient :: ClientApp ()
webDriverBiDiClient connection = do
  putStrLn "Connected to WebDriver BiDi endpoint!"
  
  -- Create a reference to track message IDs
  messageIdRef <- newIORef 1
  
  -- Handle incoming messages in a separate thread
  void . forkIO . forever $ do
    message <- receiveData connection
    handleIncomingMessage (message :: ByteString)
  
  -- Create minimal capabilities for session
  let capabilities = MkCapabilities
        { alwaysMatch = Just $ MkCapability
            { acceptInsecureCerts = Just True
            , browserName = Just "firefox"
            , browserVersion = Nothing
            , platformName = Nothing
            , proxy = Nothing
            , unhandledPromptBehavior = Nothing
            }
        , firstMatch = Nothing
        }
  
  -- Send session.new command
  msgId <- readIORef messageIdRef
  let sessionNewMsg = createSessionNewMessage msgId capabilities
  putStrLn $ "Sending session.new command: " <> pack (show sessionNewMsg)
  sendTextData connection (BL.toStrict $ encode sessionNewMsg)
  writeIORef messageIdRef (msgId + 1)
  
  -- Wait for user to type "quit" to end the session
  let loop = do
        line <- getLine
        if line == "quit" 
          then do
            -- Send session.end command
            endMsgId <- readIORef messageIdRef
            let sessionEndMsg = createSessionEndMessage endMsgId
            putStrLn $ "Sending session.end command: " <> pack (show sessionEndMsg)
            sendTextData connection (BL.toStrict $ encode sessionEndMsg)
            writeIORef messageIdRef (endMsgId + 1)
          else do
            putStrLn $ "Type 'quit' to end the session"
            loop
  
  -- Run the interaction loop and ensure we close the connection properly
  finally loop (sendClose connection (pack "Closing BiDi session"))
  where
    handleIncomingMessage :: ByteString -> IO ()
    handleIncomingMessage msg = do
      putStrLn $ "Received message: " <> pack (show (BL.toStrict msg))
      
      -- Try to parse as a generic BiDi message
      case eitherDecode msg of
        Left err -> putStrLn $ "Error parsing message: " <> pack err
        Right (parsedMsg :: WebDriverBiDiMessage Aeson.Value) -> do
          -- Check for session.new result
          case msgResult parsedMsg of
            Just result -> 
              case Aeson.fromJSON result of
                Aeson.Success (MkSessionNewResult sessionId' caps) -> do
                  putStrLn $ "Session created successfully!"
                  putStrLn $ "Session ID: " <> sessionId'
                  putStrLn $ "Browser: " <> browserName caps <> " " <> browserVersion caps
                Aeson.Error _ -> 
                  putStrLn $ "Received result: " <> pack (show result)
            Nothing -> 
              putStrLn $ "Received message without result: " <> pack (show parsedMsg)

-- | Run the example with default GeckoDriver config
runBiDiExample :: IO ()
runBiDiExample = runWebDriverBiDi defaultGeckoDriverConfig
