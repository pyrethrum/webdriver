module BiDi.BiDiRunner where

import Control.Applicative ((<*>))
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever, unless, void)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Functor ((<$>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text as T (Text, null, pack, unpack, splitOn, breakOn)
import Data.Text.IO as T (getLine, putStrLn)
import Network.WebSockets (ClientApp, receiveData, runClient, sendClose, sendTextData)
import WebDriverPreCore.BiDi.Session
import Wuss (runSecureClient)
import Prelude (Bool (True), Either (..), Eq ((==)), IO, Int, Maybe (..), Show (..), maybe, ($), (+), (.), (<>))
import Http.HttpAPI (newSessionFull, FullCapabilities, SessionResponse)
import Text.Read (readEither)
import Data.Function ((&))


parseUrl :: Text -> Either Text BiDiPath
parseUrl url = do
  (scheme, host, portPath) <- splitCol
  (port, path) <- portAndPath portPath
  Right $ MkBiDiPath { 
            host = scheme <> ":" <> host, 
            port = port, 
            path = path }
  where 
    -- "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"
    -- -> [ws, //127.0.0.1, 9222/session/e43698d9-b02a-4284-a936-12041deb3552]
    splitCol = splitOn ":" url
               & \case 
                     [scheme, host, portPath] -> Right (scheme, host, portPath)
                     _ -> failParser "Expected format: ws://host:port/path"
    -- 9222/session/e43698d9-b02a-4284-a936-12041deb3552
    -- -> (9222, session/e43698d9-b02a-4284-a936-12041deb3552)
    portAndPath :: Text -> Either Text (Int, Text)
    portAndPath pp = 
      (,path) <$> portEth
      where 
        (portTxt, path) = T.breakOn "/" pp
        portEth = case readEither $ T.unpack portTxt of
                 Left msg -> failParser $ "Could not extract port (an Int) from prefix of: " 
                                  <> portTxt
                                  <> "\n"
                                  <> "Error on read Int: " <> T.pack msg
                 Right p -> Right p
          
    failParser :: forall a. Text -> Either Text a
    failParser msg = Left $ "Failed to parse URL: " <> url <> "\n" <> msg


newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull 

-- >>> runBiDiExample
-- *** Exception: MalformedResponse (ResponseHead {responseCode = 405, responseMessage = "Method Not Allowed", responseHeaders = [("content-type","text/plain; charset=utf-8"),("content-length","23"),("date","Sat, 24 May 2025 10:40:13 GMT")]}) "Wrong response status or message."
runBiDiExample :: IO ()
runBiDiExample = runWebDriverBiDi defaultGeckoDriverConfig


-- | WebDriver BiDi client configuration
data BiDiPath= MkBiDiPath
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show)

-- | Default WebDriver BiDi configuration for GeckoDriver
defaultGeckoDriverConfig :: BiDiPath
defaultGeckoDriverConfig =
  MkBiDiPath
    { host = "127.0.0.1",
      port = 4444,
      path = "/session"
    }

-- | WebDriver BiDi message
data WebDriverBiDiMessage a = WebDriverBiDiMessage
  { msgId :: Int,
    msgMethod :: Maybe Text,
    msgParams :: Maybe a,
    msgResult :: Maybe Aeson.Value,
    msgError :: Maybe Aeson.Value
  }
  deriving (Show)

instance (FromJSON a) => FromJSON (WebDriverBiDiMessage a) where
  parseJSON = Aeson.withObject "WebDriverBiDiMessage" $ \v ->
    WebDriverBiDiMessage
      <$> v .: "id"
      <*> v .:? "method"
      <*> v .:? "params"
      <*> v .:? "result"
      <*> v .:? "error"

instance (ToJSON a) => ToJSON (WebDriverBiDiMessage a) where
  toJSON (WebDriverBiDiMessage id' method params _ _) =
    Aeson.object $
      ["id" .= id']
        <> maybe [] (\m -> ["method" .= m]) method
        <> maybe [] (\p -> ["params" .= p]) params

-- | Create a BiDi message for session.new command
createSessionNewMessage :: Int -> Capabilities -> WebDriverBiDiMessage Capabilities
createSessionNewMessage msgId caps =
  WebDriverBiDiMessage
    { msgId = msgId,
      msgMethod = Just "session.new",
      msgParams = Just caps,
      msgResult = Nothing,
      msgError = Nothing
    }

-- | Create a BiDi message for session.end command
createSessionEndMessage :: Int -> WebDriverBiDiMessage ()
createSessionEndMessage msgId =
  WebDriverBiDiMessage
    { msgId = msgId,
      msgMethod = Just "session.end",
      msgParams = Nothing,
      msgResult = Nothing,
      msgError = Nothing
    }

-- | Run WebDriver BiDi client
runWebDriverBiDi :: BiDiPath -> IO ()
runWebDriverBiDi MkBiDiPath {host, port, path} = do
  putStrLn $ "Connecting to WebDriver at " <> host <> ":" <> pack (show port) <> path
  runClient (unpack host) port (unpack path) webDriverBiDiClient

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
  let capabilities =
        MkCapabilities
          { alwaysMatch =
              Just $
                MkCapability
                  { acceptInsecureCerts = Just True,
                    browserName = Just "firefox",
                    webSocketUrl = True,
                    browserVersion = Nothing,
                    platformName = Nothing,
                    proxy = Nothing,
                    unhandledPromptBehavior = Nothing
                  },
            firstMatch = []
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
          case parsedMsg.msgResult of
            Just result ->
              case Aeson.fromJSON result of
                Aeson.Success (MkSessionNewResult sessionId' caps) -> do
                  putStrLn $ "Session created successfully!"
                  putStrLn $ "Session ID: " <> sessionId'
                  putStrLn $ "Browser: " <> caps.browserName <> " " <> caps.browserVersion
                Aeson.Error _ ->
                  putStrLn $ "Received result: " <> pack (show result)
            Nothing ->
              putStrLn $ "Received message without result: " <> pack (show parsedMsg)

-- | Run the example with default GeckoDriver config




---- wuss example ----

wussDemo :: IO ()
wussDemo = runSecureClient "echo.websocket.org" 443 "/" ws

ws :: ClientApp ()
ws connection = do
  T.putStrLn "Connected!"

  void . forkIO . forever $ do
    message <- receiveData connection
    T.putStrLn (message :: Text)

  let loop = do
        line <- getLine
        unless (null line) $ do
          sendTextData connection line
          loop
  loop

  sendClose connection (pack "Bye!")
