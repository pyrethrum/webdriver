module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, unless, void)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, encode, toJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text as T (Text, breakOn, null, pack, splitOn, unpack)
import Data.Text.IO as T (getLine, putStrLn)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import IOUtils (ppTxt)
import Network.WebSockets (ClientApp, Connection, receiveData, runClient, sendClose, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import Text.Read (readEither)
import UnliftIO.Async (async, cancel, waitAny, waitEither_)
import UnliftIO.STM
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (prettyPrintJson)
import Wuss (runSecureClient)
import Prelude hiding (getLine, log, null, putStrLn)

getBiDiPath :: SessionResponse -> Either Text BiDiPath
getBiDiPath r =
  r.webSocketUrl
    & maybe
      (Left $ "WebSocket URL not provided in session response:\n" <> ppTxt r)
      parseUrl

parseUrl :: Text -> Either Text BiDiPath
parseUrl url = do
  (scheme, host, portPath) <- splitCol
  (port, path) <- portAndPath portPath
  Right $
    MkBiDiPath
      { host = scheme <> ":" <> host,
        port = port,
        path = path
      }
  where
    -- "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"
    -- -> [ws, //127.0.0.1, 9222/session/e43698d9-b02a-4284-a936-12041deb3552]
    splitCol =
      splitOn ":" url
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
          Left msg ->
            failParser $
              "Could not extract port (an Int) from pBiDi.BiDiRunnerrefix of: "
                <> portTxt
                <> "\n"
                <> "Error on read Int: "
                <> T.pack msg
          Right p -> Right p

    failParser :: forall a. Text -> Either Text a
    failParser msg = Left $ "Failed to parse URL: " <> url <> "\n" <> msg

newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull

-- Bidi capabilities request is the same as regular HTTP capabilities,
-- but with the `webSocketUrl` field set to `True`
httpBidiCapabilities :: Config -> Http.FullCapabilities
httpBidiCapabilities cfg =
  (httpFullCapabilities cfg)
    { Http.alwaysMatch =
        Just $
          (httpCapabilities cfg)
            { Caps.webSocketUrl = Just True
            }
    }

sessionViaHttp :: IO BiDiPath
sessionViaHttp = do
  cfg <- loadConfig
  ses <- newHttpSession $ httpBidiCapabilities cfg
  getBiDiPath ses & either (error . T.unpack) pure

-- >>> biDiDemo

-- *** Exception: user error (WebDriver error thrown:

--  WebDriverError {error = SessionNotCreated, description = "A new session could not be created", httpResponse = MkHttpResponse {statusCode = 500, statusMessage = "Internal Server Error", body = Object (fromList [("value",Object (fromList [("error",String "session not created"),("message",String "Session is already started"),("stacktrace",String "")]))])}})
biDiDemo :: IO ()
biDiDemo = sessionViaHttp >>= newBidiSessionDemo

-- | WebDriver BiDi client configuration
data BiDiPath = MkBiDiPath
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
data WebDriverBiDiMessage = WebDriverBiDiMessage
  { msgId :: Int,
    msgMethod :: Maybe Text,
    msgParams :: Maybe Value,
    msgResult :: Maybe Aeson.Value,
    msgError :: Maybe Aeson.Value
  }
  deriving (Show)

instance FromJSON WebDriverBiDiMessage where
  parseJSON = Aeson.withObject "WebDriverBiDiMessage" $ \v ->
    WebDriverBiDiMessage
      <$> v .: "id"
      <*> v .:? "method"
      <*> v .:? "params"
      <*> v .:? "result"
      <*> v .:? "error"

instance ToJSON WebDriverBiDiMessage where
  toJSON (WebDriverBiDiMessage id' method params _ _) =
    Aeson.object $
      ["id" .= id']
        <> maybe [] (\m -> ["method" .= m]) method
        <> maybe [] (\p -> ["params" .= p]) params

fireFoxCapability :: Capability
fireFoxCapability =
  MkCapability
    { acceptInsecureCerts = Just True,
      browserName = Just "firefox",
      webSocketUrl = True,
      browserVersion = Nothing,
      platformName = Nothing,
      proxy = Nothing,
      unhandledPromptBehavior = Nothing
    }

firefoxCapabilies :: Capabilities
firefoxCapabilies =
  MkCapabilities
    { alwaysMatch = Just fireFoxCapability,
      firstMatch = []
    }

-- | Create a BiDi message for session.new command
createSessionNewMessage :: Int -> Capabilities -> WebDriverBiDiMessage
createSessionNewMessage msgId caps =
  WebDriverBiDiMessage
    { msgId = msgId,
      msgMethod = Just "session.new",
      msgParams = Just $ toJSON caps,
      msgResult = Nothing,
      msgError = Nothing
    }

-- | Create a BiDi message for session.end command
createSessionEndMessage :: Int -> WebDriverBiDiMessage
createSessionEndMessage msgId =
  WebDriverBiDiMessage
    { msgId = msgId,
      msgMethod = Just "session.end",
      msgParams = Nothing,
      msgResult = Nothing,
      msgError = Nothing
    }

-- | WebDriver BiDi client with communication channels
data WebDriverBiDiClient = MkWebDriverBiDiClient
  { sendMessage :: Value -> IO (),
    receiveChannel :: TChan Value,
    disconnect :: IO ()
  }

-- | Run WebDriver BiDi client and return a client interface
runWebDriverBiDi :: BiDiPath -> IO WebDriverBiDiClient
runWebDriverBiDi MkBiDiPath {host, port, path} = do
  putStrLn $ "Connecting to WebDriver at " <> host <> ":" <> pack (show port) <> path

  -- Create communication channels
  inChan <- newTChanIO
  outChan <- newTChanIO
  logChan <- newTChanIO

  let log :: Text -> IO ()
      log msg = do
        atomically $ writeTChan logChan msg

      withErrorHandling :: IO () -> IO ()
      withErrorHandling action =
        action `catch` \(e :: SomeException) -> do
          log $ "Thread failed: " <> pack (show e)

  -- Set up the client
  log "Creating async client..."
  clientAsync <- async $ do
    log "Starting WebSocket client..."
    runClient (unpack host) port (unpack path) $ \conn -> do
      log "WebSocket connection established!"

      let runForever name action = async $ do
            log $ "Starting " <> name <> " thread..."
            forever $ withErrorHandling action

      -- Start all threads
      logAsync <- runForever "logger" $ do
        msg <- atomically $ readTChan logChan
        putStrLn $ "[LOG OUTPUT] " <> msg

      receiverAsync <- runForever "receiver" $ do
        msg <- receiveData conn
        log $ "Received raw data: " <> pack (take 100 (show msg)) <> "..."
        case eitherDecode msg of
          Left err -> log $ "Error parsing message: " <> pack err
          Right parsedMsg -> do
            log "Successfully parsed message, writing to channel..."
            atomically $ writeTChan inChan parsedMsg

      senderAsync <- runForever "sender" $ do
        msgToSend <- atomically $ readTChan outChan
        log $ "Sending message: " <> pack (show msgToSend)
        sendTextData conn (BL.toStrict $ encode msgToSend)

      -- Wait for any thread to fail (they shouldn't unless there's an error)
      waitAny  [receiverAsync, senderAsync, logAsync]
      log "One of the WebSocket threads terminated, closing connection."

  -- Return the client interface
  pure $
    MkWebDriverBiDiClient
      { sendMessage = \msg -> atomically $ writeTChan outChan $ toJSON msg,
        receiveChannel = inChan,
        disconnect = do
          log "Disconnecting client..."
          cancel clientAsync
      }

-- | Run an example with the STM-based WebDriver BiDi client
newBidiSessionDemo :: BiDiPath -> IO ()
newBidiSessionDemo bidiPath = do
  client <-
    runWebDriverBiDi bidiPath `catch` \(e :: SomeException) -> do
      putStrLn $ "Connection error: " <> pack (show e)
      error "Failed to connect to WebDriver BiDi endpoint"

  putStrLn "Connection established successfully!"
  let send :: forall a. (ToJSON a) => a -> IO ()
      send msgJSON = do
        client.sendMessage $ toJSON msgJSON

  -- Start message handler
  void $ forkIO $ forever $ do
    msg <- atomically $ readTChan client.receiveChannel
    putStrLn $ "Received message"
    prettyPrintJson msg
  -- case msg.msgResult of
  --   Just result ->
  --     case Aeson.fromJSON result of
  --       -- Aeson.Success (MkSessionNewResult sessionId' caps) -> do
  --       --   putStrLn $ "Session created successfully!"
  --       --   putStrLn $ "Session ID: " <> sessionId'
  --       --   putStrLn $ "Browser: " <> caps.browserName <> " " <> caps.browserVersion
  --       Aeson.Success val -> do
  --         putStrLn "!!!! Got Response !!!!"
  --         prettyPrintJson val
  --       -- Handle specific error cases
  --       Aeson.Error _ ->
  --         putStrLn $ "Received result: " <> pack (show result)
  --   Nothing ->
  --     putStrLn $ "Received message: " <> pack (show msg)

  -- Send session.new command
  -- send firefoxCapabilies
  send $ createSessionNewMessage 1 firefoxCapabilies

  -- Interactive loop
  let loop = do
        putStrLn "Enter command (or 'quit' to exit):"
        cmd <- getLine
        case cmd of
          "q" -> do
            send $ createSessionEndMessage 2
            client.disconnect
          "r" -> do
            -- Example of sending a browsing context command
            let browsingContextMsg =
                  WebDriverBiDiMessage
                    { msgId = 3,
                      msgMethod = Just "browsingContext.reload",
                      msgParams = Just $ Aeson.object ["context" .= ("current" :: Text)],
                      msgResult = Nothing,
                      msgError = Nothing
                    }
            send $ toJSON browsingContextMsg
            loop
          _ -> do
            putStrLn "Unknown command"
            loop

  loop

---- wuss example - works but driver is not a secure connection ----
---- keep around for later - may be useful for remote providers such as LambdaTest / BrowserStack  ----

-- >>> wussDemo
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
