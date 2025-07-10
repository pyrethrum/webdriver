module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (Exception (displayException), SomeException, catch, finally, throwIO, try)
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
import UnliftIO.Async (async, cancel, wait, waitAny, waitEither_)
import UnliftIO.STM
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (prettyPrintJson, jsonToText)
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
  (_scheme, hostPortPath) <- splitScheme
  (host, portPath) <- splitHost hostPortPath
  (port, path) <- portAndPath portPath
  Right $
    MkBiDiPath
      { 
        host,
        port,
        path
      }
  where
    -- "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"
    -- -> [ws, //127.0.0.1, 9222/session/e43698d9-b02a-4284-a936-12041deb3552]
    splitScheme =
      splitOn "://" url
        & \case
          [scheme, hostPortPath] -> Right (scheme, hostPortPath)
          _ -> failParser "Expected format: ws://host:port/path"

    splitHost hostPortPath = 
      T.splitOn ":" hostPortPath
        & \case
          [host, portPath] -> Right (host, portPath)
          _ -> failParser "Expected format: host:port/path"
  
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
biDiDemo =
  -- sessionViaHttp >>= newBidiSessionDemo - does not work because the session is already created
  newBidiSessionDemo MkBiDiPath
  { host = "127.0.0.1",
    port = 9222,
    path = ""
  }

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
  { 
    log :: Text -> IO (),
    sendMessage :: Value -> IO (),
    receiveChannel :: TChan Value,
    disconnect :: IO ()
  }

-- | Run WebDriver BiDi client and return a client interface
runWebDriverBiDi :: BiDiPath -> IO WebDriverBiDiClient
runWebDriverBiDi bidiPth@MkBiDiPath {host, port, path} = do
  -- Create communication channels
  outChan <- newTChanIO
  receiveChan <- newTChanIO
  logChan <- newTChanIO

  print bidiPth

  forkIO $ forever $ do
    msg <- atomically $ readTChan logChan
    putStrLn $ "[LOG] " <> msg

  let log :: Text -> IO ()
      log msg = do
        -- putStrLn $ "[LOG] !!!!!!!!!!!!!! " <> msg
        atomically $ writeTChan logChan msg

      withErrorHandling :: IO () -> IO ()
      withErrorHandling action =
        action `catch` \(e :: SomeException) -> do
          log $ "Thread failed: " <> pack (show e)

  -- Set up the client
  log "Creating async client..."
  clientAsync <- async $ do
    log $ "Connecting to WebDriver at " <> host <> ":" <> pack (show port) <> path
    runClient (unpack host) port (unpack path) $ \conn -> do
      log "WebSocket connection established!"

      let runForever name action = async $ do
            log $ "Starting " <> name <> " thread..."
            forever $ withErrorHandling action

      -- -- Start all threads
      -- logAsync <- runForever "logger" $ do
      --   msg <- atomically $ readTChan logChan
      --   putStrLn $ "[LOG OUTPUT] " <> msg

      receiverAsync <- runForever "receiver" $ do
        msg <- receiveData conn
        log $ "Received raw data: " <> pack (take 100 (show msg)) <> "..."
        case eitherDecode msg of
          Left err -> log $ "Error parsing message: " <> pack err
          Right parsedMsg -> do
            log "Successfully parsed message, writing to channel..."
            atomically $ writeTChan receiveChan parsedMsg

      senderAsync <- runForever "sender" $ do
        msgToSend <- atomically $ readTChan outChan
        log $ "Sending message: " <> pack (show msgToSend)
        sendTextData conn (BL.toStrict $ encode msgToSend)

      -- Wait for any thread to fail (they shouldn't unless there's an error)
      waitAny [receiverAsync, senderAsync]
      log "One of the WebSocket threads terminated, closing connection."

  -- Wait for client to be ready or to fail
  -- This will either wait for the ready signal or propagate any exception
  -- from the clientAsync thread
  log "Starting client"

  -- forkIO $ wait clientAsync `catch` \(e :: SomeException) -> do 
  --   -- threadDelay 100_000
  --   throwIO e

  -- Replace the problematic forkIO line with:
  void $ async $ do
    result <- try $ wait clientAsync
    case result of
      Left (e :: SomeException) -> do
        log $ "WebSocket connection failed: " <> pack (show e)
        throwIO e
      Right _ -> 
        log "WebSocket connection closed normally"

  log "Client started"
  -- Return the client interface
  pure $
    MkWebDriverBiDiClient
      { 
        log,
        sendMessage = \msg -> atomically $ writeTChan outChan $ toJSON msg,
        receiveChannel = receiveChan,
        disconnect = do
          log "Disconnecting client..."
          cancel clientAsync
      }

-- | Run an example with the STM-based WebDriver BiDi client
newBidiSessionDemo :: BiDiPath -> IO ()
newBidiSessionDemo bidiPath = do
  client <-
    runWebDriverBiDi bidiPath `catch` \(e :: SomeException) ->
      error $ "Failed to initialise client:\n" <> displayException e

  let send :: forall a. (ToJSON a) => a -> IO ()
      send msgJSON = do
        client.sendMessage $ toJSON msgJSON

  -- Start message handler
  void $ forkIO $ forever $ do
    msg <- atomically $ readTChan client.receiveChannel
    client.log $ "MESSAGE RECEIVED: " <> jsonToText msg
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
        client.log "Enter command (or 'q' to exit 'r' to reload):"
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
            client.log "Unknown command"
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
