module BiDi.BiDiRunner where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever, unless, void)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, (.:), (.:?), (.=), toJSON, Value)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text as T (Text, breakOn, null, pack, splitOn, unpack)
import Data.Text.IO as T (getLine, putStrLn)
import Http.HttpAPI (FullCapabilities, SessionResponse(..), newSessionFull)
import IOUtils (ppTxt)
import Network.WebSockets (ClientApp, receiveData, runClient, sendClose, sendTextData, Connection)
import Text.Read (readEither)
import WebDriverPreCore.BiDi.Session
import Wuss (runSecureClient)
import Prelude hiding (getLine, null, putStrLn)
import Config ( loadConfig, Config )
import qualified WebDriverPreCore.Http as Http
import Http.HttpAPI qualified as Caps (Capabilities (..))
import RuntimeConst (httpFullCapabilities, httpCapabilities)
import UnliftIO.STM 
import UnliftIO.Async (async, cancel, waitEither_)

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
  { sendMessage :: Value -> IO ()
  , receiveChannel :: TChan WebDriverBiDiMessage
  , disconnect :: IO ()
  }

-- | Run WebDriver BiDi client and return a client interface
runWebDriverBiDi :: BiDiPath -> IO WebDriverBiDiClient
runWebDriverBiDi MkBiDiPath {host, port, path} = do
  putStrLn $ "Connecting to WebDriver at " <> host <> ":" <> pack (show port) <> path
  
  -- Create communication channels
  inChan <- newTChanIO
  outChan <- newTChanIO
  
  -- Set up the client
  clientAsync <- async $ runClient (unpack host) port (unpack path) $ \conn -> do
    -- Start message receiver thread
    receiverAsync <- async $ forever $ do
      msg <- receiveData conn
      case eitherDecode msg of
        Left err -> putStrLn $ "Error parsing message: " <> pack err
        Right parsedMsg -> atomically $ writeTChan inChan parsedMsg
    
    -- Start message sender thread
    senderAsync <- async $ forever $ do
      msgToSend <- atomically $ readTChan outChan
      sendTextData conn (BL.toStrict $ encode msgToSend)
    
    -- Wait for both threads to complete (they shouldn't unless there's an error)
    waitEither_ receiverAsync senderAsync
  
  -- Return the client interface
  pure $ MkWebDriverBiDiClient
    { sendMessage = \msg -> atomically $ writeTChan outChan $ toJSON msg
    , receiveChannel = inChan
    , disconnect = cancel clientAsync
    }


-- | Run an example with the STM-based WebDriver BiDi client
newBidiSessionDemo :: BiDiPath -> IO ()
newBidiSessionDemo bidiPath = do
  client <- runWebDriverBiDi bidiPath
  let send = client.sendMessage . toJSON

  -- Start message handler
  void $ forkIO $ forever $ do
    msg <- atomically $ readTChan client.receiveChannel
    case msg.msgResult of
      Just result ->
        case Aeson.fromJSON result of
          Aeson.Success (MkSessionNewResult sessionId' caps) -> do
            putStrLn $ "Session created successfully!"
            putStrLn $ "Session ID: " <> sessionId'
            putStrLn $ "Browser: " <> caps.browserName <> " " <> caps.browserVersion
          Aeson.Error _ ->
            putStrLn $ "Received result: " <> pack (show result)
      Nothing ->
        putStrLn $ "Received message: " <> pack (show msg)
  
  -- Send session.new command
  let capabilities = MkCapabilities
        { alwaysMatch = Just $ MkCapability
            { acceptInsecureCerts = Just True
            , browserName = Just "firefox"
            , webSocketUrl = True
            , browserVersion = Nothing
            , platformName = Nothing
            , proxy = Nothing
            , unhandledPromptBehavior = Nothing
            }
        , firstMatch = []
        }

  send $ createSessionNewMessage 1 capabilities

  -- Interactive loop
  let loop = do
        putStrLn "Enter command (or 'quit' to exit):"
        cmd <- getLine
        case cmd of
          "quit" -> do
            send $ createSessionEndMessage 2
            client.disconnect
          "reload" -> do
            -- Example of sending a browsing context command
            let browsingContextMsg = WebDriverBiDiMessage
                  { msgId = 3
                  , msgMethod = Just "browsingContext.reload"
                  , msgParams = Just $ Aeson.object ["context" .= ("current" :: Text)]
                  , msgResult = Nothing
                  , msgError = Nothing
                  }
            client.sendMessage $ toJSON browsingContextMsg
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
