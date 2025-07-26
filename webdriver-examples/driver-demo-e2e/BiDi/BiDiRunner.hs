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
import UnliftIO.Async (Async, async, cancel, wait, waitAny, waitEither_)
import UnliftIO.STM
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol (newSession, sessionEnd)
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (jsonToText, prettyPrintJson)
import WebDriverPreCore.Internal.Utils (txt)
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
      { host,
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
-- sessionViaHttp >>= newBidiSessionDemo - does not work because the session is already created
biDiDemo :: IO ()
biDiDemo = sessionViaHttp >>= bidiSession

--  newBidiSessionDemo
--   MkBiDiPath
--     { host = "127.0.0.1",
--       port = 9222,
--       path = ""
--     }
-- threadDelay 1000_000 -- Wait for a while to see the output
-- newBidiSessionDemo
--   MkBiDiPath
--     { host = "127.0.0.1",
--       port = 9222,
--       path = "/session"
--     }

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
  { log :: Text -> IO (),
    sendMessage :: forall a. (ToJSON a) => a -> IO (),
    receiveChannel :: TChan Value,
    disconnect :: IO ()
  }

catchLog :: Text -> (Text -> IO ()) -> IO () -> IO ()
catchLog message log action =
  action `catch` \(e :: SomeException) -> do
    log $ message <> ": " <> pack (show e)

asyncForever :: (Text -> IO ()) -> Text -> IO () -> IO (Async ())
asyncForever log name action = async $ do
  log message
  forever $ catchLog message log action
  where
    message = "Starting " <> name <> " thread"

startClient :: BiDiPath -> (Text -> IO ()) -> TChan Value -> TChan Value -> IO (Async ())
startClient pth@MkBiDiPath {host, port, path} log sendChan receiveChan =
  async $ do
    log $ "Connecting to WebDriver at " <> txt pth
    catch
      webSocketGo
      ( \(e :: SomeException) -> do
          log $ "WebSocket failure: " <> pack (displayException e)
          -- flush log channel
          threadDelay 100_000
          throwIO e
      )
  where
    webSocketGo = runClient (unpack host) port (unpack path) $ \conn -> do
      log "WebSocket connection established!"

      let runForever = asyncForever log

      receiver <- runForever "receiver" $ do
        msg <- receiveData conn
        log $ "Received raw data: " <> pack (take 100 (show msg)) <> "..."
        case eitherDecode msg of
          Left err -> log $ "Error parsing message: " <> pack err
          Right parsedMsg -> do
            log "Successfully parsed message, writing to channel..."
            atomically $ writeTChan receiveChan parsedMsg

      sender <- runForever "sender" $ do
        msgToSend <- atomically $ readTChan sendChan
        log $ "Sending Message: " <> pack (show msgToSend)
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend))

      -- Wait for any thread to fail (they shouldn't unless there's an error)
      waitAny [receiver, sender]
      threadDelay 1000_000 -- Wait a bit before closing
      putStrLn "One of the WebSocket threads terminated, closing connection."
     

-- | Run WebDriver BiDi client and return a client interface
runWebDriverBiDi :: BiDiPath -> IO WebDriverBiDiClient
runWebDriverBiDi bidiPth = do
  -- Create communication channels
  sendChan <- newTChanIO
  receiveChan <- newTChanIO
  logChan <- newTChanIO

  putStrLn "Starting logger thread"
  loggerAsync <- async . forever $ do
    msg <- atomically $ readTChan logChan
    putStrLn $ "[LOG] " <> msg
  
  -- Give logger thread time to start
  threadDelay 50_000

  let log :: Text -> IO ()
      log = atomically . writeTChan logChan

  -- -- Set up the client
  -- log "Creating async client..."
  clientAsync <- startClient bidiPth log sendChan receiveChan

  pure $
    MkWebDriverBiDiClient
      { log,
        sendMessage = atomically . writeTChan sendChan . toJSON,
        receiveChannel = receiveChan,
        disconnect = do
          log "Disconnecting client... TODO: implement proper cleanup"
          cancel clientAsync
          threadDelay 1000_000
          cancel loggerAsync
      }

bidiSession :: BiDiPath -> IO ()
bidiSession bidiPath = do
  MkWebDriverBiDiClient
    { disconnect,
      sendMessage,
      log,
      receiveChannel
    } <-
    runWebDriverBiDi bidiPath `catch` \(e :: SomeException) ->
      error $ "Failed to initialise client:\n" <> displayException e

  let send :: forall a. (ToJSON a) => a -> IO ()
      send msg = do
        log $ "Queuing message: " <> jsonToText (toJSON msg)
        catch
          (sendMessage msg)
          ( \(e :: SomeException) -> do
              log $ "Failed to queue message: " <> pack (displayException e)
              threadDelay 100_000
              throwIO e
          )

  -- Start message handler
  async . forever $ do
    msg <- atomically $ readTChan receiveChannel
    log $ "MESSAGE RECEIVED: " <> jsonToText msg

  -- No session initialization needed - session was created via HTTP
  log "BiDi WebSocket connected to existing session"
  
  -- Wait a bit for things to settle
  threadDelay 1000_000

  -- Interactive loop
  let loop = do
        log "Enter command (or 'q' to exit 'r' to reload):"
        cmd <- getLine
        case cmd of
          "q" -> do
            send $ sessionEnd $ MkJSUInt 2
            threadDelay 3000_000 -- Wait a bit before disconnecting
            log "Disconnecting..."
            disconnect
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
            log "Unknown command"
            loop

  loop


-- | Pure BiDi session that creates its own session via BiDi
pureBidiSession :: BiDiPath -> IO ()
pureBidiSession bidiPath = do
  MkWebDriverBiDiClient
    { disconnect,
      sendMessage,
      log,
      receiveChannel
    } <-
    runWebDriverBiDi bidiPath `catch` \(e :: SomeException) ->
      error $ "Failed to initialise client:\n" <> displayException e

  let send :: forall a. (ToJSON a) => a -> IO ()
      send msg = do
        log $ "Queuing message: " <> jsonToText (toJSON msg)
        catch
          (sendMessage msg)
          ( \(e :: SomeException) -> do
              log $ "Failed to queue message: " <> pack (displayException e)
              threadDelay 100_000
              throwIO e
          )

  -- Start message handler
  async . forever $ do
    msg <- atomically $ readTChan receiveChannel
    log $ "MESSAGE RECEIVED: " <> jsonToText msg

  -- Create BiDi session properly
  log "Creating new BiDi session..."
  send $ newSession firefoxCapabilies (MkJSUInt 1)
  
  -- Wait for session creation response
  threadDelay 3000_000

  -- Interactive loop
  let loop = do
        putStrLn "Enter command (or 'q' to exit 'r' to reload):"
        cmd <- getLine
        case cmd of
          "q" -> do
            send $ sessionEnd $ MkJSUInt 99
            threadDelay 1000_000
            log "Disconnecting..."
            disconnect
          "r" -> do
            log "Reload command not implemented yet"
            loop
          _ -> do
            log "Unknown command"
            loop

  loop
  
-- | Pure BiDi demo - creates session entirely via BiDi protocol
-- NOTE: This will fail unless port 9222 is open (requires HTTP session first)
-- >>> pureBiDiDemo  
pureBiDiDemo :: IO ()
pureBiDiDemo = do
  putStrLn "Starting pure BiDi demo..."
  putStrLn "WARNING: This will fail unless port 9222 is already open!"
  putStrLn "Use hybridBiDiDemo instead for reliable connection"
  putStrLn "Connecting directly to GeckoDriver BiDi endpoint..."
  
  -- Connect to GeckoDriver's BiDi endpoint (no HTTP session)
  let pureBiDiPath = MkBiDiPath 
        { host = "127.0.0.1"
        , port = 9222  -- BiDi port (may not be open!)
        , path = ""    -- Root BiDi endpoint (not session-specific)
        }
  pureBidiSession pureBiDiPath


  -- | Hybrid BiDi demo - minimal HTTP session creation to open port 9222, then pure BiDi
-- This is the correct approach for GeckoDriver
-- >>> hybridBiDiDemo
hybridBiDiDemo :: IO ()
hybridBiDiDemo = do
  putStrLn "Starting hybrid BiDi demo..."
  putStrLn "Step 1: Creating minimal HTTP session to open port 9222..."
  
  -- Create minimal HTTP session (just to open port 9222)
  cfg <- loadConfig
  _httpSession <- newHttpSession $ httpBidiCapabilities cfg
  putStrLn "HTTP session created, port 9222 should now be open"
  
  -- Small delay to ensure port is ready
  threadDelay 500_000
  
  putStrLn "Step 2: Connecting to pure BiDi on port 9222..."
  -- Now connect to pure BiDi endpoint (not session-specific)
  let pureBiDiPath = MkBiDiPath 
        { host = "127.0.0.1"
        , port = 9222  -- Now available thanks to HTTP session
        , path = ""    -- Root BiDi endpoint
        }
  pureBidiSession pureBiDiPath

  
-- | Direct BiDi connection demo - connects directly to GeckoDriver's BiDi endpoint
-- This avoids the HTTP session creation step
-- >>> biDiDirectDemo
biDiDirectDemo :: IO ()
biDiDirectDemo = do
  putStrLn "Starting direct BiDi demo..."
  putStrLn "Note: This requires an existing session. Creating one first..."
  
  -- First create an HTTP session to get the WebSocket URL
  cfg <- loadConfig
  sesResponse <- newHttpSession $ httpBidiCapabilities cfg
  bidiPath <- getBiDiPath sesResponse & either (error . T.unpack) pure
  
  putStrLn $ "Got BiDi path: " <> txt bidiPath
  bidiSession bidiPath

  -- Original hardcoded approach (doesn't work without existing session):
  -- let directPath =
  --       MkBiDiPath
  --         { host = "127.0.0.1",
  --           port = 9222, -- GeckoDriver's WebSocket port
  --           path = "/session" -- BiDi endpoint
  --         }
  -- bidiSession directPath

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

