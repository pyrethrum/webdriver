module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), SomeException, catch, throwIO)
import Control.Monad (forever)
import Data.Aeson (ToJSON, Value, eitherDecode, encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Text as T (Text, breakOn, pack, splitOn, unpack)
import Data.Text.IO as T (getLine, putStrLn)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import IOUtils (ppTxt)
import Network.WebSockets (receiveData, runClient, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import Text.Read (readEither)
import UnliftIO.Async (Async, async, cancel, waitAny)
import UnliftIO.STM
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol (sessionStatus, browsingContextCreate)
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (jsonToText)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, putStrLn)
import WebDriverPreCore.BiDi.BrowsingContext (Create(MkCreate))
import WebDriverPreCore.BiDi.BrowsingContext (CreateType(..))

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
  ses <- loadConfig >>= newHttpSession . httpBidiCapabilities
  getBiDiPath ses & either (error . T.unpack) pure

-- >>> biDiDemo
biDiDemo :: IO ()
biDiDemo = sessionViaHttp >>= bidiSession

-- | WebDriver BiDi client configuration
data BiDiPath = MkBiDiPath
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show)

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
        threadDelay 1_000_000 
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend))

      -- Wait for any thread to fail (they shouldn't unless there's an error)
      waitAny [receiver, sender]
      threadDelay 1_000_000 -- Wait a bit before closing
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
        log "Enter c for new context:"
        msg <- getLine
        if msg == "c" then do
          log "Creating new tab"
          send . browsingContextCreate (MkCreate Tab Nothing Nothing Nothing) $ MkJSUInt 2
        else
          send . sessionStatus $ MkJSUInt 2
        threadDelay 500_000 -- Give some time for the server to respond
        loop

  loop
