module BiDi.BiDiRunner where

import Config (Config, loadConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), SomeException, catch)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, Value, encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Text as T (Text, pack, unpack)
import Data.Text.IO as T (putStrLn)
import Http.HttpAPI (FullCapabilities, SessionResponse (..), deleteSession, newSessionFull)
import Http.HttpAPI qualified as Caps (Capabilities (..))
import Network.WebSockets (receiveData, runClient, sendTextData)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import UnliftIO (bracket, throwIO)
import UnliftIO.Async (Async, async, cancel, waitAny)
import UnliftIO.STM
import WebDriverPreCore.BiDi.BiDiPath (BiDiPath (..), getBiDiPath)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import WebDriverPreCore.BiDi.ResponseEvent (MatchedResponse (..), ResponseError, ResponseObject, decodeResponse, parseResponse)
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (getLine, log, null, putStrLn)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt(..))
import WebDriverPreCore.BiDi.Command

-- note: just throws an exception if an error is encountered 
-- no timeout implemented - will just hang if bidi does not behave
sendCommand :: forall c r. (FromJSON r, ToJSON c) => WebDriverBiDiClient -> Command c r  -> IO r
sendCommand
  MkWebDriverBiDiClient {sendMessage, receiveChannel, nextId}
  command = do
    id' <- nextId
    sendMessage $ commandValue command id'
    matchedResponse id'
    where
      matchedResponse :: JSUInt -> IO r
      matchedResponse id' = do
        response <- atomically $ readTChan receiveChannel
        parseResponse id' response
          & either
            (error . unpack . txt)
            ( maybe
                (matchedResponse id')
                (pure . (.response))
            )

withBiDiSession :: (WebDriverBiDiClient -> IO a) -> IO a
withBiDiSession action =
  bracket
    httpSession
    (deleteSession . (.sessionId))
    \s' -> do
      let bidiPath = getBiDiPath s' & either (error . T.unpack) id
      withBiDi bidiPath action

withBiDi :: BiDiPath -> (WebDriverBiDiClient -> IO a) -> IO a
withBiDi bidiPath action =
  bracket
    (runWebDriverBiDi bidiPath)
    ( \client ->
        putStrLn "Ending BiDi session - will need some clean up here"
          >> client.disconnect
    )
    action

-- | WebDriver BiDi client with communication channels
data WebDriverBiDiClient = MkWebDriverBiDiClient
  { log :: Text -> IO (),
    nextId :: IO JSUInt,
    sendMessage :: forall a. (ToJSON a) => a -> IO (),
    receiveChannel :: TChan (Either ResponseError ResponseObject),
    disconnect :: IO ()
  }

-- | Run WebDriver BiDi client and return a client interface
runWebDriverBiDi :: BiDiPath -> IO WebDriverBiDiClient
runWebDriverBiDi bidiPth = do
  -- Create communication channels
  sendChan <- newTChanIO
  receiveChan <- newTChanIO
  logChan <- newTChanIO
  counter <- newTVarIO $ MkJSUInt 1 

  loggerAsync <- async . forever $ do
    msg <- atomically $ readTChan logChan
    putStrLn $ "[LOG] " <> msg

  let log = atomically . writeTChan logChan

  clientAsync <- startClient bidiPth log sendChan receiveChan

  pure $
    MkWebDriverBiDiClient
      { log,
        nextId = atomically $ do
          i <- readTVar counter
          writeTVar counter $ succ i
          pure i,
        sendMessage = atomically . writeTChan sendChan . toJSON,
        receiveChannel = receiveChan,
        disconnect = do
          log "Disconnecting client... TODO: implement proper cleanup"
          cancel clientAsync
          threadDelay 1000_000
          cancel loggerAsync
      }

startClient :: BiDiPath -> (Text -> IO ()) -> TChan Value -> TChan (Either ResponseError ResponseObject) -> IO (Async ())
startClient pth@MkBiDiPath {host, port, path} log sendChan receiveChan =
  async $ do
    log $ "Connecting to WebDriver at " <> txt pth
    catch
      runSocketClient
      ( \(e :: SomeException) -> do
          log $ "WebSocket failure: " <> pack (displayException e)
          -- flush log channelnewSessionFull .
          threadDelay 1000_000
          throwIO e
      )
  where
    runSocketClient = runClient (unpack host) port (unpack path) $ \conn -> do
      log "WebSocket connection established!"

      let runForever = asyncForever log

      receiver <- runForever "receiver" $ do
        msg <- receiveData conn
        log $ "Received raw data: " <> pack (take 100 (show msg)) <> "..."
        atomically . writeTChan receiveChan $ decodeResponse msg

      sender <- runForever "sender" $ do
        msgToSend <- atomically $ readTChan sendChan
        log $ "Sending Message: " <> pack (show msgToSend)
        catchLog
          "Message Send Failed"
          log
          (sendTextData conn (BL.toStrict $ encode msgToSend))

      -- Wait for any thread to fail (they shouldn't unless there's an error)
      waitAny [receiver, sender]
      threadDelay 1_000_000 -- Wait a bit before closing
      putStrLn "One of the WebSocket threads terminated, closing connection."

httpSession :: IO SessionResponse
httpSession = loadConfig >>= newSessionFull . httpBidiCapabilities

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

catchLog :: Text -> (Text -> IO ()) -> IO () -> IO ()
catchLog message log action =
  action `catch` \(e :: SomeException) -> do
    log $ message <> ": " <> pack (show e)
    threadDelay 1000_000 -- Wait a bit before rethrowing
    throwIO e

asyncForever :: (Text -> IO ()) -> Text -> IO () -> IO (Async ())
asyncForever log name action = async $ do
  log message
  forever $ catchLog message log action
  where
    message = "Starting " <> name <> " thread"

newHttpSession :: FullCapabilities -> IO SessionResponse
newHttpSession = newSessionFull
