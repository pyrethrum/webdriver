module BiDi.WussDemo where

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
import WebDriverPreCore.BiDi.Protocol (newSession, sessionEnd, sessionStatus)

import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.Http qualified as Http
import WebDriverPreCore.Internal.AesonUtils (jsonToText, prettyPrintJson)
import WebDriverPreCore.Internal.Utils (txt)
import Wuss (runSecureClient)
import Prelude hiding (getLine, log, null, putStrLn)

-- | A simple WebSocket client that connects to 'echo.websocket.org' and
-- echoes back any messages sent to it. This is a demonstration of using
-- the 'wuss' library to create a secure WebSocket connection.
-- Not in use now but We will keep it around for future reference, could be useful for third-party WebSocket services.

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

