module BiDi.WussDemo where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text as T (Text, null, pack)
import Data.Text.IO as T (getLine, putStrLn)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

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

