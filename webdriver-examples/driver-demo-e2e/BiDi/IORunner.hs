module BiDi.IORunner where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Wuss (runSecureClient)
import Prelude (Foldable (..), IO, getLine, print, ($), (.))

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
