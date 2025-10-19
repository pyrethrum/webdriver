module TestServer
  ( withTestServer,
    authTestUrl,
    invalidUrl,
    testServerHomeUrl,
    malformedResponseUrl,
    boringHelloUrl,
    boringHelloUrl2,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (void, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import IOUtils (findWebDriverRoot)
import Network.HTTP.Req (GET (..), NoReqBody (..), defaultHttpConfig, http, ignoreResponse, port, req, runReq)
import System.Directory (getCurrentDirectory)
import UnliftIO (finally)
import UnliftIO.Process (createProcess, cwd, proc, terminateProcess)
import Prelude

testServerHomeUrl :: Text
testServerHomeUrl = "http://localhost:8000/"

subPage :: Text -> Text
subPage subPath = testServerHomeUrl <> "/" <> subPath

authTestUrl :: Text
authTestUrl = subPage "authtest"

invalidUrl :: Text
invalidUrl = subPage "invalid-page"

boringHelloUrl :: Text
boringHelloUrl = subPage "boringHello"

boringHelloUrl2 :: Text
boringHelloUrl2 = subPage "boringHello2"

malformedResponseUrl :: Text
malformedResponseUrl = subPage "malformed-response"

-- | Ping the test server until it responds with 200 OK
-- Throws an exception if the server doesn't respond within 10 seconds
pingUntilReady :: Text -> IO ()
pingUntilReady url = do
  startTime <- getCurrentTime
  let plusTenSecs = addUTCTime 10 startTime
  tryPing plusTenSecs
  where
    tryPing expiryTime = do
      currentTime <- getCurrentTime
      when (currentTime > expiryTime) $
        throwIO $
          userError $
            "Test server at " <> T.unpack url <> " did not become ready within 10 seconds"

      catch
        ( runReq defaultHttpConfig $
            void $
              req GET (http "localhost") NoReqBody ignoreResponse (port 8000)
        )
        \(_ :: SomeException) ->
          threadDelay 100_000 -- 100ms
            >> tryPing expiryTime

-- | Run an IO action with the test server running
withTestServer :: forall a. IO a -> IO a
withTestServer action = do
  currentDir <- getCurrentDirectory
  case findWebDriverRoot currentDir of
    Nothing ->
      fail $
        "Could not find 'webdriver' root directory from: "
          <> currentDir
          <> "\n withTestServer expects to be run from the 'webdriver' directory"
    Just webdriverRoot -> do
      let serverStart = (proc "cabal" ["run", "test-server"]) {cwd = Just webdriverRoot}
      (_, _, _, handle) <- createProcess serverStart
      pingUntilReady testServerHomeUrl
      finally
        action
        $ terminateProcess handle
