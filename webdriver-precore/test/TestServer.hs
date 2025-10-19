module TestServer
  ( withTestServer,
    authTestUrl
  )
where

import IOUtils (findWebDriverRoot)
import System.Directory (getCurrentDirectory)
import UnliftIO (finally)
import UnliftIO.Process (createProcess, cwd, proc, terminateProcess)
import Prelude
import Data.Text (Text)

authTestUrl :: Text
authTestUrl = "http://localhost:8000/authtest"

-- | Run an IO action with the test server running
withTestServer :: forall a. IO a -> IO a
withTestServer action = do
  currentDir <- getCurrentDirectory
  case findWebDriverRoot currentDir of
    Nothing ->
      error $
        "Could not find 'webdriver' root directory from: "
          <> currentDir
          <> "\n withTestServer expects to be run from the 'webdriver' directory"
    Just webdriverRoot -> do
      let processConfig = (proc "cabal" ["run", "test-server"]) {cwd = Just webdriverRoot}
      (_, _, _, handle) <- createProcess processConfig
      finally
        action
        $ terminateProcess handle
