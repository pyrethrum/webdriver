module Http.HttpRunner
  ( mkRunner,
    HttpRunner (..),
    Extended (..),
  )
where

import Data.Aeson (FromJSON (..), Value)
import GHC.Exception (throw)
import Http.HttpEndpoint (callWebDriver, mkRequest)
import Http.HttpUtils (parseResultIO)
import IOUtils (DemoActions (..))
import Network.HTTP.Req
  ( Scheme (..),
    Url,
  )
import UnliftIO (catchAny)
import WebDriverPreCore.Http.Command (Command (..))
import WebDriverPreCore.Http.HttpResponse (HttpResponse)
import Prelude hiding (log)

-- ############# Runner #############

data Extended r = MkExtended
  { parsed :: r,
    fullResponse :: Value
  }

data HttpRunner = MkHttpRunner
  { run :: forall r. (FromJSON r) => Command r -> IO r,
    run_ :: Command () -> IO (),
    -- general fallback functions
    run' :: forall r. (FromJSON r) => Command r -> IO (Extended r),
    fullResponse :: Command () -> IO Value
  }

mkRunner :: Url 'Http -> Int -> DemoActions -> HttpRunner
mkRunner driverUrl port da =
  MkHttpRunner
    { run = run False driverUrl port da,
      run_ = run True driverUrl port da,
      run' = runExtended False driverUrl port da,
      fullResponse = logCall driverUrl port da
    }

logCall :: forall r. Url 'Http -> Int -> DemoActions -> Command r -> IO Value
logCall driverUrl port da cmd = do
  logCommand da cmd
  callWebDriver da $ mkRequest driverUrl port cmd

runExtended :: forall r. (FromJSON r) => Bool -> Url 'Http -> Int -> DemoActions -> Command r -> IO (Extended r)
runExtended expectNull driverUrl port da@MkDemoActions {logShow} cmd = do
  rsp <- logCall driverUrl port da cmd
  r <-
    catchAny
      (parseResultIO expectNull rsp cmd.description)
      ( \e -> do
          logShow "Command Execution Failed" e
          throw e
      )
  pure $ MkExtended r rsp

run :: forall r. (FromJSON r) => Bool -> Url 'Http -> Int -> DemoActions -> Command r -> IO r
run expectNull driverUrl port da cmd =
  (.parsed) <$> runExtended expectNull driverUrl port da cmd

logCommand :: DemoActions -> Command r -> IO ()
logCommand da@MkDemoActions {logShow} cmd = do
  logShow (">>>>> Command: " <> cmd.description <> " >>>>>\n") cmd
  case cmd of
    Get {} -> pure ()
    Post {body} -> do
      da.logJSON "body" body
    PostEmpty {} -> pure ()
    Delete {} -> pure ()
