module HTTP.Runner
  ( mkRunner,
    HttpRunner (..)
  )
where

import Data.Aeson (FromJSON (..), Value, withObject, (.:))
import Data.Aeson.Types (Parser, Value (..), parseEither, parseMaybe)
import Data.Function ((&))
import GHC.Exception (throw)
import IOUtils (DemoActions (..))
import Network.HTTP.Req
  ( Scheme (..),
    Url,
  )
import UnliftIO (catchAny)
import WebDriverPreCore.HTTP.Protocol ( Command(..), WebDriverException(..), parseWebDriverException)
import Prelude hiding (log)

import HTTP.HttpClient ( callWebDriver', mkRequest)
import Data.Text (pack)


-- ############# Runner #############

data HttpRunner = MkHttpRunner
  { run :: forall r. (FromJSON r) => Command r -> IO r,
    fullResponse :: forall r. (FromJSON r) => Command r -> IO Value
  }

mkRunner :: Url 'Http -> Int -> DemoActions -> HttpRunner
mkRunner driverUrl port da =
  MkHttpRunner
    { run = run driverUrl port da,
      fullResponse = logCall driverUrl port da
    }

logCall :: forall r. Url 'Http -> Int -> DemoActions -> Command r -> IO Value
logCall driverUrl port da cmd = do
  logCommand da cmd
  callWebDriver' da $ mkRequest driverUrl port cmd

run :: forall r. (FromJSON r) => Url 'Http -> Int -> DemoActions -> Command r -> IO r
run driverUrl port da@MkDemoActions {logShow} cmd = do
  rsp <- logCall driverUrl port da cmd
  r <-
    catchAny
      (parseResultIO rsp)
      ( \e ->
          logShow "Command Execution Failed" e
            >> throw e
      )
  pure r

logCommand :: DemoActions -> Command r -> IO ()
logCommand da@MkDemoActions {logShow} cmd = do
  logShow (">>>>> Command: " <> cmd.description <> " >>>>>\n") cmd
  case cmd of
    Get {} -> pure ()
    Post {body} -> do
      da.logJSON "body" $ Object body
    PostEmpty {} -> pure ()
    Delete {} -> pure ()


parseResultIO :: forall r. (FromJSON r) => Value -> IO r
parseResultIO body  =
  parseMaybe valueParser body
    & maybe
      ( throw $ ResponseParseException "No value property found in WebDriver response" body
      )
      ( \val ->
          parseEither @_ @r parseJSON val & either
            (\e -> throw $ parseWebDriverException (pack e) val)
            pure
      )
  

valueParser :: Value -> Parser Value
valueParser body = body & withObject "body value" (.: "value")
