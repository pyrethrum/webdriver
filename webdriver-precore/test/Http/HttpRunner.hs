module Http.HttpRunner
  ( mkRunner,
    HttpRunner (..),
  )
where

import Data.Aeson (FromJSON (..), Result (..), Value, withObject, (.:))
import Data.Aeson.Types (Parser, Value (..), parse)
import Data.Function ((&))
import Data.Text (Text, unpack)
import GHC.Exception (throw)
import Http.HttpEndpoint (callWebDriver', mkRequest)
import IOUtils (DemoActions (..))
import Network.HTTP.Req
  ( Scheme (..),
    Url,
  )
import Text.Show.Pretty (ppShow)
import UnliftIO (catchAny)

import WebDriverPreCore.Http.Protocol (Command (..), WebDriverException (..), parseWebDriverException)

import Prelude hiding (log)

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
      (parseResultIO rsp cmd.description)
      ( \e -> do
          logShow "Command Execution Failed" e
          throw e
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

-- todo: fix to throw proper exceptions when moved to own library
parseResultIO :: forall r. (FromJSON r) => Value -> Text -> IO r
parseResultIO body description =
  case parse fromBodyValue body of
    Error msg ->
      fail $
        parseWebDriverException body & \case
          e@ResponeParseException {} -> unpack description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> ppShow e
          e@UnrecognisedException {} -> "UnrecognisedError:\n " <> "\nin response:" <> ppShow e
          e@ProtocolException {} -> "WebDriver Protocol Error thrown:\n " <> ppShow e
    Success r -> pure r

fromBodyValue :: forall a. (FromJSON a) => Value -> Parser a
fromBodyValue body =
  body & withObject "body value" \b -> do
    val <- b .: "value"
    parseJSON val
