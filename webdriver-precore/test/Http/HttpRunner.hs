module Http.HttpRunner
  ( mkRunner,
    HttpRunner (..),
    Extended (..),
  )
where

import Data.Aeson (FromJSON (..), Value, Result (..), withObject, (.:))
import GHC.Exception (throw)
import Http.HttpEndpoint (mkRequest, callWebDriver')
import IOUtils (DemoActions (..))
import Network.HTTP.Req
  ( Scheme (..),
    Url,
  )
import UnliftIO (catchAny)
import WebDriverPreCore.Http.Command (Command (..))
import Prelude hiding (log)
import WebDriverPreCore.Http qualified as HTTP
import Data.Text (Text, unpack)
import Data.Aeson.Types (parse, Parser, Value (..))
import Data.Function ((&))
import Control.Monad (when)
import WebDriverPreCore.Internal.AesonUtils (jsonToText)
import Text.Show.Pretty (ppShow)

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
  callWebDriver' da $ mkRequest driverUrl port cmd


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
      da.logJSON "body" $ Object body
    PostEmpty {} -> pure ()
    Delete {} -> pure ()


parseResultIO :: forall r. (FromJSON r) => Bool -> Value -> Text -> IO r
parseResultIO expectNull body description =
  case parse (fromBodyValue expectNull) body of
    Error msg ->
      fail $
        HTTP.parseWebDriverError body & \case
          e@HTTP.ResponeParseError {} -> unpack description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> ppShow e
          e@HTTP.UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> ppShow e
          e@HTTP.WebDriverError {} -> "WebDriver error thrown:\n " <> ppShow e
    Success r -> pure r


fromBodyValue :: forall a. (FromJSON a) => Bool -> Value -> Parser a
fromBodyValue expectNull body =
  body & withObject "body value" \b -> do
    val <- b .: "value"
    when (expectNull && val /= Null) $
      fail $
        unpack $
          "Null value expected but got:\n" <> jsonToText val
    parseJSON $ val




