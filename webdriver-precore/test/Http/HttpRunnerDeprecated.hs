module Http.HttpRunnerDeprecated
  ( run,
    mkRunner,
    HttpRunnerDeprecated (..)
  )
where

import Const (ReqRequestParams (..))
import Data.Aeson (object)
import IOUtils (DemoActions (..))
import Network.HTTP.Req (Scheme (Http), Url)
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
  )
import WebDriverPreCore.Http
  ( WebDriverException (..),
    HttpSpec (..),
    parseWebDriverException,
  )
import WebDriverPreCore.Http qualified as W
import Prelude hiding (log)
import Http.HttpEndpoint (callWebDriver, fullCommandPath)
import Data.Aeson (Result(..))
import Data.Text (unpack)
import Data.Function ((&))
import WebDriverPreCore.Internal.Utils (UrlPath(..))

-- ############# Runner #############

newtype HttpRunnerDeprecated = MkHttpRunnerDeprecated {run :: forall r. (Show r) => HttpSpec r -> IO r}

mkRunner :: Url 'Http -> Int -> DemoActions -> HttpRunnerDeprecated
mkRunner driverUrl port da = MkHttpRunnerDeprecated $ run driverUrl port da

run :: forall r. (Show r) => Url 'Http -> Int -> DemoActions -> HttpSpec r -> IO r
run url port da spec = do
  logSpec da spec
  callWebDriver da (mkRequest url port spec) >>= parseIO spec

logSpec :: (Show a) => DemoActions -> HttpSpec a -> IO ()
logSpec MkDemoActions {logTxt, logShow, logJSON} spec = do
  logTxt "Request"
  logShow "HttpSpec" spec
  case spec of
    Get {} -> pure ()
    Post {body} -> do
      logJSON "body PP" body
    PostEmpty {} -> pure ()
    Delete {} -> pure ()

mkRequest :: forall r. Url 'Http -> Int -> HttpSpec r -> ReqRequestParams
mkRequest driverUrl port spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody port
  Post {body} -> MkRequestParams url POST (ReqBodyJson body) port
  PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) port
  Delete {} -> MkRequestParams url DELETE NoReqBody port
  where
    url = fullCommandPath driverUrl spec.path.segments


parseIO :: HttpSpec a -> W.HttpResponse -> IO a
parseIO spec r =
  spec.parser r
    & \case
      Error msg ->
        fail $
          parseWebDriverException r.body & \case
            e@ResponeParseException {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
            e@UnrecognisedException {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
            e@ProtocolException {} -> "WebDriver Protocol Error thrown:\n " <> show e
      Success a -> pure a

