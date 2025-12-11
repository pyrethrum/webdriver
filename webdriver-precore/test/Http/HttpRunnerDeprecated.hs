module Http.HttpRunnerDeprecated
  ( run,
    mkRunner,
    HttpRunnerDeprecated (..),
  )
where

import Const (ReqRequestParams (..))
import Data.Aeson (Result (..), object)
import Data.Function ((&))
import Http.HttpEndpoint (callWebDriver, fullCommandPath)
import IOUtils (DemoActions (..))
import Network.HTTP.Req (Scheme (Http), Url)
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
  )
import Utils (UrlPath (..))
import WebDriverPreCore.Error
import WebDriverPreCore.Http
  ( HttpSpec (..),
  )
import WebDriverPreCore.Http qualified as W
import Prelude hiding (log)
import UnliftIO (throwIO)

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
      Error _->
        throwIO $ parseWebDriverException r.body
      Success a -> pure a
