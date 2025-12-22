module HTTP.Runner
  ( mkRunner,
    HttpRunner (..),
    -- share with deprecated runner
    fullCommandPath,
    responseStatusText,
    callWebDriver
  )
where

import Data.Aeson (FromJSON (..), Value, withObject, (.:))
import Data.Aeson.Types (Parser, Value (..), object, parseEither, parseMaybe)
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

import Const (ReqRequestParams (..))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable qualified as F
import Data.Text as T (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.HTTP.Req
  ( DELETE (DELETE),
    GET (GET),
    HttpConfig (httpConfigCheckResponse),
    JsonResponse,
    NoReqBody (NoReqBody),
    POST (POST),
    Req,
    ReqBodyJson (ReqBodyJson),
    defaultHttpConfig,
    jsonResponse,
    req,
    responseBody,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    (/:),
  )
import Network.HTTP.Req qualified as R
import Utils (UrlPath (..))
import WebDriverPreCore.HTTP.HttpResponse (HttpResponse (..))


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
            (const . throw $ parseWebDriverException val)
            pure
      )
  

valueParser :: Value -> Parser Value
valueParser body = body & withObject "body value" (.: "value")

-- Http Interaction -----

mkRequest :: forall r. Url 'Http -> Int -> Command r -> ReqRequestParams
mkRequest driverUrl port cmd =
  case cmd of
    Get {} -> MkRequestParams url GET NoReqBody port
    Post {body} -> MkRequestParams url POST (ReqBodyJson body) port
    PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) port
    Delete {} -> MkRequestParams url DELETE NoReqBody port
  where
    url = fullCommandPath driverUrl cmd.path.segments

fullCommandPath :: Url 'Http -> [Text] -> Url 'Http
fullCommandPath basePath = F.foldl' (/:) basePath

-- call webdriver returning the body of the response as a JSON Value
callWebDriver' :: DemoActions -> ReqRequestParams -> IO Value
callWebDriver' MkDemoActions {logShow = logShow', logJSON = logJSON'} MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    logShow "URL" url
    r <- req method url body jsonResponse $ R.port prt

    let body' = responseBody r :: Value

    logShow "Status Code" $ responseStatusCode r
    logShow "Status Message" $ responseStatusText r
    logJSON "Response Body" body'

    pure body'
  where
    logShow :: (Show a) => Text -> a -> Req ()
    logShow msg = liftIO . logShow' msg
    logJSON msg = liftIO . logJSON' msg

-- call webdriver returning the full HttpResponse (kept for now to support deprecated runner)
callWebDriver :: DemoActions -> ReqRequestParams -> IO HttpResponse
callWebDriver MkDemoActions {logShow = logShow', logJSON = logJSON'} MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    logShow "URL" url
    r <- req method url body jsonResponse $ R.port prt

    let body' = responseBody r :: Value
        fr =
          MkHttpResponse
            { statusCode = responseStatusCode r,
              statusMessage = responseStatusText r,
              body = body'
            }

    logShow "Status Code" fr.statusCode
    logShow "Status Message" fr.statusMessage
    logJSON "Response Body" fr.body
    logShow "Framework Response Object" fr

    pure fr
  where
    logShow :: (Show a) => Text -> a -> Req ()
    logShow msg = liftIO . logShow' msg
    logJSON msg = liftIO . logJSON' msg

-- ############# Utils #############

responseStatusText :: JsonResponse Value -> Text
responseStatusText = decodeUtf8Lenient . responseStatusMessage
