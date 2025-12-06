module Http.HttpEndpoint
  ( callWebDriver,
    mkRequest,
    -- share with deprecated runner
    fullCommandPath,
    responseStatusText,
    callWebDriver'
  )
where

import Const (ReqRequestParams (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object)
import Data.Foldable qualified as F
import Data.Text as T (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import IOUtils (DemoActions (..))
import Network.HTTP.Req
  ( DELETE (DELETE),
    GET (GET),
    HttpConfig (httpConfigCheckResponse),
    JsonResponse,
    NoReqBody (NoReqBody),
    POST (POST),
    Req,
    ReqBodyJson (ReqBodyJson),
    Scheme (..),
    Url,
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
import WebDriverPreCore.Http.Protocol (Command (..))
import WebDriverPreCore.Internal.Utils (UrlPath (..))
import Prelude hiding (log)
import WebDriverPreCore.Http (HttpResponse)
import WebDriverPreCore.Http.HttpResponse (HttpResponse(..))

-- ############# Runner #############

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

callWebDriver' :: DemoActions -> ReqRequestParams -> IO Value
callWebDriver' MkDemoActions {logShow = logShow', logJSON = logJSON'} MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    logShow "URL" url
    r <- req method url body jsonResponse $ R.port prt

    let 
      body' = responseBody r :: Value


    logShow "Status Code" $ responseStatusCode r
    logShow "Status Message" $ responseStatusText r
    logJSON "Response Body" body'

    pure body'
  where
    logShow :: (Show a) => Text -> a -> Req ()
    logShow msg = liftIO . logShow' msg
    logJSON msg = liftIO . logJSON' msg


callWebDriver :: DemoActions -> ReqRequestParams -> IO HttpResponse
callWebDriver MkDemoActions {logShow = logShow', logJSON = logJSON'} MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    logShow "URL" url
    r <- req method url body jsonResponse $ R.port prt

    let 
      body' = responseBody r :: Value
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
