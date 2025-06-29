module Http.HttpRunner
  ( run,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (..), Value, object)
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Text as T (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Const (ReqRequestParams (..))
import Network.HTTP.Req (JsonResponse, Req)
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    HttpConfig (httpConfigCheckResponse),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    responseBody,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    (/:),
  )
import WebDriverPreCore.Http
  ( ErrorClassification (..),
    HttpResponse (..),
    HttpSpec (..),
    parseWebDriverError,
  )
import WebDriverPreCore.Http qualified as W
import WebDriverPreCore.Internal.AesonUtils (prettyPrintJson)
import Prelude hiding (log)
import IOUtils qualified as U 
import Config (loadConfig, Config (..))


-- ############# Runner #############

run :: (Show a) => HttpSpec a -> IO a
run spec = do
  cfg <- loadConfig
  let wantLog = cfg.wantConsoleLogging
  when wantLog $
   logSpec spec
  callWebDriver wantLog (mkRequest spec) >>= parseIO spec

logSpec :: (Show a) => HttpSpec a -> IO ()
logSpec spec = do
  U.logTxt "Request"
  U.logShow "HttpSpec" spec
  case spec of
    Get {} -> pure ()
    Post {body} -> do
      U.logTxt "body PP"
      prettyPrintJson body
    PostEmpty {} -> pure ()
    Delete {} -> pure ()

mkRequest :: forall a. HttpSpec a -> ReqRequestParams
mkRequest spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody port'
  Post {body} -> MkRequestParams url POST (ReqBodyJson body) port'
  PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) port'
  Delete {} -> MkRequestParams url DELETE NoReqBody port'
  where
    url = F.foldl' (/:) (http "127.0.0.1") spec.path.segments
    port' = 4444 -- firefox

parseIO :: HttpSpec a -> W.HttpResponse -> IO a
parseIO spec r =
  spec.parser r
    & \case
      Error msg ->
        fail $
          parseWebDriverError r & \case
            e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
            e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
            e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a

callWebDriver :: Bool -> ReqRequestParams -> IO HttpResponse
callWebDriver wantLog MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    logShow "URL" url
    r <- req method url body jsonResponse $ port prt

    let fr =
          MkHttpResponse
            { statusCode = responseStatusCode r,
              statusMessage = responseStatusText r,
              body = responseBody r :: Value
            }

    logShow "Status Code" fr.statusCode
    logShow "Status Message" fr.statusMessage
    log "Response Body"
    logJSON fr.body
    logShow "Framework Response Object"  fr

    pure fr
  where
    liftLog = liftIO . when wantLog
    log = liftLog . U.logTxt 
    logShow :: Show a => Text -> a -> Req ()
    logShow message = liftLog . U.logShow message
    logJSON = liftLog . prettyPrintJson

-- ############# Utils #############

responseStatusText :: Network.HTTP.Req.JsonResponse Value -> Text
responseStatusText = decodeUtf8Lenient . responseStatusMessage


