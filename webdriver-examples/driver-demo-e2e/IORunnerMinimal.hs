module IORunnerMinimal ( run ) where

import Data.Aeson (Result (..), Value, object)

import Data.Function ((&))
import Data.Text  as T (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
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
    HttpConfig (httpConfigCheckResponse), (/:),
  )
import E2EConst (RequestArgs (..))
import WebDriverPreCore.Spec (HttpResponse (..), W3Spec (..), parseWebDriverError, ErrorClassification (..))
import WebDriverPreCore.Spec qualified as W
import Prelude hiding (log)
import Network.HTTP.Req (JsonResponse)

run :: W3Spec a -> IO a
run spec = callWebDriver (mkRequest spec) >>= applyParser spec

mkRequest :: forall a. W3Spec a -> RequestArgs
mkRequest = \case
  Get {path} -> RequestParams path GET NoReqBody 4444
  Post {path, body} -> RequestParams path POST (ReqBodyJson body) 4444
  PostEmpty {path} -> RequestParams path POST (ReqBodyJson $ object []) 4444
  Delete {path} -> RequestParams path DELETE NoReqBody 4444

applyParser :: W3Spec a -> W.HttpResponse -> IO a
applyParser spec r =
  spec.parser r
    & \case
      Error msg -> fail $ parseWebDriverError r & \case
          e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
          e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
          e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a


responseStatusText :: Network.HTTP.Req.JsonResponse Value -> Text
responseStatusText = decodeUtf8Lenient . responseStatusMessage

callWebDriver :: RequestArgs -> IO HttpResponse
callWebDriver RequestParams {path, method, body, port = prt} =
  runReq defaultHttpConfig  {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    let url =  foldl' (/:) (http "127.0.0.1") path.segments
    r <- req method url body jsonResponse $ port prt 
    pure $ Response
            { statusCode = responseStatusCode r,
              statusMessage = responseStatusText r,
              body = responseBody r :: Value
            }
