module IORunner
  ( run,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (..), Value, object)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function ((&))
import Data.Text as T (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import E2EConst (ReqRequestParams (..))
import Network.HTTP.Req (JsonResponse)
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
import WebDriverPreCore.Http.Internal.Utils (prettyPrintJson, txt)
import WebDriverPreCore
  ( ErrorClassification (..),
    HttpResponse (..),
    W3Spec (..),
    parseWebDriverError,
  )
import WebDriverPreCore.Http qualified as W
import Prelude hiding (log)

-- ############# Config #############

wantConsoleLogging :: Bool
wantConsoleLogging = False

-- ############# Runner #############

run :: (Show a) => W3Spec a -> IO a
run spec = do
  when wantConsoleLogging $ do
    devLog "Request"
    devLog . txt $ spec
    case spec of
      Get {} -> pure ()
      Post {body} -> do
        devLog "body PP"
        prettyPrintJson body
        devLog "Body Raw"
        T.putStrLn (LT.toStrict $ encodeToLazyText body)
      PostEmpty {} -> pure ()
      Delete {} -> pure ()
  callWebDriver wantConsoleLogging (mkRequest spec) >>= parseIO spec

mkRequest :: forall a. W3Spec a -> ReqRequestParams
mkRequest spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody port'
  Post {body} -> MkRequestParams url POST (ReqBodyJson body) port'
  PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) port'
  Delete {} -> MkRequestParams url DELETE NoReqBody port'
  where
    url = foldl' (/:) (http "127.0.0.1") spec.path.segments
    port' = 4444 -- firefox


parseIO :: W3Spec a -> W.HttpResponse -> IO a
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
    log $ "URL: " <> txt url
    r <- req method url body jsonResponse $ port prt
    log $ "JSON Response:\n" <> txt r
    let fr =
          MkHttpResponse
            { statusCode = responseStatusCode r,
              statusMessage = responseStatusText r,
              body = responseBody r :: Value
            }
    log $ "Framework Response:\n" <> txt fr
    pure fr
  where
    log m = liftIO $ when wantLog $ devLog m

-- ############# Utils #############

responseStatusText :: Network.HTTP.Req.JsonResponse Value -> Text
responseStatusText = decodeUtf8Lenient . responseStatusMessage

devLog :: Text -> IO ()
devLog = T.putStrLn
