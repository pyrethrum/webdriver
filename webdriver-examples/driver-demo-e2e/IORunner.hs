module IORunner
  ( 
    run
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (..), Value, object)

import Data.Function ((&))
import Data.Text  as T (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.IO qualified as T
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (Http),
    Url,
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
import WebDriverPreCore.Internal.Utils (txt, prettyPrintJson)
import E2EConst (RequestArgs (..))
import WebDriverPreCore.Spec (HttpResponse (..), W3Spec (..), parseWebDriverError, ErrorClassification (..))
import WebDriverPreCore.Spec qualified as W
import Prelude hiding (log)
import Network.HTTP.Req (JsonResponse)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy qualified as LT

-- ############# Config #############

wantConsoleLogging :: Bool
wantConsoleLogging = True

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
        T.putStrLn ( LT.toStrict $ encodeToLazyText body)
      PostEmpty {} -> pure ()
      Delete {} -> pure ()
  callWebDriver wantConsoleLogging (mkRequest spec) >>= parseIO spec

mkRequest :: forall a. W3Spec a -> RequestArgs
mkRequest = \case
  Get {path} -> RequestParams path GET NoReqBody 4444
  Post {path, body} -> RequestParams path POST (ReqBodyJson body) 4444
  PostEmpty {path} -> RequestParams path POST (ReqBodyJson $ object []) 4444
  Delete {path} -> RequestParams path DELETE NoReqBody 4444

parseIO :: W3Spec a -> W.HttpResponse -> IO a
parseIO spec r =
  spec.parser r
    & \case
      Error msg -> fail $ parseWebDriverError r & \case
          e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
          e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
          e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a


callWebDriver :: Bool -> RequestArgs -> IO HttpResponse
callWebDriver wantLog RequestParams {path, method, body, port = prt} =
  runReq defaultHttpConfig  {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    log $ "URL: " <> txt url
    r <- req method url body jsonResponse $ port prt 
    log $ "JSON Response:\n" <> txt r
    let fr =
          Response
            { statusCode = responseStatusCode r,
              statusMessage = responseStatusText r,
              body = responseBody r :: Value
            }
    log $ "Framework Response:\n" <> txt fr
    pure fr
  where
    log m = liftIO $ when wantLog $ devLog m
    url :: Url 'Http
    url =  foldl' (/:) (http "127.0.0.1") path.segments

-- ############# Utils #############

responseStatusText :: Network.HTTP.Req.JsonResponse Value -> Text
responseStatusText = decodeUtf8Lenient . responseStatusMessage

devLog :: Text -> IO ()
devLog = T.putStrLn

