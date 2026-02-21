module WebDriverPreCore.Test.Const
  ( ReqRequestParams (..),
    Timeout (..),
    second,
    seconds,
    minute,
    minutes,
    hour,
    hours,
    defaultRequest,
    millisecond,
    milliseconds,
  )
where

import Network.HTTP.Req as R
  ( GET (GET),
    HttpBody,
    HttpBodyAllowed,
    HttpMethod (AllowsBody),
    NoReqBody (NoReqBody),
    ProvidesBody,
    Scheme (..),
    Url,
    http,
  )
import WebDriverPreCore.Utils.Timeout
  ( Timeout (..),
    hour,
    hours,
    millisecond,
    milliseconds,
    minute,
    minutes,
    second,
    seconds,
  )


-- ################### request ##################

data ReqRequestParams where
  MkRequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { url :: Url 'Http,
      method :: method,
      body :: body,
      port :: Int
    } ->
    ReqRequestParams

defaultRequest :: ReqRequestParams
defaultRequest =
  MkRequestParams
    { url = http "127.0.0.1",
      method = GET,
      body = NoReqBody,
      port = 4444
    }
