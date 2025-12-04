module Const
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


-- ################### time ##################

newtype Timeout = MkTimeout {microseconds :: Int}
  deriving (Show, Eq)
  deriving newtype (Num)

millisecond :: Timeout
millisecond = MkTimeout 1_000

milliseconds :: Timeout
milliseconds = millisecond

second :: Timeout
second = 1_000 * milliseconds

seconds :: Timeout
seconds = second

minute :: Timeout
minute = 60 * seconds

minutes :: Timeout
minutes = minute

hour :: Timeout
hour = 60 * minutes

hours :: Timeout
hours = hour

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
