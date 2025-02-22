{-# LANGUAGE UndecidableInstances #-}

module WebDriverPure
  ( RequestArgs (..),
    defaultRequest,
    jsonToText,
    prettyPrintJson,
    parseJson,
    second,
    seconds,  
    minute, 
    minutes,
    hour,
    hours
  )
where

-- import Effectful.Reader.Dynamic

import Data.Aeson
import Network.HTTP.Req as R
  ( GET (GET),
    HttpBody,
    HttpBodyAllowed,
    HttpMethod (AllowsBody),
    NoReqBody (NoReqBody),
    ProvidesBody,
  )

import Prelude
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as T
import Data.Text (Text)
import Control.Exception (try, Exception (displayException), SomeException)

{- Pure types and functions used in Webdriver -}


second :: Int
second = 1_000

seconds :: Int
seconds = second

minute :: Int
minute = 60 * seconds

minutes :: Int
minutes = minute

hour :: Int
hour = 60 * minutes

hours :: Int
hours = hour

-- todo stand alone instance of Show
data RequestArgs where
  RequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { subDirs :: [Text],
      method :: method,
      body :: body,
      port :: Int
    } ->
    RequestArgs
  

defaultRequest :: RequestArgs
defaultRequest = RequestParams [] GET NoReqBody 4444

-- Todo move to separte library
-- Aeson stuff to help debugging
-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
lsbToText :: LBS.ByteString -> Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> Text
jsonToText = lsbToText . encodePretty


prettyPrintJson :: Value -> IO ()
prettyPrintJson v = do
  e <- (try @SomeException @_)$ T.putStrLn (jsonToText v)
  either (print . displayException) print e
  
parseJson :: Text -> Either String Value
parseJson input =
  eitherDecodeStrict (E.encodeUtf8 input)



