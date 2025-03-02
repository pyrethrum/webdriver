{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}


module IORunnerMinimal ( run ) where

import Data.Aeson (Result (..), Value, object)

import Data.Function ((&), ($), (.))
import Data.Text  as T (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    JsonResponse,
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    responseBody,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    HttpConfig (httpConfigCheckResponse), (/:), HttpBodyAllowed, HttpMethod (..), ProvidesBody, HttpBody, Url, Scheme (..),
  )
import WebDriverPreCore.Spec (
  HttpResponse (..),
   W3Spec (..), 
   parseWebDriverError, 
   ErrorClassification (..),
   UrlPath (..) )
import GHC.IO (IO)
import Data.Int (Int)
import Control.Monad (Monad(..))
import Data.Foldable (foldl')
import GHC.Maybe (Maybe(..))
import Control.Applicative (Applicative(..))
import Control.Monad.Fail (MonadFail(..))
import Data.Monoid ((<>))
import GHC.Show (Show(..))

{-
# Minimal WebDriver IO Runner

`webdriver-w3c-typed-endpoints` does not provide any implementation to drive a browser. 

This is a minimal example of a \runner\ that implements the interaction with WebDriver endpoint definitions as provided by this library.

To \run\ a W3Spec, requires the following:

1. Chose a library to make the HTTP request to the WebDriver server. In this case, we use `req`.
2. Define a function to convert a `W3Spec` to a `RequestParams` (such as url, port and body) that can be used by the chosen library.
3. Call the WebDriver server with the `ReqRequestParams` and construct the result in the form of a simplified `HttpResponse`.
4. Use the parser provided by the `W3Spec` to transform the `HttpResponse` to the desired result type and handle any errors.

-}
run :: W3Spec a -> IO a
run spec = 
  -- req chosen               -- 1. HTTP library chosen is req
  mkRequest spec              -- 2. Convert W3Spec to params for req
    & callReq                 -- 3. Call WebDriver server (via req) and return a simplified HttpResponse
    >>= parseResponse spec    -- 4. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors


data ReqRequestParams where
  MkRequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { url :: Url 'Http,
      method :: method,
      body :: body,
      port :: Int
    } ->
    ReqRequestParams
  
-- 2. Define a function to convert a `W3Spec` to a `RequestParams`
mkRequest :: forall a. W3Spec a -> ReqRequestParams
mkRequest spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody 4444
  Post {body} -> MkRequestParams url POST (ReqBodyJson body) 4444
  PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) 4444
  Delete {} -> MkRequestParams url DELETE NoReqBody 4444
  where 
    url =  foldl' (/:) (http "127.0.0.1") spec.path.segments

-- 3. Call the WebDriver server with the `ReqRequestParams` and return the result in the form of a simplified `HttpResponse` 
callReq :: ReqRequestParams -> IO HttpResponse
callReq MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig  {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    r <- req method url body jsonResponse $ port prt 
    pure $ MkHttpResponse
            { statusCode = responseStatusCode r,
              statusMessage = responseStatusText r,
              body = responseBody r :: Value
            }


-- 4. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors (in this case just throwing an exception)
parseResponse :: W3Spec a -> HttpResponse -> IO a
parseResponse spec r =
  spec.parser r
    & \case
      Error msg -> fail $ parseWebDriverError r & \case
          e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
          e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
          e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a


responseStatusText :: R.JsonResponse Value -> Text
responseStatusText = decodeUtf8Lenient . responseStatusMessage



{-
The following assumes that an appropriate browser and WebDriver have been installed, and the WebDriver has been started.

e.g. For Firefox and geckodriver on Linux or WSL:
```bash
pkill -f geckodriver || true  && geckodriver --log trace &

-}