module Main where

import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy.Encoding qualified as TLE
import Network.HTTP.Types.Status
import TestPages (helloHtml)
import Web.Scotty
import Prelude

main :: IO ()
main = do
  putStrLn "Serving on http://localhost:8000 ..."
  putStrLn "  - / (root)"
  putStrLn "  - /authtest"
  putStrLn "  - /malformed-response"
  scotty 8000 $ do
    get "/" $ do
      html helloHtml

    get "/boringHello" $ do
      html "<html><body><h1>Hello</h1></body></html>"

    get "/boringHello2" $ do
      html "<html><body><h1>Hello Again</h1></body></html>"

    get "/authtest" $ do
      hdr <- header "Authorization"
      case hdr of
        Nothing -> do
          status unauthorized401
          setHeader "WWW-Authenticate" "Basic realm=\"Test Realm\""
          text "Auth required."
        Just h ->
          case parseBasicAuth (BL.toStrict $ TLE.encodeUtf8 h) of
            Just (user, pass)
              | user == "test" && pass == "secret" -> text "Authenticated!"
              | otherwise -> do
                  status forbidden403
                  text "Forbidden."
            Nothing -> do
              status unauthorized401
              setHeader "WWW-Authenticate" "Basic realm=\"Test Realm\""
              text "Invalid Authorization header."

    get "/malformed-response" $ do
      -- Send invalid chunked encoding or abruptly close connection
        setHeader "Content-Length" "99999"  -- Promise 99999 bytes
        text "short"  -- But only send 5 bytes, then connection closes

-- Parse Basic authorization header: "Basic base64(user:pass)"
parseBasicAuth :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseBasicAuth authHeader =
  if "Basic " `BS.isPrefixOf` authHeader
    then
      let encoded = BS.drop 6 authHeader
       in case B64.decode encoded of
            Right decoded ->
              case BS.break (== ':') decoded of
                (u, p) | not (BS.null p) -> Just (u, BS.drop 1 p)
                _ -> Nothing
            Left _ -> Nothing
    else Nothing
