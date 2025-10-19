module Main where

import Prelude
import Web.Scotty
import Network.HTTP.Types.Status
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import TestPages (helloHtml)

main :: IO ()
main = do
  putStrLn "Serving on http://localhost:8000 ..."
  putStrLn "  - / (root)"
  putStrLn "  - /authtest"
  scotty 8000 $ do
    get "/" $ do
      html helloHtml
    
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
