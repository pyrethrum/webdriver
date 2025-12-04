module WebDriverPreCore.BiDi.BiDiUrl where

import Data.Function ((&))
import Data.Text (Text, breakOn, pack, splitOn, unpack)
import Text.Read (readEither)
import WebDriverPreCore.Http.Protocol (SessionResponse(..))
import WebDriverPreCore.Internal.Utils (txt)

getBiDiUrl :: SessionResponse -> Either Text BiDiUrl
getBiDiUrl r =
  r.webSocketUrl
    & maybe
      (Left $ "WebSocket URL not provided in session response:\n" <> txt r)
      parseUrl

parseUrl :: Text -> Either Text BiDiUrl
parseUrl url = do
  (_scheme, hostPortPath) <- splitScheme
  (host, portPath) <- splitHost hostPortPath
  (port, path) <- portAndPath portPath
  Right $
    MkBiDiUrl
      { host,
        port,
        path
      }
  where
    -- "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"
    -- -> [ws, //127.0.0.1, 9222/session/e43698d9-b02a-4284-a936-12041deb3552]
    splitScheme =
      splitOn "://" url
        & \case
          [scheme, hostPortPath] -> Right (scheme, hostPortPath)
          _ -> failParser "Expected format: ws://host:port/path"

    splitHost hostPortPath =
      splitOn ":" hostPortPath
        & \case
          [host, portPath] -> Right (host, portPath)
          _ -> failParser "Expected format: host:port/path"

    -- 9222/session/e43698d9-b02a-4284-a936-12041deb3552
    -- -> (9222, session/e43698d9-b02a-4284-a936-12041deb3552)
    portAndPath :: Text -> Either Text (Int, Text)
    portAndPath pp =
      (,path) <$> portEth
      where
        (portTxt, path) = breakOn "/" pp
        portEth = case readEither $ unpack portTxt of
          Left msg ->
            failParser $
              "Could not extract port (an Int) from pBiDi.BiDiRunnerrefix of: "
                <> portTxt
                <> "\n"
                <> "Error on read Int: "
                <> pack msg
          Right p -> Right p

    failParser :: forall a. Text -> Either Text a
    failParser msg = Left $ "Failed to parse URL: " <> url <> "\n" <> msg

-- | WebDriver BiDi client configuration
data BiDiUrl = MkBiDiUrl
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show)