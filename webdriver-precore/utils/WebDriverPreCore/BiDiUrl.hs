module WebDriverPreCore.BiDiUrl
  ( BiDiUrl(..),
    parseBiDiUrl,
    SocketConnectionException(..),
    parseBiDiUrlProperty
  )
where
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)
import UnliftIO (Exception)

-- | BiDi WebSocket URL components
data BiDiUrl = MkBiDiUrl
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show, Eq)


-- | Parse a WebSocket URL into BiDi components
-- Example: "ws://127.0.0.1:9222/session/abc123"
parseBiDiUrl :: Text -> Maybe BiDiUrl
parseBiDiUrl url = do
  -- Strip ws:// prefix
  rest <- T.stripPrefix "ws://" url
  -- Split host:port from path
  let (hostPort, pathWithSlash) = T.break (== '/') rest
      path = if T.null pathWithSlash then "/" else pathWithSlash
  -- Split host from port
  case T.break (== ':') hostPort of
    (host, portStr) -> do
      port <- readMaybe . T.unpack =<< T.stripPrefix ":" portStr
      pure $ MkBiDiUrl {host, port, path}

data SocketConnectionException 
  = MkSocketConnectionException Text
  deriving (Show, Eq)

instance Exception SocketConnectionException

-- | Parse a BiDi WebSocket URL, throwing 'IOError' on failure.
parseBiDiUrlProperty :: Maybe Text -> Either SocketConnectionException BiDiUrl
parseBiDiUrlProperty = maybe
    (failConnection
        "withBiDiSession: driver did not return a WebSocket URL \
        \(set webSocketUrl = True in capabilities)")
    \t ->
    case parseBiDiUrl t of
      Nothing -> failConnection $ "withBiDiSession: could not parse WebSocket URL: " <> t
      Just u  -> pure u
    where 
      failConnection = Left . MkSocketConnectionException
