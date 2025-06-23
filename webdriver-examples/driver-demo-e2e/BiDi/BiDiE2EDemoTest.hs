module BiDi.BiDiE2EDemoTest where

import BiDi.BiDiRunner (newHttpSession)

import Config (Config, loadConfig)
import Control.Exception (finally)
import E2EConst (httpCapabilities, httpFullCapabilities)
-- custom import needed to disambiguate capabilities
import Http.HttpAPI qualified as Caps (Capabilities (..))
import Http.HttpAPI qualified as Http
import IOUtils (logShow)
import Prelude hiding (putStrLn)

-- WORK IN PROGRESS

-- Get the initial BiDi connection by making an HTTP request
-- with webSocketUrl set to True
httpBidiCapabilities :: Config -> Http.FullCapabilities
httpBidiCapabilities cfg =
  (httpFullCapabilities cfg)
    { Http.alwaysMatch =
        Just $
          (httpCapabilities cfg)
            { Caps.webSocketUrl = Just True
            }
    }

-- >>> unit_demoNewSessionViaHttp
unit_demoNewSessionViaHttp :: IO ()
unit_demoNewSessionViaHttp = do
  cfg <- loadConfig
  ses <- Http.newSessionFull $ httpBidiCapabilities cfg
  finally
    (logShow "new session response:\n" ses)
    (Http.deleteSession ses.sessionId)


parseUrl :: Text -> Either Text WebDriverBiDiConfig
parseUrl url = do
  (scheme, host, portPath) <- splitCol url
  (port, path) <- portAndPath portPath
  Right $ WebDriverBiDiConfig { 
            host = scheme <> ":" <> host, 
            port = port, 
            path = path }
  where 
    -- "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"
    -- -> [ws, //127.0.0.1, 9222/session/e43698d9-b02a-4284-a936-12041deb3552]
    splitCol = T.splitOn ":" url
               & \case 
                     [scheme, host, portPath] -> Right (scheme, host, portPath)
                     _ -> failParser "Expected format: ws://host:port/path"
    -- 9222/session/e43698d9-b02a-4284-a936-12041deb3552
    -- -> (9222, /session/e43698d9-b02a-4284-a936-12041deb3552)
    portAndPath :: Text -> Either Text (Int, Text)
    portAndPath pp = 
      (,path) <$> portEth
      where 
        (portTxt, path) = T.breakOn "/" pp
        portEth = case readEither $ unpack portTxt of
                 Left msg -> failParser $ "Could not extract port (an Int) from prefix of: " 
                                  <> portTxt
                                  <> "\n"
                                  <> "Error on read Int: " <> msg
                 Right p -> Right p
          
    failParser :: forall a. Text -> Either Text a
    failParser msg = Left $ "Failed to parse URL: " <> T.unpack url <> "\n" <> T.unpack msg


unit_bidiSession :: IO ()
unit_bidiSession = do
  cfg <- loadConfig
  ses <- newHttpSession $ httpBidiCapabilities cfg
  let sesTxt = prettyPack ses
      wsUrl = fromMaybe (error $ "WebSocket URL not provided in session response\n" <> sesTxt) ses.webSocketUrl
  log "new session response:\n" ses
  let wsUrl = case ses.webSocketUrl of
    Just url -> url
    Nothing -> (error "WebSocket URL not provided in session response")

  -- let bidiCOnfig = WebDriverBiDiConfig
  --       { host = cfg.host,
  --         port = cfg.port,
  --         path = "/session/" <> ses.sessionId <> "/bidi"
  --       }
  uu
