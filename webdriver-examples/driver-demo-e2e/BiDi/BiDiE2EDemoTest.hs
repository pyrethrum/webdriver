module BiDi.BiDiE2EDemoTest where


import Config (loadConfig)
import Control.Exception (finally)
-- custom import needed to disambiguate capabilities
import Http.HttpAPI qualified as Http
import IOUtils (logShow)
import Prelude hiding (putStrLn, log)
import BiDi.BiDiRunner (httpBidiCapabilities)
import Data.Text (Text)
import WebDriverPreCore.Internal.Utils (txt)
import WebDriverPreCore.BiDi.BiDiPath (parseUrl)

-- WORK IN PROGRESS

-- >>> demo_parseUrl
-- "Right\n  MkBiDiPath\n    { host = \"127.0.0.1\"\n    , port = 9222\n    , path = \"/session/e43698d9-b02a-4284-a936-12041deb3552\"\n    }"
demo_parseUrl :: Text
demo_parseUrl = txt $ parseUrl "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"


-- >>> unit_newBidiSessionViaHttp
unit_newBidiSessionViaHttp :: IO ()
unit_newBidiSessionViaHttp = do
  cfg <- loadConfig
  ses <- Http.newSessionFull $ httpBidiCapabilities cfg
  finally
    (logShow "new session response:\n" ses)
    (Http.deleteSession ses.sessionId)


-- unit_bidiSession :: IO ()
-- unit_bidiSession = do
--   cfg <- loadConfig
--   ses <- newHttpSession $ httpBidiCapabilities cfg
--   let sesTxt = prettyPack ses
--       wsUrl = fromMaybe (error $ "WebSocket URL not provided in session response\n" <> sesTxt) ses.webSocketUrl
--   log "new session response:\n" ses
--   let wsUrl = case ses.webSocketUrl of
--     Just url -> url
--     Nothing -> (error "WebSocket URL not provided in session response")

  -- let bidiCOnfig = WebDriverBiDiConfig
  --       { host = cfg.host,
  --         port = cfg.port,
  --         path = "/session/" <> ses.sessionId <> "/bidi"
  --       }

