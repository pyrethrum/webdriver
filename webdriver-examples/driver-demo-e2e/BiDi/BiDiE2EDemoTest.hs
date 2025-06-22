module BiDi.BiDiE2EDemoTest where

import BiDi.BiDiRunner (newHttpSession)
-- custom import needed to disambiguate capabilities

import Config (Config, loadConfig)
import Control.Exception (finally)
import E2EConst (httpCapabilities, httpFullCapabilities)
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
