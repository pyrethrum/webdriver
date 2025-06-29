module RuntimeConst
  ( 
    httpCapabilities,
    httpFullCapabilities
  )
where

import Config as CFG
import WebDriverPreCore.Http as WPC
  ( BrowserName (..),
    Capabilities (..),
    FullCapabilities (..),
    VendorSpecific (..),
  )
import Prelude as P
  ( Maybe (..),
    ($),
    (<>), 
    null
  )

-- "Constants" that depend Config so can't be resolved until runtime

-- ################### capabilities ##################

httpCapabilities :: Config -> Capabilities
httpCapabilities MkConfig {browser} =
  MkCapabilities
    { browserName = Just $ if (isFirefox browser) then WPC.Firefox else WPC.Chrome,
      browserVersion = Nothing,
      platformName = Nothing,
      acceptInsecureCerts = Nothing,
      pageLoadStrategy = Nothing,
      proxy = Nothing,
      setWindowRect = Nothing,
      timeouts = Nothing,
      strictFileInteractability = Nothing,
      unhandledPromptBehavior = Nothing,
      webSocketUrl = Nothing,
      vendorSpecific =
        case browser of
          CFG.Firefox {headless, profilePath} -> Just $  FirefoxOptions
                { firefoxArgs = if null allArgs then  Nothing else Just allArgs,
                  firefoxBinary = Nothing,
                  firefoxProfile = Nothing,
                  firefoxLog = Nothing
                }
              where 
                headlessArgs = if headless then  ["--headless"] else []
                profileArgs = case profilePath of
                  Just path -> ["--profile", path]
                  Nothing -> []
                allArgs = headlessArgs <> profileArgs

          CFG.Chrome -> Nothing
    }


httpFullCapabilities :: Config -> FullCapabilities
httpFullCapabilities cfg =
  MkFullCapabilities
    { alwaysMatch =
        Just $ httpCapabilities cfg,
      firstMatch = []
    }
