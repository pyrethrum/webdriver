module RuntimeConst
  ( 
    httpCapabilities,
    httpFullCapabilities
  )
where

import Config as CFG
import WebDriverPreCore.HTTP.Protocol as WPC
  ( BrowserName (..),
    Capabilities (..),
    FullCapabilities (..),
    UnhandledPromptBehavior (..),
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
      unhandledPromptBehavior = Just Ignore,
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

          CFG.Chrome {headless} -> Just $ ChromeOptions
                { chromeArgs = if headless then Just ["--headless=new"] else Nothing,
                  chromeBinary = Nothing,
                  chromeExtensions = Nothing,
                  chromeLocalState = Nothing,
                  chromeMobileEmulation = Nothing,
                  chromePrefs = Nothing,
                  chromeDetach = Nothing,
                  chromeDebuggerAddress = Nothing,
                  chromeExcludeSwitches = Nothing,
                  chromeMinidumpPath = Nothing,
                  chromePerfLoggingPrefs = Nothing,
                  chromeWindowTypes = Nothing
                }
    }
 


httpFullCapabilities :: Config -> FullCapabilities
httpFullCapabilities cfg =
  MkFullCapabilities
    { alwaysMatch =
        Just $ httpCapabilities cfg,
      firstMatch = []
    }
