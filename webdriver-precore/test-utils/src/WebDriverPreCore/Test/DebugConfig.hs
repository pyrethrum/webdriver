module WebDriverPreCore.Test.DebugConfig
  ( debugConfig,
  )
where

import WebDriverPreCore.Test.Config
  ( Config (..),
    DemoBrowser (..),
  )
import Data.Text (Text)

debugConfig :: Config
debugConfig = 
  --
  _firefoxSilent
  -- _chromeSilent
  -- _firefoxHeadlessLogging
  -- _chromeHeadlessLogging
  -- _firefoxDebug
  -- _chromeDebug

_firefoxSilent :: Config
_firefoxSilent =
  MkConfig
    { browser = Firefox {headless = True, profilePath},
      logging = False,
      httpUrl = "127.0.0.1",
      httpPort = 4444,
      pauseMS = 0
    }

_firefoxHeadlessLogging :: Config
_firefoxHeadlessLogging =
  MkConfig
    { browser = Firefox {headless = True, profilePath},
      logging = True,
      httpUrl = "127.0.0.1",
      httpPort = 4444,
      pauseMS = 0
    }

_firefoxDebug :: Config
_firefoxDebug =
  MkConfig
    { browser = Firefox {headless = False, profilePath},
      logging = True,
      httpUrl = "127.0.0.1",
      httpPort = 4444,
      pauseMS = 0
    }

_chromeSilent :: Config
_chromeSilent =
  MkConfig
    { browser = Chrome {headless = True},
      logging = False,
      httpUrl = "127.0.0.1",
      httpPort = 4444,
      pauseMS = 0
    }

_chromeHeadlessLogging :: Config
_chromeHeadlessLogging =
  MkConfig
    { browser = Chrome {headless = True},
      logging = True,
      httpUrl = "127.0.0.1",
      httpPort = 4444,
      pauseMS = 0
    }

_chromeDebug :: Config
_chromeDebug =
  MkConfig
    { browser = Chrome {headless = False},
      logging = True,
      httpUrl = "127.0.0.1",
      httpPort = 4444,
      pauseMS = 0
    }

profilePath :: Maybe Text
profilePath = Just "/home/john-walker/test-firefox-profile"

