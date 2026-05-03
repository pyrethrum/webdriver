module HTTP.BaseLocateTest where


import Effectful (Eff, IOE, (:>), Effect)
import HTTP.Runner (runHttpTest, testUrl)
import WebDriver.Effectful
  ( Logger,
    Pause,
    WebDriverHttp,
    log,
    pause,
  )
import Prelude hiding (log)

