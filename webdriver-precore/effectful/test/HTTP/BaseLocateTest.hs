module HTTP.BaseLocateTest where


import Effectful (Eff, IOE, (:>), Effect)
import HTTP.Runner (runHttpTest, testUrl)
import WebDriver.Effectful.Logger (Logger, log)
import WebDriver.Effectful
  ( 
    Pause,
    WebDriverHttp,
    pause,
  )
import Prelude hiding (log)




