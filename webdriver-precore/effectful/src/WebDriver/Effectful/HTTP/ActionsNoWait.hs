
module WebDriver.Effectful.HTTP.ActionsNoWait where

import WebDriverPreCore.Extended.Common.NoWait
import WebDriverPreCore.Extended.HTTP.Locate
import WebDriver.Effectful.HTTP.Base.Actions qualified as BA
import WebDriver.Effectful.HTTP.Base.Effect (WebDriver)
import Effectful ((:>), Eff)
import WebDriverPreCore.HTTP.Protocol (ElementId)
import WebDriver.Effectful.Logger.Effect (Logger)
import WebDriverPreCore.Extended.Common.Locate (Locator)
import Effectful.Error.Dynamic (Error)




click :: (WebDriver :> es, Logger :> es, Error ??? :> es) => Locator -> Eff es ()
click = undefined




