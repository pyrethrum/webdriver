
-- |
-- Module: WebDriver.Effectful.HTTP.ActionsNoWait
-- Description: Effectful-style HTTP WebDriver actions without implicit waiting
--
-- Compound actions that locate an element and act on it in a single step,
-- signalling failures through the 'Error' effect.
module WebDriver.Effectful.HTTP.ActionsNoWait where

import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Exception (catch, throwIO)
import WebDriver.Effectful.HTTP.Base.Actions qualified as BA
import WebDriver.Effectful.HTTP.Base.Effect (WebDriver)
import WebDriver.Effectful.Logger.Effect (Logger, logDebug)
import WebDriverPreCore.Extended.Common.Locate (Locator)
import WebDriverPreCore.Extended.HTTP.Locate qualified as HL
import WebDriverPreCore.Extended.Common.NoWait (ActionException)
import WebDriverPreCore.Extended.Common.NoWait qualified as CNW
import WebDriverPreCore.Extended.HTTP.Locate
  ( ExtendedRoleLocateSingleton (..),
    LocateActions (..),
    LocateOpts (..),
    SingletonCardinality (..),
  )
import WebDriverPreCore.Extended.HTTP.NoWait qualified as NW
import WebDriverPreCore.Extended.Locators (DisplayedCheck (..), elmId, LocateException)
import WebDriverPreCore.Utils.Utils (txt)
import Effectful.Labeled.Reader (Reader, ask)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId)

-- | Locate an element and click it, signalling any failure via 'Error'.
click2 :: (WebDriver :> es, Logger :> es, Error ActionException :> es) => Locator -> Eff es ()
click2 loc =
  NW.runAction mkLocateActions defHttpOpts (fmap Right . BA.elementClick) loc
    >>= either throwError pure

-- | Locate an element and click it, signalling any failure via 'Error'.
click :: (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es ()
click loc = do 
  opts <- ask
  ethVoid <- CNW.runElmAction (elmActions opts)(fmap Right . BA.elementClick) loc
  either throwError pure ethVoid
  where 
    elmActions opts = CNW.MKElmActionHandles
                          { locate = locate opts,
                            catch,
                            trace = logDebug . txt
                          }
    locate :: LocateOpts -> Locator -> Eff es (Either LocateException ElementId)               
    locate opts = HL.locate mkLocateActions opts

-- | Build HTTP 'LocateActions' from the 'WebDriver' and 'Logger' effects.
mkLocateActions :: (WebDriver :> es, Logger :> es) => LocateActions (Eff es)
mkLocateActions =
  MkLocateActions
    { throw = throwIO,
      catch,
      trace = logDebug . txt,
      findElement = BA.findElement,
      findElementFromElement = BA.findElementFromElement,
      findElements = BA.findElements,
      findElementsFromElement = BA.findElementsFromElement,
      executeScript = BA.executeScript,
      getElementAttribute = BA.getElementAttribute,
      getElementText = BA.getElementText
    }

-- | Default locate options for no-wait actions.
defHttpOpts :: LocateOpts
defHttpOpts =
  MkLocateOpts
    { extendedRoleLocation = ExtLocateNever,
      jsRecheckDisplayed = DisplayedCheckAlways,
      singletonCardinality = First,
      mkDefaultLoc = elmId
    }