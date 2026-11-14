-- |
-- Module: WebDriver.Effectful.HTTP.ActionsNoWait
-- Description: Effectful-style HTTP WebDriver actions without implicit waiting
--
-- Compound actions that locate an element and act on it in a single step,
-- signalling failures through the 'Error' effect.
module WebDriver.Effectful.HTTP.ActionsNoWait where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Exception (catch, throwIO)
import Effectful.Reader.Static (Reader, ask)
import WebDriver.Effectful.HTTP.Base.Actions qualified as BA
import WebDriver.Effectful.HTTP.Base.Effect (WebDriver)
import WebDriver.Effectful.Logger.Effect (Logger, logDebug)
import WebDriverPreCore.Extended.Common.Locate (Locator)
import WebDriverPreCore.Extended.Common.NoWait (ActionException)
import WebDriverPreCore.Extended.Common.NoWait qualified as CNW
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, Selector, ShadowRootElementId, WindowRect)
import WebDriverPreCore.Extended.HTTP.Locate
  ( ExtendedRoleLocateSingleton (..),
    LocateActions (..),
    LocateOpts (..),
    SingletonCardinality (..),
  )
import WebDriverPreCore.Extended.HTTP.Locate qualified as HL
import WebDriverPreCore.Extended.HTTP.NoWait qualified as NW
import WebDriverPreCore.Extended.Locators (DisplayedCheck (..), LocateException, elmId)
import WebDriverPreCore.Utils.Utils (txt)

-- | Locate an element and click it, signalling any failure via 'Error'.
click2 :: (WebDriver :> es, Logger :> es, Error ActionException :> es) => Locator -> Eff es ()
click2 loc =
  NW.runAction mkLocateActions defHttpOpts (fmap Right . BA.elementClick) loc
    >>= either throwError pure

-- | Default locate options for no-wait actions.
defHttpOpts :: LocateOpts
defHttpOpts =
  MkLocateOpts
    { extendedRoleLocation = ExtLocateNever,
      jsRecheckDisplayed = DisplayedCheckAlways,
      singletonCardinality = First,
      mkDefaultLoc = elmId
    }

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

-- | Locate an element, returning it or the locate error.
locate :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => Locator -> Eff es (Either LocateException ElementId)
locate loc = do
  opts <- ask @LocateOpts
  HL.locate mkLocateActions opts loc

-- | Locate an element and run the given element action on it, signalling any failure via 'Error'.
elmRunner :: forall es r. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => (ElementId -> Eff es r) -> Locator -> Eff es r
elmRunner action loc = do
  ethRslt <- CNW.runElmAction elmActions (fmap Right . action) loc
  either throwError pure ethRslt
  where
    elmActions =
      CNW.MKElmActionHandles
        { locate,
          catch,
          trace = logDebug . txt
        }

-- | Locate an element and click it, signalling any failure via 'Error'.
click :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es ()
click = elmRunner BA.elementClick

-- | Locate an element and clear it, signalling any failure via 'Error'.
clear :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es ()
clear = elmRunner BA.elementClear

-- | Locate an element and send keys to it, signalling any failure via 'Error'.
sendKeys :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es ()
sendKeys loc keys = elmRunner (\el -> BA.elementSendKeys el keys) loc

-- | Locate an element and take a screenshot of it, signalling any failure via 'Error'.
screenshot :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
screenshot = elmRunner BA.takeElementScreenshot

-- | Locate an element and check whether it is selected, signalling any failure via 'Error'.
isSelected :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Bool
isSelected = elmRunner BA.isElementSelected

-- | Locate an element and get one of its attributes, signalling any failure via 'Error'.
getAttribute :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es (Maybe Text)
getAttribute loc name = elmRunner (\el -> BA.getElementAttribute el name) loc

-- | Locate an element and get one of its properties, signalling any failure via 'Error'.
getProperty :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es (Maybe Value)
getProperty loc name = elmRunner (\el -> BA.getElementProperty el name) loc

-- | Locate an element and get one of its CSS values, signalling any failure via 'Error'.
getCssValue :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es Text
getCssValue loc prop = elmRunner (\el -> BA.getElementCssValue el prop) loc

-- | Locate an element and get its shadow root, signalling any failure via 'Error'.
getShadowRoot :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es ShadowRootElementId
getShadowRoot = elmRunner BA.getElementShadowRoot

-- | Locate an element and get its text, signalling any failure via 'Error'.
getText :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getText = elmRunner BA.getElementText

-- | Locate an element and get its tag name, signalling any failure via 'Error'.
getTagName :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getTagName = elmRunner BA.getElementTagName

-- | Locate an element and get its bounding rectangle, signalling any failure via 'Error'.
getRect :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es WindowRect
getRect = elmRunner BA.getElementRect

-- | Locate an element and check whether it is enabled, signalling any failure via 'Error'.
isEnabled :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Bool
isEnabled = elmRunner BA.isElementEnabled

-- | Locate an element and get its computed role, signalling any failure via 'Error'.
getComputedRole :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getComputedRole = elmRunner BA.getElementComputedRole

-- | Locate an element and get its computed label, signalling any failure via 'Error'.
getComputedLabel :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getComputedLabel = elmRunner BA.getElementComputedLabel

-- | Locate an element and find a descendant matching the selector, signalling any failure via 'Error'.
findElementFrom :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Selector -> Eff es ElementId
findElementFrom loc sel = elmRunner (\el -> BA.findElementFromElement el sel) loc

-- | Locate an element and find all descendants matching the selector, signalling any failure via 'Error'.
findElementsFrom :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Selector -> Eff es [ElementId]
findElementsFrom loc sel = elmRunner (\el -> BA.findElementsFromElement el sel) loc
