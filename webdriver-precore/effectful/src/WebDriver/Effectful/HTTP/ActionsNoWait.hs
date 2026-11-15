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
import WebDriver.Effectful.HTTP.Base.Actions
import WebDriver.Effectful.HTTP.Base.Effect (WebDriver)
import WebDriver.Effectful.Logger.Effect (Logger, logDebug)
import WebDriverPreCore.Extended.Common.Locate (Locator)
import WebDriverPreCore.Extended.Common.NoWait (ActionException)
import WebDriverPreCore.Extended.Common.NoWait qualified as CNW
import WebDriverPreCore.Extended.HTTP.Locate
  ( ExtendedRoleLocateSingleton (..),
    LocateActions (..),
    LocateOpts (..),
    SingletonCardinality (..),
  )
import WebDriverPreCore.Extended.HTTP.Locate qualified as HL
import WebDriverPreCore.Extended.HTTP.NoWait qualified as NW
import WebDriverPreCore.Extended.Locators (DisplayedCheck (..), LocateException, elmId)
import WebDriverPreCore.HTTP.Protocol
  ( ElementId,
    ShadowRootElementId,
    WindowRect,
  )
import WebDriverPreCore.Utils.Utils (txt)

-- TODO DELETE

-- | Locate an element and click it, signalling any failure via 'Error'.
click2 :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es) => Locator -> Eff es ()
click2 loc =
  NW.runAction locActions defHttpOpts (fmap Right . elementClick) loc
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
locActions :: (WebDriver :> es, Logger :> es) => LocateActions (Eff es)
locActions =
  MkLocateActions
    { throw = throwIO,
      catch,
      trace = logDebug . txt,
      findElement,
      findElementFromElement,
      findElements,
      findElementsFromElement,
      executeScript,
      getElementAttribute,
      getElementText,
      getElementShadowRoot,
      findElementFromShadowRoot,
      findElementsFromShadowRoot
    }

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

-- | Locate an element, returning it or the locate error.
locate :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => Locator -> Eff es (Either LocateException ElementId)
locate loc = do
  opts <- ask @LocateOpts
  HL.locate locActions opts loc

-- | Locate an element, returning it or the locate error.
locateFromElm :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => ElementId -> Locator -> Eff es (Either LocateException ElementId)
locateFromElm el loc = do
  opts <- ask @LocateOpts
  HL.locateFromElement locActions opts el loc

-- | Locate an element, returning it or the locate error.
locateAll :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => Locator -> Eff es (Either LocateException [ElementId])
locateAll loc = do
  opts <- ask @LocateOpts
  HL.locateAll locActions opts loc

-- | Locate an element, returning it or the locate error.
locateAllFromElm :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => ElementId -> Locator -> Eff es (Either LocateException [ElementId])
locateAllFromElm el loc = do
  opts <- ask @LocateOpts
  HL.locateAllFromElement locActions opts el loc

bindRight :: (Monad m) => (a -> m (Either e b)) -> (Either e a) -> m (Either e b)
bindRight = either (pure . Left)

locateElementShadowRoot :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => Locator -> Eff es (Either LocateException ShadowRootElementId)
locateElementShadowRoot loc =
  locate loc
    >>= bindRight (fmap Right . getElementShadowRoot)

shadowRoot :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => Locator -> Eff es (Either LocateException ShadowRootElementId)
shadowRoot shadowLoc =
  locate shadowLoc
    >>= bindRight (fmap Right . getElementShadowRoot)

locateFromShadowElement :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => Locator -> Locator -> Eff es (Either LocateException ElementId)
locateFromShadowElement shadowLoc loc = do
  opts <- ask @LocateOpts
  shadowRoot shadowLoc
    >>= bindRight \sroot -> HL.locateFromShadowElement locActions opts sroot loc

locateAllFromShadowElement :: forall es. (WebDriver :> es, Logger :> es, Reader LocateOpts :> es) => Locator -> Locator -> Eff es (Either LocateException [ElementId])
locateAllFromShadowElement shadowLoc loc = do
  opts <- ask @LocateOpts
  shadowRoot shadowLoc
    >>= bindRight \sroot -> HL.locateAllFromShadowElement locActions opts sroot loc

-- | Locate an element and click it, signalling any failure via 'Error'.
click :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es ()
click = elmRunner elementClick

-- | Locate an element and clear it, signalling any failure via 'Error'.
clear :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es ()
clear = elmRunner elementClear

-- | Locate an element and send keys to it, signalling any failure via 'Error'.
sendKeys :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es ()
sendKeys loc keys = elmRunner (\el -> elementSendKeys el keys) loc

-- | Locate an element and take a screenshot of it, signalling any failure via 'Error'.
screenshot :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
screenshot = elmRunner takeElementScreenshot

-- | Locate an element and check whether it is selected, signalling any failure via 'Error'.
isSelected :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Bool
isSelected = elmRunner isElementSelected

-- | Locate an element and get one of its attributes, signalling any failure via 'Error'.
getAttribute :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es (Maybe Text)
getAttribute loc name = elmRunner (\el -> getElementAttribute el name) loc

-- | Locate an element and get one of its properties, signalling any failure via 'Error'.
getProperty :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es (Maybe Value)
getProperty loc name = elmRunner (\el -> getElementProperty el name) loc

-- | Locate an element and get one of its CSS values, signalling any failure via 'Error'.
getCssValue :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Text -> Eff es Text
getCssValue loc prop = elmRunner (\el -> getElementCssValue el prop) loc

-- | Locate an element and get its shadow root, signalling any failure via 'Error'.
getShadowRoot :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es ShadowRootElementId
getShadowRoot = elmRunner getElementShadowRoot

-- | Locate an element and get its text, signalling any failure via 'Error'.
getText :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getText = elmRunner getElementText

-- | Locate an element and get its tag name, signalling any failure via 'Error'.
getTagName :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getTagName = elmRunner getElementTagName

-- | Locate an element and get its bounding rectangle, signalling any failure via 'Error'.
getRect :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es WindowRect
getRect = elmRunner getElementRect

-- | Locate an element and check whether it is enabled, signalling any failure via 'Error'.
isEnabled :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Bool
isEnabled = elmRunner isElementEnabled

-- | Locate an element and get its computed role, signalling any failure via 'Error'.
getComputedRole :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getComputedRole = elmRunner getElementComputedRole

-- | Locate an element and get its computed label, signalling any failure via 'Error'.
getComputedLabel :: forall es. (WebDriver :> es, Logger :> es, Error ActionException :> es, Reader LocateOpts :> es) => Locator -> Eff es Text
getComputedLabel = elmRunner getElementComputedLabel
