module Common.WebDriver.InterpreterHttp
  ( runWebDriverHttp,
  )
where

import Control.Monad (void)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (catch)
import UnliftIO (throwIO)
import WebDriver.Effectful (WebDriverHttp)
import WebDriver.Effectful.HTTP.Base.Actions qualified as H
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId)
import WebDriverPreCore.Extended.Locate qualified as L

import Common.WebDriver.Effect (WebDriver (..))

-- | Interpret 'WebDriver' in terms of the 'WebDriverHttp' effect.
runWebDriverHttp ::
  forall es a.
  (IOE :> es, WebDriverHttp :> es) =>
  L.HttpLocateOpts ->
  Eff (WebDriver ElementId : es) a ->
  Eff es a
runWebDriverHttp opts = interpret $ \_ -> \case
  MaximizeWindow -> void H.maximizeWindow
  MinimizeWindow -> void H.minimizeWindow
  NavigateTo url -> H.navigateTo url
  Locate loc -> L.locateHttp actions opts loc
  LocateAll loc -> L.locateAllHttp actions opts loc
  LocateFromElement el loc -> L.locateFromElementHttp actions opts el loc
  LocateAllFromElement el loc -> L.locateAllFromElementHttp actions opts el loc
  GetProperty el name -> H.getElementProperty el name
  GetAttribute el name -> H.getElementAttribute el name
  where
    actions = mkHttpLocateActions

-- | Build HTTP 'L.LocateActions' from the 'WebDriverHttp' effect.
mkHttpLocateActions :: forall es. (IOE :> es, WebDriverHttp :> es) => L.LocateActions (Eff es)
mkHttpLocateActions =
  L.MkLocateActions
    { throw = throwIO,
      catch,
      trace = \_ -> pure (),
      findElement = H.findElement,
      findElementFromElement = H.findElementFromElement,
      findElements = H.findElements,
      findElementsFromElement = H.findElementsFromElement,
      executeScript = H.executeScript,
      getElementAttribute = H.getElementAttribute,
      getElementText = H.getElementText
    }
