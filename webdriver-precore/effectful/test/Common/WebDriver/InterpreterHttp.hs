module Common.WebDriver.InterpreterHttp
  ( runWebDriverHttp,
  )
where

import Control.Monad (void)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (catch)
import UnliftIO (throwIO)
import WebDriver.Effectful.HTTP.Base.Actions qualified as H
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, Script (MkScript, args, script))
import WebDriverPreCore.Extended.HTTP.Locate qualified as L
import WebDriver.Effectful.HTTP.Base.Effect qualified as HTTPEff
import Common.WebDriver.Effect ( WebDriver(..) )

-- | Interpret 'WebDriver' in terms of the 'WebDriverHttp' effect.
runWebDriverHttp ::
  forall es a.
  (IOE :> es, HTTPEff.WebDriver :> es) =>
  L.LocateOpts ->
  Eff (WebDriver ElementId : es) a ->
  Eff es a
runWebDriverHttp opts = interpret $ \_ -> \case
  MaximizeWindow -> void H.maximizeWindow
  MinimizeWindow -> void H.minimizeWindow
  NavigateTo url -> H.navigateTo url
  ExecuteScript script' args' -> void $ H.executeScript (MkScript {script = script', args = args'})
  Locate loc -> L.locate actions opts loc
  LocateAll loc -> L.locateAll actions opts loc
  LocateFromElement el loc -> L.locateFromElement actions opts el loc
  LocateAllFromElement el loc -> L.locateAllFromElement actions opts el loc
  GetProperty el name -> H.getElementProperty el name
  GetAttribute el name -> H.getElementAttribute el name
  where
    actions = mkHttpLocateActions

-- | Build HTTP 'L.LocateActions' from the 'WebDriverHttp' effect.
mkHttpLocateActions :: forall es. (IOE :> es, HTTPEff.WebDriver :> es) => L.LocateActions (Eff es)
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
