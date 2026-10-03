module Common.WebDriver.Effect
  ( -- * Effect
    WebDriver (..),

    -- * Operations
    maximizeWindow,
    minimizeWindow,
    navigateTo,
    locate,
    locateAll,
    locateFromElement,
    locateAllFromElement,
    getProperty,
    getAttribute,
  )
where

import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Text (Text)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator)

-- | A test-level algebraic effect abstracting over the WebDriver operations
-- shared by the HTTP and BiDi test modules.
--
-- The effect is parameterised over the element type: HTTP tests use
-- 'ElementId' while BiDi tests use 'BiDiP.NodeRemoteValue'.
data WebDriver (elem :: Type) :: Effect where
  MaximizeWindow :: WebDriver elem m ()
  MinimizeWindow :: WebDriver elem m ()
  NavigateTo :: URL -> WebDriver elem m ()
  Locate :: Locator -> WebDriver elem m (Either L.LocateException elem)
  LocateAll :: Locator -> WebDriver elem m (Either L.LocateException [elem])
  LocateFromElement :: elem -> Locator -> WebDriver elem m (Either L.LocateException elem)
  LocateAllFromElement :: elem -> Locator -> WebDriver elem m (Either L.LocateException [elem])
  GetProperty :: elem -> Text -> WebDriver elem m (Maybe Value)
  GetAttribute :: elem -> Text -> WebDriver elem m (Maybe Text)

type instance DispatchOf (WebDriver elem) = Dynamic

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

maximizeWindow :: forall elem es. (WebDriver elem :> es) => Eff es ()
maximizeWindow = send (MaximizeWindow @elem)

minimizeWindow :: forall elem es. (WebDriver elem :> es) => Eff es ()
minimizeWindow = send (MinimizeWindow @elem)

navigateTo :: forall elem es. (WebDriver elem :> es) => URL -> Eff es ()
navigateTo = send . NavigateTo @elem

locate :: (WebDriver elem :> es) => Locator -> Eff es (Either L.LocateException elem)
locate = send . Locate

locateAll :: (WebDriver elem :> es) => Locator -> Eff es (Either L.LocateException [elem])
locateAll = send . LocateAll

locateFromElement :: (WebDriver elem :> es) => elem -> Locator -> Eff es (Either L.LocateException elem)
locateFromElement el = send . LocateFromElement el

locateAllFromElement :: (WebDriver elem :> es) => elem -> Locator -> Eff es (Either L.LocateException [elem])
locateAllFromElement el = send . LocateAllFromElement el

getProperty :: (WebDriver elem :> es) => elem -> Text -> Eff es (Maybe Value)
getProperty el = send . GetProperty el

getAttribute :: (WebDriver elem :> es) => elem -> Text -> Eff es (Maybe Text)
getAttribute el = send . GetAttribute el
