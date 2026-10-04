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
data WebDriver (elm :: Type) :: Effect where
  MaximizeWindow :: WebDriver elm m ()
  MinimizeWindow :: WebDriver elm m ()
  NavigateTo :: URL -> WebDriver elm m ()
  Locate :: Locator -> WebDriver elm m (Either L.LocateException elm)
  LocateAll :: Locator -> WebDriver elm m (Either L.LocateException [elm])
  LocateFromElement :: elm -> Locator -> WebDriver elm m (Either L.LocateException elm)
  LocateAllFromElement :: elm -> Locator -> WebDriver elm m (Either L.LocateException [elm])
  GetProperty :: elm -> Text -> WebDriver elm m (Maybe Value)
  GetAttribute :: elm -> Text -> WebDriver elm m (Maybe Text)

type instance DispatchOf (WebDriver elm) = Dynamic

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

maximizeWindow :: forall elm es. (WebDriver elm :> es) => Eff es ()
maximizeWindow = send (MaximizeWindow @elm)

minimizeWindow :: forall elm es. (WebDriver elm :> es) => Eff es ()
minimizeWindow = send (MinimizeWindow @elm)

navigateTo :: forall elm es. (WebDriver elm :> es) => URL -> Eff es ()
navigateTo = send . NavigateTo @elm

locate :: forall elm es. (WebDriver elm :> es) => Locator -> Eff es (Either L.LocateException elm)
locate = send . Locate

locateAll :: forall elm es. (WebDriver elm :> es) => Locator -> Eff es (Either L.LocateException [elm])
locateAll = send . LocateAll

locateFromElement :: forall elm es. (WebDriver elm :> es) => elm -> Locator -> Eff es (Either L.LocateException elm)
locateFromElement el = send . LocateFromElement el

locateAllFromElement :: forall elm es. (WebDriver elm :> es) => elm -> Locator -> Eff es (Either L.LocateException [elm])
locateAllFromElement el = send . LocateAllFromElement el

getProperty :: forall elm es. (WebDriver elm :> es) => elm -> Text -> Eff es (Maybe Value)
getProperty el = send . GetProperty el

getAttribute :: forall elm es. (WebDriver elm :> es) => elm -> Text -> Eff es (Maybe Text)
getAttribute el = send . GetAttribute el
