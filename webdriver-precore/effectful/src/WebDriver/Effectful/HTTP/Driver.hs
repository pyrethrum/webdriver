module WebDriver.Effectful.HTTP.Driver where

import WebDriver.Effectful.HTTP.Base.Actions
import  WebDriverPreCore.Extended.Locate
import  WebDriverPreCore.Extended.Locaters

import WebDriver.Effectful.BiDi.Base.Effect (BiDiInfo (..), WebDriverBiDi (..))


-- locateHttp ::
--   forall m.
--   (Monad m) =>
--   -- | throw exceptions
--   (forall a e. (HasCallStack, Exception e) => e -> m a) ->
--   -- | catch exceptions
--   (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
--   -- | runner
--   (forall b. Command b -> m b) ->
--   -- | locate opts
--   HttpLocateOpts ->
--   -- | session
--   Session ->
--   -- | locator
--   Locator ->
--   m (Either LocateException LocateResult)

findElement :: (WebDriverHttp :> es) => Locator -> Eff es ElementId
findElement = undefined