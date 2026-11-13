-- |
-- Module: WebDriverPreCore.Extended.HTTP.NoWait
-- Description: HTTP compound actions composed from the Base action functions
module WebDriverPreCore.Extended.HTTP.NoWait where

import WebDriverPreCore.Extended.Common.Locate (Locator)
import WebDriverPreCore.Extended.Common.NoWait qualified as C
import WebDriverPreCore.Extended.HTTP.Locate (LocateActions (..), LocateOpts, WDTrace, locate)
import WebDriverPreCore.HTTP.Protocol

runAction ::
  (Monad m) =>
  LocateActions m ->
  LocateOpts ->
  (ElementId -> m (Either C.ActionException r)) ->
  Locator ->
  m (Either C.ActionException r)
runAction la@MkLocateActions {catch, trace} opts =
  C.runAction actionHandles
  where
    actionHandles =
      C.MKElmActionHandles
        { locate = locate la opts,
          catch,
          trace
        }
