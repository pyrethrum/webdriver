-- |
-- Module: WebDriverPreCore.Extended.Common.NoWait
-- Description: Common compound actions composed from the Base action functions
--
-- TODO: implementation pending.
module WebDriverPreCore.Extended.Common.NoWait
  ( ElmActionHandles (..),
    ActionException (..),
    runLocElmAction,
  )
where

import Control.Exception (Exception, SomeException)
import GHC.Stack (HasCallStack)
import WebDriverPreCore.Error (WebDriverException)
import WebDriverPreCore.Extended.Common.Locate (LocateException, Locator)

data ActionException
  = LocateException LocateException
  | ElmException WebDriverException
  | UnknownException SomeException
  deriving (Show)

data ElmActionHandles m elm r t = MKElmActionHandles
  { locate :: Locator -> m (Either LocateException elm),
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    trace :: t -> m ()
  }

runLocElmAction :: forall m elm r t loc. (Monad m, Locatable loc elm) => ElmActionHandles m elm r t -> (elm -> m (Either ActionException r)) -> loc -> m (Either ActionException r)
runLocElmAction MKElmActionHandles {catch, locate} action loc =
  catch
    (applyLocate locate loc >>= either (pure . Left . LocateException) action)
    (pure . Left . UnknownException)
