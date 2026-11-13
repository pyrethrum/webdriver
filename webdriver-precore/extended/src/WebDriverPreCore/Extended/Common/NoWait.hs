-- |
-- Module: WebDriverPreCore.Extended.Common.NoWait
-- Description: Common compound actions composed from the Base action functions
--
-- TODO: implementation pending.
module WebDriverPreCore.Extended.Common.NoWait
  ( ActionHandles (..),
    ActionException (..),
    runAction
  )
where

import Data.Text (Text, pack)
import GHC.Stack (HasCallStack)
import WebDriverPreCore.Error (WebDriverException)
import WebDriverPreCore.Extended.Common.Locate (LocateException, Locator)
import Control.Exception (Exception, SomeException)
import WebDriverPreCore.Utils.Utils (txt)
import Control.Exception.Base (Exception(..))

data ActionException
  = LocateException LocateException
  | ElmException WebDriverException
  | UnknownException SomeException

data ActionHandles m elm r t = MKLocateHandles
  { locate :: Locator -> m (Either LocateException elm),
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    trace :: t -> m ()
  }


runAction :: Monad m => ActionHandles m elm r t -> (elm -> m (Either ActionException r)) -> Locator -> m (Either ActionException r)
runAction MKLocateHandles {catch, locate} action loc = 
  catch 
    (locate loc >>= either (pure . Left . LocateException) action)
    (pure . Left . UnknownException)
