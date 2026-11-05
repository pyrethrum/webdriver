-- |
-- Module: WebDriverPreCore.Extended.Common.NoWait
-- Description: Common compound actions composed from the Base action functions
--
-- TODO: implementation pending.
module WebDriverPreCore.Extended.Common.NoWait
  ( ActionParams (..),
    ActionException (..),
    ElmAction,
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

data ActionParams m elm r t = MKLocateAction
  { locate :: Locator -> m (Either LocateException elm),
    -- throw :: forall a. HasCallStack => ActionException -> m a,
    catch :: forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a,
    trace :: t -> m ()
  }

type ElmAction m elm r = elm -> m (Either ActionException r)

runAction :: Monad m => ActionParams m elm r t -> ElmAction m elm r -> Locator -> m (Either ActionException r)
runAction MKLocateAction {catch, locate} action loc = 
  catch 
    (locate loc >>= either (pure . Left . LocateException) action)
    (pure . Left . UnknownException)
