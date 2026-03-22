module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    Cardinality (..),
    LocateOps (..),
  )
where

import Control.Exception (Exception)
import Data.Text
import WebDriverPreCore.Extended.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, Session)
import WebDriverPreCore.Extended.HTTP.Internal (Runner)
import WebDriverPreCore.Extended.Locators.Internal (Locator, prepare, Protocol)
import GHC.Stack (HasCallStack)
import WebDriverPreCore.HTTP.Protocol (Command)

data LocateException
  = AmbiguousLocateResult {description :: Text}
  | InvalidLocator InvalidLocator
  deriving (Show, Eq)

instance Exception LocateException

data Cardinality = Unique | First | Many deriving (Show, Eq)

data LocateOps = MkLocateOps
  { cardinality :: Cardinality,
    protocol :: Protocol
  }
  deriving (Show, Eq)

-- browsingContextLocateNodes :: forall m. Runner m LocateNodesResult -> LocateNodes -> m LocateNodesResult

-- findElement :: forall m. Runner m ElementId -> Session -> Selector -> m ElementId
-- findElements :: forall m. Runner m [ElementId] -> Session -> Selector -> m [ElementId]

locateHttp ::
  forall a m e.
  -- | throw exceptions
  ((HasCallStack, Exception e) => e -> m a) ->
  -- | catch exceptions
  ((HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | runner
  (forall b. Command b -> m b)->
  -- | default locator
  (Text -> Locator) ->
  -- | session
  Session ->
  -- | locate ops
  LocateOps ->
  -- | locator
  Locator -> _ ->
  m (Either LocateException ElementId)
locateHttp throw catch runner defLoc ses ops loc (&) = 
    preparedLoc & 
      either 
        (throw . InvalidLocator) 
        \loc -> undefined
   where
    preparedLoc = prepare defLoc ops.protocol loc
