module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    Cardinality (..),
    LocateOps (..),
    locateHttp
  )
where

import Control.Exception (Exception, SomeException)
import Data.Text
import WebDriverPreCore.Extended.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, Session)
import WebDriverPreCore.Extended.HTTP.Internal (Runner)
import WebDriverPreCore.Extended.Locators.Internal (Locator(..), prepare, Protocol)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.HTTP.Protocol (Command, Selector)
import GHC.Stack (HasCallStack)
import Data.Function ((&))

data LocateException
  = AmbiguousLocateResult {description :: Text}
  | InvalidLocator LI.InvalidLocator
  | WebDriverException SomeException
  deriving Show

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
  forall m. (Functor m, Applicative m )=>
  -- | throw exceptions
  (forall a e. (HasCallStack, Exception e) => e -> m a) ->
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | runner
  (forall b. Command b -> m b)->
  -- | default locator
  (Text -> Locator) ->
  -- | session
  Session ->
  -- | locate ops
  LocateOps ->
  -- | locator
  Locator -> 
  m (Either LocateException ElementId)
locateHttp throw catch runner defLoc ses ops loc = 
    preparedLoc & 
      either 
        (throw . InvalidLocator) 
        \loc -> undefined
   where
    preparedLoc = prepare defLoc ops.protocol loc

    elmFind :: forall a. ((Command a -> m a) -> Session -> Selector -> m a) -> Selector -> m (Either LocateException a)
    elmFind f sel = 
      catch 
        (Right <$> f runner ses sel)
        (pure . Left . WebDriverException)

    findElm :: Selector -> m (Either LocateException ElementId)  
    findElm = elmFind findElement 


    findElms :: Selector -> m (Either LocateException [ElementId])
    findElms = elmFind findElements

    locate :: Locator -> m (Either LocateException ElementId)
    locate = \case
      CSS {} -> undefined
      XPath {} -> undefined
      AllElms -> undefined
      ID {} -> undefined
      Class {} -> undefined
      Attribute {} -> undefined
      Tag {} -> undefined
      Default {} -> undefined
      Role {} -> undefined
      InnerText {} -> undefined
      BiDiContext {} -> undefined
      Parent {} -> undefined
      All {} -> undefined
      Any {} -> undefined
      None {} -> undefined
      PostFilter _ -> undefined

