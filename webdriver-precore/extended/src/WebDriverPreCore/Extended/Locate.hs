module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    Cardinality (..),
    LocateOps (..),
    locateHttp,
  )
where

import Control.Exception (Exception, SomeException)
import Data.Function ((&))
import Data.Text
import GHC.Stack (HasCallStack)
import WebDriverPreCore.Extended.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, Session)
import WebDriverPreCore.Extended.HTTP.Internal (Runner)
import WebDriverPreCore.Extended.Locators.Internal (Locator, Protocol)
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.Extended.SimplifiedLocator.Internal (SimplifiedLocator (..), prepareSimplify)
import WebDriverPreCore.HTTP.Protocol (Command, Selector)

data LocateException
  = AmbiguousLocateResult {description :: Text}
  | InvalidLocator LI.InvalidLocator
  | WebDriverException SomeException
  deriving (Show)

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
  forall m.
  (Monad m) =>
  -- | throw exceptions
  (forall a e. (HasCallStack, Exception e) => e -> m a) ->
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | runner
  (forall b. Command b -> m b) ->
  -- | default locator
  (Text -> Locator) ->
  -- | session
  Session ->
  -- | locate ops
  LocateOps ->
  -- | locator
  Locator ->
  m ElementId
locateHttp throw catch runner defLoc ses MkLocateOps {cardinality, protocol} loc =
  preparedLoc
    & either
      (throw . InvalidLocator)
      \loc -> do
        case cardinality of
          Unique -> findElm loc
          First -> findElm loc
          Many ->
            findElms loc >>= \case
              Left err -> pure $ Left err
              Right [] -> pure $ Left $ WebDriverException $ toException $ userError "Expected at least one element, but found none."
              Right (x : xs) -> case cardinality of
                Unique -> undefined
                  -- if null xs
                  --   then pure $ Right x
                  --   else pure $ Left $ AmbiguousLocateResult $ "Expected exactly one element, but found " <> pack (show (1 + length xs)) <> "."
                First -> pure $ Right x
                Many -> pure $ Right x -- TODO: return all elements, not just the first one
  where
    preparedLoc = prepareSimplify defLoc protocol loc

    runLocate :: forall a. ((Command a -> m a) -> Session -> Selector -> m a) -> Selector -> m a
    runLocate f sel =
      catch
        (f runner ses sel)
        (throw . WebDriverException)

    findElm :: Selector -> m ElementId
    findElm = runLocate findElement

    findElms :: Selector -> m [ElementId]
    findElms = runLocate findElements

    httpLocate :: SimplifiedLocator -> m ElementId
    httpLocate = \case
      CSS {} -> undefined
      XPath {} -> undefined
      Role {} -> undefined
      InnerText {} -> undefined
      BiDiContext {} -> undefined
      Parent {} -> undefined
      All {} -> undefined
      Any {} -> undefined
      None {} -> undefined
      PostFilter _ -> undefined
