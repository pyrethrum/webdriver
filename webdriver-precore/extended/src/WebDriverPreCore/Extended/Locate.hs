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

data DisplayedCheck = Never | DisambiguateUnique | Always deriving (Show, Eq)

data LocateDirectives = MkLocateDirectives
  { cardinality :: Cardinality,
    protocol :: Protocol
  }
  deriving (Show, Eq)

data LocateOps = MkLocateOps
  { displayedCheck :: DisplayedCheck
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

    runCommand :: forall a. ((Command a -> m a) -> Session -> Selector -> m a) -> Selector -> m a
    runCommand f sel =
      catch
        (f runner ses sel)
        (throw . WebDriverException)

    findElm :: Selector -> m ElementId
    findElm = runCommand findElement

    findElms :: Selector -> m [ElementId]
    findElms = runCommand findElements

    getSingleton :: Selector -> m ElementId
    getSingleton sel =
      case cardinality of
        Unique -> do
          elms <- findElms sel
          findElm sel >>= \eid -> do
            -- check if there are more elements
            findElms sel >>= \case
              Left err -> throw err
              Right [] -> throw $ WebDriverException $ toException $ userError "Expected at least one element, but found none."
              Right (x : xs) ->
                if null xs
                  then pure eid
                  else throw $ AmbiguousLocateResult $ "Expected exactly one element, but found " <> pack (show (1 + length xs)) <> "."
        First -> findElm sel
        Many -> findElms sel

    httpLocate :: SimplifiedLocator -> m [ElementId]
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

{-
function bidiIsVisible(el) {
  if (!el || !el.isConnected) return false;

  const style = getComputedStyle(el);

  if (style.display === "none") return false;
  if (style.visibility === "hidden" || style.visibility === "collapse") return false;

  if (el.tagName === "INPUT" && el.type === "hidden")
    return false;

  const rect = el.getBoundingClientRect();

  if (rect.width === 0 || rect.height === 0)
    return false;

  const vpW = window.innerWidth;
  const vpH = window.innerHeight;

  if (
    rect.bottom < 0 ||
    rect.right < 0 ||
    rect.top > vpH ||
    rect.left > vpW
  )
    return false;

  const points = [
    [rect.left + rect.width / 2, rect.top + rect.height / 2],
    [rect.left + 1, rect.top + 1],
    [rect.right - 1, rect.bottom - 1]
  ];

  for (const [x, y] of points) {
    if (x < 0 || y < 0 || x > vpW || y > vpH)
      continue;

    const hit = document.elementFromPoint(x, y);

    if (hit === el || el.contains(hit))
      return true;
  }

  return false;
}
-}