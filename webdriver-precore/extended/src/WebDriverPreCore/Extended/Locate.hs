module WebDriverPreCore.Extended.Locate
  ( LocateException (..),
    Cardinality (..),
    LocateOps (..),
    locateHttp,
    displayedJS,
    isDisplayedHttp,
  )
where

import Control.Exception (Exception, SomeException)
import Data.Aeson as A (Result (..), Value, fromJSON, toJSON) 
import Data.Function ((&))
import Data.Text
import GHC.Stack (HasCallStack)
import WebDriverPreCore.Extended.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, Session)
import WebDriverPreCore.Extended.HTTP.Internal (Runner)
import WebDriverPreCore.Extended.Locators.Internal (Locator, Protocol(..))
import WebDriverPreCore.Extended.Locators.Internal qualified as LI
import WebDriverPreCore.Extended.Protocol
import WebDriverPreCore.Extended.SimplifiedLocator.Internal (SimplifiedLocator (..), prepareSimplify)
import WebDriverPreCore.HTTP.Protocol (Command, Script (..), Selector)
import Prelude as P

data LocateException
  = AmbiguousLocateResult
      { description :: Text,
        locator :: Locator
      }
  | NoElementFound
      { description :: Text,
        locator :: Locator
      }
  | InvalidLocator LI.InvalidLocator
  | DriverException WebDriverException
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
  -- | cardinality
  Cardinality ->
  -- | locate ops
  LocateOps ->
  -- | locator
  Locator ->
  m ElementId
locateHttp throw catch runner defLoc ses cardinality MkLocateOps {displayedCheck} loc =
  preparedLoc
    & either
      (throw . InvalidLocator)
      \loc -> undefined
  where
    -- do
    -- case cardinality of
    --   Unique -> findElm loc
    --   First -> findElm loc
    --   Many ->
    --     findElms loc >>= \case
    --       Left err -> pure $ Left err
    --       Right [] -> pure $ Left $ WebDriverException $ toException $ userError "Expected at least one element, but found none."
    --       Right (x : xs) -> case cardinality of
    --         Unique -> undefined
    --         -- if null xs
    --         --   then pure $ Right x
    --         --   else pure $ Left $ AmbiguousLocateResult $ "Expected exactly one element, but found " <> pack (show (1 + length xs)) <> "."
    --         First -> pure $ Right x
    --         Many -> pure $ Right x -- TODO: return all elements, not just the first one

    preparedLoc = prepareSimplify defLoc HTTP loc

    runCommand :: forall a. ((Command a -> m a) -> Session -> Selector -> m a) -> Selector -> m a
    runCommand f sel =
      catch
        (f runner ses sel)
        (throw . DriverException)

    findElm :: Selector -> m ElementId
    findElm = runCommand findElement

    findElms :: Selector -> m [ElementId]
    findElms = runCommand findElements

    getSingleton :: Selector -> m ElementId
    getSingleton sel =
      case cardinality of
        Unique -> do
          elms <- findElms sel
          case elms of
            [] -> throw $ NoElementFound { description = "Expected exactly one element, but found none.", locator = loc }
            [x] -> pure x
            xs -> throw $ AmbiguousLocateResult { description = "Expected exactly one element, but found: " <> pack (show (P.length xs)) <> ".", locator = loc }
        -- findElm sel >>= \eid -> do
        --   -- check if there are more elements
        --   findElms sel >>= \case
        --     Left err -> throw err
        --     Right [] -> throw $ NoElementFound
        --     Right (x : xs) ->
        --       if null xs
        --         then pure eid
        --         else throw $ AmbiguousLocateResult $ "Expected exactly one element, but found " <> pack (show (1 + length xs)) <> "."
        First -> undefined -- findElm sel
        Many -> undefined -- findElms sel

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

displayedJS :: Text
displayedJS =
  "function isDisplayed(el) {\n\
  \  if (!el || !el.isConnected) return false;\n\
  \\n\
  \  const style = getComputedStyle(el);\n\
  \\n\
  \  if (style.display === \"none\") return false;\n\
  \  if (style.visibility === \"hidden\" || style.visibility === \"collapse\") return false;\n\
  \\n\
  \  if (el.tagName === \"INPUT\" && el.type === \"hidden\")\n\
  \    return false;\n\
  \\n\
  \  const rect = el.getBoundingClientRect();\n\
  \\n\
  \  if (rect.width === 0 || rect.height === 0)\n\
  \    return false;\n\
  \\n\
  \  const vpW = window.innerWidth;\n\
  \  const vpH = window.innerHeight;\n\
  \\n\
  \  if (\n\
  \    rect.bottom < 0 ||\n\
  \    rect.right < 0 ||\n\
  \    rect.top > vpH ||\n\
  \    rect.left > vpW\n\
  \  )\n\
  \    return false;\n\
  \\n\
  \  const points = [\n\
  \    [rect.left + rect.width / 2, rect.top + rect.height / 2],\n\
  \    [rect.left + 1, rect.top + 1],\n\
  \    [rect.right - 1, rect.bottom - 1]\n\
  \  ];\n\
  \\n\
  \  for (const [x, y] of points) {\n\
  \    if (x < 0 || y < 0 || x > vpW || y > vpH)\n\
  \      continue;\n\
  \\n\
  \    const hit = document.elementFromPoint(x, y);\n\
  \\n\
  \    if (hit === el || el.contains(hit))\n\
  \      return true;\n\
  \  }\n\
  \\n\
  \  return false;\n\
  \}\n\
  \return isDisplayed(arguments[0]);"

isDisplayedHttp ::
  forall m.
  (Monad m) =>
  -- | throw exceptions
  (forall a e. (HasCallStack, Exception e) => e -> m a) ->
  -- | catch exceptions
  (forall a e. (HasCallStack, Exception e) => m a -> (e -> m a) -> m a) ->
  -- | runner
  (forall b. Command b -> m b) ->
  -- | session
  Session ->
  -- | element to check
  ElementId ->
  m Bool
isDisplayedHttp throw catch runner ses eid = do
  result <-
    catch
      (executeScript runner ses MkScript {script = displayedJS, args = [toJSON eid]})
      (throw . DriverException)
  case (fromJSON result :: A.Result Bool) of
    A.Success b -> pure b
    A.Error msg -> error $ "isDisplayedHttp: isDisplayed script returned unexpected value: " <> msg

locateHttpBiDi = undefined

--  use all findElements but limit to 2 results (not supported in standard HTTP WebDriver, but available in BiDi via maxNodeCount).