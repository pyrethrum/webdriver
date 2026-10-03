module Common.WebDriver.InterpreterBiDi
  ( runWebDriverBiDi,
  )
where

import Control.Monad (void)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (catch)
import UnliftIO (throwIO)
import WebDriver.Effectful (WebDriverBiDi)
import WebDriver.Effectful.BiDi.Base.Actions qualified as B
import WebDriverPreCore.BiDi.Protocol hiding (Locator)
import WebDriverPreCore.Extended.BiDi.Locate qualified as BL
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator)

import Common.WebDriver.Effect (WebDriver (..))

-- | Interpret 'WebDriver' in terms of the 'WebDriverBiDi' effect.
runWebDriverBiDi ::
  forall es a.
  (IOE :> es, WebDriverBiDi :> es) =>
  BL.BiDiLocateOpts ->
  BrowsingContext ->
  Eff (WebDriver NodeRemoteValue : es) a ->
  Eff es a
runWebDriverBiDi opts bc = interpret $ \_ -> \case
  MaximizeWindow -> setFirstWindowState MaximizedState
  MinimizeWindow -> setFirstWindowState MinimizedState
  NavigateTo url ->
    void $
      B.browsingContextNavigate $
        MkNavigate {context = bc, url = url, wait = Nothing}
  Locate loc -> BL.locateBiDi actions opts bc loc
  LocateAll loc -> BL.locateAllBiDi actions opts bc loc
  LocateFromElement el loc ->
    case bidiRequireSharedRef loc el of
      Left err -> pure $ Left err
      Right sr -> BL.locateFromElementBiDi actions opts bc sr loc
  LocateAllFromElement el loc ->
    case bidiRequireSharedRef loc el of
      Left err -> pure $ Left err
      Right sr -> BL.locateAllFromElementBiDi actions opts bc sr loc
  GetProperty el name -> bidiGetProperty bc el name
  GetAttribute el name -> bidiGetAttribute bc el name
  where
    actions = mkBiDiLocateActions bc

-- | Build BiDi 'BL.LocateActions' from the 'WebDriverBiDi' effect.
mkBiDiLocateActions :: forall es. (IOE :> es, WebDriverBiDi :> es) => BrowsingContext -> BL.LocateActions (Eff es)
mkBiDiLocateActions bc =
  BL.MkLocateActions
    { throw = throwIO,
      catch,
      trace = \_ -> pure (),
      locateNodes = B.browsingContextLocateNodes,
      getElementText = bidiGetElementText bc
    }

-- | Set the current window to a named state (maximized/minimized/fullscreen).
--
-- BiDi has no "current window" concept for this command, so this operates on
-- the first client window reported by the browser.
setFirstWindowState :: (WebDriverBiDi :> es) => NamedState -> Eff es ()
setFirstWindowState named = do
  MkGetClientWindowsResult {clientWindows} <- B.browserGetClientWindows
  case clientWindows of
    [] -> pure ()
    MkClientWindowInfo {clientWindow} : _ ->
      void $
        B.browserSetClientWindowState $
          MkSetClientWindowState
            { clientWindow,
              windowState = ClientWindowNamedState named
            }

-- | Resolve a located node into a 'SharedReference' for use as a start node.
bidiRequireSharedRef :: Locator -> NodeRemoteValue -> Either L.LocateException SharedReference
bidiRequireSharedRef loc node =
  maybe
    (Left $ L.ElementNotFound {description = "Cannot resolve element to a BiDi shared reference", locator = loc})
    Right
    (bidiNodeToSharedRef node)

-- | Convert a located node into a 'SharedReference', returning 'Nothing' if the
--   node has no shared id.
bidiNodeToSharedRef :: NodeRemoteValue -> Maybe SharedReference
bidiNodeToSharedRef (MkNodeRemoteValue {sharedId, handle}) =
  MkSharedReference <$> sharedId <*> pure handle <*> pure Nothing

-- | Read an element attribute via @script.callFunction@.
bidiGetAttribute :: (WebDriverBiDi :> es) => BrowsingContext -> NodeRemoteValue -> Text -> Eff es (Maybe Text)
bidiGetAttribute bc node name =
  case bidiNodeToLocalValue node of
    Nothing -> pure Nothing
    Just nodeArg' -> do
      rslt <-
        bidiCallFunction
          bc
          "function(el, name) { return el.getAttribute(name); }"
          [ nodeArg',
            PrimitiveLocalValue (StringValue (MkStringValue {value = name}))
          ]
      pure $ bidiResultMaybeText rslt

-- | Read a live JS property via @script.callFunction@.
bidiGetProperty :: (WebDriverBiDi :> es) => BrowsingContext -> NodeRemoteValue -> Text -> Eff es (Maybe Value)
bidiGetProperty bc node name =
  case bidiNodeToLocalValue node of
    Nothing -> pure Nothing
    Just nodeArg' -> do
      rslt <-
        bidiCallFunction
          bc
          "function(el, name) { return el[name]; }"
          [ nodeArg',
            PrimitiveLocalValue (StringValue (MkStringValue {value = name}))
          ]
      pure $ case rslt of
        EvaluateResultSuccess {result} -> bidiRemoteValueToValue result
        EvaluateResultException {} -> Nothing

-- | Read the rendered text of a node via @script.callFunction@.
bidiGetElementText :: (WebDriverBiDi :> es) => BrowsingContext -> SharedReference -> Eff es Text
bidiGetElementText bc (MkSharedReference {sharedId, handle}) =
  case handle of
    Nothing -> error "InterpreterBiDi.bidiGetElementText: node has no handle - cannot call script.callFunction"
    Just h -> do
      rslt <- bidiCallFunction bc "function(el) { return el.innerText; }" [bidiRefArg sharedId h]
      pure $ bidiResultText rslt

-- | Build a @script.callFunction@ local value referencing a node.
bidiNodeToLocalValue :: NodeRemoteValue -> Maybe LocalValue
bidiNodeToLocalValue (MkNodeRemoteValue {sharedId, handle}) = do
  sid <- sharedId
  h <- handle
  pure $ bidiRefArg sid h

bidiRefArg :: SharedId -> Handle -> LocalValue
bidiRefArg sid h =
  RemoteReference $
    MkRemoteReference
      { sharedreference = MkSharedReference {sharedId = sid, handle = Just h, extensions = Nothing},
        remoteObjectReference = MkRemoteObjectReference {handle = h, shartedId = Just sid, extensions = Nothing}
      }

bidiCallFunction :: (WebDriverBiDi :> es) => BrowsingContext -> Text -> [LocalValue] -> Eff es EvaluateResult
bidiCallFunction bc declaration args =
  B.scriptCallFunction $
    MkCallFunction
      { functionDeclaration = declaration,
        awaitPromise = False,
        target = ContextTarget (MkContextTarget {context = bc, sandbox = Nothing}),
        arguments = Just args,
        resultOwnership = Nothing,
        serializationOptions = Nothing,
        this = Nothing
      }

bidiResultText :: EvaluateResult -> Text
bidiResultText = \case
  EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue {value}))} -> value
  EvaluateResultSuccess {} -> ""
  EvaluateResultException {} -> ""

bidiResultMaybeText :: EvaluateResult -> Maybe Text
bidiResultMaybeText = \case
  EvaluateResultSuccess {result = PrimitiveValue NullValue} -> Nothing
  EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue {value}))} -> Just value
  _ -> Nothing

bidiRemoteValueToValue :: RemoteValue -> Maybe Value
bidiRemoteValueToValue = \case
  PrimitiveValue NullValue -> Just Aeson.Null
  PrimitiveValue (StringValue (MkStringValue {value})) -> Just (Aeson.String value)
  PrimitiveValue (BooleanValue b) -> Just (Aeson.Bool b)
  PrimitiveValue (NumberValue (Left n)) -> Just (Aeson.Number (realToFrac n))
  PrimitiveValue (BigIntValue t) -> Just (Aeson.String t)
  _ -> Nothing
