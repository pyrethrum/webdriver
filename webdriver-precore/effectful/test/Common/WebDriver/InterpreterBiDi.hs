module Common.WebDriver.InterpreterBiDi
  ( runWebDriverBiDi,
  )
where

import Common.WebDriver.Effect (WebDriver (..))
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.List (singleton)
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

-- | Interpret 'WebDriver' in terms of the 'WebDriverBiDi' effect.
runWebDriverBiDi ::
  forall es a.
  (IOE :> es, WebDriverBiDi :> es) =>
  BL.BiDiLocateOpts ->
  BrowsingContext ->
  Eff (WebDriver NodeRemoteValue : es) a ->
  Eff es a
runWebDriverBiDi opts context = interpret $ \_ -> \case
  MaximizeWindow -> setFirstWindowState MaximizedState
  MinimizeWindow -> setFirstWindowState MinimizedState
  NavigateTo url ->
    void
      $ B.browsingContextNavigate
      $ MkNavigate {context, url, wait = Nothing}
  Locate loc -> BL.locateBiDi actions opts context loc
  LocateAll loc -> BL.locateAllBiDi actions opts context loc
  LocateFromElement el loc ->
    case requireSharedRef loc el of
      Left err -> pure $ Left err
      Right sr -> BL.locateFromElementBiDi actions opts context sr loc
  LocateAllFromElement el loc ->
    case requireSharedRef loc el of
      Left err -> pure $ Left err
      Right sr -> BL.locateAllFromElementBiDi actions opts context sr loc
  GetProperty el name -> getProperty context el name
  GetAttribute el name -> getAttribute context el name
  where
    actions = mkBiDiLocateActions context

-- | Build BiDi 'BL.LocateActions' from the 'WebDriverBiDi' effect.
mkBiDiLocateActions :: forall es. (IOE :> es, WebDriverBiDi :> es) => BrowsingContext -> BL.LocateActions (Eff es)
mkBiDiLocateActions bc =
  BL.MkLocateActions
    { throw = throwIO,
      catch,
      trace = \_ -> pure (),
      locateNodes = B.browsingContextLocateNodes,
      getElementText = getElementText bc
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
      void
        $ B.browserSetClientWindowState
        $ MkSetClientWindowState
          { clientWindow,
            windowState = ClientWindowNamedState named
          }

-- | Resolve a located node into a 'SharedReference' for use as a start node.
requireSharedRef :: Locator -> NodeRemoteValue -> Either L.LocateException SharedReference
requireSharedRef loc node =
  (nodeToSharedRef node)
    & maybe
      (Left $ L.ElementNotFound {description = "Cannot resolve element to a BiDi shared reference", locator = loc})
      Right

-- | Convert a located node into a 'SharedReference', returning 'Nothing' if the
--   node has no shared id.
nodeToSharedRef :: NodeRemoteValue -> Maybe SharedReference
nodeToSharedRef (MkNodeRemoteValue {sharedId, handle}) =
  MkSharedReference <$> sharedId <*> pure handle <*> pure Nothing

-- | Read an element attribute via @script.callFunction@.
getAttribute :: (WebDriverBiDi :> es) => BrowsingContext -> NodeRemoteValue -> Text -> Eff es (Maybe Text)
getAttribute bc node name =
  case nodeToLocalValue node of
    Nothing -> pure Nothing
    Just nodeArg' ->
      resultMaybeText
        <$> callFunction
          bc
          "function(el, name) { return el.getAttribute(name); }"
          [ nodeArg',
            PrimitiveLocalValue (StringValue (MkStringValue {value = name}))
          ]

-- | Read a live JS property via @script.callFunction@.
getProperty :: (WebDriverBiDi :> es) => BrowsingContext -> NodeRemoteValue -> Text -> Eff es (Maybe Value)
getProperty bc node name =
  nodeToLocalValue node
    & maybe
      (pure Nothing)
      \nodeArg' ->
        extractValue
          <$> callFunction
            bc
            "function(el, name) { return el[name]; }"
            [ nodeArg',
              PrimitiveLocalValue (StringValue (MkStringValue {value = name}))
            ]
  where
    extractValue = \case
      EvaluateResultSuccess {result} -> remoteValueToValue result
      EvaluateResultException {} -> Nothing

-- | Read the rendered text of a node via @script.callFunction@.
getElementText :: (WebDriverBiDi :> es) => BrowsingContext -> SharedReference -> Eff es Text
getElementText bc (MkSharedReference {sharedId, handle}) =
  handle
    & maybe
      (error "InterpreterBiDi.getElementText: node has no handle - cannot call script.callFunction")
      (fmap resultText . callFunction bc "function(el) { return el.innerText; }" . singleton . refArg sharedId)

-- | Build a @script.callFunction@ local value referencing a node.
nodeToLocalValue :: NodeRemoteValue -> Maybe LocalValue
nodeToLocalValue (MkNodeRemoteValue {sharedId, handle}) = refArg <$> sharedId <*> handle

refArg :: SharedId -> Handle -> LocalValue
refArg sid h =
  RemoteReference $
    MkRemoteReference
      { sharedreference = MkSharedReference {sharedId = sid, handle = Just h, extensions = Nothing},
        remoteObjectReference = MkRemoteObjectReference {handle = h, shartedId = Just sid, extensions = Nothing}
      }

callFunction :: (WebDriverBiDi :> es) => BrowsingContext -> Text -> [LocalValue] -> Eff es EvaluateResult
callFunction bc declaration args =
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

resultText :: EvaluateResult -> Text
resultText = \case
  EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue {value}))} -> value
  EvaluateResultSuccess {} -> ""
  EvaluateResultException {} -> ""

resultMaybeText :: EvaluateResult -> Maybe Text
resultMaybeText = \case
  EvaluateResultSuccess {result = PrimitiveValue NullValue} -> Nothing
  EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue {value}))} -> Just value
  _ -> Nothing

remoteValueToValue :: RemoteValue -> Maybe Value
remoteValueToValue = \case
  PrimitiveValue NullValue -> Just Aeson.Null
  PrimitiveValue (StringValue (MkStringValue {value})) -> Just (Aeson.String value)
  PrimitiveValue (BooleanValue b) -> Just (Aeson.Bool b)
  PrimitiveValue (NumberValue (Left n)) -> Just (Aeson.Number (realToFrac n))
  PrimitiveValue (BigIntValue t) -> Just (Aeson.String t)
  _ -> Nothing
