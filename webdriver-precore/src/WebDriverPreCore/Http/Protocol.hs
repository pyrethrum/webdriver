module WebDriverPreCore.Http.Protocol
  ( -- * Re-exported modules
    -- * Command
    module WebDriverPreCore.Http.Command,

    -- * Capabilities
    module WebDriverPreCore.Http.Capabilities,

    -- * Error
    module WebDriverPreCore.Http.Error,

    -- * Core Types
    Cookie (..),
    Status (..),
    ElementId (..),
    ShadowRootElementId (..),
    FrameReference (..),
    HandleType (..),
    SameSite (..),
    Script (..),
    Selector (..),
    SessionId (..),
    SessionResponse (..),
    Timeouts (..),
    Handle (..),
    WindowHandleSpec (..),
    WindowRect (..),
    module Url,

    -- * Action Types
    Action (..),
    Actions (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    WheelAction (..),
  )
where

import Data.Aeson as A
  ( FromJSON (..),
    Key,
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, notMember)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Generics (Generic)
import WebDriverPreCore.Http.Capabilities
import WebDriverPreCore.Http.Command
import WebDriverPreCore.Http.Error
import WebDriverPreCore.Internal.AesonUtils (nonEmpty, opt, parseObject)
import WebDriverPreCore.Internal.HttpBidiCommon as Url (URL(..)) 
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (id)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dfn-get-window-handle)
newtype Handle = MkHandle {handle :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Handle where 
  toJSON :: Handle -> Value
  toJSON (MkHandle handle) = object ["handle" .= handle]

instance FromJSON Handle where
  parseJSON :: Value -> Parser Handle
  parseJSON = \case 
    String t -> pure $ MkHandle t
    Object o -> do  
      h <- o .: "handle"
      pure $ MkHandle h 
    v -> fail $ unpack $ "Expected Handle as String or Object with handle property, got: " <> txt v


-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#new-window)
data WindowHandleSpec = HandleSpec
  { handle :: Handle,
    handletype :: HandleType
  }
  deriving (Show, Eq)

instance ToJSON WindowHandleSpec where
  toJSON :: WindowHandleSpec -> Value
  toJSON HandleSpec {handle, handletype} =
    object
      [ "handle" .= handle.handle,
        "type" .= handletype
      ]

instance FromJSON WindowHandleSpec where
  parseJSON :: Value -> Parser WindowHandleSpec
  parseJSON = withObject "WindowHandleSpec" $ \v -> do
    handle <- MkHandle <$> v .: "handle"
    handletype <- v .: "type"
    pure $ HandleSpec {..}

data HandleType
  = Window
  | Tab
  deriving (Show, Eq)

instance ToJSON HandleType where
  toJSON :: HandleType -> Value
  toJSON = String . T.toLower . pack . show

instance FromJSON HandleType where
  parseJSON :: Value -> Parser HandleType
  parseJSON = withText "HandleType" $ \case
    "window" -> pure Window
    "tab" -> pure Tab
    v -> fail $ unpack $ "Unknown HandleType " <> v

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dfn-find-element)
newtype ElementId = MkElement {id :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON ElementId where
  toJSON :: ElementId -> Value
  toJSON (MkElement id) = object [elementFieldName .= id]

instance FromJSON ElementId where
  parseJSON :: Value -> Parser ElementId
  parseJSON =
    withObject "ElementId" $
      fmap MkElement . (.: elementFieldName)

newtype ShadowRootElementId = MkShadowRootElementId {id :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON ShadowRootElementId where
  toJSON :: ShadowRootElementId -> Value
  toJSON (MkShadowRootElementId id) = object [shadowRootFieldName .= id]

instance FromJSON ShadowRootElementId where
  parseJSON :: Value -> Parser ShadowRootElementId
  parseJSON =
    withObject "ElementId" $
      fmap MkShadowRootElementId . (.: shadowRootFieldName)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dfn-new-sessions)
newtype SessionId = Session {id :: Text}
  deriving (Show, Eq, Generic)

data SessionResponse = MkSessionResponse
  { sessionId :: SessionId,
    webSocketUrl :: Maybe Text,
    capabilities :: Capabilities,
    extensions :: Maybe (M.Map Text Value)
  }
  deriving (Show, Eq, Generic)

webSocketKey :: Key
webSocketKey = "webSocketUrl"

data Script = MkScript
  { script :: Text,
    args :: [Value]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Script

instance ToJSON SessionResponse where
  toJSON :: SessionResponse -> Value
  toJSON MkSessionResponse {sessionId, webSocketUrl, capabilities, extensions} =
    object $
      [ "sessionId" .= sessionId.id,
        "capabilities" .= mergedCaps,
        webSocketKey .= webSocketUrl
      ]
    where
      capsVal = toJSON capabilities
      mergedCaps = extensions & maybe capsVal mergeExtensions
      mergeExtensions :: M.Map Text Value -> Value
      mergeExtensions mv =
        case capsVal of
          Object capsObj -> Object . KM.union capsObj . KM.fromMapText $ mv
          -- this will never happen - capabilities is always an Object
          _ -> error "SessionResponse - toJSON: capabilities must be an Object"

instance FromJSON SessionResponse where
  parseJSON :: Value -> Parser SessionResponse
  parseJSON =
    withObject
      "SessionResponse.value"
      ( \valueObj -> do
          sessionId <- Session <$> valueObj .: "sessionId"
          --
          capabilitiesVal' :: Value <- valueObj .: "capabilities"
          allCapsObject <- parseObject "capabilities property returned from newSession should be an object" capabilitiesVal'
          webSocketUrl <- allCapsObject .:? webSocketKey
          -- webSocketUrl will come back as a url but is is a Bool flag in Capabilities
          -- so it must be converted or there will be a parse error
          let capabilitiesVal = webSocketUrlToBool allCapsObject
          capabilities :: Capabilities <- parseJSON $ Object capabilitiesVal
          standardCapsProps <- parseObject "JSON from Capabilities Object must be a JSON Object" $ toJSON capabilities
          let keys = fromList . KM.keys
              capsKeys = keys standardCapsProps
              nonNullExtensionKey k v = k `notMember` capsKeys && k /= webSocketKey && nonEmpty v
              extensionsMap = KM.toMapText $ KM.filterWithKey nonNullExtensionKey $ allCapsObject
              extensions =
                if null extensionsMap
                  then Nothing
                  else Just extensionsMap

          pure $ MkSessionResponse {sessionId, webSocketUrl, capabilities, extensions}
      )

webSocketUrlToBool :: KM.KeyMap Value -> KM.KeyMap Value
webSocketUrlToBool o =
  case KM.lookup webSocketKey o of
    Just (String url) ->
      if (T.null url)
        then
          KM.delete webSocketKey o
        else
          KM.insert webSocketKey (Bool True) o -- change to Bool
    _ -> o -- no change if property not present

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dfn-status)
data Status = MkStatus
  { ready :: Bool,
    message :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Status

instance FromJSON Status

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#cookies)
data SameSite
  = Lax
  | Strict
  | None
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SameSite

instance ToJSON SameSite where
  toJSON :: SameSite -> Value
  toJSON = String . txt

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dfn-switch-to-frame)
data FrameReference
  = TopLevelFrame
  | FrameNumber Word16
  | FrameElementId ElementId
  deriving (Show, Eq)

instance ToJSON FrameReference where
  toJSON :: FrameReference -> Value
  toJSON fr =
    object
      ["id" .= toJSON (frameVariant fr)]
    where
      frameVariant =
        \case
          TopLevelFrame -> Null
          FrameNumber n -> Number $ fromIntegral n
          FrameElementId elm -> object [elementFieldName .= elm.id]

-- https://www.w3.org/TR/webdriver2/#elements
elementFieldName :: Key
elementFieldName = "element-6066-11e4-a52e-4f735466cecf"

-- https://www.w3.org/TR/webdriver2/#shadow-root
shadowRootFieldName :: Key
shadowRootFieldName = "shadow-6066-11e4-a52e-4f735466cecf"

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#cookies)
data Cookie = MkCookie
  { name :: Text,
    value :: Text,
    -- optional
    path :: Maybe Text,
    domain :: Maybe Text,
    secure :: Maybe Bool,
    httpOnly :: Maybe Bool,
    sameSite :: Maybe SameSite,
    -- When the cookie expires, specified in seconds since Unix Epoch.
    expiry :: Maybe Int
  }
  deriving (Show, Eq)

instance FromJSON Cookie where
  parseJSON :: Value -> Parser Cookie
  parseJSON = withObject "Cookie" $ \v -> do
    name <- v .: "name"
    value <- v .: "value"
    path <- v .:? "path"
    domain <- v .:? "domain"
    secure <- v .:? "secure"
    httpOnly <- v .:? "httpOnly"
    sameSite <- v .:? "sameSite"
    expiry <- v .:? "expiry"
    pure MkCookie {..}

instance ToJSON Cookie where
  toJSON :: Cookie -> Value
  toJSON MkCookie {name, value, path, domain, secure, httpOnly, sameSite, expiry} =
    object $
      [ "name" .= name,
        "value" .= value
      ]
        <> catMaybes
          [ opt "path" path,
            opt "domain" domain,
            opt "secure" secure,
            opt "httpOnly" httpOnly,
            opt "sameSite" sameSite,
            opt "expiry" expiry
          ]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#locator-strategies)
data Selector
  = CSS Text
  | XPath Text
  | LinkText Text
  | PartialLinkText Text
  | TagName Text
  deriving (Show, Eq)

instance ToJSON Selector where
  toJSON :: Selector -> Value
  toJSON = \case
    CSS css -> sJSON "css selector" css
    XPath xpath -> sJSON "xpath" xpath
    LinkText lt -> sJSON "link text" lt
    PartialLinkText plt -> sJSON "partial link text" plt
    TagName tn -> sJSON "tag name" tn
    where
      sJSON using value = object ["using" .= using, "value" .= value]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#dfn-get-element-rect)
data WindowRect = Rect
  { x :: Int,
    y :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON WindowRect

instance FromJSON WindowRect where
  parseJSON :: Value -> Parser WindowRect
  parseJSON = withObject "WindowRect" $ \v -> do
    x <- v .: "x"
    y <- v .: "y"
    width <- v .: "width"
    height <- v .: "height"
    pure
      Rect
        { x = floor x,
          y = floor y,
          width = floor width,
          height = floor height
        }

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#actions)
newtype Actions = MkActions {actions :: [Action]}
  deriving (Show, Eq, Generic)

instance ToJSON Actions where
  toJSON :: Actions -> Value
  toJSON MkActions {actions} =
    object
      [ "actions" .= actions
      ]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#actions)
data KeyAction
  = PauseKey {duration :: Maybe Int} -- ms
  | KeyDown
      { value :: Text
      }
  | KeyUp
      { value :: Text
      }
  deriving (Show, Eq)

instance ToJSON KeyAction where
  toJSON :: KeyAction -> Value
  toJSON PauseKey {duration} =
    object $
      [ "type" .= ("pause" :: Text)
      ]
        <> catMaybes [opt "duration" duration]
  toJSON KeyDown {value} =
    object
      [ "type" .= ("keyDown" :: Text),
        "value" .= String value
      ]
  toJSON KeyUp {value} =
    object
      [ "type" .= ("keyUp" :: Text),
        "value" .= String value
      ]

-- Pointer subtypes

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#actions)
data Pointer
  = Mouse
  | Pen
  | Touch
  deriving (Show, Eq)

mkLwrTxt :: (Show a) => a -> Value
mkLwrTxt = String . T.toLower . txt

instance ToJSON Pointer where
  toJSON :: Pointer -> Value
  toJSON = mkLwrTxt

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#actions)
data PointerOrigin
  = Viewport
  | OriginPointer
  | OriginElement ElementId
  deriving (Show, Eq)

instance ToJSON PointerOrigin where
  toJSON :: PointerOrigin -> Value
  toJSON = \case
    Viewport -> "viewport"
    OriginPointer -> "pointer"
    OriginElement (MkElement id') -> object ["element" .= id']

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#actions)
data Action
  = NoneAction
      { id :: Text,
        -- the numeric id of the pointing device. This is a positive integer, with the values 0 and 1 reserved for mouse-type pointers.
        noneActions :: [Maybe Int] -- delay
      }
  | Key
      { id :: Text,
        keyActions :: [KeyAction]
        -- https://github.com/jlipps/simple-wd-spec?tab=readme-ov-file#perform-actions
        -- keys codepoint https://www.w3.org/TR/webdriver2/#keyboard-actions
      }
  | Pointer
      { id :: Text,
        subType :: Pointer,
        -- the numeric id of the pointing device. This is a positive integer, with the values 0 and 1 reserved for mouse-type pointers.
        pointerId :: Int,
        pressed :: Set Int, -- pressed buttons
        x :: Int, -- start x location in viewport coordinates.
        y :: Int, -- start y location in viewport coordinates
        actions :: [PointerAction]
      }
  | Wheel
      { id :: Text,
        wheelActions :: [WheelAction]
      }
  deriving (Show, Eq)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#actions)
data WheelAction
  = PauseWheel {duration :: Maybe Int} -- ms
  | Scroll
      { origin :: PointerOrigin,
        x :: Int,
        y :: Int,
        deltaX :: Int,
        deltaY :: Int,
        duration :: Maybe Int -- ms
      }
  deriving (Show, Eq)

instance ToJSON WheelAction where
  toJSON :: WheelAction -> Value
  toJSON wa =
    object $ base <> catMaybes [opt "duration" wa.duration]
    where
      base = case wa of
        PauseWheel _ -> ["type" .= ("pause" :: Text)]
        Scroll
          { origin,
            x,
            y,
            deltaX,
            deltaY
          } ->
            [ "type" .= ("scroll" :: Text),
              "origin" .= origin,
              "x" .= x,
              "y" .= y,
              "deltaX" .= deltaX,
              "deltaY" .= deltaY
            ]

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20251028/#actions)
data PointerAction
  = PausePointer {duration :: Maybe Int} -- ms
  | Up
      { button :: Int,
        width :: Maybe Int,
        height :: Maybe Int,
        pressure :: Maybe Float, -- 0 -> 1
        tangentialPressure :: Maybe Float, -- -1 -> 1
        tiltX :: Maybe Int, -- -90 -> 90
        tiltY :: Maybe Int, -- -90 -> 90
        twist :: Maybe Int, -- 0 -> 359
        altitudeAngle :: Maybe Double, -- 0 -> pi/2
        azimuthAngle :: Maybe Double -- 0 -> 2pi-- button} -- button
      }
  | Down
      { button :: Int,
        width :: Maybe Int,
        height :: Maybe Int,
        pressure :: Maybe Float, -- 0 -> 1
        tangentialPressure :: Maybe Float, -- -1 -> 1
        tiltX :: Maybe Int, -- -90 -> 90
        tiltY :: Maybe Int, -- -90 -> 90
        twist :: Maybe Int, -- 0 -> 359
        altitudeAngle :: Maybe Double, -- 0 -> pi/2
        azimuthAngle :: Maybe Double -- 0 -> 2pi-- button
      }
  | Move
      { origin :: PointerOrigin,
        duration :: Maybe Int, -- ms
        -- where to move to
        -- though the spec seems to indicate width and height are double
        -- gecko driver was blowing up with anything other than int
        width :: Maybe Int,
        height :: Maybe Int,
        pressure :: Maybe Float, -- 0 -> 1
        tangentialPressure :: Maybe Float, -- -1 -> 1
        tiltX :: Maybe Int, -- -90 -> 90
        tiltY :: Maybe Int, -- -90 -> 90
        twist :: Maybe Int, -- 0 -> 359
        altitudeAngle :: Maybe Double, -- 0 -> pi/2
        azimuthAngle :: Maybe Double, -- 0 -> 2pi
        x :: Int,
        y :: Int
      }
  | -- looks like not supported yet by gecko driver 02-02-2025
    -- https://searchfox.org/mozilla-central/source/remote/shared/webdriver/Actions.sys.mjs#2340
    Cancel
  deriving (Show, Eq)

instance ToJSON PointerAction where
  toJSON :: PointerAction -> Value
  toJSON = \case
    PausePointer d ->
      object $
        ["type" .= ("pause" :: Text)]
          <> catMaybes [opt "duration" d]
    Up
      { -- https://www.w3.org/TR/pointerevents/#dom-pointerevent-pointerid
        button,
        width, -- magnitude on the X axis), in CSS pixels (see [CSS21]) -- default = 1
        height, -- (magnitude on the Y axis), in CSS pixels (see [CSS21]) -- default = 1
        pressure, -- 0 - 1
        tangentialPressure, -- -1 -> 1
        tiltX, -- -90 -> 90
        tiltY, -- -90 -> 90
        twist, -- 0 -> 359
        altitudeAngle, -- 0 -> pi/2
        azimuthAngle -- 0 -> 2pi
      } ->
        object $
          [ "type" .= ("pointerUp" :: Text),
            "button" .= button
          ]
            <> catMaybes
              [ opt "height" height,
                opt "width" width,
                opt "pressure" pressure,
                opt "tangentialPressure" tangentialPressure,
                opt "tiltX" tiltX,
                opt "tiltY" tiltY,
                opt "twist" twist,
                opt "altitudeAngle" altitudeAngle,
                opt "azimuthAngle" azimuthAngle
              ]
    Down
      { button,
        width,
        height,
        pressure,
        tangentialPressure, -- -1 -> 1
        tiltX, -- -90 -> 90
        tiltY, -- -90 -> 90
        twist, -- 0 -> 359
        altitudeAngle, -- 0 -> pi/2
        azimuthAngle -- 0 -> 2pi
      } ->
        object $
          [ "type" .= ("pointerDown" :: Text),
            "button" .= button
          ]
            <> catMaybes
              [ opt "height" height,
                opt "width" width,
                opt "pressure" pressure,
                opt "tangentialPressure" tangentialPressure,
                opt "tiltX" tiltX,
                opt "tiltY" tiltY,
                opt "twist" twist,
                opt "altitudeAngle" altitudeAngle,
                opt "azimuthAngle" azimuthAngle
              ]
    Move
      { origin,
        duration,
        width,
        height,
        pressure,
        tangentialPressure, -- -1 -> 1
        tiltX, -- -90 -> 90
        tiltY, -- -90 -> 90
        twist, -- 0 -> 359
        altitudeAngle, -- 0 -> pi/2
        azimuthAngle, -- 0 -> 2pi
        x,
        y
      } ->
        object $
          [ "type" .= ("pointerMove" :: Text),
            "origin" .= origin,
            "x" .= x,
            "y" .= y
          ]
            <> catMaybes
              [ opt "duration" duration,
                opt "height" height,
                opt "width" width,
                opt "pressure" pressure,
                opt "tangentialPressure" tangentialPressure,
                opt "tiltX" tiltX,
                opt "tiltY" tiltY,
                opt "twist" twist,
                opt "altitudeAngle" altitudeAngle,
                opt "azimuthAngle" azimuthAngle
              ]
    -- looks like Cancel not supported yet by gecko driver 02-02-2025
    -- https://searchfox.org/mozilla-central/source/remote/shared/webdriver/Actions.sys.mjs#2340
    Cancel -> object ["type" .= ("pointerCancel" :: Text)]

mkPause :: Maybe Int -> Value
mkPause d = object $ ["type" .= ("pause" :: Text)] <> catMaybes [opt "duration" d]

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = \case
    NoneAction
      { id,
        noneActions
      } ->
        object
          [ "type" .= ("none" :: Text),
            "id" .= id,
            "actions" .= (mkPause <$> noneActions)
          ]
    Key {id, keyActions} ->
      object
        [ "id" .= id,
          "type" .= ("key" :: Text),
          "actions" .= keyActions
        ]
    Pointer
      { subType,
        actions,
        pointerId,
        pressed,
        id,
        x,
        y
      } ->
        object
          [ "id" .= id,
            "type" .= ("pointer" :: Text),
            "subType" .= subType,
            "pointerId" .= pointerId,
            "pressed" .= pressed,
            "x" .= x,
            "y" .= y,
            "actions" .= actions
          ]
    Wheel {id, wheelActions} ->
      object
        [ "id" .= id,
          "type" .= ("wheel" :: Text),
          "actions" .= wheelActions
        ]
