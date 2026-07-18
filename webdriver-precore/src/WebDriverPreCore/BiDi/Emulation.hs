module WebDriverPreCore.BiDi.Emulation
  ( SetGeolocationOverride (..),
    SetLocaleOverride (..),
    SetScreenOrientationOverride (..),
    SetScreenSettingsOverride (..),
    SetTimezoneOverride (..),
    SetTouchOverride (..),
    SetForcedColorsModeThemeOverride (..),
    SetNetworkConditions (..),
    SetUserAgentOverride (..),
    ScriptingOverride (..),
    SetScriptingEnabled (..),
    SetScrollbarTypeOverride (..),
    ScrollbarType (..),
    GeoProperty (..),
    GeolocationCoordinates (..),
    GeolocationPositionError (..),
    ScreenArea (..),
    ScreenOrientationOverride (..),
    ScreenOrientationNatural (..),
    ScreenOrientationType (..),
    ForcedColorsModeTheme (..),
    NetworkConditions (..),
    NetworkConditionsOffline (..),
  )
where

import AesonUtils (opt)
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSUInt, UserContext)

-- ######### Remote #########

-- Note: emulation module does not have a local end

data GeoProperty
  = Coordinates GeolocationCoordinates
  | ClearCoodrdinates
  | PositionError GeolocationPositionError
  deriving (Show, Eq, Generic)

data SetGeolocationOverride = MkSetGeolocationOverride
  { override :: GeoProperty,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetGeolocationOverride where
  toJSON :: SetGeolocationOverride -> Value
  toJSON MkSetGeolocationOverride {override, contexts, userContexts} =
    object $ geoField <> catMaybes [opt "contexts" contexts, opt "userContexts" userContexts]
    where
      geoField = case override of
        Coordinates coords -> [("coordinates" .= coords)]
        ClearCoodrdinates -> [("coordinates" .= Null)]
        PositionError err -> [("error" .= err)]

data SetLocaleOverride = MkSetLocaleOverride
  { locale :: Maybe Text,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetLocaleOverride where
  toJSON :: SetLocaleOverride -> Value
  toJSON MkSetLocaleOverride {locale, contexts, userContexts} =
    object $
      ["locale" .= locale]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

data SetScreenOrientationOverride = MkSetScreenOrientationOverride
  { screenOrientation :: Maybe ScreenOrientationOverride,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetScreenOrientationOverride where
  toJSON :: SetScreenOrientationOverride -> Value
  toJSON MkSetScreenOrientationOverride {screenOrientation, contexts, userContexts} =
    object $
      ["screenOrientation" .= screenOrientation]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

data SetScreenSettingsOverride = MkSetScreenSettingsOverride
  { screenArea :: Maybe ScreenArea,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

-- Note: screenArea is a required field that can be null, while contexts and userContexts are optional
-- Required nullable fields must be included in the JSON with their value (even if null)
-- Optional fields are omitted when Nothing
instance ToJSON SetScreenSettingsOverride where
  toJSON :: SetScreenSettingsOverride -> Value
  toJSON MkSetScreenSettingsOverride {screenArea, contexts, userContexts} =
    object $
      ["screenArea" .= screenArea]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

data SetTimezoneOverride = MkSetTimezoneOverride
  { timezone :: Maybe Text,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetTimezoneOverride where
  toJSON :: SetTimezoneOverride -> Value
  toJSON MkSetTimezoneOverride {timezone, contexts, userContexts} =
    object $
      ["timezone" .= timezone]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

data SetForcedColorsModeThemeOverride = MkSetForcedColorsModeThemeOverride
  { theme :: Maybe ForcedColorsModeTheme,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetForcedColorsModeThemeOverride where
  toJSON :: SetForcedColorsModeThemeOverride -> Value
  toJSON MkSetForcedColorsModeThemeOverride {theme, contexts, userContexts} =
    object $
      ["theme" .= theme]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

data SetNetworkConditions = MkSetNetworkConditions
  { networkConditions :: Maybe NetworkConditions,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetNetworkConditions where
  toJSON :: SetNetworkConditions -> Value
  toJSON MkSetNetworkConditions {networkConditions, contexts, userContexts} =
    object $
      ["networkConditions" .= networkConditions]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

data SetUserAgentOverride = MkSetUserAgentOverride
  { userAgent :: Maybe Text,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetUserAgentOverride where
  toJSON :: SetUserAgentOverride -> Value
  toJSON MkSetUserAgentOverride {userAgent, contexts, userContexts} =
    object $
      ["userAgent" .= userAgent]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

-- | Scripting override for setScriptingEnabled command
-- Per spec: enabled can be false or null (not true)
data ScriptingOverride
  = ForceDisableScripting -- ^ Encode as false - explicitly disable scripting
  | RestoreDefaultScripting -- ^ Encode as null - restore to initial browser configuration
  deriving (Show, Eq, Generic)

instance ToJSON ScriptingOverride where
  toJSON :: ScriptingOverride -> Value
  toJSON = \case
    ForceDisableScripting -> Bool False
    RestoreDefaultScripting -> Null

-- | for setScriptingEnabled command
data SetScriptingEnabled = MkSetScriptingEnabled
  { enabled :: ScriptingOverride,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetScriptingEnabled where
  toJSON :: SetScriptingEnabled -> Value
  toJSON MkSetScriptingEnabled {enabled, contexts, userContexts} =
    object $
      ["enabled" .= enabled]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

-- | Parameters for emulation.setTouchOverride command
-- maxTouchPoints: (js-uint .ge 1) / null - the maximum number of touch points to emulate, or null to clear
data SetTouchOverride = MkSetTouchOverride
  { maxTouchPoints :: Maybe JSUInt,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetTouchOverride where
  toJSON :: SetTouchOverride -> Value
  toJSON MkSetTouchOverride {maxTouchPoints, contexts, userContexts} =
    object $
      ["maxTouchPoints" .= maxTouchPoints]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]

data ScreenArea = MkScreenArea
  { width :: JSUInt,
    height :: JSUInt
  }
  deriving (Show, Eq, Generic)

instance ToJSON ScreenArea

data GeolocationCoordinates = MkGeolocationCoordinates
  { latitude :: Float, -- -90.0 to 90.0
    longitude :: Float, -- -180.0 to 180.0
    accuracy :: Maybe Float, -- >= 0.0, defaults to 1.0
    altitude :: Maybe Float,
    altitudeAccuracy :: Maybe Float, -- >= 0.0
    heading :: Maybe Float, -- 0.0 to 360.0
    speed :: Maybe Float -- >= 0.0
  }
  deriving (Show, Eq, Generic)

instance ToJSON GeolocationCoordinates

newtype GeolocationPositionError = MkGeolocationPositionError
  { errorType :: Text -- "positionUnavailable"
  }
  deriving (Show, Eq, Generic)

instance ToJSON GeolocationPositionError where
  toJSON :: GeolocationPositionError -> Value
  toJSON MkGeolocationPositionError {errorType} =
    object ["type" .= errorType]

data ScreenOrientationOverride = MkScreenOrientationOverride
  { natural :: ScreenOrientationNatural,
    screenOrientationType :: ScreenOrientationType
  }
  deriving (Show, Eq, Generic)

instance ToJSON ScreenOrientationOverride where
  toJSON :: ScreenOrientationOverride -> Value
  toJSON MkScreenOrientationOverride {natural, screenOrientationType} =
    object
      [ "natural" .= natural,
        "type" .= screenOrientationType
      ]

data ScreenOrientationNatural = PortraitNatural | LandscapeNatural
  deriving (Show, Eq, Generic)

instance ToJSON ScreenOrientationNatural where
  toJSON :: ScreenOrientationNatural -> Value
  toJSON = \case
    PortraitNatural -> "portrait"
    LandscapeNatural -> "landscape"

data ScreenOrientationType
  = PortraitPrimary
  | PortraitSecondary
  | LandscapePrimary
  | LandscapeSecondary
  deriving (Show, Eq, Generic)

instance ToJSON ScreenOrientationType where
  toJSON :: ScreenOrientationType -> Value
  toJSON = \case
    PortraitPrimary -> "portrait-primary"
    PortraitSecondary -> "portrait-secondary"
    LandscapePrimary -> "landscape-primary"
    LandscapeSecondary -> "landscape-secondary"

data ForcedColorsModeTheme = ForcedColorsLight | ForcedColorsDark
  deriving (Show, Eq, Generic)

instance ToJSON ForcedColorsModeTheme where
  toJSON :: ForcedColorsModeTheme -> Value
  toJSON = \case
    ForcedColorsLight -> "light"
    ForcedColorsDark -> "dark"

newtype NetworkConditions = MkNetworkConditions NetworkConditionsOffline
  deriving (Show, Eq, Generic)

instance ToJSON NetworkConditions where
  toJSON :: NetworkConditions -> Value
  toJSON (MkNetworkConditions offline) = toJSON offline

newtype NetworkConditionsOffline = MkNetworkConditionsOffline
  { networkConditionsType :: Text -- "offline"
  }
  deriving (Show, Eq, Generic)

instance ToJSON NetworkConditionsOffline where
  toJSON :: NetworkConditionsOffline -> Value
  toJSON _ = object ["type" .= "offline"]

-- | Scrollbar type for emulation.setScrollbarTypeOverride command
-- Per spec: scrollbarType can be "classic" / "overlay" / null
-- Classic and Overlay encode as their string values, PlatformDefault encodes as null
data ScrollbarType
  = Classic -- Always-visible scrollbars (typical on Windows/Linux)
  | Overlay -- Auto-hiding scrollbars (typical on macOS)
  | PlatformDefault -- Restore platform default (encodes as null)
  deriving (Show, Eq, Generic)

instance ToJSON ScrollbarType where
  toJSON :: ScrollbarType -> Value
  toJSON = \case
    Classic -> String "classic"
    Overlay -> String "overlay"
    PlatformDefault -> Null

data SetScrollbarTypeOverride = MkSetScrollbarTypeOverride
  { scrollbarType :: ScrollbarType,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetScrollbarTypeOverride where
  toJSON :: SetScrollbarTypeOverride -> Value
  toJSON MkSetScrollbarTypeOverride {scrollbarType, contexts, userContexts} =
    object $
      ["scrollbarType" .= scrollbarType]
        <> catMaybes
          [ opt "contexts" contexts,
            opt "userContexts" userContexts
          ]
