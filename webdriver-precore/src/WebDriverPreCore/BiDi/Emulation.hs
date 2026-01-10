module WebDriverPreCore.BiDi.Emulation
  ( 
    SetGeolocationOverride (..),
    SetLocaleOverride (..),
    SetScreenOrientationOverride (..),
    SetScreenSettingsOverride (..),
    SetTimezoneOverride (..),
    SetForcedColorsModeThemeOverride (..),
    SetNetworkConditions (..),
    SetUserAgentOverride (..),
    SetScriptingEnabled (..),
    GeoProperty (..),
    GeolocationCoordinates (..),
    GeolocationPositionError (..),
    ScreenArea (..),
    ScreenOrientationOverride  (..),
    ScreenOrientationNatural  (..),
    ScreenOrientationType  (..),
    ForcedColorsModeTheme (..),
    NetworkConditions (..),
    NetworkConditionsOffline (..)
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, UserContext, JSUInt)
import Data.Aeson (ToJSON (..), object, (.=), Value(..))
import AesonUtils (opt, toJSONOmitNothing)

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
  toJSON = toJSONOmitNothing

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
  toJSON = toJSONOmitNothing

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

data SetScriptingEnabled = MkSetScriptingEnabled
  { enabled :: Maybe Bool,
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
  toJSON _ = toJSON (("type", "offline") :: (Text, Text))
