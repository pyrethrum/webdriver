module WebDriverPreCore.BiDi.Emulation
  ( 
    SetGeolocationOverride (..),
    SetLocaleOverride (..),
    SetScreenOrientationOverride (..),
    SetTimezoneOverride (..),
    GeolocationCoordinates (..),
    GeolocationPositionError (..),
    ScreenOrientationOverride  (..),
    ScreenOrientationNatural  (..),
    ScreenOrientationType  (..)
  )
where

{-
create types to represent the remote  end for emulation:

1. preface singleton data constructors (ie the constructor for types with only one type constructor) with Mk
2. use newtypes where possible
3. ordering - order types such that types that are used by a type are declared immediately below that type in the order they are used
4. derive Show, Eq and Generic for all types
5. use Text rather than String
5. use the cddl in this file remote first under the -- ######### Remote ######### header
-}

import Data.Maybe (Maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, UserContext)
import Prelude (Eq, Float, Show)
import Data.Aeson (ToJSON (..), Value)
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase)

-- ######### Remote #########

-- Note: emulation module does not have a local end

data SetGeolocationOverride = MkSetGeolocationOverride
  { coordinates :: Maybe GeolocationCoordinates,
    error :: Maybe GeolocationPositionError,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

data SetLocaleOverride = MkSetLocaleOverride
  { locale :: Maybe Text,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

data SetScreenOrientationOverride = MkSetScreenOrientationOverride
  { screenOrientation :: Maybe ScreenOrientationOverride,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

data SetTimezoneOverride = MkSetTimezoneOverride
  { timezone :: Maybe Text,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

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

newtype GeolocationPositionError = MkGeolocationPositionError
  { errorType :: Text -- "positionUnavailable"
  }
  deriving (Show, Eq, Generic)

data ScreenOrientationOverride = MkScreenOrientationOverride
  { natural :: ScreenOrientationNatural,
    screenOrientationType :: ScreenOrientationType
  }
  deriving (Show, Eq, Generic)

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
  toJSON = enumCamelCase


instance ToJSON SetGeolocationOverride
instance ToJSON SetLocaleOverride
instance ToJSON SetScreenOrientationOverride
instance ToJSON SetTimezoneOverride
instance ToJSON GeolocationCoordinates
instance ToJSON GeolocationPositionError
instance ToJSON ScreenOrientationOverride
