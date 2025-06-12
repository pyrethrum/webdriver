
module WebDriverPreCore.BiDi.Emulation (
  EmulationCommand (..),
  SetGeolocationOverride (..),
  SetGeolocationOverrideParams (..),
  GeolocationCoordinates (..),
  GeolocationPositionError (..),
) where

{-
create types to represent the remote  end for emulation:

1. preface singleton data constructors (ie the constructor for types with only one type constructor) with Mk
2. use newtypes where possible
3. ordering - order types such that types that are used by a type are declared immediately below that type in the order they are used
4. derive Show, Eq and Generic for all types
5. use Text rather than String
5. use the cddl in this file remote first under the -- ######### Remote ######### header 
-}

import GHC.Generics (Generic)
import Data.Maybe (Maybe)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext)
import WebDriverPreCore.BiDi.Browser (UserContext)
import Prelude (Show, Eq, Float)
import Data.Text (Text)

-- ######### Remote #########
-- Note: emulation module does not have a local end

data EmulationCommand = MkEmulationSetGeolocationOverride
  deriving (Show, Eq, Generic)

data SetGeolocationOverride = MkSetGeolocationOverride
  { method :: Text
  , params :: SetGeolocationOverrideParams
  } deriving (Show, Eq, Generic)

data SetGeolocationOverrideParams = MkSetGeolocationOverrideParams
  { coordinates :: Maybe GeolocationCoordinates
  , error :: Maybe GeolocationPositionError
  , contexts :: Maybe [BrowsingContext]
  , userContexts :: Maybe [UserContext]
  } deriving (Show, Eq, Generic)

data GeolocationCoordinates = MkGeolocationCoordinates
  { latitude :: Float  -- -90.0 to 90.0
  , longitude :: Float  -- -180.0 to 180.0
  , accuracy :: Maybe Float  -- >= 0.0, defaults to 1.0
  , altitude :: Maybe Float
  , altitudeAccuracy :: Maybe Float  -- >= 0.0
  , heading :: Maybe Float  -- 0.0 to 360.0
  , speed :: Maybe Float  -- >= 0.0
  } deriving (Show, Eq, Generic)

newtype GeolocationPositionError = MkGeolocationPositionError
  { errorType :: Text  -- "positionUnavailable"
  } deriving (Show, Eq, Generic)

