module WebDriverPreCore.BiDi.Emulation
  ( EmulationCommand (..),
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
import WebDriverPreCore.BiDi.Browser (UserContext)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext)
import Prelude (Eq, Float, Show)

-- ######### Remote #########
-- Note: emulation module does not have a local end

data EmulationCommand
  = SetGeolocationOverride
      { coordinates :: Maybe GeolocationCoordinates,
        error :: Maybe GeolocationPositionError,
        contexts :: Maybe [BrowsingContext],
        userContexts :: Maybe [UserContext]
      }
  | SetLocaleOverride
      { locale :: Maybe Text,
        contexts :: Maybe [BrowsingContext],
        userContexts :: Maybe [UserContext]
      }
  | SetScreenOrientationOverride
      { screenOrientation :: Maybe ScreenOrientationOverride,
        contexts :: Maybe [BrowsingContext],
        userContexts :: Maybe [UserContext]
      }
  | SetTimezoneOverride
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

data ScreenOrientationNatural = Portrait | Landscape
  deriving (Show, Eq, Generic)

data ScreenOrientationType
  = PortraitPrimary
  | PortraitSecondary
  | LandscapePrimary
  | LandscapeSecondary
  deriving (Show, Eq, Generic)

{-

7.4.2.2. The emulation.setLocaleOverride Command

The emulation.setLocaleOverride command modifies locale on the given top-level traversables or user contexts.

Command Type

    emulation.SetLocaleOverride = (
      method: "emulation.setLocaleOverride",
      params: emulation.SetLocaleOverrideParameters
    )

    emulation.SetLocaleOverrideParameters = {
      locale: text / null,
      ? contexts: [+browsingContext.BrowsingContext],
      ? userContexts: [+browser.UserContext],
    }

Result Type
    EmptyResult

The DefaultLocale algorithm is implementation defined. A WebDriver-BiDi remote end must have an implementation that runs the following steps:

    Let emulated locale be null.

    Let realm be current Realm Record.

    Let environment settings be the environment settings object whose realm execution context’s Realm component is realm.

    Let related navigables be the result of get related navigables given environment settings.

    For each navigable of related navigables:

        Let top-level traversable be navigable’s top-level traversable.

        Let user context be top-level traversable’s associated user context.

        If locale overrides map contains top-level traversable, set emulated locale to locale overrides map[top-level traversable].

        Otherwise, if locale overrides map contains user context, set emulated locale to locale overrides map[user context].

    If emulated locale is not null, return emulated locale.

    Return the result of implementation-defined steps in accordance with the requirements of the DefaultLocale specification.

The remote end steps with command parameters are:

    If command parameters contains "userContexts" and command parameters contains "context", return error with error code invalid argument.

    If command parameters doesn’t contain "userContexts" and command parameters doesn’t contain "context", return error with error code invalid argument.

    Let emulated locale be null.

    If command parameters contains "locale":

        Set emulated locale to command parameters["locale"].

        If IsStructurallyValidLanguageTag(emulated locale) returns false, return error with error code invalid argument.

    Let navigables be a set.

    If the contexts field of command parameters is present:

        Let navigables be the result of trying to get valid top-level traversables by ids with command parameters["contexts"].

    Otherwise:

        Assert the userContexts field of command parameters is present.

        Let user contexts be the result of trying to get valid user contexts with command parameters["userContexts"].

        For each user context of user contexts:

            Set locale overrides map[user context] to emulated locale.

            For each top-level traversable of the list of all top-level traversables whose associated user context is user context:

                Append top-level traversable to navigables.

    For each navigable of navigables:

        Set locale overrides map[navigable] to emulated locale.

    Return success with data null.

7.4.2.3. The emulation.setScreenOrientationOverride Command

The emulation.setScreenOrientationOverride command emulates screen orientation of the given top-level traversables or user contexts.

Command Type

    emulation.SetScreenOrientationOverride = (
      method: "emulation.setScreenOrientationOverride",
      params: emulation.SetScreenOrientationOverrideParameters
    )

    emulation.ScreenOrientationNatural = "portrait" / "landscape"
    emulation.ScreenOrientationType = "portrait-primary" / "portrait-secondary" / "landscape-primary" / "landscape-secondary"

    emulation.ScreenOrientation = {
      natural: emulation.ScreenOrientationNatural,
      type: emulation.ScreenOrientationType
    }

    emulation.SetScreenOrientationOverrideParameters = {
      screenOrientation: emulation.ScreenOrientation / null,
      ? contexts: [+browsingContext.BrowsingContext],
      ? userContexts: [+browser.UserContext],
    }

Result Type
    EmptyResult

To set emulated screen orientation given navigable and emulated screen orientation:

Move this algorithm to screen orientation specification.

    If emulated screen orientation is null:

        Set navigable’s current orientation angle to implementation-defined default.

        Set navigable’s current orientation type to implementation-defined default.

    Otherwise:

        Let emulated orientation type be emulated screen orientation["type"].

        Let emulated orientation angle be the angle associated with emulated orientation type for screens with emulated screen orientation["natural"] orientations as defined in screen orientation values lists.

        Set current orientation angle to emulated orientation angle.

        Set current orientation type to emulated orientation type.

    Run the screen orientation change steps with the navigable’s active document.

The remote end steps with command parameters are:

    If the implementation is unable to adjust the screen orientations parameters with the given command parameters for any reason, return error with error code unsupported operation.

    If command parameters contains "userContexts" and command parameters contains "contexts", return error with error code invalid argument.

    If command parameters doesn’t contain "userContexts" and command parameters doesn’t contain "contexts", return error with error code invalid argument.

    Let emulated screen orientation be command parameters["screenOrientation"].

    Let navigables be a set.

    If the contexts field of command parameters is present:

        Let navigables be the result of trying to get valid top-level traversables by ids with command parameters["contexts"].

    Otherwise, if the userContexts field of command parameters is present:

        Let user contexts be the result of trying to get valid user contexts with command parameters["userContexts"].

        For each user context of user contexts:

            Set screen orientation overrides map[user context] to emulated screen orientation.

            For each top-level traversable of the list of all top-level traversables whose associated user context is user context:

                Append top-level traversable to navigables.

    For each navigable of navigables:

        Set emulated screen orientation with navigable and emulated screen orientation.

    Return success with data null.

7.4.2.4. The emulation.setTimezoneOverride Command

The emulation.setTimezoneOverride command modifies timezone on the given top-level traversables or user contexts.

Command Type

    emulation.SetTimezoneOverride = (
      method: "emulation.setTimezoneOverride",
      params: emulation.SetTimezoneOverrideParameters
    )

    emulation.SetTimezoneOverrideParameters = {
      timezone: text / null,
      ? contexts: [+browsingContext.BrowsingContext],
      ? userContexts: [+browser.UserContext],
    }

Result Type
    EmptyResult

The SystemTimeZoneIdentifier algorithm is implementation defined. A WebDriver-BiDi remote end must have an implementation that runs the following steps:

    Let emulated timezone be null.

    Let realm be current Realm Record.

    Let environment settings be the environment settings object whose realm execution context’s Realm component is realm.

    Let related navigables be the result of get related navigables given environment settings.

    For each navigable of related navigables:

        Let top-level traversable be navigable’s top-level traversable.

        Let user context be top-level traversable’s associated user context.

        If timezone overrides map contains top-level traversable, set emulated timezone to timezone overrides map[top-level traversable].

        Otherwise, if timezone overrides map contains user context, set emulated timezone to timezone overrides map[user context].

    If emulated timezone is not null, return emulated timezone.

    Return the result of implementation-defined steps in accordance with the requirements of the SystemTimeZoneIdentifier specification.

The remote end steps with command parameters are:

    If command parameters contains "userContexts" and command parameters contains "context", return error with error code invalid argument.

    If command parameters doesn’t contain "userContexts" and command parameters doesn’t contain "context", return error with error code invalid argument.

    Let emulated timezone be null.

    If command parameters contains "timezone":

        Set emulated timezone to command parameters["timezone"].

        If IsTimeZoneOffsetString(emulated timezone) returns false and AvailableNamedTimeZoneIdentifiers does not contain emulated timezone, return error with error code invalid argument.

    Let navigables be a set.

    If the contexts field of command parameters is present:

        Let navigables be the result of trying to get valid top-level traversables by ids with command parameters["contexts"].

    Otherwise:

        Assert the userContexts field of command parameters is present.

        Let user contexts be the result of trying to get valid user contexts with command parameters["userContexts"].

        For each user context of user contexts:

            Set timezone overrides map[user context] to emulated timezone.

            For each top-level traversable of the list of all top-level traversables whose associated user context is user context:

                Append top-level traversable to navigables.

    For each navigable of navigables:

        Set timezone overrides map[navigable] to emulated timezone.

    Return success with data null.

   -}
