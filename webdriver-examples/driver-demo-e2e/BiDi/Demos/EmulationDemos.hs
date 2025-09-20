module BiDi.Demos.EmulationDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)

{-
Emulation Module Commands (4 total):

1. emulation.setGeolocationOverride - Modifies geolocation characteristics on given contexts/user contexts ✓
2. emulation.setLocaleOverride - Modifies locale on given contexts/user contexts ✓
3. emulation.setScreenOrientationOverride - Emulates screen orientation of given contexts/user contexts ✓
4. emulation.setTimezoneOverride - Modifies timezone on given contexts/user contexts ✓

Implemented demos:
- emulationSetGeolocationOverrideDemo - Set/clear geolocation coordinates and position errors
- emulationSetLocaleOverrideDemo - Set/clear locale overrides (e.g., "en-US", "fr-FR", "ja-JP")
- emulationSetScreenOrientationOverrideDemo - Set/clear screen orientation (portrait/landscape variants)
- emulationSetTimezoneOverrideDemo - Set/clear timezone overrides (e.g., "America/New_York", "Europe/London")
- emulationCompleteWorkflowDemo - Comprehensive demo showing all emulation features together
-}

-- >>> runDemo emulationSetGeolocationOverrideDemo
emulationSetGeolocationOverrideDemo :: BiDiDemo
emulationSetGeolocationOverrideDemo =
  demo "Emulation - Set Geolocation Override" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Test 1: Set geolocation coordinates (New York City)"
      let nycCoordinates = MkGeolocationCoordinates
            { latitude = 40.7128,
              longitude = -74.0060,
              accuracy = Just 10.0,
              altitude = Just 10.0,
              altitudeAccuracy = Just 5.0,
              heading = Just 90.0,
              speed = Just 0.0
            }
      let geoOverride = MkSetGeolocationOverride
            { coordinates = Just nycCoordinates,
              error = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetGeolocationOverride geoOverride
      logShow "Geolocation set to NYC" result1
      pause

      logTxt "Test 2: Set geolocation position error"
      let positionError = MkGeolocationPositionError { errorType = "positionUnavailable" }
      let errorOverride = MkSetGeolocationOverride
            { coordinates = Nothing,
              error = Just positionError,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetGeolocationOverride errorOverride
      logShow "Geolocation error set" result2
      pause

      logTxt "Test 3: Clear geolocation override"
      let clearOverride = MkSetGeolocationOverride
            { coordinates = Nothing,
              error = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result3 <- emulationSetGeolocationOverride clearOverride
      logShow "Geolocation override cleared" result3
      pause

-- >>> runDemo emulationSetLocaleOverrideDemo
emulationSetLocaleOverrideDemo :: BiDiDemo
emulationSetLocaleOverrideDemo =
  demo "Emulation - Set Locale Override" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Test 1: Set locale to French (France)"
      let frenchLocale = MkSetLocaleOverride
            { locale = Just "fr-FR",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetLocaleOverride frenchLocale
      logShow "Locale set to fr-FR" result1
      pause

      logTxt "Test 2: Set locale to Japanese (Japan)"
      let japaneseLocale = MkSetLocaleOverride
            { locale = Just "ja-JP",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetLocaleOverride japaneseLocale
      logShow "Locale set to ja-JP" result2
      pause

      logTxt "Test 3: Set locale to German (Germany)"
      let germanLocale = MkSetLocaleOverride
            { locale = Just "de-DE",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result3 <- emulationSetLocaleOverride germanLocale
      logShow "Locale set to de-DE" result3
      pause

      logTxt "Test 4: Clear locale override"
      let clearLocale = MkSetLocaleOverride
            { locale = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result4 <- emulationSetLocaleOverride clearLocale
      logShow "Locale override cleared" result4
      pause

-- >>> runDemo emulationSetScreenOrientationOverrideDemo
emulationSetScreenOrientationOverrideDemo :: BiDiDemo
emulationSetScreenOrientationOverrideDemo =
  demo "Emulation - Set Screen Orientation Override" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      {-  TODO Not supported by geckodriver - add expectations when we gget to errors
      logTxt "Test 1: Set orientation to portrait primary"
      let portraitOrientation = MkScreenOrientationOverride
            { natural = PortraitNatural,
              screenOrientationType = PortraitPrimary
            }
      let portraitOverride = MkSetScreenOrientationOverride
            { screenOrientation = Just portraitOrientation,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetScreenOrientationOverride portraitOverride
      logShow "Orientation set to portrait primary" result1
      pause

      logTxt "Test 2: Set orientation to landscape primary"
      let landscapeOrientation = MkScreenOrientationOverride
            { natural = LandscapeNatural,
              screenOrientationType = LandscapePrimary
            }
      let landscapeOverride = MkSetScreenOrientationOverride
            { screenOrientation = Just landscapeOrientation,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetScreenOrientationOverride landscapeOverride
      logShow "Orientation set to landscape primary" result2
      pause

      logTxt "Test 3: Set orientation to portrait secondary"
      let portraitSecondaryOrientation = MkScreenOrientationOverride
            { natural = PortraitNatural,
              screenOrientationType = PortraitSecondary
            }
      let portraitSecondaryOverride = MkSetScreenOrientationOverride
            { screenOrientation = Just portraitSecondaryOrientation,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result3 <- emulationSetScreenOrientationOverride portraitSecondaryOverride
      logShow "Orientation set to portrait secondary" result3
      pause

      logTxt "Test 4: Clear orientation override"
      let clearOrientation = MkSetScreenOrientationOverride
            { screenOrientation = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result4 <- emulationSetScreenOrientationOverride clearOrientation
      logShow "Orientation override cleared" result4
      -}
      pause

-- >>> runDemo emulationSetTimezoneOverrideDemo
emulationSetTimezoneOverrideDemo :: BiDiDemo
emulationSetTimezoneOverrideDemo =
  demo "Emulation - Set Timezone Override" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      {-  TODO Not supported by geckodriver - add expectations when we gget to errors  
      logTxt "Test 1: Set timezone to New York"
      let nyTimezone = MkSetTimezoneOverride
            { timezone = Just "America/New_York",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetTimezoneOverride nyTimezone
      logShow "Timezone set to America/New_York" result1
      pause

      logTxt "Test 2: Set timezone to London"
      let londonTimezone = MkSetTimezoneOverride
            { timezone = Just "Europe/London",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetTimezoneOverride londonTimezone
      logShow "Timezone set to Europe/London" result2
      pause

      logTxt "Test 3: Set timezone to Tokyo"
      let tokyoTimezone = MkSetTimezoneOverride
            { timezone = Just "Asia/Tokyo",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result3 <- emulationSetTimezoneOverride tokyoTimezone
      logShow "Timezone set to Asia/Tokyo" result3
      pause

      logTxt "Test 4: Set timezone using offset (+05:30 for India)"
      let offsetTimezone = MkSetTimezoneOverride
            { timezone = Just "+05:30",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result4 <- emulationSetTimezoneOverride offsetTimezone
      logShow "Timezone set to +05:30" result4
      pause

      logTxt "Test 5: Clear timezone override"
      let clearTimezone = MkSetTimezoneOverride
            { timezone = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result5 <- emulationSetTimezoneOverride clearTimezone
      logShow "Timezone override cleared" result5
      -}
      pause

-- >>> runDemo emulationCompleteWorkflowDemo
emulationCompleteWorkflowDemo :: BiDiDemo
emulationCompleteWorkflowDemo =
  demo "Emulation - Complete Workflow Demo" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      {-  TODO Not supported by geckodriver - add expectations when we get to errors 
      logTxt "=== Creating multiple browsing contexts for emulation demo ==="
      bc1 <- rootContext utils cmds
      bc2 <- newWindowContext utils cmds
      pause

      logTxt "=== Setting up context 1 with NYC geolocation and US locale ==="
      -- Set NYC geolocation
      let nycCoordinates = MkGeolocationCoordinates
            { latitude = 40.7128,
              longitude = -74.0060,
              accuracy = Just 10.0,
              altitude = Nothing,
              altitudeAccuracy = Nothing,
              heading = Nothing,
              speed = Nothing
            }
      let geoOverride1 = MkSetGeolocationOverride
            { coordinates = Just nycCoordinates,
              error = Nothing,
              contexts = Just [bc1],
              userContexts = Nothing
            }
      geoResult1 <- emulationSetGeolocationOverride geoOverride1
      logShow "Context 1 geolocation" geoResult1

      -- Set US locale
      let usLocale = MkSetLocaleOverride
            { locale = Just "en-US",
              contexts = Just [bc1],
              userContexts = Nothing
            }
      localeResult1 <- emulationSetLocaleOverride usLocale
      logShow "Context 1 locale" localeResult1

      -- Set New York timezone
      let nyTimezone = MkSetTimezoneOverride
            { timezone = Just "America/New_York",
              contexts = Just [bc1],
              userContexts = Nothing
            }
      timezoneResult1 <- emulationSetTimezoneOverride nyTimezone
      logShow "Context 1 timezone" timezoneResult1

      -- Set portrait orientation
      let portraitOrientation = MkScreenOrientationOverride
            { natural = PortraitNatural,
              screenOrientationType = PortraitPrimary
            }
      let orientationOverride1 = MkSetScreenOrientationOverride
            { screenOrientation = Just portraitOrientation,
              contexts = Just [bc1],
              userContexts = Nothing
            }
      orientationResult1 <- emulationSetScreenOrientationOverride orientationOverride1
      logShow "Context 1 orientation" orientationResult1
      pause

      logTxt "=== Setting up context 2 with London geolocation and UK locale ==="
      -- Set London geolocation
      let londonCoordinates = MkGeolocationCoordinates
            { latitude = 51.5074,
              longitude = -0.1278,
              accuracy = Just 15.0,
              altitude = Just 35.0,
              altitudeAccuracy = Just 10.0,
              heading = Just 180.0,
              speed = Just 5.0
            }
      let geoOverride2 = MkSetGeolocationOverride
            { coordinates = Just londonCoordinates,
              error = Nothing,
              contexts = Just [bc2],
              userContexts = Nothing
            }
      geoResult2 <- emulationSetGeolocationOverride geoOverride2
      logShow "Context 2 geolocation" geoResult2

      -- Set UK locale
      let ukLocale = MkSetLocaleOverride
            { locale = Just "en-GB",
              contexts = Just [bc2],
              userContexts = Nothing
            }
      localeResult2 <- emulationSetLocaleOverride ukLocale
      logShow "Context 2 locale" localeResult2

      -- Set London timezone
      let londonTimezone = MkSetTimezoneOverride
            { timezone = Just "Europe/London",
              contexts = Just [bc2],
              userContexts = Nothing
            }
      timezoneResult2 <- emulationSetTimezoneOverride londonTimezone
      logShow "Context 2 timezone" timezoneResult2

      -- Set landscape orientation
      let landscapeOrientation = MkScreenOrientationOverride
            { natural = LandscapeNatural,
              screenOrientationType = LandscapePrimary
            }
      let orientationOverride2 = MkSetScreenOrientationOverride
            { screenOrientation = Just landscapeOrientation,
              contexts = Just [bc2],
              userContexts = Nothing
            }
      orientationResult2 <- emulationSetScreenOrientationOverride orientationOverride2
      logShow "Context 2 orientation" orientationResult2
      pause

      logTxt "=== Clearing all emulation overrides ==="
      -- Clear context 1 overrides
      let clearGeo1 = MkSetGeolocationOverride
            { coordinates = Nothing,
              error = Nothing,
              contexts = Just [bc1],
              userContexts = Nothing
            }
      clearGeoResult1 <- emulationSetGeolocationOverride clearGeo1

      let clearLocale1 = MkSetLocaleOverride
            { locale = Nothing,
              contexts = Just [bc1],
              userContexts = Nothing
            }
      clearLocaleResult1 <- emulationSetLocaleOverride clearLocale1

      let clearTimezone1 = MkSetTimezoneOverride
            { timezone = Nothing,
              contexts = Just [bc1],
              userContexts = Nothing
            }
      clearTimezoneResult1 <- emulationSetTimezoneOverride clearTimezone1

      let clearOrientation1 = MkSetScreenOrientationOverride
            { screenOrientation = Nothing,
              contexts = Just [bc1],
              userContexts = Nothing
            }
      clearOrientationResult1 <- emulationSetScreenOrientationOverride clearOrientation1

      logShow "Context 1 overrides cleared" (clearGeoResult1, clearLocaleResult1, clearTimezoneResult1, clearOrientationResult1)

      -- Clear context 2 overrides
      let clearGeo2 = MkSetGeolocationOverride
            { coordinates = Nothing,
              error = Nothing,
              contexts = Just [bc2],
              userContexts = Nothing
            }
      let clearLocale2 = MkSetLocaleOverride
            { locale = Nothing,
              contexts = Just [bc2],
              userContexts = Nothing
            }
      let clearTimezone2 = MkSetTimezoneOverride
            { timezone = Nothing,
              contexts = Just [bc2],
              userContexts = Nothing
            }
      let clearOrientation2 = MkSetScreenOrientationOverride
            { screenOrientation = Nothing,
              contexts = Just [bc2],
              userContexts = Nothing
            }

      clearGeoResult2 <- emulationSetGeolocationOverride clearGeo2
      clearLocaleResult2 <- emulationSetLocaleOverride clearLocale2
      clearTimezoneResult2 <- emulationSetTimezoneOverride clearTimezone2
      clearOrientationResult2 <- emulationSetScreenOrientationOverride clearOrientation2

      logShow "Context 2 overrides cleared" (clearGeoResult2, clearLocaleResult2, clearTimezoneResult2, clearOrientationResult2)
      pause

      logTxt "=== Cleaning up contexts ==="
      -}
      closeContext utils cmds bc2
