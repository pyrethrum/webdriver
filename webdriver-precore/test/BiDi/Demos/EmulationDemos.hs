module BiDi.Demos.EmulationDemos where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoActions (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)


-- >>> runDemo emulationSetGeolocationOverrideDemo
emulationSetGeolocationOverrideDemo :: BiDiDemo
emulationSetGeolocationOverrideDemo =
  demo "Emulation - Set Geolocation Override" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

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
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

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
      pause

-- >>> runDemo emulationSetScreenSettingsOverrideDemo
-- *** Exception: Error executing BiDi command: With JSON: 
-- {
--     "id": 2,
--     "method": "emulation.setScreenSettingsOverride",
--     "params": {
--         "contexts": [
--             "a09acf51-3f41-4728-a27a-8864b7dcee8d"
--         ],
--         "screenArea": {
--             "height": 1080,
--             "width": 1920
--         }
--     }
-- }
-- BiDi driver error: 
-- MkDriverError
--   { id = Just 2
--   , error = UnknownCommand
--   , description = "The command sent is not known"
--   , message = "emulation.setScreenSettingsOverride"
--   , stacktrace =
--       Just
--         "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nUnknownCommandError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:944:5\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:407:13\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n"
--   , extensions = MkEmptyResult { extensible = fromList [] }
--   }
emulationSetScreenSettingsOverrideDemo :: BiDiDemo
emulationSetScreenSettingsOverrideDemo =
  demo "Emulation - Set Screen Settings Override - since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Set screen area to 1920x1080 (Full HD)"
      let fullHDArea = MkScreenArea
            { width = MkJSUInt 1920,
              height = MkJSUInt 1080
            }
      let fullHDOverride = MkSetScreenSettingsOverride
            { screenArea = Just fullHDArea,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetScreenSettingsOverride fullHDOverride
      logShow "Screen area set to 1920x1080" result1
      pause

      logTxt "Test 2: Set screen area to 1366x768 (HD)"
      let hdArea = MkScreenArea
            { width = MkJSUInt 1366,
              height = MkJSUInt 768
            }
      let hdOverride = MkSetScreenSettingsOverride
            { screenArea = Just hdArea,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetScreenSettingsOverride hdOverride
      logShow "Screen area set to 1366x768" result2
      pause

      logTxt "Test 3: Set screen area to 375x667 (iPhone SE)"
      let mobileArea = MkScreenArea
            { width = MkJSUInt 375,
              height = MkJSUInt 667
            }
      let mobileOverride = MkSetScreenSettingsOverride
            { screenArea = Just mobileArea,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result3 <- emulationSetScreenSettingsOverride mobileOverride
      logShow "Screen area set to 375x667 (mobile)" result3
      pause

      logTxt "Test 4: Clear screen area override"
      let clearScreenArea = MkSetScreenSettingsOverride
            { screenArea = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result4 <- emulationSetScreenSettingsOverride clearScreenArea
      logShow "Screen area override cleared" result4
      pause

-- >>> runDemo emulationSetTimezoneOverrideDemo
emulationSetTimezoneOverrideDemo :: BiDiDemo
emulationSetTimezoneOverrideDemo =
  demo "Emulation - Set Timezone Override" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
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
      pause

-- >>> runDemo emulationCompleteWorkflowDemo
emulationCompleteWorkflowDemo :: BiDiDemo
emulationCompleteWorkflowDemo =
  demo "Emulation - Complete Workflow Demo" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      logTxt "=== Creating multiple browsing contexts for emulation demo ==="
      bc1 <- rootContext utils bidi
      bc2 <- newWindowContext utils bidi
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
      closeContext utils bidi bc2
      pause

-- >>> runDemo emulationSetForcedColorsModeThemeOverrideDemo
emulationSetForcedColorsModeThemeOverrideDemo :: BiDiDemo
emulationSetForcedColorsModeThemeOverrideDemo =
  demo "Emulation - Set Forced Colors Mode Theme Override - since https://www.w3.org/TR/2025/WD-webdriver-bidi-20250729" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Set forced colors mode theme to light"
      let lightTheme = MkSetForcedColorsModeThemeOverride
            { theme = Just ForcedColorsLight,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetForcedColorsModeThemeOverride lightTheme
      logShow "Theme set to light" result1
      pause

      logTxt "Test 2: Set forced colors mode theme to dark"
      let darkTheme = MkSetForcedColorsModeThemeOverride
            { theme = Just ForcedColorsDark,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetForcedColorsModeThemeOverride darkTheme
      logShow "Theme set to dark" result2
      pause

      logTxt "Test 3: Clear forced colors mode theme override"
      let clearTheme = MkSetForcedColorsModeThemeOverride
            { theme = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result3 <- emulationSetForcedColorsModeThemeOverride clearTheme
      logShow "Theme override cleared" result3
      pause

-- >>> runDemo emulationSetNetworkConditionsDemo
emulationSetNetworkConditionsDemo :: BiDiDemo
emulationSetNetworkConditionsDemo =
  demo "Emulation - Set Network Conditions - since https://www.w3.org/TR/2025/WD-webdriver-bidi-20251007" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Set network to offline mode"
      let offlineCondition = MkNetworkConditionsOffline { networkConditionsType = "offline" }
      let networkOverride = MkSetNetworkConditions
            { networkConditions = Just (MkNetworkConditions offlineCondition),
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetNetworkConditions networkOverride
      logShow "Network set to offline" result1
      pause

      logTxt "Test 2: Clear network conditions"
      let clearNetwork = MkSetNetworkConditions
            { networkConditions = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetNetworkConditions clearNetwork
      logShow "Network conditions cleared" result2
      pause

-- >>> runDemo emulationSetUserAgentOverrideDemo
emulationSetUserAgentOverrideDemo :: BiDiDemo
emulationSetUserAgentOverrideDemo =
  demo "Emulation - Set User Agent Override - since https://www.w3.org/TR/2025/WD-webdriver-bidi-20250910" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Set custom User-Agent (Chrome on Windows)"
      let chromeUA = MkSetUserAgentOverride
            { userAgent = Just "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetUserAgentOverride chromeUA
      logShow "User-Agent set to Chrome" result1
      pause

      logTxt "Test 2: Set custom User-Agent (Mobile Safari)"
      let safariUA = MkSetUserAgentOverride
            { userAgent = Just "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1",
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetUserAgentOverride safariUA
      logShow "User-Agent set to Mobile Safari" result2
      pause

      logTxt "Test 3: Clear User-Agent override"
      let clearUA = MkSetUserAgentOverride
            { userAgent = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result3 <- emulationSetUserAgentOverride clearUA
      logShow "User-Agent override cleared" result3
      pause

-- >>> runDemo emulationSetScriptingEnabledDemo
emulationSetScriptingEnabledDemo :: BiDiDemo
emulationSetScriptingEnabledDemo =
  demo "Emulation - Set Scripting Enabled - since https://www.w3.org/TR/2025/WD-webdriver-bidi-20250811" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Disable JavaScript"
      let disableJS = MkSetScriptingEnabled
            { enabled = Just False,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result1 <- emulationSetScriptingEnabled disableJS
      logShow "JavaScript disabled" result1
      pause

      logTxt "Test 2: Re-enable JavaScript (clear override)"
      let enableJS = MkSetScriptingEnabled
            { enabled = Nothing,
              contexts = Just [bc],
              userContexts = Nothing
            }
      result2 <- emulationSetScriptingEnabled enableJS
      logShow "JavaScript re-enabled" result2
      pause
