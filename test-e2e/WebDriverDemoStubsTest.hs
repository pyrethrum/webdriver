module WebDriverDemoStubsTest where

import Capabilities
    ( StandardCapabilities(..),
      VendorSpecific(..),
      FullCapabilities(..),
      minStandardCapabilities,
      BrowserName(Firefox),
      MatchCapabilities(..) )
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, assertBool, (@=?))
import Utils (txt)
import WebDriverDemoUtils
  ( alertsUrl,
    anyElmCss,
    bottomFrameCss,
    checkBoxesCss,
    checkBoxesLinkCss,
    checkBoxesUrl,
    contentCss,
    divCss,
    framesUrl,
    h3TagCss,
    infinitScrollUrl,
    inputTagCss,
    inputsUrl,
    jsAlertXPath,
    jsPromptXPath,
    loginUrl,
    midFrameCss,
    midFrameTitle,
    shadowDomUrl,
    theInternet,
    topFrameCSS,
    userNameCss,
  )
import WebDriverIO
  ( Action (..),
    Actions (..),
    Cookie (..),
    FrameReference (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    SameSite (..),
    Selector (..),
    SessionId (..),
    Timeouts (..),
    WheelAction (..),
    WindowHandleSpec (..),
    WindowRect (..),
    acceptAlert,
    addCookie,
    back,
    closeWindow,
    deleteAllCookies,
    deleteCookie,
    deleteSession,
    dismissAlert,
    elementClear,
    elementClick,
    elementSendKeys,
    executeScript,
    executeScriptAsync,
    encodeFileToBase64,
    findElement,
    findElementFromElement,
    findElementFromShadowRoot,
    findElements,
    findElementsFromElement,
    findElementsFromShadowRoot,
    forward,
    fullScreenWindow,
    getActiveElement,
    getAlertText,
    getAllCookies,
    getCurrentUrl,
    getElementAttribute,
    getElementComputedLabel,
    getElementComputedRole,
    getElementCssValue,
    getElementProperty,
    getElementRect,
    getElementShadowRoot,
    getElementTagName,
    getElementText,
    getNamedCookie,
    getPageSource,
    getTimeouts,
    getTitle,
    getWindowHandle,
    getWindowHandles,
    getWindowRect,
    isElementEnabled,
    isElementSelected,
    maximizeWindow,
    -- minFirefoxSession,
    minimizeWindow,
    navigateTo,
    newWindow,
    performActions,
    printPage,
    refresh,
    releaseActions,
    sendAlertText,
    setTimeouts,
    setWindowRect,
    sleepMs,
    status,
    switchToFrame,
    switchToParentFrame,
    switchToWindow,
    takeElementScreenshot,
    takeScreenshot, newSession,
  )
import WebDriverPure (second, seconds)
import WebDriverSpec (DriverStatus (..))
import Prelude hiding (log)
import System.Directory (getCurrentDirectory)

logTxt :: Text -> IO ()
logTxt = TIO.putStrLn

log :: Text -> Text -> IO ()
log l t = logTxt $ l <> ": " <> t

logShow :: (Show a) => Text -> a -> IO ()
logShow l = log l . txt

logM :: Text -> IO Text -> IO ()
logM l t = t >>= log l

logShowM :: (Show a) => Text -> IO a -> IO ()
logShowM l t = t >>= logShow l

sleep1 :: IO ()
sleep1 = sleepMs $ 1 * second

sleep2 :: IO ()
sleep2 = sleepMs $ 2 * seconds

capsWithCustomFirefoxProfile :: IO StandardCapabilities
capsWithCustomFirefoxProfile = do
  -- profile <- encodeFileToBase64 "./test-e2e/FirefoxWebDriverProfile.zip"
  pure $ (minStandardCapabilities Firefox)
    { vendorSpecific =
        Just
          FirefoxOptions
            { firefoxArgs = Just ["-profile", "./test-e2e/.profile/FirefoxWebDriverProfile"],
              firefoxBinary = Nothing,
              -- firefoxProfile = Just profile
              firefoxProfile = Nothing
            }
    }

mkExtendedTimeoutsSession :: IO SessionId
mkExtendedTimeoutsSession = do
  -- ses <- minFirefoxSession

  cwd <- getCurrentDirectory
  putStrLn $ "Current working directory: " ++ cwd
  profileBase64 <- capsWithCustomFirefoxProfile
  ses <- newSession . MkFullCapabilities $ MkMatchCapabilities {
    alwaysMatch = Just profileBase64,
    firstMatch = []
  }

  setTimeouts ses $
    MkTimeouts
      { pageLoad = Just $ 30 * seconds,
        script = Just $ 11 * seconds,
        implicit = Just $ 12 * seconds
      }
  pure ses

-- todo: test extras - split off

(===) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  Assertion
(===) = (@=?)


-- >>> unit_demoSessionDriverStatus
unit_demoSessionDriverStatus :: IO ()
unit_demoSessionDriverStatus = do
  ses <- mkExtendedTimeoutsSession
  log "new session" $ txt ses
  s <- status
  Ready === s
  logShowM "driver status" status
  deleteSession ses

-- >>> unit_demoSendKeysClear
unit_demoSendKeysClear :: IO ()
unit_demoSendKeysClear = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses loginUrl
  usr <- findElement ses userNameCss

  logTxt "fill in user name"
  elementSendKeys ses usr "user name"
  sleep2

  logTxt "clear user name"
  elementClear ses usr
  sleep2
  deleteSession ses

-- >>> unit_demoForwardBackRefresh
unit_demoForwardBackRefresh :: IO ()
unit_demoForwardBackRefresh = do
  ses <- mkExtendedTimeoutsSession

  navigateTo ses theInternet
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  sleep1

  link <- findElement ses checkBoxesLinkCss
  logTxt "navigating to check boxes page"
  elementClick ses link

  sleep1
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses
  logTxt "navigating back"
  back ses
  sleep1

  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses
  logTxt "navigating forward"

  forward ses
  sleep1

  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses
  logTxt "refreshing"
  refresh ses
  sleep1

  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  deleteSession ses

-- >>> unit_demoWindowHandles
unit_demoWindowHandles :: IO ()
unit_demoWindowHandles = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet

  logShowM "window Handle" $ getWindowHandle ses

  w <- newWindow ses
  log "new window Handle" $ txt w
  sleep1

  switchToWindow ses w.handle

  logShowM "all windows handles" $ getWindowHandles ses

  closeWindow ses
  log "windows closed" $ txt ses

  logShowM "all windows handles" $ getWindowHandles ses
  deleteSession ses

-- >>> unit_demoWindowSizes
unit_demoWindowSizes :: IO ()
unit_demoWindowSizes = do
  ses <- mkExtendedTimeoutsSession
  ---
  maximizeWindow ses
  navigateTo ses theInternet
  sleep1

  logShowM "minimizeWindow" $ minimizeWindow ses
  sleep1

  logShowM "fullscreen" $ fullScreenWindow ses
  sleep1

  logShowM "maximizeWindow" $ maximizeWindow ses
  sleep1

  deleteSession ses

-- >>> unit_demoElementPageProps
unit_demoElementPageProps :: IO ()
unit_demoElementPageProps = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  link <- findElement ses checkBoxesLinkCss
  logM "check box link text" $ getElementText ses link
  elementClick ses link

  cbs <- findElements ses checkBoxesCss
  forM_ cbs $ \cb -> do
    logShowM "checkBox checked property" $ getElementProperty ses cb "checked"
    logShowM "getElementAttribute type" $ getElementAttribute ses cb "type"
    logShowM "getElementCssValue display" $ getElementCssValue ses cb "display"
    logShowM "getElementTagName" $ getElementTagName ses cb
    logShowM "getElementRect" $ getElementRect ses cb
    logShowM "isElementEnabled" $ isElementEnabled ses cb
    logShowM "getElementComputedRole" $ getElementComputedRole ses cb
    logShowM "getElementComputedLabel" $ getElementComputedLabel ses cb

  header <- findElement ses h3TagCss
  logShowM "header computed role" $ getElementComputedRole ses header
  logShowM "header computed label" $ getElementComputedLabel ses header

  divs <- findElements ses divCss
  forM_ divs $ \d ->
    logShowM "div overflow value" $ getElementCssValue ses d "overflow"

  deleteSession ses

-- >>> unit_demoTimeouts
unit_demoTimeouts :: IO ()
unit_demoTimeouts = do
  ses <- mkExtendedTimeoutsSession
  log "new session" $ txt ses
  ---
  logShowM "timeouts" $ getTimeouts ses
  let timeouts =
        MkTimeouts
          { pageLoad = Just $ 50 * seconds,
            script = Just $ 11 * seconds,
            implicit = Just $ 12 * seconds
          }
  setTimeouts ses timeouts
  timeouts' <- getTimeouts ses

  logShow "updated timeouts" timeouts'
  timeouts === timeouts'

  deleteSession ses

-- >>> unit_demoWindowRecs
unit_demoWindowRecs :: IO ()
unit_demoWindowRecs = do
  ses <- mkExtendedTimeoutsSession
  ---
  let wr = Rect 500 300 500 500
  logShowM "set window rect" $ setWindowRect ses wr
  r <- getWindowRect ses
  sleepMs $ 2 * seconds
  logShow "window rect" r

  wr === r

  navigateTo ses inputsUrl
  div' <- findElement ses contentCss
  input <- findElementFromElement ses div' inputTagCss
  logShow "input tag" input

  els <- findElementsFromElement ses div' anyElmCss
  logShow "elements in div" els

  deleteSession ses

chkHasElms :: (Foldable t) => t a -> Assertion
chkHasElms els = assertBool "elements should be found" $ not (null els)

-- >>> unit_demoWindowFindElement
unit_demoWindowFindElement :: IO ()
unit_demoWindowFindElement = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses inputsUrl
  allElms <- findElements ses anyElmCss

  chkHasElms allElms

  logShow "all elements" allElms
  div' <- findElement ses contentCss
  input <- findElementFromElement ses div' inputTagCss
  logShow "input tag" input

  els <- findElementsFromElement ses div' anyElmCss

  chkHasElms els
  logShow "elements in div" els

  deleteSession ses

-- >>> unit_demoFrames
unit_demoFrames :: IO ()
unit_demoFrames = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses framesUrl

  logTxt "At top level frame"
  hasBottomFrame <- bottomFameExists ses

  logShow "bottom frame exists" hasBottomFrame
  assertBool "bottom frame should exist" hasBottomFrame

  -- switch frames using element id
  tf <- findElement ses topFrameCSS
  logShow "switch to top frame" tf
  switchToFrame ses (FrameElementId tf)

  hasBottomFrame' <- bottomFameExists ses
  logShow "bottom frame exists after switching to top frame" hasBottomFrame'
  assertBool "bottom frame should not exist after switching to top frame" $ not hasBottomFrame'

  mf <- findElement ses midFrameCss
  switchToFrame ses (FrameElementId mf)

  fTitle <- findElement ses midFrameTitle
  titleTxt <- getElementText ses fTitle
  log "middle frame title" titleTxt
  "MIDDLE" === titleTxt

  logTxt "switch to top level frame"
  switchToFrame ses TopLevelFrame
  logShowM "bottom frame exists" $ bottomFameExists ses
  logShowM "active element" $ getActiveElement ses

  -- drill back down to middle frame (repeat the above steps)
  tf' <- findElement ses topFrameCSS
  logShow "switch back to top frame" tf'
  switchToFrame ses (FrameElementId tf')
  logShowM "active element" $ getActiveElement ses

  mf' <- findElement ses midFrameCss
  logShow "drill back down to middle frame" mf'
  switchToFrame ses (FrameElementId mf')
  logShowM "active element" $ getActiveElement ses

  logTxt "switch to parent frame"
  switchToParentFrame ses
  logShowM "active element" $ getActiveElement ses

  logTxt "switch to parent frame again"
  switchToParentFrame ses
  logShowM "active element" $ getActiveElement ses

  hasBottomFrame'' <- bottomFameExists ses
  logShow "bottom frame exists" hasBottomFrame''
  assertBool "bottom frame should exist" hasBottomFrame''

  logTxt "Switch to frame 1"
  switchToFrame ses $ FrameNumber 1

  logShowM "active element" $ getActiveElement ses

  deleteSession ses

bottomFameExists :: SessionId -> IO Bool
bottomFameExists ses = not . null <$> findElements ses bottomFrameCss

-- >>> unit_demoShadowDom
unit_demoShadowDom :: IO ()
unit_demoShadowDom = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses shadowDomUrl

  -- Find the custom element:
  myParagraphId <- findElement ses (CSS "my-paragraph")
  logShow "my-paragraph" myParagraphId

  -- Get its shadow root:
  shadowRootId <- getElementShadowRoot ses myParagraphId
  logShow "shadowRootId" shadowRootId

  -- From the shadow root, find all elements
  -- allInsideShadow <- findElementsFromShadowRoot ses shadowRootId (CSS "*")
  allInsideShadow <- findElementsFromShadowRoot ses shadowRootId anyElmCss
  logShow "shadow root elements" allInsideShadow

  chkHasElms allInsideShadow
  logTxt "got root elements"

  srootElm <- findElementFromShadowRoot ses shadowRootId anyElmCss
  logShow "shadow root element" srootElm

  -- Retrieve text from the shadow element:
  logShowM "shadow text" $ getElementText ses srootElm
  deleteSession ses

-- >>> unit_demoIsElementSelected
unit_demoIsElementSelected :: IO ()
unit_demoIsElementSelected = do
  ses <- mkExtendedTimeoutsSession
  logShowM "driver status" status
  navigateTo ses checkBoxesUrl
  allCbs <- findElements ses checkBoxesCss
  forM_ allCbs $ \cb -> do
    before <- isElementSelected ses cb
    logShow "checkBox isElementSelected before" before

    elementClick ses cb
    logTxt "clicked"

    after <- isElementSelected ses cb
    logShow "checkBox isElementSelected after click" after

    assertBool "checkBox state should change after click" $ not before == after
    logTxt "------------------"

  deleteSession ses

-- >>> unit_demoGetPageSourceScreenShot
unit_demoGetPageSourceScreenShot :: IO ()
unit_demoGetPageSourceScreenShot = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logTxt "!!!!! Page Source !!!!!"
  logShowM "page source" $ getPageSource ses

  logTxt "!!!!! Screenshot!!!!!"
  logShowM "take screenshot" $ takeScreenshot ses

  logTxt "!!!!! Screenshot Element !!!!!"
  chkBoxLink <- findElement ses checkBoxesLinkCss
  logShowM "take element screenshot" $ takeElementScreenshot ses chkBoxLink
  deleteSession ses

-- >>> unit_demoPrintPage
unit_demoPrintPage :: IO ()
unit_demoPrintPage = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  -- pdf (encoded string)
  logM "print page" $ printPage ses
  deleteSession ses

--- >>> unit_demoExecuteScript
unit_demoExecuteScript :: IO ()
unit_demoExecuteScript = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logShowM "executeScript" $ executeScript ses "return arguments[0];" [String "Hello from Pyrethrum!", Number 2000]
  sleep2
  logTxt "executing asynch alert"
  executeScriptAsync ses "setTimeout(() => alert('Hello from Pyrethrum!'), 2000); return 5;" []
  logTxt "after asynch alert"
  sleep2
  deleteSession ses

-- >>> unit_demoCookies
unit_demoCookies :: IO ()
unit_demoCookies = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logShowM "cookies" $ getAllCookies ses

  logShowM "getNamedCookie: optimizelyEndUserId" $ getNamedCookie ses "optimizelyEndUserId"

  let myCookie =
        MkCookie
          { name = "myCookie",
            value = "myCookieValue",
            path = Just "/",
            domain = Just ".the-internet.herokuapp.com",
            secure = Just True,
            sameSite = Just Strict,
            httpOnly = Just False,
            expiry = Just 2772072677
          }

  logShow "cookie to add" myCookie
  logShowM "addCookie" $ addCookie ses myCookie
  logShowM "cookies after add" $ getAllCookies ses

  myCookie' <- getNamedCookie ses "myCookie"
  myCookie === myCookie'

  logShowM "deleteCookie (myCookie)" $ deleteCookie ses "myCookie"
  afterRemove <- getAllCookies ses
  logShow "cookies after delete" afterRemove

  assertBool "cookie should be removed" $ not (any ((== "myCookie") . (.name)) afterRemove)
  assertBool "there still should be cookies in the list" $ not (null afterRemove)

  logShowM "deleteAllCookies" $ deleteAllCookies ses
  afterDeleteAll <- getAllCookies ses
  logShow "cookies after delete all" afterDeleteAll
  assertBool "all cookies should be removed" $ null afterDeleteAll

  deleteSession ses

-- >>> unit_demoAlerts
unit_demoAlerts :: IO ()
unit_demoAlerts = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses alertsUrl

  alert <- findElement ses jsAlertXPath
  elementClick ses alert

  sleep2
  at <- getAlertText ses
  logShow "get alert text" at
  "I am a JS Alert" === at

  sleep2
  logShowM "acceptAlert" $ acceptAlert ses

  sleep1
  prompt <- findElement ses jsPromptXPath
  elementClick ses prompt

  sleep1
  logShowM "sendAlertText: I am Dave" $ sendAlertText ses "I am Dave"

  sleep2
  dismissAlert ses

  sleep1
  deleteSession ses

-- >>> unit_demoPointerNoneActions
unit_demoPointerNoneActions :: IO ()
unit_demoPointerNoneActions = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet

  let pointer =
        MkActions
          [ Pointer
              { id = "mouse1",
                subType = Mouse,
                pointerId = 0,
                pressed = Set.empty,
                x = 0,
                y = 0,
                actions =
                  [ PausePointer Nothing,
                    Down
                      { button = 0,
                        width = Nothing,
                        height = Nothing,
                        pressure = Nothing,
                        tangentialPressure = Nothing,
                        tiltX = Nothing,
                        tiltY = Nothing,
                        twist = Nothing,
                        altitudeAngle = Nothing,
                        azimuthAngle = Nothing
                      },
                    Move
                      { origin = Viewport,
                        duration = Just $ 4 * seconds,
                        x = 150,
                        y = 150,
                        width = Just 2,
                        height = Just 2,
                        pressure = Just 0.5,
                        tangentialPressure = Just $ -0.4,
                        tiltX = Just $ -50,
                        tiltY = Just $ -50,
                        twist = Just 5,
                        altitudeAngle = Just 1.5,
                        azimuthAngle = Just 6.2
                      },
                    PausePointer $ Just 1000,
                    Up
                      { button = 0,
                        width = Nothing,
                        height = Nothing,
                        pressure = Nothing,
                        tangentialPressure = Nothing,
                        tiltX = Nothing,
                        tiltY = Nothing,
                        twist = Nothing,
                        altitudeAngle = Nothing,
                        azimuthAngle = Nothing
                      }
                      -- looks like Cancel not supported yet by gecko driver 02-02-2025
                      -- https://searchfox.org/mozilla-central/source/remote/shared/webdriver/Actions.sys.mjs#2340
                      -- , Cancel
                  ]
              },
            NoneAction
              { id = "NullAction",
                noneActions =
                  [ Nothing,
                    Just $ 1 * second,
                    Just $ 4 * seconds,
                    Nothing,
                    Nothing
                  ]
              }
              --
          ]

  logTxt "move and None actions"
  performActions ses pointer
  deleteSession ses

-- >>> unit_demoKeyAndReleaseActions
unit_demoKeyAndReleaseActions :: IO ()
unit_demoKeyAndReleaseActions = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses loginUrl
  usr <- findElement ses userNameCss
  elementClick ses usr

  let keys =
        MkActions
          [ Key
              { id = "keyboard1",
                keyActions =
                  [ PauseKey Nothing,
                    KeyDown "a",
                    -- a random pause to test the API
                    PauseKey . Just $ 2 * seconds,
                    KeyUp "a",
                    -- select the a
                    -- send special control key not a raw control character
                    -- Use \xE009 to represent the Unicode code point U+E009
                    KeyDown "\xE009",
                    KeyDown "a",
                    -- this will do nothing - just used for correlating frames
                    -- just testing tha API
                    PauseKey Nothing
                  ]
              }
          ]

  sleep2
  logTxt "key actions"
  performActions ses keys

  sleep2
  releaseActions ses
  sleep2
  deleteSession ses

-- >>> manyWheelActions
manyWheelActions :: IO ()
manyWheelActions = do
  unit_demoKeyAndReleaseActions
  unit_demoWheelActions

-- unit_demoWheelActions
-- unit_demoWheelActions

-- >>> unit_demoWheelActions
unit_demoWheelActions :: IO ()
unit_demoWheelActions = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses infinitScrollUrl

  let wheel =
        MkActions
          [ Wheel
              { id = "wheel1",
                wheelActions =
                  [ Scroll
                      { origin = Viewport,
                        x = 10,
                        y = 10,
                        deltaX = 400,
                        deltaY = 4000,
                        duration = Just $ 4 * seconds
                      },
                    PauseWheel $ Just 1000,
                    Scroll
                      { origin = Viewport,
                        x = 10,
                        y = 10,
                        deltaX = -400,
                        deltaY = -4000,
                        duration = Just $ 4 * seconds
                      }
                  ]
              }
          ]

  logTxt "wheel actions"
  performActions ses wheel

  sleep2
  deleteSession ses
