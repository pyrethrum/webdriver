module Http.HttpE2EDemoTest where

-- minFirefoxSession,

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text (Text, isInfixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import E2EConst
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
    infiniteScrollUrl,
    inputTagCss,
    inputsUrl,
    jsAlertXPath,
    jsPromptXPath,
    loginUrl,
    midFrameCss,
    midFrameTitle,
    second,
    seconds,
    shadowDomUrl,
    theInternet,
    topFrameCSS,
    userNameCss,
  )
import GHC.IO (catchAny)
import Http.HttpAPI
  ( Action (..),
    Actions (..),
    BrowserName (..),
    Capabilities (..),
    Cookie (..),
    DriverStatus (..),
    FrameReference (..),
    FullCapabilities (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    SameSite (..),
    Selector (..),
    SessionId (..),
    SessionResponse (..),
    Timeouts (..),
    VendorSpecific (..),
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
    encodeFileToBase64,
    executeScript,
    executeScriptAsync,
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
    minCapabilities,
    minFirefoxSession,
    minimizeWindow,
    navigateTo,
    newSession,
    newSessionFull,
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
    takeScreenshot,
  )
import IOUtils
  ( log,
    logM,
    logShow,
    logShowM,
    logTxt,
    sleep1,
    sleep2,
    (===),
  )
import WebDriverPreCore.Http (alwaysMatchCapabilities, minChromeCapabilities, minFullCapabilities)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log)
import Test.Tasty.HUnit (Assertion, assertBool)
import Config (useFirefox, customFirefoxProfilePath, firefoxHeadless)

-- #################### The Tests ######################

configuredCapabilities :: FullCapabilities
configuredCapabilities = MkFullCapabilities
    { alwaysMatch =
        Just $
          MkCapabilities
            { browserName = Just $ if useFirefox then Firefox else Chrome,
              browserVersion = Nothing,
              platformName = Nothing,
              acceptInsecureCerts = Nothing,
              pageLoadStrategy = Nothing,
              proxy = Nothing,
              setWindowRect = Nothing,
              timeouts = Nothing,
              strictFileInteractability = Nothing,
              unhandledPromptBehavior = Nothing,
              vendorSpecific =
                if
                  | useFirefox ->
                      let headless = if firefoxHeadless then ["--headless"] else []
                          profile = maybe [] (\p -> ["-profile", p]) customFirefoxProfilePath
                          args = headless <> profile
                          mArgs = if null args then Nothing else Just args
                       in mArgs & maybe Nothing \_ ->
                            Just FirefoxOptions
                              { -- requires a path to the profile directory
                                firefoxArgs = mArgs,
                                firefoxBinary = Nothing,
                                firefoxProfile = Nothing,
                                firefoxLog = Nothing
                              }
                  | otherwise -> Nothing
            },
      firstMatch = []
    }
    
-- >>> unit_demoNewSession
unit_demoNewSession :: IO ()
unit_demoNewSession = do
  ses <- newSessionFull configuredCapabilities
  logShow "new session response:\n" ses

  deleteSession ses.sessionId

-- >>> unit_demoSessionDriverStatus
unit_demoSessionDriverStatus :: IO ()
unit_demoSessionDriverStatus = do
  ses <- newSession configuredCapabilities
  log "new session:" $ txt ses

  s <- status
  Ready === s
  logShowM "driver status" status

  deleteSession ses

-- >>> unit_demoSendKeysClear
unit_demoSendKeysClear :: IO ()
unit_demoSendKeysClear = withSession \ses -> do
  navigateTo ses loginUrl
  usr <- findElement ses userNameCss

  logTxt "fill in user name"
  elementSendKeys ses usr "user name"
  sleep2

  logTxt "clear user name"
  elementClear ses usr
  sleep2

-- >>> unit_demoForwardBackRefresh
unit_demoForwardBackRefresh :: IO ()
unit_demoForwardBackRefresh = withSession \ses -> do
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

-- example used in haddock leave here for testing
demoForwardBackRefresh :: IO ()
demoForwardBackRefresh = do
  ses <- newSession $ minFullCapabilities Firefox
  navigateTo ses "https://the-internet.herokuapp.com/"
  link <- findElement ses $ CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"
  elementClick ses link
  back ses
  forward ses
  refresh ses
  deleteSession ses

-- >>> unit_demoWindowHandles
unit_demoWindowHandles :: IO ()
unit_demoWindowHandles = withSession \ses -> do
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

-- >>> unit_demoWindowSizes
unit_demoWindowSizes :: IO ()
unit_demoWindowSizes = withSession \ses -> do
  maximizeWindow ses
  navigateTo ses theInternet
  sleep1

  logShowM "minimizeWindow" $ minimizeWindow ses
  sleep1

  logShowM "fullscreen" $ fullScreenWindow ses
  sleep1

  logShowM "maximizeWindow" $ maximizeWindow ses
  sleep1

-- >>> unit_demoElementPageProps
unit_demoElementPageProps :: IO ()
unit_demoElementPageProps = withSession \ses -> do
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

-- >>> unit_demoTimeouts
unit_demoTimeouts :: IO ()
unit_demoTimeouts = withSession \ses -> do
  log "new session" $ txt ses
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

-- >>> unit_demoWindowRecs
unit_demoWindowRecs :: IO ()
unit_demoWindowRecs = withSession \ses -> do
  let wr =
        Rect
          { x = 500,
            y = 300,
            width = 600,
            height = 400
          }
  logShowM "set window rect" $ setWindowRect ses wr
  sleepMs $ 2 * seconds
  r <- getWindowRect ses
  logShow "window rect" r

  wr === r

  navigateTo ses inputsUrl
  div' <- findElement ses contentCss
  input <- findElementFromElement ses div' inputTagCss
  logShow "input tag" input

  els <- findElementsFromElement ses div' anyElmCss
  logShow "elements in div" els

-- >>> unit_demoWindowFindElement
unit_demoWindowFindElement :: IO ()
unit_demoWindowFindElement = withSession \ses -> do
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

-- >>> unit_demoFrames
unit_demoFrames :: IO ()
unit_demoFrames = withSession \ses -> do
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

-- >>> unit_demoShadowDom
unit_demoShadowDom :: IO ()
unit_demoShadowDom = withSession \ses -> do
  navigateTo ses shadowDomUrl

  -- Find the custom element:
  myParagraphId <- findElement ses (CSS "my-paragraph")
  logShow "my-paragraph" myParagraphId

  -- Get its shadow root:
  shadowRootId <- getElementShadowRoot ses myParagraphId
  logShow "shadowRootId" shadowRootId

  -- From the shadow root, find all elements
  allInsideShadow <- findElementsFromShadowRoot ses shadowRootId anyElmCss
  logShow "shadow root elements" allInsideShadow

  chkHasElms allInsideShadow
  logTxt "got root elements"

  srootElm <- findElementFromShadowRoot ses shadowRootId anyElmCss
  logShow "shadow root element" srootElm

  -- Retrieve text from the shadow element:
  logShowM "shadow text" $ getElementText ses srootElm

-- >>> unit_demoIsElementSelected
unit_demoIsElementSelected :: IO ()
unit_demoIsElementSelected = withSession \ses -> do
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

-- >>> unit_demoGetPageSourceScreenShot
unit_demoGetPageSourceScreenShot :: IO ()
unit_demoGetPageSourceScreenShot = withSession \ses -> do
  navigateTo ses theInternet
  logTxt "!!!!! Page Source !!!!!"
  logShowM "page source" $ getPageSource ses

  logTxt "!!!!! Screenshot!!!!!"
  logShowM "take screenshot" $ takeScreenshot ses

  logTxt "!!!!! Screenshot Element !!!!!"
  chkBoxLink <- findElement ses checkBoxesLinkCss
  logShowM "take element screenshot" $ takeElementScreenshot ses chkBoxLink

-- >>> unit_demoPrintPage
unit_demoPrintPage :: IO ()
unit_demoPrintPage = withSession \ses -> do
  navigateTo ses theInternet
  -- pdf (encoded string)
  logM "print page" $ printPage ses

chkHasElms :: (Foldable t) => t a -> Assertion
chkHasElms els = assertBool "elements should be found" $ not (null els)

bottomFameExists :: SessionId -> IO Bool
bottomFameExists ses = not . null <$> findElements ses bottomFrameCss

--- >>> unit_demoExecuteScript
unit_demoExecuteScript :: IO ()
unit_demoExecuteScript =
  withSession \ses -> do
    navigateTo ses theInternet
    logShowM "executeScript" $ executeScript ses "return arguments[0];" [String "Hello from Pyrethrum!", Number 2000]
    sleep2
    logTxt "executing asynch alert"
    executeScriptAsync ses "setTimeout(() => alert('Hello from Pyrethrum!'), 2000); return 5;" []
    logTxt "after asynch alert"
    sleep2

epochSeconds :: IO Int
epochSeconds = round <$> getPOSIXTime

-- >>> unit_demoCookies
unit_demoCookies :: IO ()
unit_demoCookies =
  withSession \ses -> do
    navigateTo ses theInternet
    logShowM "cookies" $ getAllCookies ses

    logShowM "getNamedCookie: optimizelyEndUserId" $ getNamedCookie ses "optimizelyEndUserId"
    epocSecs <- epochSeconds

    let myCookie =
          MkCookie
            { name = "myCookie",
              value = "myCookieValue",
              path = Just "/",
              domain = Just ".the-internet.herokuapp.com",
              secure = Just True,
              sameSite = Just Strict,
              httpOnly = Just False,
              -- expire in 10 mins (Chrome has a 400 day limit)
              expiry = Just $ epocSecs + 600
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

-- >>> unit_demoAlerts
unit_demoAlerts :: IO ()
unit_demoAlerts =
  withSession \ses -> do
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

-- >>> unit_demoPointerNoneActions
unit_demoPointerNoneActions :: IO ()
unit_demoPointerNoneActions =
  withSession \ses -> do
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

-- >>> unit_demoKeyAndReleaseActions
unit_demoKeyAndReleaseActions :: IO ()
unit_demoKeyAndReleaseActions =
  withSession \ses -> do
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

-- >>> unit_demoWheelActions
unit_demoWheelActions :: IO ()
unit_demoWheelActions = withSession \ses -> do
  navigateTo ses infiniteScrollUrl

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

-- >>> unit_demoError
unit_demoError :: IO ()
unit_demoError = withSession \ses -> do
  -- this tests error mapping of one error type by checking the text of the error
  -- thrown by the runner with a deliberately incorrect selector

  -- reset timeouts so we don't wait too long for our failure
  setTimeouts ses $
    MkTimeouts
      { pageLoad = Just $ 30 * seconds,
        script = Just $ 11 * seconds,
        implicit = Just $ 1 * seconds
      }
  navigateTo ses inputsUrl

  -- if the runner has mapped the error as expected (using parseWebDriverError) we expect it to rethrow the text of the mapped webdriver error
  -- including  the text:
  -- "WebDriverError {error = NoSuchElement, description = "An element could not be located on the page using the given search parameters"
  -- other libraries will use the error mapping function in more sophisticated ways
  catchAny
    ( do
        findElement ses $ CSS "#id-that-does-not-exist-on-this-page"
        error "should not get here - no such element"
    )
    $ \e -> do
      logShow "caught error" e
      let errTxt = txt e
          expectedText = "WebDriverError {error = NoSuchElement, description = \"An element could not be located on the page using the given search parameters\""
      assertBool "NoSuchElement error should be mapped" $ expectedText `isInfixOf` errTxt

-- #################### Utils ######################

session :: IO SessionId
session =
  if useFirefox
    then mkExtendedFirefoxTimeoutsSession
    else mkExtendedChromeTimeoutsSession

withSession :: (SessionId -> IO ()) -> IO ()
withSession =
  bracket session deleteSession

{-
This fails on my machine with the following error:

```Your Firefox profile cannot be loaded. It may be missing or inaccessible.```

This appears to be due to the profile being unpacked into tmp and the driver not being able to access it.
If I copy the unpacked profile to "./webdriver-examples/driver-demo-e2e/.profile/FirefoxWebDriverProfile" and reference in
capabilites as follows see (capsWithCustomFirefoxProfile):

```firefoxArgs = Just ["-profile", "./webdriver-examples/driver-demo-e2e/.profile/FirefoxWebDriverProfile"]```

then it works.
-}
capsWithCustomFirefoxProfileNotWorking :: IO Capabilities
capsWithCustomFirefoxProfileNotWorking = do
  profile <- encodeFileToBase64 "./webdriver-examples/driver-demo-e2e/FirefoxWebDriverProfile.zip"
  pure $
    (minCapabilities Firefox)
      { vendorSpecific =
          Just
            FirefoxOptions
              { firefoxArgs = Nothing,
                firefoxBinary = Nothing,
                firefoxProfile = Just profile,
                firefoxLog = Nothing
              }
      }

capsWithCustomFirefoxProfile :: Text -> Capabilities
capsWithCustomFirefoxProfile firefoxProfilePath =
  (minCapabilities Firefox)
    { vendorSpecific =
        Just
          FirefoxOptions
            { -- requires a path to the profile directory
              firefoxArgs = Just ["-profile", firefoxProfilePath],
              firefoxBinary = Nothing,
              firefoxProfile = Nothing,
              firefoxLog = Nothing
            }
    }

mkExtendedFirefoxTimeoutsSession :: IO SessionId
mkExtendedFirefoxTimeoutsSession =
  customFirefoxProfilePath
    & maybe
      minFirefoxSession
      (\profilepath -> newSession . alwaysMatchCapabilities $ capsWithCustomFirefoxProfile profilepath)
    >>= extendTimeouts

mkExtendedChromeTimeoutsSession :: IO SessionId
mkExtendedChromeTimeoutsSession =
  newSession minChromeCapabilities >>= extendTimeouts

extendTimeouts :: SessionId -> IO SessionId
extendTimeouts ses = do
  setTimeouts ses $
    MkTimeouts
      { pageLoad = Just $ 30 * seconds,
        script = Just $ 11 * seconds,
        implicit = Just $ 12 * seconds
      }
  pure ses
