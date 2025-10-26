module Http.HttpDemo where

-- minFirefoxSession,

import Config (loadConfig)
import Const (theInternet, Timeout (MkTimeout))
import Const qualified as Const
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO (catchAny, finally)
import Http.HttpAPI
  ( Action (..),
    Actions (..),
    BrowserName (..),
    Capabilities (..),
    Cookie (..),
    DriverStatus (..),
    FrameReference (..),
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
    sleep,
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
import RuntimeConst (httpFullCapabilities)
import Test.Tasty.HUnit (Assertion, assertBool)
import TestData
  ( checkboxesUrl,
    indexUrl,
    infiniteScrollUrl,
    inputsUrl,
    loginUrl,
    nestedFramesUrl,
    promptUrl,
    shadowDomUrl,
  )
import TestServerAPI (testServerHomeUrl, withTestServer)
import WebDriverPreCore.Http (minFullCapabilities)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log)

-- #################### The Tests ######################

seconds :: Int
seconds = 1000

-- >>> unit_demoNewSession
unit_demoNewSession :: IO ()
unit_demoNewSession = do
  cfg <- loadConfig
  let caps = httpFullCapabilities cfg
  logShow "capabilities" caps
  ses <- newSessionFull caps
  finally
    (logShow "new session response:\n" ses)
    (deleteSession ses.sessionId)

-- >>> unit_demoSessionDriverStatus
unit_demoSessionDriverStatus :: IO ()
unit_demoSessionDriverStatus = do
  cfg <- loadConfig
  sesId <- newSession $ httpFullCapabilities cfg
  finally
    ( do
        log "new session:" $ txt sesId
        s <- status
        Ready === s
        logShowM "driver status" status
    )
    (deleteSession sesId)

-- >>> unit_demoSendKeysClear
unit_demoSendKeysClear :: IO ()
unit_demoSendKeysClear = withSession \ses -> do
  url <- loginUrl
  navigateTo ses url
  usr <- findElement ses $ CSS "#username"

  logTxt "fill in user name"
  elementSendKeys ses usr "user name"
  sleep2

  logTxt "clear user name"
  elementClear ses usr
  sleep2

-- >>> unit_demoForwardBackRefresh
unit_demoForwardBackRefresh :: IO ()
unit_demoForwardBackRefresh = withSession \ses -> do
  url <- indexUrl
  navigateTo ses url
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  sleep1

  link <- findElement ses $ CSS "a[href='checkboxes.html']"
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
  url <- indexUrl
  navigateTo ses url

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
  url <- indexUrl
  navigateTo ses url
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
  url <- indexUrl
  navigateTo ses url
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  link <- findElement ses $ CSS "a[href='checkboxes.html']"
  logM "check box link text" $ getElementText ses link
  elementClick ses link

  cbs <- findElements ses $ CSS "input[type='checkbox']"
  forM_ cbs $ \cb -> do
    logShowM "checkBox checked property" $ getElementProperty ses cb "checked"
    logShowM "getElementAttribute type" $ getElementAttribute ses cb "type"
    logShowM "getElementCssValue display" $ getElementCssValue ses cb "display"
    logShowM "getElementTagName" $ getElementTagName ses cb
    logShowM "getElementRect" $ getElementRect ses cb
    logShowM "isElementEnabled" $ isElementEnabled ses cb
    logShowM "getElementComputedRole" $ getElementComputedRole ses cb
    logShowM "getElementComputedLabel" $ getElementComputedLabel ses cb

  header <- findElement ses $ CSS "h3"
  logShowM "header computed role" $ getElementComputedRole ses header
  logShowM "header computed label" $ getElementComputedLabel ses header

  divs <- findElements ses $ CSS "div"
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
  sleep $ 2 * Const.seconds
  r <- getWindowRect ses
  logShow "window rect" r

  wr === r

  url <- inputsUrl
  navigateTo ses url
  div' <- findElement ses $ CSS "#content"
  input <- findElementFromElement ses div' $ CSS "input"
  logShow "input tag" input

  els <- findElementsFromElement ses div' $ CSS "*"
  logShow "elements in div" els

-- >>> unit_demoWindowFindElement
unit_demoWindowFindElement :: IO ()
unit_demoWindowFindElement = withSession \ses -> do
  url <- inputsUrl
  navigateTo ses url
  allElms <- findElements ses $ CSS "*"

  chkHasElms allElms

  logShow "all elements" allElms
  div' <- findElement ses $ CSS "#content"
  input <- findElementFromElement ses div' $ CSS "input"
  logShow "input tag" input

  els <- findElementsFromElement ses div' $ CSS "*"

  chkHasElms els
  logShow "elements in div" els

-- >>> unit_demoFrames
unit_demoFrames :: IO ()
unit_demoFrames = withSession \ses -> do
  url <- nestedFramesUrl
  navigateTo ses url

  logTxt "At top level frame"
  hasBottomFrame <- bottomFameExists ses

  logShow "bottom frame exists" hasBottomFrame
  assertBool "bottom frame should exist" hasBottomFrame

  -- switch frames using element id
  tf <- findElement ses $ CSS "frame[name='frame-top']"
  logShow "switch to top frame" tf
  switchToFrame ses (FrameElementId tf)

  hasBottomFrame' <- bottomFameExists ses
  logShow "bottom frame exists after switching to top frame" hasBottomFrame'
  assertBool "bottom frame should not exist after switching to top frame" $ not hasBottomFrame'
  
  mf <- findElement ses $ CSS "frame[name='frame-middle']"
  switchToFrame ses (FrameElementId mf)

  fTitle <- findElement ses $ CSS "h1"
  titleTxt <- getElementText ses fTitle
  log "middle frame title" titleTxt
  "Test Page" === titleTxt

  logTxt "switch to top level frame"
  switchToFrame ses TopLevelFrame
  logShowM "bottom frame exists" $ bottomFameExists ses
  logShowM "active element" $ getActiveElement ses

  -- drill back down to middle frame (repeat the above steps)
  tf' <- findElement ses $ CSS "frame[name='frame-top']"
  logShow "switch back to top frame" tf'
  switchToFrame ses (FrameElementId tf')
  logShowM "active element" $ getActiveElement ses

  mf' <- findElement ses $ CSS "frame[name='frame-middle']"
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
  url <- shadowDomUrl
  navigateTo ses url

  -- Find the custom element:
  myParagraphId <- findElement ses (CSS "my-paragraph")
  logShow "my-paragraph" myParagraphId

  -- Get its shadow root:
  shadowRootId <- getElementShadowRoot ses myParagraphId
  logShow "shadowRootId" shadowRootId

  -- From the shadow root, find all elements
  allInsideShadow <- findElementsFromShadowRoot ses shadowRootId $ CSS "*"
  logShow "shadow root elements" allInsideShadow

  chkHasElms allInsideShadow
  logTxt "got root elements"

  srootElm <- findElementFromShadowRoot ses shadowRootId $ CSS "*"
  logShow "shadow root element" srootElm

  -- Retrieve text from the shadow element:
  logShowM "shadow text" $ getElementText ses srootElm

-- >>> unit_demoIsElementSelected
unit_demoIsElementSelected :: IO ()
unit_demoIsElementSelected = withSession \ses -> do
  logShowM "driver status" status
  url <- checkboxesUrl
  navigateTo ses url
  allCbs <- findElements ses $ CSS "input[type='checkbox']"
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
  url <- indexUrl
  navigateTo ses url
  logTxt "!!!!! Page Source !!!!!"
  logShowM "page source" $ getPageSource ses

  logTxt "!!!!! Screenshot!!!!!"
  logShowM "take screenshot" $ takeScreenshot ses

  logTxt "!!!!! Screenshot Element !!!!!"
  chkBoxLink <- findElement ses $ CSS "a[href='checkboxes.html']"
  logShowM "take element screenshot" $ takeElementScreenshot ses chkBoxLink

-- >>> unit_demoPrintPage
unit_demoPrintPage :: IO ()
unit_demoPrintPage = withSession \ses -> do
  url <- indexUrl
  navigateTo ses url
  -- pdf (encoded string)
  logM "print page" $ printPage ses

chkHasElms :: (Foldable t) => t a -> Assertion
chkHasElms els = assertBool "elements should be found" $ not (null els)

bottomFameExists :: SessionId -> IO Bool
bottomFameExists ses = not . null <$> findElements ses (CSS "frame[name='frame-bottom']")

--- >>> unit_demoExecuteScript
unit_demoExecuteScript :: IO ()
unit_demoExecuteScript =
  withSession \ses -> do
    url <- indexUrl
    navigateTo ses url
    logShowM "executeScript" $ executeScript ses "return arguments[0];" [String "Hello from Pyrethrum!", Number 2000]
    sleep2
    logTxt "executing asynch alert"
    executeScriptAsync ses "setTimeout(() => alert('Hello from Pyrethrum!'), 2000); return 5;" []
    logTxt "after asynch alert"
    sleep2

epochSeconds :: IO Int
epochSeconds = round <$> getPOSIXTime

{-
leaving this test hewre as a commnet  - it proves that setting the domain works correctly when using a real remote server
-- >>> unit_demoCookiesRemote
unit_demoCookiesRemote :: IO ()
unit_demoCookiesRemote =
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
-}

-- >>> unit_demoCookies
unit_demoCookies :: IO ()
unit_demoCookies =
  withTestServer $
    withSession \ses -> do
      -- navigateTo ses theInternet
      
      navigateTo ses testServerHomeUrl
      logShowM "cookies before add" $ getAllCookies ses

      logShowM "getNamedCookie: helloCookie" $ getNamedCookie ses "helloCookie"
      epocSecs <- epochSeconds
      let cookieName = "myCookie" <> txt epocSecs
      let myCookie =
            MkCookie
              { name = cookieName,
                value = "myCookieValue",
                path = Just "/",
                -- for some reason this does not work on the test server but you can set the domain on
                -- a remote server as in the comment above
                -- domain = Just "localhost",
                -- domain = Just ".localhost",
                domain = Nothing,
                secure = Just True,
                sameSite = Just Strict,
                httpOnly = Just False,
                -- expire in 10 mins (Chrome has a 400 day limit)
                expiry = Just $ epocSecs + 600
              }

      logShow "cookie to add" myCookie
      logShowM "addCookie" $ addCookie ses myCookie
      logShowM "cookies after add" $ getAllCookies ses

      actualCookie <- getNamedCookie ses cookieName
      myCookie {domain = Just "localhost"} === actualCookie

      logShowM "deleteCookie (myCookie)" $ deleteCookie ses cookieName
      afterRemove <- getAllCookies ses
      logShow "cookies after delete" afterRemove

      assertBool "cookie should be removed" $ not (any ((== cookieName) . (.name)) afterRemove)
      assertBool "there still should be cookies in the list" $ not (null afterRemove)

      logShowM "deleteAllCookies" $ deleteAllCookies ses
      afterDeleteAll <- getAllCookies ses
      logShow "cookies after delete all" afterDeleteAll
      assertBool "all cookies should be removed" $ null afterDeleteAll

-- >>> unit_demoAlerts
unit_demoAlerts :: IO ()
unit_demoAlerts =
  withSession \ses -> do
    url <- promptUrl
    navigateTo ses url

    alert <- findElement ses $ XPath "//button[@id='alertBtn']"
    elementClick ses alert

    sleep2
    at <- getAlertText ses
    logShow "get alert text" at
    "This is an alert!" === at

    sleep2
    logShowM "acceptAlert" $ acceptAlert ses

    sleep1
    prompt <- findElement ses $ XPath "//button[@id='promptBtn']"
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
    url <- indexUrl
    navigateTo ses url

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
                      Just $ 1 * seconds,
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
    url <- loginUrl
    navigateTo ses url
    usr <- findElement ses $ CSS "#username"
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
  url <- infiniteScrollUrl
  navigateTo ses url

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
  url <- inputsUrl
  navigateTo ses url

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
  httpFullCapabilities <$> loadConfig
    >>= newSessionFull
    >>= extendTimeouts . (.sessionId)

extendTimeouts :: SessionId -> IO SessionId
extendTimeouts ses = do
  setTimeouts ses $
    MkTimeouts
      { pageLoad = Just $ 30 * seconds,
        script = Just $ 11 * seconds,
        implicit = Just $ 12 * seconds
      }
  pure ses

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
