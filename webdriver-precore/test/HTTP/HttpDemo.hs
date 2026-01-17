module HTTP.HttpDemo where

-- minFirefoxSession,

import ConfigLoader (loadConfig)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO (catchAny)
import HTTP.DemoUtils (HttpDemo, demo, runDemo, sessionDemo)
import HTTP.Actions (HttpActions (..))
import IOUtils
  ( DemoActions (..),
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
import Utils (txt)
import WebDriverPreCore.HTTP.Protocol
  ( Action (..),
    Actions (..),
    Cookie (..),
    FrameReference (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    SameSite (..),
    Script (..),
    Selector (..),
    Session (..),
    SessionResponse (..),
    Status (..),
    Timeouts (..),
    URL (..),
    WheelAction (..),
    WindowHandleSpec (..),
    WindowRect (..),
  )
import Prelude hiding (log)

_stopDemoUnusedWarning :: HttpDemo -> IO ()
_stopDemoUnusedWarning = runDemo

-- #################### The Tests ######################

-- >>> runDemo newSessionDemo
newSessionDemo :: HttpDemo
newSessionDemo =
  demo "new Session" action
  where
    action :: DemoActions -> HttpActions -> IO ()
    action MkDemoActions {..} MkHttpActions {..} = do
      cfg <- loadConfig
      let caps = httpFullCapabilities cfg
      logShow "capabilities" caps
      bracket
        (newSession caps)
        (\(MkSessionResponse {sessionId = sid}) -> deleteSession sid)
        (logShow "new session response:\n")

-- >>> runDemo driverStatusDemo
driverStatusDemo :: HttpDemo
driverStatusDemo =
  sessionDemo "driver status" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      log "new session:" $ txt sesId
      s <- status
      {- Per W3C WebDriver spec section 8.4, status.ready must be false when active HTTP sessions exist.
         Geckodriver (Firefox) correctly implements this - returns ready: false when serving a session.
         Chromedriver diverges from spec - returns ready: true even with active sessions because it 
         supports multiple concurrent sessions. This test will fail on Chrome. -}
      False === s.ready
      logShowM "driver status" status

-- >>> runDemo demoSendKeysClear
demoSendKeysClear :: HttpDemo
demoSendKeysClear =
  sessionDemo "send keys clear" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- loginUrl
      navigateTo sesId $ url
      usr <- findElement sesId $ CSS "#username"

      logTxt "fill in user name"
      elementSendKeys sesId usr "user name"
      pause

      logTxt "clear user name"
      elementClear sesId usr
      pause

-- >>> runDemo demoForwardBackRefresh
demoForwardBackRefresh :: HttpDemo
demoForwardBackRefresh =
  sessionDemo "forward back refresh" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- indexUrl
      logTxt "navigating to index page"
      navigateTo sesId $ url
      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId

      pause

      link <- findElement sesId $ CSS "a[href='checkboxes.html']"
      logTxt "navigating to check boxes page"
      elementClick sesId link

      pause
      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId
      logTxt "navigating back"
      back sesId
      pause

      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId
      logTxt "navigating forward"

      forward sesId
      pause

      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId
      logTxt "refreshing"
      refresh sesId
      pause

      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId

-- >>> runDemo documentationDemo
documentationDemo :: HttpDemo
documentationDemo =
  sessionDemo "forward back refresh - external doc demo" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      navigateTo sesId $ MkUrl "https://the-internet.herokuapp.com/"

      link <- findElement sesId $ CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"
      elementClick sesId link

      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId

      logTxt "navigating back"
      back sesId

      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId

      logTxt "navigating forward"
      forward sesId

      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId

      logTxt "refreshing"
      refresh sesId
      pause

-- >>> runDemo demoWindowHandles
demoWindowHandles :: HttpDemo
demoWindowHandles =
  sessionDemo "window handles" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- indexUrl
      navigateTo sesId $ url

      logShowM "window Handle" $ getWindowHandle sesId

      w <- newWindow sesId
      log "new window Handle" $ txt w
      pause

      switchToWindow sesId w.handle

      logShowM "all windows handles" $ getWindowHandles sesId

      closeWindow sesId
      log "windows closed" $ txt sesId

      logShowM "all windows handles" $ getWindowHandles sesId

-- >>> runDemo demoWindowSizes
demoWindowSizes :: HttpDemo
demoWindowSizes =
  sessionDemo "window sizes" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      maximizeWindow sesId
      url <- indexUrl
      navigateTo sesId $ url
      pause

      {- ChromeDriver limitation: Transitioning from fullscreen => maximized fails intermittently with
         "failed to change window state to 'normal', current state is 'fullscreen'" on some systems.
         This is a known ChromeDriver bug on Linux/Wayland/X11 where the window manager state doesn't
         sync properly with ChromeDriver's internal state machine. Even with delays and state sync
         calls (getWindowRect), the transition remains unreliable.
         
         Workaround: minimizeWindow => maximizeWindow => fullscreen  -}

      logShowM "minimizeWindow" $ minimizeWindow sesId
      pause

      logShowM "maximizeWindow" $ maximizeWindow sesId
      pause 

      logShowM "fullscreen" $ fullScreenWindow sesId
      pause

-- >>> runDemo demoElementPageProps
demoElementPageProps :: HttpDemo
demoElementPageProps =
  sessionDemo "element page props" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- indexUrl
      navigateTo sesId $ url
      logShowM "current url" $ getCurrentUrl sesId
      logM "title" $ getTitle sesId

      link <- findElement sesId $ CSS "a[href='checkboxes.html']"
      logM "check box link text" $ getElementText sesId link
      elementClick sesId link

      cbs <- findElements sesId $ CSS "input[type='checkbox']"
      forM_ cbs $ \cb -> do
        logShowM "checkBox checked property" $ getElementProperty sesId cb "checked"
        logShowM "getElementAttribute type" $ getElementAttribute sesId cb "type"
        logShowM "getElementCssValue display" $ getElementCssValue sesId cb "display"
        logShowM "getElementTagName" $ getElementTagName sesId cb
        logShowM "getElementRect" $ getElementRect sesId cb
        logShowM "isElementEnabled" $ isElementEnabled sesId cb
        logShowM "getElementComputedRole" $ getElementComputedRole sesId cb
        logShowM "getElementComputedLabel" $ getElementComputedLabel sesId cb

      header <- findElement sesId $ CSS "h3"
      logShowM "header computed role" $ getElementComputedRole sesId header
      logShowM "header computed label" $ getElementComputedLabel sesId header

      divs <- findElements sesId $ CSS "div"
      forM_ divs $ \d ->
        logShowM "div overflow value" $ getElementCssValue sesId d "overflow"

-- >>> runDemo demoTimeouts
demoTimeouts :: HttpDemo
demoTimeouts =
  sessionDemo "timeouts" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      log "new session" $ txt sesId
      logShowM "timeouts" $ getTimeouts sesId
      let timeouts =
            MkTimeouts
              { pageLoad = Just $ 50_000,
                script = Just $ 11_000,
                implicit = Just $ 12_000
              }
      setTimeouts sesId timeouts
      timeouts' <- getTimeouts sesId

      logShow "updated timeouts" timeouts'
      timeouts === timeouts'

-- >>> runDemo demoWindowRecs
demoWindowRecs :: HttpDemo
demoWindowRecs =
  sessionDemo "window recs" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      {- Note:
       There is a known issue with geckodriver and Wayland that prevents setting withe x y window position
       see the links below for more details:
         - https://github.com/mozilla/geckodriver/issues/2224
         - https://github.com/SeleniumHQ/selenium/issues/15584
         - https://bugzilla.mozilla.org/show_bug.cgi?id=1959040
        This test passes because x and y are set to 0,0.
      -}
      let wr =
            Rect
              { x = 0,
                y = 0,
                width = 600,
                height = 400
              }
      logShowM "set window rect" $ setWindowRect sesId wr
      r <- getWindowRect sesId
      logShow "window rect" r

      wr === r

      url <- inputsUrl
      navigateTo sesId $ url
      div' <- findElement sesId $ CSS "#content"
      input <- findElementFromElement sesId div' $ CSS "input"
      logShow "input tag" input

      els <- findElementsFromElement sesId div' $ CSS "*"
      logShow "elements in div" els

-- >>> runDemo demoWindowFindElement
demoWindowFindElement :: HttpDemo
demoWindowFindElement =
  sessionDemo "window find element" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- inputsUrl
      navigateTo sesId $ url
      allElms <- findElements sesId $ CSS "*"

      chkHasElms allElms

      logShow "all elements" allElms
      div' <- findElement sesId $ CSS "#content"
      input <- findElementFromElement sesId div' $ CSS "input"
      logShow "input tag" input

      els <- findElementsFromElement sesId div' $ CSS "*"

      chkHasElms els
      logShow "elements in div" els

-- >>> runDemo demoFrames
demoFrames :: HttpDemo
demoFrames =
  sessionDemo "frames" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action
      sesId
      MkDemoActions {logTxt, log, logShow, logShowM}
      MkHttpActions
        { navigateTo,
          switchToFrame,
          switchToParentFrame,
          getActiveElement,
          findElement,
          findElements,
          getElementText
        } = do
        let bottomFameExists = not . null <$> findElements sesId (CSS "frame[name='frame-bottom']")
        url <- nestedFramesUrl
        navigateTo sesId $ url

        logTxt "At top level frame"
        hasBottomFrame <- bottomFameExists

        logShow "bottom frame exists" hasBottomFrame
        assertBool "bottom frame should exist" hasBottomFrame

        -- switch frames using element id
        tf <- findElement sesId $ CSS "frame[name='frame-top']"
        logShow "switch to top frame" tf
        switchToFrame sesId (FrameElementId tf)

        hasBottomFrame' <- bottomFameExists
        logShow "bottom frame exists after switching to top frame" hasBottomFrame'
        assertBool "bottom frame should not exist after switching to top frame" $ not hasBottomFrame'

        mf <- findElement sesId $ CSS "frame[name='frame-middle']"
        switchToFrame sesId (FrameElementId mf)

        fTitle <- findElement sesId $ CSS "h1"
        titleTxt <- getElementText sesId fTitle
        log "middle frame title" titleTxt
        "Test Page" === titleTxt

        logTxt "switch to top level frame"
        switchToFrame sesId TopLevelFrame
        logShowM "bottom frame exists" $ bottomFameExists

        -- drill back down to middle frame (repeat the above steps)
        tf' <- findElement sesId $ CSS "frame[name='frame-top']"
        logShow "switch back to top frame" tf'
        switchToFrame sesId (FrameElementId tf')
        logShowM "active element" $ getActiveElement sesId

        mf' <- findElement sesId $ CSS "frame[name='frame-middle']"
        logShow "drill back down to middle frame" mf'
        switchToFrame sesId (FrameElementId mf')
        logShowM "active element" $ getActiveElement sesId

        logTxt "switch to parent frame"
        switchToParentFrame sesId
        logShowM "active element" $ getActiveElement sesId

        logTxt "switch to parent frame again"
        switchToParentFrame sesId
        logShowM "active element" $ getActiveElement sesId

        hasBottomFrame'' <- bottomFameExists
        logShow "bottom frame exists" hasBottomFrame''
        assertBool "bottom frame should exist" hasBottomFrame''

        logTxt "Switch to frame 1"
        switchToFrame sesId $ FrameNumber 1

        logShowM "active element" $ getActiveElement sesId

-- >>> runDemo demoShadowDom
demoShadowDom :: HttpDemo
demoShadowDom =
  sessionDemo "shadow dom" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- shadowDomUrl
      navigateTo sesId $ url

      -- Find the custom element:
      myParagraphId <- findElement sesId (CSS "my-paragraph")
      logShow "my-paragraph" myParagraphId

      -- Get its shadow root:
      shadowRootId <- getElementShadowRoot sesId myParagraphId
      logShow "shadowRootId" shadowRootId

      -- From the shadow root, find all elements
      allInsideShadow <- findElementsFromShadowRoot sesId shadowRootId $ CSS "*"
      logShow "shadow root elements" allInsideShadow

      chkHasElms allInsideShadow
      logTxt "got root elements"

      srootElm <- findElementFromShadowRoot sesId shadowRootId $ CSS "*"
      logShow "shadow root element" srootElm

      -- Retrieve text from the shadow element:
      logShowM "shadow text" $ getElementText sesId srootElm

-- >>> runDemo demoIsElementSelected
demoIsElementSelected :: HttpDemo
demoIsElementSelected =
  sessionDemo "is element selected" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      logShowM "driver status" status
      url <- checkboxesUrl
      navigateTo sesId $ url
      allCbs <- findElements sesId $ CSS "input[type='checkbox']"
      forM_ allCbs $ \cb -> do
        before <- isElementSelected sesId cb
        logShow "checkBox isElementSelected before" before

        elementClick sesId cb
        logTxt "clicked"

        after <- isElementSelected sesId cb
        logShow "checkBox isElementSelected after click" after

        assertBool "checkBox state should change after click" $ not before == after
        logTxt "------------------"

-- >>> runDemo demoGetPageSourceScreenShot
demoGetPageSourceScreenShot :: HttpDemo
demoGetPageSourceScreenShot =
  sessionDemo "get page source screenshot" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- indexUrl
      navigateTo sesId $ url
      logTxt "!!!!! Page Source !!!!!"
      logShowM "page source" $ getPageSource sesId

      logTxt "!!!!! Screenshot!!!!!"
      logShowM "take screenshot" $ takeScreenshot sesId

      logTxt "!!!!! Screenshot Element !!!!!"
      chkBoxLink <- findElement sesId $ CSS "a[href='checkboxes.html']"
      logShowM "take element screenshot" $ takeElementScreenshot sesId chkBoxLink

-- >>> runDemo demoPrintPage
demoPrintPage :: HttpDemo
demoPrintPage =
  sessionDemo "print page" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- indexUrl
      navigateTo sesId $ url
      -- pdf (encoded string)
      logM "print page" $ printPage sesId

chkHasElms :: (Foldable t) => t a -> Assertion
chkHasElms els = assertBool "elements should be found" $ not (null els)

-- >>> runDemo demoExecuteScript
demoExecuteScript :: HttpDemo
demoExecuteScript =
  sessionDemo "execute script" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- indexUrl
      navigateTo sesId $ url
      logShowM "executeScript" . executeScript sesId $
        MkScript
          { script = "return arguments[0];",
            args = [String "Hello from Pyrethrum!", Number 2000]
          }
      pause
      logTxt "executing asynch alert"
      executeScriptAsync sesId $
        MkScript
          { script = "setTimeout(() => alert('Hello from Pyrethrum!'), 2000); return 5;",
            args = []
          }
      logTxt "after asynch alert"
      pause

epochSeconds :: IO Int
epochSeconds = round <$> getPOSIXTime

-- >>> runDemo demoCookies
demoCookies :: HttpDemo
demoCookies =
  sessionDemo "cookies" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      withTestServer $ do
        navigateTo sesId $ MkUrl testServerHomeUrl
        logShowM "cookies before add" $ getAllCookies sesId

        epocSecs <- epochSeconds
        let cookieName = "myCookieWithDomain" <> txt epocSecs
        let myCookie =
              MkCookie
                { name = cookieName,
                  value = "myCookieValue",
                  path = Just "/",
                  -- can't set the domain on the test server but have been
                  -- able to on a remote server
                  domain = Nothing,
                  secure = Just True,
                  sameSite = Just Strict,
                  httpOnly = Just False,
                  -- expire in 10 mins (Chrome has a 400 day limit)
                  expiry = Just $ epocSecs + 600
                }

        logShow "cookie to add (with domain)" myCookie
        logShowM "addCookie" $ addCookie sesId myCookie
        logShowM "cookies after add" $ getAllCookies sesId

        actualCookie <- getNamedCookie sesId cookieName
        logShow "retrieved cookie" actualCookie
        --  server fills in domain
        myCookie {domain = Just "localhost"} === actualCookie

        logShowM "deleteCookie (myCookie)" $ deleteCookie sesId cookieName
        afterRemove <- getAllCookies sesId
        logShow "cookies after delete" afterRemove

        assertBool "cookie should be removed" $ not (any ((== cookieName) . (.name)) afterRemove)
        assertBool "there still should be cookies in the list" $ not (null afterRemove)

        logShowM "deleteAllCookies" $ deleteAllCookies sesId
        afterDeleteAll <- getAllCookies sesId
        logShow "cookies after delete all" afterDeleteAll
        assertBool "all cookies should be removed" $ null afterDeleteAll

-- >>> runDemo demoCookiesWithDomain
demoCookiesWithDomain :: HttpDemo
demoCookiesWithDomain =
  sessionDemo "cookies with domain" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      navigateTo sesId $ MkUrl "https://example.com"
      logShowM "cookies before add" $ getAllCookies sesId

      epocSecs <- epochSeconds
      let cookieName = "myExampleCookie" <> txt epocSecs
      let myCookie =
            MkCookie
              { name = cookieName,
                value = "exampleValue",
                path = Just "/",
                domain = Just ".example.com",
                secure = Just True,
                sameSite = Just Lax,
                httpOnly = Just False,
                -- expire in 10 mins (Chrome has a 400 day limit)
                expiry = Just $ epocSecs + 600
              }

      logShow "cookie to add (with domain set)" myCookie
      logShowM "addCookie" $ addCookie sesId myCookie
      logShowM "cookies after add" $ getAllCookies sesId

      actualCookie <- getNamedCookie sesId cookieName
      logShow "retrieved cookie" actualCookie
      myCookie === actualCookie

-- >>> runDemo demoAlerts
demoAlerts :: HttpDemo
demoAlerts =
  sessionDemo "alerts" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- promptUrl
      navigateTo sesId $ url

      alert <- findElement sesId $ XPath "//button[@id='alertBtn']"
      elementClick sesId alert

      pause
      at <- getAlertText sesId
      logShow "get alert text" at
      "This is an alert!" === at
      pause

      logShowM "acceptAlert" $ acceptAlert sesId
      pause

      prompt <- findElement sesId $ XPath "//button[@id='promptBtn']"
      elementClick sesId prompt
      pause

      logShowM "sendAlertText: I am Dave" $ sendAlertText sesId "I am Dave"
      pause

      dismissAlert sesId
      pause

-- >>> runDemo demoPointerNoneActions
demoPointerNoneActions :: HttpDemo
demoPointerNoneActions =
  sessionDemo "pointer none actions" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- indexUrl
      navigateTo sesId $ url

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
                            duration = Just $ 2_000,
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
                        Just $ 1_000,
                        Just $ 2_000,
                        Nothing,
                        Nothing
                      ]
                  }
                  --
              ]
      logTxt "move and None actions"
      performActions sesId pointer

{-

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

-}

-- >>> runDemo demoKeyAndReleaseActions
demoKeyAndReleaseActions :: HttpDemo
demoKeyAndReleaseActions =
  sessionDemo "key and release actions" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- loginUrl
      navigateTo sesId $ url
      usr <- findElement sesId $ CSS "#username"
      elementClick sesId usr

      let keys =
            MkActions
              [ Key
                  { id = "keyboard1",
                    keyActions =
                      [ PauseKey Nothing,
                        KeyDown "a",
                        -- a random pause to test the API
                        PauseKey . Just $ 2_000,
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

      pause
      logTxt "key actions"
      performActions sesId keys

      pause
      releaseActions sesId
      pause

-- >>> runDemo demoWheelActions
demoWheelActions :: HttpDemo
demoWheelActions =
  sessionDemo "wheel actions" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      url <- infiniteScrollUrl
      navigateTo sesId $ url

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
                            duration = Just $ 4_000
                          },
                        PauseWheel $ Just 1000,
                        Scroll
                          { origin = Viewport,
                            x = 10,
                            y = 10,
                            deltaX = -400,
                            deltaY = -4000,
                            duration = Just $ 4_000
                          }
                      ]
                  }
              ]

      logTxt "wheel actions"
      performActions sesId wheel
      pause

-- >>> runDemo demoError
demoError :: HttpDemo
demoError =
  sessionDemo "error" action
  where
    action :: Session -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      -- this tests error mapping of one error type by checking the text of the error
      -- thrown by the runner with a deliberately incorrect selector

      -- reset timeouts so we don't wait too long for our failure
      setTimeouts sesId $
        MkTimeouts
          { pageLoad = Just $ 30_000,
            script = Just $ 11_000,
            implicit = Just $ 1_000
          }
      url <- inputsUrl
      navigateTo sesId $ url

      -- if the runner has mapped the error as expected (using parseWebDriverError) we expect it to rethrow the text of the mapped webdriver error
      -- including  the text:
      -- "WebDriverError {error = NoSuchElement, description = "An element could not be located on the page using the given search parameters"
      -- other libraries will use the error mapping function in more sophisticated ways
      catchAny
        ( do
            findElement sesId $ CSS "#id-that-does-not-exist-on-this-page"
            error "should not get here - no such element"
        )
        $ \e -> do
          logShow "caught error" e
          let errTxt = txt e
              expectedText = "An element could not be located on the page using the given search parameters"
          assertBool "NoSuchElement error should be mapped" $ expectedText `isInfixOf` errTxt
