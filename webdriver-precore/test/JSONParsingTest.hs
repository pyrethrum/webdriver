module JSONParsingTest where

import Data.Set as S (Set, difference, fromList, null)
import Data.Text as T (Text, lines, null, pack, replace, strip, unwords, words, split, intercalate)
import GHC.Utils.Misc (filterOut)
import Test.Tasty.HUnit as HUnit ( assertBool, Assertion, (@=?) )
import Text.RawString.QQ (r)
import WebDriverPreCore.Spec
    ( SessionId(Session),
      ElementId(Element),
      Selector(CSS),
      WindowHandle(Handle),
      WindowRect(Rect),
      FrameReference(TopLevelFrame),
      Cookie(MkCookie),
      Actions(MkActions),
      Timeouts(..),
      W3Spec(description, Get, Post, PostEmpty, Delete, path),
      errorTypeToErrorCode,
      minFirefoxCapabilities,
      errorDescription,
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
      errorCodeToErrorType,
      executeScript,
      executeScriptAsync,
      findElement,
      findElementFromElement,
      findElementFromShadowRoot,
      findElements,
      findElementsFromElement,
      findElementsFromShadowRoot,
      forward,
      fullscreenWindow,
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
      minimizeWindow,
      navigateTo,
      newSession,
      newWindow,
      performActions,
      printPage,
      refresh,
      releaseActions,
      sendAlertText,
      setTimeouts,
      setWindowRect,
      status,
      switchToFrame,
      switchToParentFrame,
      switchToWindow,
      takeElementScreenshot,
      takeScreenshot )
import GHC.Show (Show (..))
import Data.Eq (Eq ((==)))
import Data.Ord (Ord)
import Data.Function (($), (.), (&))
import Data.Semigroup ((<>))
import Data.List ((!!), drop)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import GHC.Base (error)
import WebDriverPreCore.Internal.Utils (enumerate)
import Data.Foldable (traverse_)
import Data.Either (either)
import WebDriverPreCore.Spec (UrlPath(..))


