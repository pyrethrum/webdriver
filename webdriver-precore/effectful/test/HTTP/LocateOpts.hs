module HTTP.LocateOpts (defHttpOpts) where

import WebDriverPreCore.Extended.HTTP.Locate
    ( SingletonCardinality(..),
      ExtendedRoleLocateSingleton(..),
      LocateOpts(..) ) 
import Common.Utils (autoId)
import WebDriverPreCore.Extended.Locators (DisplayedCheck(..))

defHttpOpts :: LocateOpts
defHttpOpts =
  MkHttpLocateOpts
    { extendedRoleLocation = ExtLocateNever,
      jsRecheckDisplayed = DisplayedCheckAlways,
      singletonCardinality = Unique,
      mkDefaultLoc = autoId
    }