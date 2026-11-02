module HTTP.LocateOpts (defHttpOpts) where

import WebDriverPreCore.Extended.HTTP.Locate
    ( SingletonCardinality(..),
      ExtendedRoleLocateSingleton(..),
      HttpLocateOpts(..) ) 
import Common.Utils (autoId)
import WebDriverPreCore.Extended.Locators (DisplayedCheck(..))

defHttpOpts :: HttpLocateOpts
defHttpOpts =
  MkHttpLocateOpts
    { extendedRoleLocation = ExtLocateNever,
      jsRecheckDisplayed = DisplayedCheckAlways,
      singletonCardinality = Unique,
      mkDefaultLoc = autoId
    }