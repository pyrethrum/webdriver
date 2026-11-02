module HTTP.LocateOpts (defHttpOpts) where

import WebDriverPreCore.Extended.HTTP.Locate qualified as L
import qualified WebDriverPreCore.Extended.Common.Locate as L
import Common.Utils (autoId)

defHttpOpts :: L.HttpLocateOpts
defHttpOpts =
  L.MkHttpLocateOpts
    { extendedRoleLocation = L.ExtLocateNever,
      jsRecheckDisplayed = L.DisplayedCheckAlways,
      singletonCardinality = L.Unique,
      mkDefaultLoc = autoId
    }