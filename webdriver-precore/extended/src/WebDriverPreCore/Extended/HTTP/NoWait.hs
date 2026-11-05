-- |
-- Module: WebDriverPreCore.Extended.HTTP.NoWait
-- Description: HTTP compound actions composed from the Base action functions
--
-- TODO: implementation pending.
module WebDriverPreCore.Extended.HTTP.NoWait  where


import WebDriverPreCore.Extended.Common.NoWait qualified as C
import WebDriverPreCore.Extended.Common.Locate (Locator)
import WebDriverPreCore.Extended.Common.NoWait
import WebDriverPreCore.HTTP.Protocol


  -- Locator -> m (Either LocateException ElementId)

type ActionParams m r t = C.ActionParams m ElementId r t
type ElmAction m r = C.ElmAction m ElementId r

-- runAction :: Monad m =>  LocateActions m ->  -> Locator -> m (Either ActionException r)
-- runAction  = C.runAction 

-- | Locate a unique or first-matching element from the document root.
-- locate :: forall m. Monad m => LocateActions m -> HttpLocateOpts -> Locator -> m (Either LocateException ElementId)
-- locate actions opts l = mkSingleton l <$> runHttpAction actions opts Nothing httpLocateSingleton l