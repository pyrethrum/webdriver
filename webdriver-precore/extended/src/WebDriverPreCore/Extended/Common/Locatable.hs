module WebDriverPreCore.Extended.Common.Locatable
  ( 
    Locatable (..),
    ShadowLocatable (..),
    ShadowLocatableParam (..)
  )
where

import WebDriverPreCore.Extended.Common.Locate (LocateException, Locator)
import WebDriverPreCore.HTTP.Protocol (ElementId, ShadowRootElementId)

class Locatable a b where
  applyLocate :: (Monad m) => (Locator -> m (Either LocateException b)) -> a -> m (Either LocateException b)

instance Locatable Locator ElementId where
  applyLocate :: (Locator -> m (Either LocateException ElementId)) -> Locator -> m (Either LocateException ElementId)
  applyLocate locateFn = locateFn

instance Locatable ElementId ElementId where
  applyLocate :: (Monad m) => (Locator -> m (Either LocateException ElementId)) -> ElementId -> m (Either LocateException ElementId)
  applyLocate _ = pure . Right

data ShadowLocatableParam
  = Locator Locator
  | ElementId ElementId
  | ShadowRootElementId ShadowRootElementId
  deriving (Show, Eq)

class ShadowLocatable a where
  extractShadowLocatable :: a -> ShadowLocatableParam

instance ShadowLocatable Locator  where
  extractShadowLocatable :: Locator -> ShadowLocatableParam
  extractShadowLocatable = Locator

instance ShadowLocatable ElementId where
  extractShadowLocatable :: ElementId -> ShadowLocatableParam
  extractShadowLocatable = ElementId 

instance ShadowLocatable ShadowRootElementId  where
  extractShadowLocatable :: ShadowRootElementId -> ShadowLocatableParam
  extractShadowLocatable = ShadowRootElementId