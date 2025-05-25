{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Internal.Utils
  ( txt,
    enumerate,
    -- shared path elements
    UrlPath (..),
    newSessionUrl,
    session,
    bodyValue,
    bodyText
  )
where

import Data.Function ((.))
import Data.Text (Text, pack)
import GHC.Enum (Bounded (..), Enum)
import GHC.Show (Show (..))
import Prelude (Eq, Ord, Semigroup)
import WebDriverPreCore.Http.HttpResponse (HttpResponse(..))
import Data.Aeson (Key, Result, Value)
import WebDriverPreCore.Internal.AesonUtils (lookup, bodyText')

{-
  this module is used between the library and testing modules
  it will be removed in a later release
-}

-- general utils

txt :: (Show a) => a -> Text
txt = pack . show

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound ..]

-- shared path elements
newtype UrlPath = MkUrlPath {segments :: [Text]}
  deriving newtype (Show, Eq, Ord, Semigroup)

newSessionUrl :: UrlPath
newSessionUrl = MkUrlPath [session]

session :: Text
session = "session"

bodyValue :: HttpResponse -> Result Value
bodyValue r = lookup "value" r.body

bodyText :: HttpResponse -> Key -> Result Text
bodyText r = bodyText' (bodyValue r)

