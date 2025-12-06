{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Internal.Utils
  ( txt,
    enumerate,
    -- shared path elements
    UrlPath (..),
    db
  )
where

-- debugging only remove brefore release

import Data.Text (Text, pack, unpack)
import Debug.Trace (trace)
import Text.Show.Pretty qualified as P

{-
  this module is used between the library and testing modules
  it will be removed in a later release
-}

-- general utils

txt :: (Show a) => a -> Text
txt = pack . P.ppShow

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound ..]

-- shared path elements
newtype UrlPath = MkUrlPath {segments :: [Text]}
  deriving newtype (Show, Eq, Ord, Semigroup)

-- debugging

db :: (Show a) => Text -> a -> a
db label value = trace (unpack $ label <> ":\n" <> txt value) value
