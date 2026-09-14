{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Utils
  ( txt,
    enumerate,
    ioThrow,
    JSUInt (..),
    -- shared path elements
    SubPath (..),
    db,
    Logger,
    IOLogger,
    nullLogger,
  )
where

-- debugging only remove brefore release

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text (Text, pack, unpack)
import Debug.Trace (trace)
import GHC.Word (Word64)
import Text.Show.Pretty qualified as P
import UnliftIO (Exception, throwIO)

-- general utils

txt :: (Show a) => a -> Text
txt = pack . P.ppShow

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound ..]

-- | JavaScript safe unsigned integer (0 to 2^53-1)
-- Duplicated from webdriver-precore to avoid dependency
newtype JSUInt = MkJSUInt Word64
  deriving newtype (Show, Eq, Ord, Enum, FromJSON, ToJSON, Num)

-- shared path elements
newtype SubPath = MkSubPath {parts :: [Text]}
  deriving newtype (Show, Eq, Ord, Semigroup)

-- | Logger type alias
type Logger m = Text -> m ()

type IOLogger = Text -> IO ()

ioThrow :: (Exception l) => Either l r -> IO r
ioThrow = either throwIO pure

-- | Null logger
nullLogger :: (Applicative m) => Logger m
nullLogger = const $ pure ()

-- debugging

db :: (Show a) => Text -> a -> a
db label value = trace (unpack $ label <> ":\n" <> txt value) value
