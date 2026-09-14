
module WebDriverPreCore.Types.BaseTypes
  ( JSUInt (..),
    SubPath (..),
    Logger,
    IOLogger,
    nullLogger,
  )
where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Word (Word64)

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

-- | Null logger
nullLogger :: (Applicative m) => Logger m
nullLogger = const $ pure ()
