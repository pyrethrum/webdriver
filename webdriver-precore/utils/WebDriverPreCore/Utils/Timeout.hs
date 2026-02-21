module WebDriverPreCore.Utils.Timeout
  ( Timeout (..),
    millisecond,
    milliseconds,
    second,
    seconds,
    minute,
    minutes,
    hour,
    hours,
  )
where

-- | Timeout in microseconds
newtype Timeout = MkTimeout {microseconds :: Int}
  deriving (Show, Eq)
  deriving newtype (Num)

millisecond :: Timeout
millisecond = MkTimeout 1_000

milliseconds :: Timeout
milliseconds = millisecond

second :: Timeout
second = 1_000 * milliseconds

seconds :: Timeout
seconds = second

minute :: Timeout
minute = 60 * seconds

minutes :: Timeout
minutes = minute

hour :: Timeout
hour = 60 * minutes

hours :: Timeout
hours = hour
