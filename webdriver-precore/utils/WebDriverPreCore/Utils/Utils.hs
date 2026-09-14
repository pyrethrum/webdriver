{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Utils.Utils
  ( txt,
    enumerate,
    ioThrow,
    db,
  )
where

import Data.Text (Text, pack, unpack)
import Debug.Trace (trace)
import Text.Show.Pretty qualified as P
import UnliftIO (Exception, throwIO)

txt :: (Show a) => a -> Text
txt = pack . P.ppShow

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound ..]

ioThrow :: (Exception l) => Either l r -> IO r
ioThrow = either throwIO pure

-- debugging
db :: (Show a) => Text -> a -> a
db label value = trace (unpack $ label <> ":\n" <> txt value) value
