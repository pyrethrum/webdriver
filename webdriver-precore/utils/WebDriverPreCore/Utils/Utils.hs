{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Utils.Utils
  ( txt,
    enumerate,
    throwLeft,
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

throwLeft  :: forall l r m. Applicative m => (l -> m r) -> Either l r -> m r
throwLeft throw = either throw pure

ioThrow :: (Exception l) => Either l r -> IO r
ioThrow = throwLeft throwIO

-- debugging
db :: (Show a) => Text -> a -> a
db label value = trace (unpack $ label <> ":\n" <> txt value) value
