{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Utils.Utils
  ( txt,
    enumerate,
    logRethrow,
    logSupress,
    ioThrow,
    throwLeft,
    db,
  )
where

import Control.Monad (when)
import Data.Text (Text, pack, unpack)
import Debug.Trace (trace)
import Text.Show.Pretty qualified as P
import UnliftIO (AsyncCancelled, Exception (displayException), Handler (Handler), MonadIO, MonadUnliftIO, SomeException, catches, throwIO)

txt :: (Show a) => a -> Text
txt = pack . P.ppShow

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound ..]

throwLeft :: forall l r m. (Applicative m) => (l -> m r) -> Either l r -> m r
throwLeft throw = either throw pure

ioThrow :: (Exception l, MonadIO m) => Either l r -> m r
ioThrow = throwLeft throwIO

-- | Logs exceptions thrown in a thread and rethrows synchronous exceptions
logRethrow ::
  (MonadUnliftIO m) =>
  -- | logger
  (Text -> m ()) ->
  -- | name of the thread
  Text ->
  -- | action to be executed
  m () ->
  m ()
logRethrow = catchLog' True

-- | Logs exceptions thrown in a thread and suppresses synchronous exceptions
logSupress ::
  (MonadUnliftIO m) =>
  -- | logger
  (Text -> m ()) ->
  -- | name of the thread
  Text ->
  -- | action to be executed
  m () ->
  m ()
logSupress = catchLog' False

catchLog' :: (MonadUnliftIO m) => Bool -> (Text -> m ()) -> Text -> m () -> m ()
catchLog' rethrowSynchExceptions logger name action =
  action
    `catches` [ Handler $ \(e :: AsyncCancelled) -> do
                  logger $ name <> " thread cancelled"
                  throwIO e,
                Handler $ \(e :: SomeException) -> do
                  logger $ "Exception thrown in " <> name <> " thread" <> ": " <> (pack $ displayException e)
                  when rethrowSynchExceptions $ throwIO e
              ]

-- debugging
db :: (Show a) => Text -> a -> a
db label value = trace (unpack $ label <> ":\n" <> txt value) value
