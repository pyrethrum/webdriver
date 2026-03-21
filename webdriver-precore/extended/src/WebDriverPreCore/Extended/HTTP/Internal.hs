-- A hidden module to export shared types
module WebDriverPreCore.Extended.HTTP.Internal (Runner) where

import WebDriverPreCore.Extended.HTTP.Protocol
  ( Command,
  )

-- ######################################################################
-- ########################### Type Aliases #############################
-- ######################################################################

-- | A 'Runner' is a function that executes a 'Command' in a monadic context.
-- This allows the Extended module to work with different execution strategies.
type Runner m a = Command a -> m a
