-- |
-- Module: WebDriverPreCore.Extended.Common.Types
-- Description: Types shared by the Extended HTTP and BiDi modules
module WebDriverPreCore.Extended.Common.Types
  ( -- * Runner Type
    Runner,
  )
where

-- | A function that executes a typed protocol 'Command' in some monad @m@.
--
-- The first type parameter is the protocol's 'Command' type constructor
-- (for example the HTTP or BiDi 'Command'), whose phantom parameter is the
-- expected response type.
type Runner c m a = c a -> m a
