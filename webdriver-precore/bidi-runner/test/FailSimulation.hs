module FailSimulation
  ( withBiDiFailTest,
  )
where

import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import Data.Word (Word64)
import UnliftIO.STM (atomically)
import WebDriverPreCore.BiDiRunner (BiDiRunner, BiDiUrl, mkBiDiRunner)
import WebDriverPreCore.BiDiRunnerBase
  ( ChannelActions (..),
    MessageActions (..),
    loopActions,
    mkMessageActions,
    withBiDiWithActions,
    initChannels,
    mkSocketActions,
    counterVar,
    mkAtomicCounter,
  )
import Utils (JSUInt (..))

-- | Run a BiDi session with failure injection for testing
withBiDiFailTest
  :: Word64              -- ^ Fail send after this many calls
  -> Word64              -- ^ Fail get after this many calls
  -> Word64              -- ^ Fail event handler after this many calls
  -> Maybe (Text -> IO ())  -- ^ Optional logger
  -> BiDiUrl
  -> (BiDiRunner IO -> IO ())
  -> IO ()
withBiDiFailTest failSendCount failGetCount failEventCount mLogger bidiUrl action =
  withBiDiWithActions mLogger bidiUrl (mkFailChannelActions failSendCount failGetCount failEventCount) $ \sa ->
    action (mkBiDiRunner sa)

-- | Create channel actions with failure injection
mkFailChannelActions
  :: Word64              -- ^ Fail send after this many calls
  -> Word64              -- ^ Fail get after this many calls
  -> Word64              -- ^ Fail event handler after this many calls
  -> (Text -> IO ())     -- ^ Logger
  -> IO (ChannelActions IO)
mkFailChannelActions failSendCount failGetCount failEventCount logger = do
  channels <- initChannels
  let baseActions = mkMessageActions logger channels
  failedActions <- failMessageActions baseActions failSendCount failGetCount failEventCount
  pure $
    MkChannelActions
      { socketActions = mkSocketActions channels,
        messageLoops = loopActions logger failedActions
      }

-- | Create message actions with failure injection
failMessageActions
  :: MessageActions IO  -- ^ Base actions
  -> Word64             -- ^ Fail send after this many calls
  -> Word64             -- ^ Fail get after this many calls
  -> Word64             -- ^ Fail event handler after this many calls
  -> IO (MessageActions IO)
failMessageActions baseActions failSendCount failGetCount failEventCount = do
  send <- failAction "send" failSendCount baseActions.send
  get <- failAction "get" failGetCount baseActions.get
  eventHandler' <- failAction "eventhandler" failEventCount $ const baseActions.eventHandler
  pure $
    MkMessageActions
      { send,
        get,
        eventHandler = eventHandler' ()
      }

-- | Create an action that fails after a specified number of calls
failAction
  :: Text            -- ^ Label for error message
  -> Word64          -- ^ Fail after this many calls
  -> (a -> IO ())    -- ^ Base action
  -> IO (a -> IO ())
failAction lbl failCallCount action = do
  counterVar' <- counterVar
  let counter = mkAtomicCounter counterVar'
  pure $ \a -> do
    n <- atomically counter
    if (coerce n :: Word64) == failCallCount
      then fail $ "Forced failure for testing: " <> unpack lbl <> " (call #" <> show n <> ")"
      else action a
