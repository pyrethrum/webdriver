module IOUtils
  ( sleepMs,
    encodeFileToBase64,
    logTxt,
    log,
    logShow,
    logM,
    logShowM,
    ppTxt,
    sleep1,
    sleep2,
    (===),
  )
where
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Base64.Types as B64T
import Data.Text (Text)
import System.IO (FilePath, IO)
import Data.Int (Int)
import Control.Applicative (pure)
import Data.Function ((.), ($))
import GHC.Num (Num(..))

import Data.Text.IO qualified as TIO
import Text.Show.Pretty qualified as P
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, (@=?))
import Data.Semigroup (Semigroup (..))
import Data.Text (pack)
import Prelude (Show, Eq, (>>=))
import Const (second, seconds)


sleepMs :: Int -> IO ()
sleepMs = threadDelay . (* 1_000)

encodeFileToBase64 :: FilePath -> IO Text
encodeFileToBase64 filePath = do
  contents <- BS.readFile filePath
  pure . B64T.extractBase64 $ B64.encodeBase64 contents


logTxt :: Text -> IO ()
logTxt = TIO.putStrLn

log :: Text -> Text -> IO ()
log l t = logTxt $ l <> ": " <> t

ppTxt :: Show a => a -> Text
ppTxt = pack . P.ppShow

logShow :: (Show a) => Text -> a -> IO ()
logShow l = log l . ppTxt

logM :: Text -> IO Text -> IO ()
logM l t = t >>= log l

logShowM :: (Show a) => Text -> IO a -> IO ()
logShowM l t = t >>= logShow l

sleep1 :: IO ()
sleep1 = sleepMs $ 1 * second

sleep2 :: IO ()
sleep2 = sleepMs $ 2 * seconds

-- todo: test extras - split off

(===) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  Assertion
(===) = (@=?)