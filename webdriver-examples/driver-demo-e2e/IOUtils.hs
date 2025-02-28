module IOUtils
  ( sleepMs,
    encodeFileToBase64,
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


sleepMs :: Int -> IO ()
sleepMs = threadDelay . (* 1_000)

encodeFileToBase64 :: FilePath -> IO Text
encodeFileToBase64 filePath = do
  contents <- BS.readFile filePath
  pure . B64T.extractBase64 $ B64.encodeBase64 contents