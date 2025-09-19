module TestPages where

import Data.Text (Text, pack)
import System.Directory (canonicalizePath)
import Prelude

-- | Convert a relative path to a canonical absolute path (resolves symlinks too)
makeCanonicalPath :: FilePath -> IO FilePath  
makeCanonicalPath = canonicalizePath


fileUrl :: FilePath -> IO Text
fileUrl path = do
  absPath <- makeCanonicalPath path
  pure $ "file://" <> pack absPath

-- | Create a data URI test page by prepending "data:text/html," to the given HTML content
mkPage :: Text -> Text
mkPage = (<>) "data:text/html,"


textAreaUrl :: IO Text
textAreaUrl = fileUrl "webdriver-examples/driver-demo-e2e/TestFiles/textArea.html"