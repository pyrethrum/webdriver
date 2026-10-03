module Common.TastyEval
  ( -- * Tasty Eval
    tastyEval,
    tastyEvalStdOut,
    tastyEvalStdOutOnFailure,
    TastyResult(..)
  )
where

import Control.Exception (throw, try)
import Data.Functor ((<&>))
import Data.Text (Text, unpack)
import System.Environment (withArgs)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, defaultMain)
import UnliftIO (throwIO)

data OutPutOpts = NoStdOut | StdOut | StdOutOnFailure

data TastyResult = Pass | Fail ExitCode

tastyEval :: Maybe Text -> TestTree -> IO TastyResult
tastyEval = tastyEval' NoStdOut

tastyEvalStdOut :: Maybe Text -> TestTree -> IO TastyResult
tastyEvalStdOut = tastyEval' StdOut

tastyEvalStdOutOnFailure :: Maybe Text -> TestTree -> IO TastyResult
tastyEvalStdOutOnFailure = tastyEval' StdOutOnFailure

-- | Run a test tree without terminating the process.
--
-- 'Test.Tasty.defaultMain' ends by throwing 'ExitCode' (via 'exitSuccess' /
-- 'exitFailure'). That is fine for a real @main@, but it defeats HLS's eval
-- plugin (@-- >>>@ comments): the plugin only reports captured @stdout@ when
-- the evaluated statement returns normally, and discards it when the
-- statement throws. Catching the 'ExitCode' here lets the statement return a
-- normal 'Bool' result so the eval plugin can stream the test output back into
-- the source file.
tastyEval' :: OutPutOpts -> Maybe Text -> TestTree -> IO TastyResult
tastyEval' outOpt mPattern tree =
  do
    try (withArgs (maybe [] (\pat -> ["-p", unpack pat]) mPattern) $ defaultMain tree)
      <&> \case
        Left ExitSuccess -> Pass
        Left extCode -> Fail extCode
        Right () -> Pass
    -- catch all and handle or rethrow if we dont want StdOut (rethrow short-circuits printing to stdOut)
    >>= \rslt -> case (outOpt, rslt) of
      (NoStdOut, Pass) -> throw ExitSuccess
      (NoStdOut, Fail extFail) -> throw extFail
      (StdOutOnFailure, Pass) -> throw ExitSuccess
      (StdOutOnFailure, _) -> pure rslt
      (StdOut, _) -> pure rslt
