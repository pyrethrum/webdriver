{-# LANGUAGE CPP #-}

module Http.DemoUtils where

import Config (Config (..), loadConfig)
import Const (milliseconds)
import Control.Exception (bracket)
import Data.Text (Text)
import Http.HttpActions (HttpActions (..))
import IOUtils (DemoActions (..), Logger, logNothingLogger, mkDemoActions)
import Logger (withChannelFileLogger)
import Network.HTTP.Req (http)
import RuntimeConst (httpFullCapabilities)
import WebDriverPreCore.Http.Protocol (FullCapabilities, SessionId, SessionResponse (..))

#ifdef LEGACY_TEST
import Http.HttpActionsDeprecated qualified as Legacy
import Http.HttpRunnerDeprecated qualified as Legacy
#else
import Http.HttpRunner (mkRunner)
import Http.HttpActions (mkActions)
#endif

data HttpDemo
  = Demo
      { name :: Text,
        action :: DemoActions -> HttpActions -> IO ()
      }
  | SessionDemo
      { name :: Text,
        sessionAction ::
          SessionId ->
          DemoActions ->
          HttpActions ->
          IO ()
      }

demo :: Text -> (DemoActions -> HttpActions -> IO ()) -> HttpDemo
demo = Demo

sessionDemo :: Text -> (SessionId -> DemoActions -> HttpActions -> IO ()) -> HttpDemo
sessionDemo = SessionDemo

runDemo :: HttpDemo -> IO ()
runDemo demo' = do
  cfg <- loadConfig
  runDemoWithConfig cfg demo'

runDemoWithConfig :: Config -> HttpDemo -> IO ()
runDemoWithConfig cfg demo' = do
  let run lgr = runDemo' cfg lgr demo'
  if cfg.logging
    then
      withChannelFileLogger run
    else
      run logNothingLogger

runDemo' :: Config -> Logger -> HttpDemo -> IO ()
runDemo' cfg@MkConfig {httpUrl, httpPort, pauseMS} lgr demo' = do
  demoActions.logTxt demo'.name
  case demo' of
    Demo _ action -> action demoActions httpActions
    SessionDemo _ action -> withSession capabilities httpActions $ \ses ->
      action ses.sessionId demoActions httpActions
  where
    capabilities = httpFullCapabilities cfg
    demoActions = mkDemoActions lgr $ fromIntegral pauseMS * milliseconds
#ifdef LEGACY_TEST
    runner = Legacy.mkRunner (http httpUrl) (fromIntegral httpPort) demoActions
    httpActions = Legacy.mkDeprecatedActions runner
#else
    runner = mkRunner (http httpUrl) (fromIntegral httpPort) demoActions
    httpActions = mkActions runner
#endif

withSession :: FullCapabilities -> HttpActions -> (SessionResponse -> IO ()) -> IO ()
withSession capabilities http' action = do
  bracket
    (http'.newSession capabilities)
    (http'.deleteSession . (.sessionId))
    action