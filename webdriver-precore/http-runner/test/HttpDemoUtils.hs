{-# LANGUAGE CPP #-}

module HttpDemoUtils
  ( HttpDemo (..),
    demo,
    sessionDemo,
    runDemo,
    runDemoWithConfig,
    withSession,
  )
where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import WebDriverPreCore.HTTP.Protocol (Command, FullCapabilities, Session, SessionResponse (..))
import WebDriverPreCore.HttpRunner (callWebDriver)
import Actions (HttpActions (..), mkActions)
import WebDriverPreCore.Test.Config (Config (..))
import WebDriverPreCore.Test.ConfigLoader (loadConfig)
import WebDriverPreCore.Test.Const (milliseconds)
import WebDriverPreCore.Test.IOUtils (DemoActions (..), Logger (..), logNothingLogger, mkDemoActions)
import WebDriverPreCore.Test.Logger (withChannelFileLogger)
import WebDriverPreCore.Test.CapabilitiesBuilder (httpFullCapabilities)
import WebDriverPreCore.HttpRunner (HttpEndpoint(..))

data HttpDemo
  = Demo
      { name :: Text,
        action :: DemoActions -> HttpActions -> IO ()
      }
  | SessionDemo
      { name :: Text,
        sessionAction ::
          Session ->
          DemoActions ->
          HttpActions ->
          IO ()
      }

demo :: Text -> (DemoActions -> HttpActions -> IO ()) -> HttpDemo
demo = Demo

sessionDemo :: Text -> (Session -> DemoActions -> HttpActions -> IO ()) -> HttpDemo
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
    -- Create runner functions from endpoint
    mLogger = if cfg.logging then Just lgr.log else Nothing
    httpEndpoint = MkHttpEndpoint {host = httpUrl, port = fromIntegral httpPort}
    run :: forall r. (FromJSON r) => Command r -> IO r
    run = callWebDriver httpEndpoint mLogger
    runBody :: forall r. (FromJSON r) => Command r -> IO Value
    runBody = callWebDriver httpEndpoint mLogger
    httpActions = mkActions run runBody

withSession :: FullCapabilities -> HttpActions -> (SessionResponse -> IO ()) -> IO ()
withSession capabilities http' action = do
  bracket
    (http'.newSession capabilities)
    (http'.deleteSession . (.sessionId))
    action
