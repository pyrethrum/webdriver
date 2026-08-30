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

import Control.Exception (bracket, throwIO)
import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import WebDriverPreCore.HTTP.Protocol (Command, FullCapabilities, Session, SessionResponse (..))
import WebDriverPreCore.HttpRunner (callWebDriver, callWebDriverBody)
import WebDriverPreCore.Error (parseFailToWDException)
import Actions (HttpActions (..), mkActions)
import WebDriverPreCore.Test.Config (Config (..))
import WebDriverPreCore.Test.ConfigLoader (loadConfig)
import WebDriverPreCore.Test.Const (milliseconds)
import WebDriverPreCore.Test.IOUtils (DemoActions (..), Logger (..), mkDemoActions)
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
      run noOpLogger

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
    logger = if cfg.logging then lgr.log else noOpLogger.log
    httpEndpoint = MkHttpEndpoint {host = httpUrl, port = fromIntegral httpPort}
    run :: forall r. (FromJSON r) => Command r -> IO r
    run cmd = callWebDriver httpEndpoint logger cmd >>= either (throwIO . parseFailToWDException) pure

    -- todo check why not an Either
    runBody :: forall r. Command r -> IO Value
    runBody cmd = callWebDriverBody httpEndpoint logger cmd

    httpActions = mkActions run runBody
    

withSession :: FullCapabilities -> HttpActions -> (SessionResponse -> IO ()) -> IO ()
withSession capabilities http' action = do
  bracket
    (http'.newSession capabilities)
    (http'.deleteSession . (.sessionId))
    action


noOpLogger :: Logger
noOpLogger = MkLogger (\_ -> pure ())