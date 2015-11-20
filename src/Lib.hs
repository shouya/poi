module Lib (runPoi) where

import Webhook
import Deploy
import Config
import Option

import Data.IORef

runPoi :: IO ()
runPoi = do
  cmd <- parseOptions
  case cmd of
    CmdDaemon  -> runWebhook
    CmdEcho x  -> putStrLn x
    CmdBuild   -> onlyBuildServices
    CmdUp      -> onlyReloadServices
    CmdSetup x -> setupServices x


runWebhook :: IO ()
runWebhook = do
  lock <- newIORef DeployIdle
  putStrLn "Starting poi daemon..."
  putStrLn "Config loaded."
  confString >>= putStrLn
  startWebhook lock deploy
