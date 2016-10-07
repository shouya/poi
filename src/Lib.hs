module Lib (runPoi) where

import Webhook
import Daemon
import Config
import Option

import Data.IORef

runPoi :: IO ()
runPoi = undefined
  -- cmd <- parseOptions
  -- case cmd of
    -- CmdDaemon config -> runDaemon

    -- CmdDaemon  -> runWebhook
    -- CmdEcho x  -> putStrLn x
    -- CmdBuild   -> onlyBuildServices
    -- CmdUp      -> onlyReloadServices
    -- CmdSetup x -> setupServices x
    -- CmdInit Nothing  -> onlyGenerateServiceBundle "."
    -- CmdInit (Just x) -> onlyGenerateServiceBundle x
