module Lib (runPoi) where

import Control.Concurrent
import Text.Printf (printf)

import Daemon
import Config
import Option
import Deploy (deploy)


runPoi :: IO ()
runPoi = do
  (cfg, command) <- parseOptions
  loadConfig cfg
  runCommand command
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

{-
data Subcommand = CmdDaemon
                | CmdRun
                | CmdGenConf
                | CmdCheckConfig
                deriving Show
-}

runCommand :: Subcommand -> IO ()

runCommand CmdDaemon = do
  deployDaemon
  webserverDaemon
  sleepForever
runCommand CmdRun = deploy
runCommand CmdGenConf = confString >>= putStrLn
runCommand CmdCheckConfig = runCommand CmdGenConf

sleepForever :: IO ()
sleepForever = threadDelay (1000 * 60 * 60 * 24 * 365 * 100) >> sleepForever
