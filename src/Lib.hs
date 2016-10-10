module Lib (runPoi) where

import Control.Monad
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
sleepForever = forever $ threadDelay 9999999
