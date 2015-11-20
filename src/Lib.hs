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
    OptDaemon -> runWebhook
    OptEcho x -> putStrLn x


runWebhook :: IO ()
runWebhook = do
  lock <- newIORef DeployIdle
  putStrLn "Starting poi daemon..."
  putStrLn "Config loaded."
  confString >>= putStrLn
  startWebhook lock deploy
