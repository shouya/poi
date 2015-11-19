module Lib (runWebhook) where

import Webhook
import Deploy
import Config

import Data.IORef

runWebhook :: IO ()
runWebhook = do
  lock <- newIORef DeployIdle
  putStrLn "Starting poi daemon..."
  putStrLn "Config loaded."
  confString >>= putStrLn
  startWebhook lock deploy
