module Lib (runWebhook) where

import Webhook
import Deploy

import Data.IORef

runWebhook :: IO ()
runWebhook = do
  lock <- newIORef DeployIdle
  startWebhook lock deploy
