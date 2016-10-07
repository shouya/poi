module Daemon
  ( deployDaemon
  , webserverDaemon
  ) where

import Prelude hiding (log)
import Control.Concurrent (forkFinally)
import Text.Printf
import Logger (resetLog, log, sendLog)
import Deploy (deployWorker)
import Status (waitTask)
import Webhook (startWebhook)

deployDaemon :: IO ()
deployDaemon = do
  _ <- forkFinally deployWorker finishUpAndRestart
  return ()
  where logEvent (Left exception) = log $ printf "error occured: %s" (show exception)
        logEvent (Right _) = log "finished"
        finishUpAndRestart x = do
          logEvent x
          sendLog
          deployWorker

webserverDaemon :: IO ()
webserverDaemon = do
  _ <- forkFinally startWebhook finishUpAndRestart
  return ()
  where logEvent (Left exception) =
          printf "Webhook exited with exception: %s, restarting." (show exception)
        logEvent (Right _) =
          printf "Webhook exited gracefully, restarting."
        finishUpAndRestart x = do
          logEvent x
          startWebhook
