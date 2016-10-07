module Daemon
  ( deployDaemon
  , webserverDaemon
  ) where

import Prelude hiding (log)
import Control.Concurrent (forkFinally)
import Text.Printf
import Logger (log, sendLog)
import Deploy (deployWorker)
import Webhook (startWebhook)

deployDaemon :: IO ()
deployDaemon = do
  _ <- forkFinally deployWorker finishUpAndRestart
  return ()
  where logEvent (Left exception) =
          log $ printf "Error occured: %s\n" (show exception)
        logEvent (Right _) =
          log "finished"
        finishUpAndRestart x = do
          logEvent x
          deployWorker
          sendLog

webserverDaemon :: IO ()
webserverDaemon = do
  _ <- forkFinally startWebhook finishUpAndRestart
  return ()
  where logEvent (Left exception) =
          log $ printf "Webhook abort: %s, restarting.\n" (show exception)
        logEvent (Right _) =
          log $ printf "Webhook exited gracefully, restarting.\n"
        finishUpAndRestart x = do
          logEvent x
          startWebhook
