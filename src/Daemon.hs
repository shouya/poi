module Daemon
  ( deployDaemon
  , webserverDaemon
  ) where

import Prelude hiding (log)
import Control.Concurrent
import Control.Monad

import Logger (sendLog)
import Deploy (deployWorker)
import Webhook (startWebhook)

deployDaemon :: IO ThreadId
deployDaemon = forkIO $ forever $ do
  deployWorker
  sendLog
  putStrLn "Successfully deployed"

webserverDaemon :: IO ThreadId
webserverDaemon = forkIO $ forever startWebhook
