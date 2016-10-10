module Daemon
  ( deployDaemon
  , webserverDaemon
  ) where

import Prelude hiding (log)
import Control.Concurrent
import Control.Monad
import Text.Printf

import Deploy (deployWorker)
import Webhook (startWebhook)

deployDaemon :: IO ()
deployDaemon = supervisor "deployer" deployWorker

webserverDaemon :: IO ()
webserverDaemon = supervisor "webserver" startWebhook


supervisor :: String -> IO a -> IO ()
supervisor name operation = void $ forkFinally operation ((>> redo) . and_then)
  where and_then (Left exception) = do
          printf "exception occured in %s\n" name
          print exception
        and_then (Right _) = printf "%s exited gracefully\n" name
        redo = do
          threadDelay 1000000 -- wait for 1 sec
          supervisor name operation
