{-# LANGUAGE ViewPatterns #-}

module Webhook
       ( startWebhook
       ) where

import Data.List
import Network.HTTP.Server
import Network.Socket.Internal
import Network.URL
import Network.HTTP.Server.Logger

import Config
import Status (pushTask)

startWebhook :: IO ()
startWebhook = config >>= flip serverWith handler

config :: IO Config
config = do
  port' <- readConf "server" "port"
  host' <- readConf "server" "host"
  return Config { srvLog = stdLogger
                , srvHost = host'
                , srvPort = port'
                }

respondWith :: StatusCode -> Response String
respondWith s = insertHeader HdrContentLength "0"
              $ respond s

handler :: SockAddr -> URL -> Request String -> IO (Response String)
handler _ url _ = do
  prefix <- readConf "server" "prefix"
  case stripPrefix prefix (url_path url) of
    Nothing   -> return $ respondWith NotFound
    Just path -> handlePath path

handlePath :: String -> IO (Response String)
handlePath (stripPrefix "webhook" -> Just _) = do
  pushTask
  return $ respondWith OK
handlePath _ = return $ respondWith NotFound
