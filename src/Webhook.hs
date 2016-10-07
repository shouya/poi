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

emptyResponse :: Int -> String -> Response String
emptyResponse code reason' =
  Response { rspCode    = transformCode code
           , rspReason  = reason'
           , rspBody    = ""
           , rspHeaders = []
           }
  where transformCode c = apply3 (`mod` 10) (c `div` 100, c `div` 10, c)
        apply3 f (a,b,c) = (f a, f b, f c)

handler :: SockAddr -> URL -> Request a -> IO (Response String)
handler _ url _ = do
  prefix <- readConf "server" "prefix"
  case stripPrefix prefix (url_path url) of
    Nothing   -> return $ emptyResponse 404 "Not Found"
    Just path -> handlePath path

handlePath :: String -> IO (Response String)
handlePath (stripPrefix "webhook" -> Just _) = do
  pushTask
  return $ emptyResponse 200 "Success"
handlePath _ = return $ emptyResponse 404 "Not found"
