{-# LANGUAGE ViewPatterns #-}

module Webhook
       ( startWebhook
       ) where

import           Data.List
import           Data.Text                  (unpack)
import           Network.HTTP.Server
import           Network.HTTP.Server.Logger
import           Network.Socket.Internal
import           Network.URL

import           Config
import           Logger                     (peekMessageText)
import           Status                     (pushTask)

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

respondWithStatus :: StatusCode -> Response String
respondWithStatus s = insertHeader HdrContentLength "0"
                    $ respond s

respondWithString :: String -> Response String
respondWithString body = insertHeader HdrContentLength (show $ length body)
                       $ insertHeader HdrContentType "text/plain"
                       $ (respond OK :: Response String) { rspBody = body }

handler :: SockAddr -> URL -> Request String -> IO (Response String)
handler _ url _ = do
  print url
  prefix <- readConf "server" "prefix"
  case stripPrefix prefix (url_path url) of
    Nothing   -> return $ respondWithStatus NotFound
    Just path -> handlePath path

handlePath :: String -> IO (Response String)
handlePath (stripPrefix "webhook" -> Just _) = do
  pushTask
  return $ respondWithStatus OK
handlePath (stripPrefix "reload" -> Just _) = do
  reloadConfig
  respondWithString <$> confString
handlePath (stripPrefix "build" -> Just _) = do
  pushTask
  return $ respondWithString "Build scheduled"
handlePath (stripPrefix "logs" -> Just _) =
  (respondWithString . unpack) <$> peekMessageText
handlePath _ = return $ respondWithStatus NotFound
