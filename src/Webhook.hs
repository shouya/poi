module Webhook
       ( startWebhook
       ) where


import qualified Data.ByteString.Char8 as B8

import Network (listenOn, PortID(..))
import Network.Socket (withSocketsDo, accept, sClose)
import Network.Socket.ByteString (sendAll)
import Control.Concurrent (forkIO)

import Deploy
import Config

startWebhook :: DeployLock -> DeployProc -> IO ()
startWebhook lock deploy = withSocketsDo $ do
  port <- (read <$> readConf "main" "listen_port" :: IO Int)
  sock <- listenOn $ PortNumber (toEnum port)
  loop sock
  where loop sock = do
          (conn, _) <- accept sock
          forkIO $ handle conn
          loop sock
        handle conn = do forkIO $ deploy lock
                         sendAll conn $ B8.pack msg
                         sClose conn
        msg = "HTTP/1.0 200 OK\r\nContent-Length: 3\r\n\r\npoi\r\n"
