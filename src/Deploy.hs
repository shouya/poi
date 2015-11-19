{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Deploy
       ( deploy
       , DeployStatus(..)
       , DeployLock
       , DeployProc
       ) where

import Data.IORef
import Control.Concurrent (threadDelay)

import Shelly
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Exception
default (T.Text)

type DeployLock = IORef DeployStatus
type DeployProc = DeployLock -> IO ()

data DeployStatus = DeployIdle
                  | Deploying
                  | DeployWaiting

deploy :: DeployProc
deploy lock = catch (deployLock dep lock) handleException
  where dep = shelly $ deployShell
        handleException ex = shelly $
          exceptionShell lock ex


exceptionShell :: DeployLock -> SomeException -> Sh ()
exceptionShell lock ex = do
  echo "--- Exception Caught ---"
  echo $ T.pack $ show ex
  liftIO $ releaseLock lock
  echo "--- Deployment Aborted ---"


deployShell :: Sh ()
deployShell = sub $ errExit True $ do
  cd "vps"
  echo "--- Updating Code ---"
  sub $ updateCode
  echo "--- Building Services ---"
  sub $ buildServices
  echo "--- Restarting Services ---"
  sub $ reloadServices
  echo "--- Deploy Finished ---"


releaseLock :: DeployLock -> IO ()
releaseLock lock = writeIORef lock DeployIdle

deployLock :: IO () -> DeployProc
deployLock deploy lock = do
  status <- readIORef lock
  case status of
   DeployIdle    -> do
     writeIORef lock Deploying
     deploy
     releaseLock lock
   DeployWaiting -> return ()
   Deploying     -> do
     writeIORef lock DeployWaiting
     let wait = do now_status <- readIORef lock
                   case now_status of
                    DeployIdle    -> return ()
                    DeployWaiting -> threadDelay 1000 >> wait
                    _             -> threadDelay 1000 >> wait
       in wait >> deployLock deploy lock


compose :: [Text] -> Sh ()
compose = command_ "docker-compose" []

git :: Text -> [Text] -> Sh ()
git = command1_ "git" []

curl :: [Text] -> Sh ()
curl = command_ "curl" []

type ServiceName = Text

serviceList :: Sh [ServiceName]
serviceList = filter nocomment <$> svrLines
  where config_file = "services"
        svrLines = T.lines <$> readfile config_file
        nocomment txt
          | "#" `T.isPrefixOf` txt = False
          | T.null txt             = False
          | otherwise              = True

reloadServices :: Sh ()
reloadServices = do
  services <- serviceList
  compose ["kill"]
  compose ["rm", "--force"]
  compose (["up", "-d"] <> services)


updateCode :: Sh ()
updateCode = do
  git "fetch" ["origin"]
  git "reset" ["--hard", "origin/master"]

buildServices :: Sh ()
buildServices = do
  compose ["build"]

sendMailMailgun :: Text -> Text -> Text -> Sh ()
sendMailMailgun subject body recipient = do
  let api = ""
      url = ""
      from = ""
      to = ""
  curl ["-s", "--user", api, url,
        "-F", "from=" <> from,
        "-F", "to=" <> to,
        "-F", "subject=" <> subject,
        "-F", "text=" <> body]

mailOutput :: Bool -> Text -> Sh ()
mailOutput isSuccessful log = do
  return ()
