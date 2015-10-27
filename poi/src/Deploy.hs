{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Deploy
       ( deploy
       , DeployStatus (..)
       , DeployLock
       , DeployProc
       ) where

import Data.IORef
import Control.Concurrent (threadDelay)

import Shelly
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
default (T.Text)

type DeployLock = IORef DeployStatus
type DeployProc = DeployLock -> IO ()

data DeployStatus = DeployIdle
                  | Deploying
                  | DeployWaiting

deploy :: DeployProc
deploy = deployLock dep
  where dep = shelly $ sub $ do

          updateCode
          buildServices
          reloadServices

deployLock :: IO () -> DeployProc
deployLock deploy lock = do
  status <- readIORef lock
  case status of
   DeployIdle    -> do
     writeIORef lock Deploying
     deploy
     writeIORef lock DeployIdle
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
serviceList = liftIO $ map T.pack <$> filter nocomment <$> lines <$> content
  where config_file = "./services"
        content = readFile config_file
        nocomment ('#':_) = False
        nocomment []      = False
        nocomment _       = True

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

mailOutput :: Bool -> Text -> Sh ()
mailOutput isSuccessful log = do
  curl ["-s", "--user", api, url,
        "-F", "from=" <> from,
        "-F", "to=" <> to]
