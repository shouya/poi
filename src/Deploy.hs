{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Deploy
       ( deploy
       , DeployStatus(..)
       , DeployLock
       , DeployProc
       , onlyUpdateCode
       , onlyBuildServices
       , onlyReloadServices
       , setupServices
       , onlyGenerateServiceBundle
       ) where

import Data.IORef
import Control.Concurrent (threadDelay)

import Shelly hiding (echo)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Exception

import Text.Printf
import Data.Either

import TextCollector
import Config
import EmailLog

default (Text)

type DeployLock = IORef DeployStatus
type DeployProc = DeployLock -> IO ()

data DeployStatus = DeployIdle
                  | Deploying
                  | DeployWaiting

dir = "vps"
serviceListFile = "services"

deploy :: DeployProc
deploy lock = deployLock dep lock >> releaseLock lock
  where dep = shelly $ collectAndSendOutput deployShell

echo :: Text -> Sh ()
echo x = command "echo" [] [x] >> return ()

withinServiceBundle :: Sh a -> Sh a
withinServiceBundle sh = sub (cd dir >> sh)

stepReported :: Text -> Sh a -> Sh a
stepReported name sh = sub $ do
  echo $ "--- "        <> name <> " ---"
  a <- sh
  echo $ "--- Finish " <> name <> " ---"
  return a

collectAndSendOutput :: Sh a -> Sh ()
collectAndSendOutput sh = liftIO $ do
  iotc <- newTextCollector
  let appText'   = appendText iotc
      appText sh = log_stderr_with appText' $
                   log_stdout_with appText' sh

  result <- try (shelly $ appText sh)
  handle result appText

  text <- extractTextCollector iotc
  final result text

  return ()
  where
    logException ex appText = shelly $ appText $ do
      echo "--- Exception Caught ---"
      echo $ T.pack $ show ex
      echo "--- Deployment Aborted ---"
    handle (Left (ex :: SomeException)) appText = logException ex appText
    handle (Right _) _       = return ()
    final result text = do
      let succ = either (const False) (const True) result
      emailDeployResult succ text

onlyDo :: Text -> Sh a -> IO ()
onlyDo name sh = onlyDoWithoutCd name sh'
  where sh' = withinServiceBundle sh

onlyDoWithoutCd :: Text -> Sh a -> IO ()
onlyDoWithoutCd name sh = shelly $
                          errExit True $
                          stepReported name $
                          sh >> return ()

onlyUpdateCode :: IO ()
onlyUpdateCode = onlyDo "Updating Code" updateCode

onlyBuildServices :: IO ()
onlyBuildServices = onlyDo "Building Services" buildServices

onlyReloadServices :: IO ()
onlyReloadServices = onlyDo "Reloading Services" reloadServices

onlyCloneServiceBundle :: String -> IO ()
onlyCloneServiceBundle url = onlyDoWithoutCd "Cloning Repo" clone
  where clone = cloneServiceBundle (T.pack url)

onlyGenerateServiceBundle :: String -> IO ()
onlyGenerateServiceBundle directory = onlyDoWithoutCd name gen
  where name = "Generating Service Bundle Template"
        gen = generateServiceBundle (fromText $ T.pack directory)

setupServices :: String -> IO ()
setupServices url = shelly logged
  where logged = stepReported "Setting Up Service Bundle" setup
        setup = errExit True $ do liftIO (onlyCloneServiceBundle url)
                                  liftIO onlyBuildServices


exceptionShell :: DeployLock -> SomeException -> Sh ()
exceptionShell lock ex = do
  echo "--- Exception Caught ---"
  echo $ T.pack $ show ex
  liftIO $ releaseLock lock
  echo "--- Deployment Aborted ---"


deployShell :: Sh ()
deployShell = sub $ errExit True $ do
  cd dir
  echo "--- Updating Code ---"
  sub updateCode
  echo "--- Building Services ---"
  sub buildServices
  echo "--- Restarting Services ---"
  sub reloadServices
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
compose x = command "docker-compose" [] x >> return ()

git :: Text -> [Text] -> Sh ()
git x y = command1 "git" [] x y >> return ()

curl :: [Text] -> Sh ()
curl x = command "curl" x [] >> return ()

type ServiceName = Text

cloneServiceBundle :: Text -> Sh ()
cloneServiceBundle url = do
  git "clone" [url, toTextIgnore dir]

serviceList :: Sh [ServiceName]
serviceList = filter nocomment <$> svrLines
  where svrLines = T.lines <$> readfile serviceListFile
        nocomment txt
          | "#" `T.isPrefixOf` txt = False
          | T.null txt             = False
          | otherwise              = True

reloadServices :: Sh ()
reloadServices = do
  services <- serviceList
  compose ["kill"]
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

generateServiceBundle :: Shelly.FilePath -> Sh ()
generateServiceBundle directory = sub $ do
  cd directory
  git "init" []
  touchfile "docker-compose.yml"
  touchfile serviceListFile
