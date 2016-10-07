{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Deploy ( deploy, deployWorker ) where

import Shelly
import Data.Text (Text)
import Data.Monoid ((<>))
import Prelude hiding (FilePath, log)
import Control.Monad (void)

import Logger (logT)
import Status (waitTask)
import Config (readConfT)
default (Text)

deployWorker :: IO ()
deployWorker = do
  waitTask
  logT "task received, starting process"
  deploy

deploy :: IO ()
deploy = shelly $ withShellConfig $ do
  checkoutLatestSource
  runScript


checkoutLatestSource :: Sh ()
checkoutLatestSource = do
  dir <- readConfPathSh "git" "dir"
  whenM (test_f dir) $
    errorExitLog $ "Target directory is a file: " <> toTextIgnore dir
  unlessM (test_d dir) $ void clone

  cdLog dir
  branch <- readConfSh "git" "branch"
  void $ git "fetch" ["origin", branch]
  void $ git "reset" ["--hard", "origin/" <> branch]

  where clone = do
          repo <- readConfSh "git" "repo"
          dir <- readConfSh "git" "dir"
          git "clone" (cloneOptions ++ [repo, dir])
        git = command1Log "git" []
        cloneOptions = ["-v", "--progress", "--recursive", "--depth", "1"]

runScript :: Sh ()
runScript = do
  dir <- readConfPathSh "script" "work_dir"
  script <- readConfPathSh "script" "run"

  unlessM (test_d dir) $
    errorExitLog $ "Target directory does not exist: " <> toTextIgnore dir

  void $ command1Log "/bin/bash" ["-c"] (toTextIgnore script) []
  return ()


commandLog :: FilePath -> [Text] -> [Text] -> Sh Text
commandLog a b c = do
  liftIO $ shellyLogger "CMD" $ show_command a (b <> c)
  command a b c

command1Log :: FilePath -> [Text] -> Text -> [Text] -> Sh Text
command1Log a b c d = do
  liftIO $ shellyLogger "CMD" $ show_command a (b <> [c] <> d)
  command1 a b c d

readConfSh :: Text -> Text -> Sh Text
readConfSh a b = liftIO $ readConfT a b

readConfPathSh :: Text -> Text -> Sh FilePath
readConfPathSh a b = fromText <$> readConfSh a b

cdLog :: FilePath -> Sh ()
cdLog a = cd a >> liftIO (logT $ "chdir to " <> toTextIgnore a)

errorExitLog :: Text -> Sh ()
errorExitLog a = errorExit a >> liftIO (logT a)

shellyLogger :: Text -> Text -> IO ()
shellyLogger typ text = logT $ "[" <> typ <> "] " <> text

logWithLogger :: Sh a -> Sh a
logWithLogger = log_stdout_with (shellyLogger "OUT") .
                log_stderr_with (shellyLogger "ERR")

withShellConfig :: Sh a -> Sh a
withShellConfig = logWithLogger .
                  escaping False .
                  tracing True .
                  errExit True .
                  print_commands True
