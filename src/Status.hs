module Status
  ( isIdle, isBusy, setIdle, setBusy
  , pushTask, waitTask
  ) where

import System.IO.Unsafe
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

{-# NOINLINE busyState #-}
busyState :: TVar Bool
busyState = unsafePerformIO $ newTVarIO False

isBusy, isIdle :: IO Bool
isBusy = readTVarIO busyState
isIdle = not <$> isBusy

setBusy, setIdle :: IO ()
setBusy = atomically $ writeTVar busyState True
setIdle = atomically $ writeTVar busyState False


{-# NOINLINE chan #-}
chan :: TChan ()
chan = unsafePerformIO newTChanIO

pushTask :: IO ()
pushTask = atomically $ do
  clearChan
  writeTChan chan ()

  where clearChan = do
          result <- tryReadTChan chan
          case result of
            Just _ -> clearChan
            Nothing -> return ()

waitTask :: IO ()
waitTask = atomically $ readTChan chan
