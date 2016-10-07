module Logger (resetLog, log, logT, sendLog) where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Text (pack, unpack, splitOn, strip, Text)
import qualified Data.ByteString.Char8 as C
import Data.List (intercalate)
import Data.Time.Clock
import Data.Time.Format
import Data.Maybe
import Text.Printf
import System.IO.Unsafe
import Mail.Hailgun

import Config

type LogT = (UTCTime, Text)

{-# NOINLINE logs #-}
logs :: TQueue LogT
logs = unsafePerformIO newTQueueIO

resetLog :: IO ()
resetLog = atomically reset
  where reset = tryReadTQueue logs >>= \x -> case x of
          Nothing -> return ()
          Just _  -> reset

log :: String -> IO ()
log = logT . pack


logT :: Text -> IO ()
logT text = do
  now <- getCurrentTime
  atomically $ writeTQueue logs (now, text)


-- this function send the log via email
sendLog :: IO ()
sendLog = do
  (collapseMaybe . runMaybeT) sendLogProc
  resetLog
  where collapseMaybe :: IO (Maybe ()) -> IO ()
        collapseMaybe = fmap (fromMaybe ())


sendLogProc :: MaybeT IO ()
sendLogProc = do
  lift (readConf "mailgun" "enabled") >>= flip when (fail "")
  message <- lift prepareMessageText >>= buildMessage
  context <- lift mailContext
  response <- MaybeT $ sendEmail' context message
  lift $ putStrLn "Successfully sent log!"
  lift $ print response
  where sendEmail' = (fmap . fmap . fmap) eitherToMaybe sendEmail


mailContext :: IO HailgunContext
mailContext = ctx (readConf "mailgun" "domain")
                  (readConf "mailgun" "api_key")
                  (return Nothing)
  where ctx = liftM3 HailgunContext


buildMessage :: Text -> MaybeT IO HailgunMessage
buildMessage body' = MaybeT $ fmap eitherToMaybe message
  where message = do
          now    <- isoFormatTime <$> getCurrentTime
          sender <- C.pack <$> readConf "mailgun" "sender"
          recips <- splitOn (pack ",") . pack <$> readConf "mailgun" "recipients"
          let subj = pack $ printf "%s %s" now "poi build result"
              body = TextOnly $ C.pack $ unpack body'
              recips'    = fmap (C.pack . unpack . strip) recips
              recipients = MessageRecipients recips' [] []
          return $ hailgunMessage subj body sender recipients []

prepareMessageText :: IO Text
prepareMessageText = atomically $ do
  xs <- fmap (takeWhile isJust) (sequence $ repeat $ tryReadTQueue logs)
  let xs' = fromMaybe [] (sequence xs)
      lns = fmap toLine xs'
      msg = joinLines lns
  return (pack msg)
  where toLine (time, text) = printf "(%s) %s" (isoFormatTime time) text
        joinLines = intercalate "\n"



isoFormatTime :: UTCTime -> String
isoFormatTime = formatTime defaultTimeLocale format
  where format = iso8601DateFormat (Just "%H:%M:%S")

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x
