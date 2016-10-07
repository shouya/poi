module Logger (resetLog, log, logT, sendLog) where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Control.Monad.Trans.Maybe
import Data.Text (pack, unpack, splitOn, strip, Text)
import Data.Monoid
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock
import Data.Time.Format
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
  enabled <- readConf "mailgun" "enabled"
  when enabled sendLogProc
  resetLog


sendLogProc :: IO ()
sendLogProc = do
  message <- prepareMessageText >>= runMaybeT . buildMessage
  case message of
    Nothing -> return ()
    Just message' -> do
      context <- mailContext
      response <- sendEmail' context message'
      print response
      putStrLn "Successfully sent log!"
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
              textBody   = C.pack $ unpack body'
              htmlBody   = C.pack $ "<pre>" <> (unpack body') <> "</pre>"
              body       = TextAndHTML textBody htmlBody
              recips'    = fmap (C.pack . unpack . strip) recips
              recipients = MessageRecipients recips' [] []
          return $ hailgunMessage subj body sender recipients []

prepareMessageText :: IO Text
prepareMessageText = do
  last_ <- atomically $ tryReadTQueue logs
  case last_ of
    Nothing -> return (pack "")
    Just (time, text) -> do
      let head' = printf "(%s) %s\n" (isoFormatTime time) text
      rest' <- prepareMessageText
      return (pack head' <> rest')




isoFormatTime :: UTCTime -> String
isoFormatTime = formatTime defaultTimeLocale format
  where format = iso8601DateFormat (Just "%H:%M:%S")

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x
