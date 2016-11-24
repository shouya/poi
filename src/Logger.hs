module Logger (resetLog, log, logT, sendLog, peekMessageText) where

import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.Loops           (unfoldM)
import           Control.Monad.STM
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8         as C
import           Data.Monoid
import           Data.Text                     (Text, pack, splitOn, strip,
                                                unpack)
import           Data.Time.Clock
import           Data.Time.Format
import           Mail.Hailgun
import           Prelude                       hiding (log)
import           System.IO.Unsafe
import           Text.Printf

import           Config
import           Util

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
  let newLog = (now, text)
  atomically $ writeTQueue logs newLog
  putStrLn $ formatLine newLog


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

mailgunRecipients :: IO [C.ByteString]
mailgunRecipients = do
  conf <- pack <$> readConf "mailgun" "recipients"
  let strList = splitOn (pack ",") conf
      recipsBS = fmap (C.pack . unpack . strip) strList
  return recipsBS



buildMessage :: Text -> MaybeT IO HailgunMessage
buildMessage body' = MaybeT $ fmap eitherToMaybe message
  where message = do
          now    <- isoFormatTime <$> getCurrentTime
          sender <- C.pack <$> readConf "mailgun" "sender"
          recips <- mailgunRecipients
          let subj = pack $ printf "%s %s" now "poi build result"
              textBody   = C.pack $ unpack body'
              htmlBody   = C.pack $ "<pre>" <> unpack body' <> "</pre>"
              body       = TextAndHTML textBody htmlBody
              recipients = MessageRecipients recips [] []
          return $ hailgunMessage subj body sender recipients []

-- prepareMessageText :: IO Text
-- prepareMessageText = do
--   last_ <- atomically $ tryReadTQueue logs
--   case last_ of
--     Nothing -> return (pack "")
--     Just log -> do
--       let head' = formatLine log
--       rest' <- prepareMessageText
--       return (pack head' <> rest')

prepareMessageText :: IO Text
prepareMessageText = logsToText <$> readAllLog

peekMessageText :: IO Text
peekMessageText = logsToText <$> peekAllLog

logsToText :: [LogT] -> Text
logsToText = pack . concatMap formatLine

peekAllLog :: IO [LogT]
peekAllLog = atomically $ do
    allLogs <- unfoldM (tryReadTQueue logs)
    forM_ allLogs (writeTQueue logs)
    return allLogs

readAllLog :: IO [LogT]
readAllLog = atomically $ unfoldM (tryReadTQueue logs)


formatLine :: LogT -> String
formatLine (time, text) = printf "(%s) %s\n" (isoFormatTime time) text



isoFormatTime :: UTCTime -> String
isoFormatTime = formatTime defaultTimeLocale format
  where format = iso8601DateFormat (Just "%H:%M:%S")

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

