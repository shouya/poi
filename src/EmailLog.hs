{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module EmailLog
       ( emailDeployResult
       ) where

import Mail.Hailgun

import Data.Text (Text)
import Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Monoid ((<>))

import Data.Time
import Text.Printf

import Config


whenM :: Monad m => m Bool -> m () -> m ()
whenM mb a = do
  b <- mb
  if b then a else return ()

poiMailgunContext :: IO HailgunContext
poiMailgunContext = HailgunContext <$> domain <*> api_key <*> pure Nothing
  where domain  = readConf "mailgun" "domain"
        api_key = readConf "mailgun" "api_key"

emailDeployResult :: Bool -> Text -> IO ()
emailDeployResult isSuccessful output =
  whenM (readConf "main" "email_log") $ do
  context   <- poiMailgunContext
  now       <- show <$> getCurrentTime
  sender    <- readConf "mailgun" "from"
  recipient <- readConf "mailgun" "to"

  let succ    = if isSuccessful then "Successful" else "Failed"
      subject = T.pack $ printf "[poi] Deployment %s" succ
      content = TextOnly $ BS.pack $ (T.unpack output <> footer)
      footer  = "\n\n--\n" <>
                "Presented by poi (https://github.com/shouya/poi)\n" <>
                now <> "\n"
      from    = BS.pack sender
      to      = emptyMessageRecipients {recipientsTo = [BS.pack recipient]}
      attach  = []
      message = hailgunMessage subject content from to attach
    in case message of
    Left  err -> return ()
    Right msg -> sendEmail context msg >> return ()
