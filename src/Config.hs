module Config
       ( readConf
       , readConfT
       , confString
       , loadConfig
       , reloadConfig
       ) where

import           Data.ConfigFile
import           Data.IORef
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Printf

import           System.Directory
import           System.IO.Unsafe

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

forceEither :: Either e a -> a
forceEither (Left _)  = error "forceEither over (Left x)!"
forceEither (Right a) = a

foo = subtract 2 1

{-# NOINLINE config #-}
config :: IORef ConfigParser
config = unsafePerformIO $ newIORef emptyCP

{-# NOINLINE configPath #-}
configPath :: IORef String
configPath = unsafePerformIO $ newIORef ""

reloadConfig :: IO ()
reloadConfig = do
  path <- readIORef configPath
  newConf <- fmap (either id id) . runExceptT $ do
    unlessFileExist path $ do
      lift $ putStrLn $ "Config path does not exist: " ++ path
      throwE defaultConfig
    mapExceptT transConfig $ ExceptT $ readfile defaultConfig path
  writeIORef config newConf

  where unlessFileExist x = lift (doesFileExist x) >.> unless
        (ma >.> ambmb) mb = ma >>= flip ambmb mb
        transConfig mea = mea >>= \ea -> case ea of
          Left err -> do putStrLn $ "Error parsing config: " ++ show err
                         return $ Left defaultConfig
          Right a  -> return $ Right a


loadConfig :: String -> IO ()
loadConfig path = do
  writeIORef configPath path
  reloadConfig

defaultConfig :: ConfigParser
defaultConfig = forceEither $ readstring emptyCP conf
  where conf = "[server]" <~>
               "host = 127.0.0.1" <~>
               "port = 8000" <~>
               "prefix = " <~>
               "" <~>
               "[git]" <~>
               "repo = https://github.com/shouya/poi.git" <~>
               "branch = demo" <~>
               "dir = /home/shou/demo" <~>
               "" <~>
               "[script]" <~>
               "mode = script" <~>
               "work_dir = /home/shou/demo" <~>
               "run = /home/shou/demo/script.sh" <~>
               "" <~>
               "[mailgun]" <~>
               "enabled = 0" <~>
               "domain = [your_domain]" <~>
               "api_key = [your_api_key]" <~>
               "sender = example@your_domain" <~>
               "recipients = user1@gmail.com,user2@hotmail.com"
        a <~> b = a ++ "\n" ++ b

instance Get_C Text where
  get a b c = T.pack <$> get a b c

readConfT :: Get_C a => Text -> Text -> IO a
readConfT a b = readConf (T.unpack a) (T.unpack b)

readConf :: Get_C a => String -> String -> IO a
readConf a b = do
  conf <- readIORef config
  case get conf a b of
    Right x -> return x
    Left _  -> error $ printf "Unknown config [%s:%s]." a b

confString :: IO String
confString = to_string <$> readIORef config

