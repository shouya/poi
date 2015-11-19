module Config
       ( readConf
       , readConfT
       , confString
       ) where

import Data.ConfigFile
import Data.Functor ((<$>))

import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf

configPath = "vps/poi.conf"

forceEither :: Either e a -> a
forceEither (Left _)  = error "forceEither over (Left x)!"
forceEither (Right a) = a

poiConf :: IO ConfigParser
poiConf = do
  result <- readfile defaultConfig configPath
  case result of
    Left  _ -> do
      print "Read config error, using default config"
      return defaultConfig
    Right a -> return a


defaultConfig :: ConfigParser
defaultConfig = forceEither $ do
  let cp = emptyCP
  cp <- add_section emptyCP "main"
  cp <- set cp "main" "email_log" "0"
  cp <- set cp "main" "listen_port" "8000"

  cp <- add_section cp "mailgun"
  cp <- set cp "mailgun" "from" ""
  cp <- set cp "mailgun" "to" ""

  return cp


readConfT :: Get_C a => Text -> Text -> IO a
readConfT a b = readConf (T.unpack a) (T.unpack b)

readConf :: Get_C a => String -> String -> IO a
readConf a b = do
  purePoiConf <- poiConf
  case get purePoiConf a b of
    Right x -> return x
    Left _  -> error $ printf "Unknown config [%s:%s]." a b



confString :: IO String
confString = to_string <$> poiConf
