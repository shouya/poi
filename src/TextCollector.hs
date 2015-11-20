{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module TextCollector
       ( newTextCollector
       , appendText
       , extractTextCollector
       , TextCollector
       ) where

import Data.IORef
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Text (Text)
default (T.Text)

newtype TextCollector = TextCollector { unTCRef :: IORef Text }


newTextCollector :: IO TextCollector
newTextCollector = TextCollector <$> newIORef ""

appendText :: TextCollector -> Text -> IO ()
appendText (TextCollector reft) t = modifyIORef reft (<> "\n" <> t)

extractTextCollector :: TextCollector -> IO Text
extractTextCollector = readIORef . unTCRef
