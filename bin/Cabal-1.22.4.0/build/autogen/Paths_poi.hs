module Paths_poi (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/tmp/.stack-work/install/x86_64-linux/lts-3.11/7.10.2/bin"
libdir     = "/tmp/.stack-work/install/x86_64-linux/lts-3.11/7.10.2/lib/x86_64-linux-ghc-7.10.2/poi-0.1.0.0-CIO5lZRGiFRJqrUdRLTgoa"
datadir    = "/tmp/.stack-work/install/x86_64-linux/lts-3.11/7.10.2/share/x86_64-linux-ghc-7.10.2/poi-0.1.0.0"
libexecdir = "/tmp/.stack-work/install/x86_64-linux/lts-3.11/7.10.2/libexec"
sysconfdir = "/tmp/.stack-work/install/x86_64-linux/lts-3.11/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "poi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "poi_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "poi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "poi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "poi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
