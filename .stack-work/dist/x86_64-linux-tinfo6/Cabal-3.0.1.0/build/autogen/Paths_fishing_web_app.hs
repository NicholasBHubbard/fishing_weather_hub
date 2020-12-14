{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_fishing_web_app (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nick/src/haskell/fishing-web-app/.stack-work/install/x86_64-linux-tinfo6/63adb297965e520643554252eff19cdeba8e099179ec88136e8451e9a9365e69/8.8.3/bin"
libdir     = "/home/nick/src/haskell/fishing-web-app/.stack-work/install/x86_64-linux-tinfo6/63adb297965e520643554252eff19cdeba8e099179ec88136e8451e9a9365e69/8.8.3/lib/x86_64-linux-ghc-8.8.3/fishing-web-app-0.1.0.0-AzulKCkP32EFEYcvLNAjb6"
dynlibdir  = "/home/nick/src/haskell/fishing-web-app/.stack-work/install/x86_64-linux-tinfo6/63adb297965e520643554252eff19cdeba8e099179ec88136e8451e9a9365e69/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/nick/src/haskell/fishing-web-app/.stack-work/install/x86_64-linux-tinfo6/63adb297965e520643554252eff19cdeba8e099179ec88136e8451e9a9365e69/8.8.3/share/x86_64-linux-ghc-8.8.3/fishing-web-app-0.1.0.0"
libexecdir = "/home/nick/src/haskell/fishing-web-app/.stack-work/install/x86_64-linux-tinfo6/63adb297965e520643554252eff19cdeba8e099179ec88136e8451e9a9365e69/8.8.3/libexec/x86_64-linux-ghc-8.8.3/fishing-web-app-0.1.0.0"
sysconfdir = "/home/nick/src/haskell/fishing-web-app/.stack-work/install/x86_64-linux-tinfo6/63adb297965e520643554252eff19cdeba8e099179ec88136e8451e9a9365e69/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fishing_web_app_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fishing_web_app_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fishing_web_app_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fishing_web_app_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fishing_web_app_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fishing_web_app_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
