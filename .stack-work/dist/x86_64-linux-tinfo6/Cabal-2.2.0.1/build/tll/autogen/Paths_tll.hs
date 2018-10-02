{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tll (
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

bindir     = "/home/tll/Documents/Coding/Web/thelostlambda.xyz/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/bin"
libdir     = "/home/tll/Documents/Coding/Web/thelostlambda.xyz/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3/tll-0.1.0.0-JBwbfu68bO9AA0Y1tAft7o-tll"
dynlibdir  = "/home/tll/Documents/Coding/Web/thelostlambda.xyz/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/tll/Documents/Coding/Web/thelostlambda.xyz/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/share/x86_64-linux-ghc-8.4.3/tll-0.1.0.0"
libexecdir = "/home/tll/Documents/Coding/Web/thelostlambda.xyz/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/libexec/x86_64-linux-ghc-8.4.3/tll-0.1.0.0"
sysconfdir = "/home/tll/Documents/Coding/Web/thelostlambda.xyz/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tll_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tll_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tll_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tll_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tll_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tll_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
