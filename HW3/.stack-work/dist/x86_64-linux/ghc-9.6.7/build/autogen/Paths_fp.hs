{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_fp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/luisbittau/VU/FP/assignments/fp-private/HW3/.stack-work/install/x86_64-linux/d31b0a5d5debf12bb649055f6edc546f0e3c4e6b3bf4275c1ff8baf472a0dff2/9.6.7/bin"
libdir     = "/home/luisbittau/VU/FP/assignments/fp-private/HW3/.stack-work/install/x86_64-linux/d31b0a5d5debf12bb649055f6edc546f0e3c4e6b3bf4275c1ff8baf472a0dff2/9.6.7/lib/x86_64-linux-ghc-9.6.7/fp-1.0.0-AV2wvIXaOHFJYKhdnafL7U"
dynlibdir  = "/home/luisbittau/VU/FP/assignments/fp-private/HW3/.stack-work/install/x86_64-linux/d31b0a5d5debf12bb649055f6edc546f0e3c4e6b3bf4275c1ff8baf472a0dff2/9.6.7/lib/x86_64-linux-ghc-9.6.7"
datadir    = "/home/luisbittau/VU/FP/assignments/fp-private/HW3/.stack-work/install/x86_64-linux/d31b0a5d5debf12bb649055f6edc546f0e3c4e6b3bf4275c1ff8baf472a0dff2/9.6.7/share/x86_64-linux-ghc-9.6.7/fp-1.0.0"
libexecdir = "/home/luisbittau/VU/FP/assignments/fp-private/HW3/.stack-work/install/x86_64-linux/d31b0a5d5debf12bb649055f6edc546f0e3c4e6b3bf4275c1ff8baf472a0dff2/9.6.7/libexec/x86_64-linux-ghc-9.6.7/fp-1.0.0"
sysconfdir = "/home/luisbittau/VU/FP/assignments/fp-private/HW3/.stack-work/install/x86_64-linux/d31b0a5d5debf12bb649055f6edc546f0e3c4e6b3bf4275c1ff8baf472a0dff2/9.6.7/etc"

getBinDir     = catchIO (getEnv "fp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "fp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "fp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "fp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fp_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
