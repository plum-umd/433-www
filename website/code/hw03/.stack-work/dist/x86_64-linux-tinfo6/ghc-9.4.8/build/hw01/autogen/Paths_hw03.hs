{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hw03 (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/lemonidas/433-www/website/code/hw03/.stack-work/install/x86_64-linux-tinfo6/5ef49d94af61c8956516f024b7e9c6bec0e6ec82f933a9743e8719bfc4b2aea8/9.4.8/bin"
libdir     = "/home/lemonidas/433-www/website/code/hw03/.stack-work/install/x86_64-linux-tinfo6/5ef49d94af61c8956516f024b7e9c6bec0e6ec82f933a9743e8719bfc4b2aea8/9.4.8/lib/x86_64-linux-ghc-9.4.8/hw03-0.1.0.0-6S0k02W8c4QBjaTffC9fov-hw01"
dynlibdir  = "/home/lemonidas/433-www/website/code/hw03/.stack-work/install/x86_64-linux-tinfo6/5ef49d94af61c8956516f024b7e9c6bec0e6ec82f933a9743e8719bfc4b2aea8/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/lemonidas/433-www/website/code/hw03/.stack-work/install/x86_64-linux-tinfo6/5ef49d94af61c8956516f024b7e9c6bec0e6ec82f933a9743e8719bfc4b2aea8/9.4.8/share/x86_64-linux-ghc-9.4.8/hw03-0.1.0.0"
libexecdir = "/home/lemonidas/433-www/website/code/hw03/.stack-work/install/x86_64-linux-tinfo6/5ef49d94af61c8956516f024b7e9c6bec0e6ec82f933a9743e8719bfc4b2aea8/9.4.8/libexec/x86_64-linux-ghc-9.4.8/hw03-0.1.0.0"
sysconfdir = "/home/lemonidas/433-www/website/code/hw03/.stack-work/install/x86_64-linux-tinfo6/5ef49d94af61c8956516f024b7e9c6bec0e6ec82f933a9743e8719bfc4b2aea8/9.4.8/etc"

getBinDir     = catchIO (getEnv "hw03_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hw03_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hw03_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hw03_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw03_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw03_sysconfdir") (\_ -> return sysconfdir)




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
