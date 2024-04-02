{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tyinf (
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
version = Version [1,3,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/vimukthiherath/.cabal/bin"
libdir     = "/Users/vimukthiherath/.cabal/lib/aarch64-osx-ghc-9.2.8/tyinf-1.3.0.0-inplace-tyinf"
dynlibdir  = "/Users/vimukthiherath/.cabal/lib/aarch64-osx-ghc-9.2.8"
datadir    = "/Users/vimukthiherath/.cabal/share/aarch64-osx-ghc-9.2.8/tyinf-1.3.0.0"
libexecdir = "/Users/vimukthiherath/.cabal/libexec/aarch64-osx-ghc-9.2.8/tyinf-1.3.0.0"
sysconfdir = "/Users/vimukthiherath/.cabal/etc"

getBinDir     = catchIO (getEnv "tyinf_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tyinf_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tyinf_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tyinf_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tyinf_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tyinf_sysconfdir") (\_ -> return sysconfdir)




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
