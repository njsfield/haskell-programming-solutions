{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_chex (
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

bindir     = "/Users/heyimnick/Documents/Web Development/haskell/haskell_programming_solutions/src/ch_14/chex/.stack-work/install/x86_64-osx/lts-9.2/8.0.2/bin"
libdir     = "/Users/heyimnick/Documents/Web Development/haskell/haskell_programming_solutions/src/ch_14/chex/.stack-work/install/x86_64-osx/lts-9.2/8.0.2/lib/x86_64-osx-ghc-8.0.2/chex-0.1.0.0-KgUEtLxuEyGCiEynp3TqoJ"
dynlibdir  = "/Users/heyimnick/Documents/Web Development/haskell/haskell_programming_solutions/src/ch_14/chex/.stack-work/install/x86_64-osx/lts-9.2/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/heyimnick/Documents/Web Development/haskell/haskell_programming_solutions/src/ch_14/chex/.stack-work/install/x86_64-osx/lts-9.2/8.0.2/share/x86_64-osx-ghc-8.0.2/chex-0.1.0.0"
libexecdir = "/Users/heyimnick/Documents/Web Development/haskell/haskell_programming_solutions/src/ch_14/chex/.stack-work/install/x86_64-osx/lts-9.2/8.0.2/libexec"
sysconfdir = "/Users/heyimnick/Documents/Web Development/haskell/haskell_programming_solutions/src/ch_14/chex/.stack-work/install/x86_64-osx/lts-9.2/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chex_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chex_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chex_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chex_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chex_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chex_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
