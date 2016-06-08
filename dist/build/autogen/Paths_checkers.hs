module Paths_checkers (
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

bindir     = "C:\\Users\\Jakub\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Jakub\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\checkers-0.1.0.0-Do8ccBkX9sDCHSBNFRBMFN"
datadir    = "C:\\Users\\Jakub\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\checkers-0.1.0.0"
libexecdir = "C:\\Users\\Jakub\\AppData\\Roaming\\cabal\\checkers-0.1.0.0-Do8ccBkX9sDCHSBNFRBMFN"
sysconfdir = "C:\\Users\\Jakub\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "checkers_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "checkers_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "checkers_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "checkers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "checkers_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
