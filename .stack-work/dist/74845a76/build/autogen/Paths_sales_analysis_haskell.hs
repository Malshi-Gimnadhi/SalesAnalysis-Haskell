{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_sales_analysis_haskell (
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
bindir     = "C:\\Users\\ASUS\\Desktop\\SalesAnalysis-Haskell\\.stack-work\\install\\50bb050f\\bin"
libdir     = "C:\\Users\\ASUS\\Desktop\\SalesAnalysis-Haskell\\.stack-work\\install\\50bb050f\\lib\\x86_64-windows-ghc-9.2.7\\sales-analysis-haskell-0.1.0.0-AyTscpRhOCIJRY3wx6GWVN"
dynlibdir  = "C:\\Users\\ASUS\\Desktop\\SalesAnalysis-Haskell\\.stack-work\\install\\50bb050f\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\Users\\ASUS\\Desktop\\SalesAnalysis-Haskell\\.stack-work\\install\\50bb050f\\share\\x86_64-windows-ghc-9.2.7\\sales-analysis-haskell-0.1.0.0"
libexecdir = "C:\\Users\\ASUS\\Desktop\\SalesAnalysis-Haskell\\.stack-work\\install\\50bb050f\\libexec\\x86_64-windows-ghc-9.2.7\\sales-analysis-haskell-0.1.0.0"
sysconfdir = "C:\\Users\\ASUS\\Desktop\\SalesAnalysis-Haskell\\.stack-work\\install\\50bb050f\\etc"

getBinDir     = catchIO (getEnv "sales_analysis_haskell_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "sales_analysis_haskell_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "sales_analysis_haskell_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "sales_analysis_haskell_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sales_analysis_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sales_analysis_haskell_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
