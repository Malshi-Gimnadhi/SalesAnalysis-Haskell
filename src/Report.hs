{-# LANGUAGE OverloadedStrings #-}
module Report
  ( writeReport
  ) where

import Types
import Analysis
import System.Directory (createDirectoryIfMissing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.FilePath ((</>), takeBaseName)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List (sortOn)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Convert Map to sorted list descending by value
sortedDesc :: Map String Double -> [(String, Double)]
sortedDesc m = reverse $ sortOn snd (Map.toList m)

-- Format revenue with 2 decimals
fmt :: Double -> String
fmt d = printf "%.2f" d

-- Ensure generated dir and write text report
writeReport :: FilePath -> (Double, Int, Int) -> Map String Double -> Map String Double -> IO FilePath
writeReport sourcePath (totalRev, totalQty, recCount) prodMap monthMap = do
  let dir = "generated"
  createDirectoryIfMissing True dir
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
      base = takeBaseName sourcePath
      outPath = dir </> (base ++ "_report_" ++ ts ++ ".txt")
      header = [ "Sales Analysis Report"
               , "Source: " ++ sourcePath
               , "Generated: " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
               , ""
               ]
      overview = [ "Overview:"
                 , "  Total Revenue: " ++ fmt totalRev
                 , "  Total Quantity: " ++ show totalQty
                 , "  Record Count: " ++ show recCount
                 , ""
                 ]
      prodLines = ("Top Products (by revenue):" : concatMap (\(p,v) -> ["  " ++ p ++ " : " ++ fmt v]) (take 20 $ sortedDesc prodMap)) ++ [""]
      monthLines = ("Sales by Month:" : concatMap (\(m,v) -> ["  " ++ m ++ " : " ++ fmt v]) (sortedDesc monthMap)) ++ [""]
      content = T.unlines $ map T.pack (header ++ overview ++ prodLines ++ monthLines ++ ["End of report"])
  TIO.writeFile outPath content
  return outPath
