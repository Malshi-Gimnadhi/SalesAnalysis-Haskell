{-# LANGUAGE OverloadedStrings #-}

module Report
  ( generateReport
  ) where

import Types
import Analysis
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO

-- Generate a report CSV file
generateReport :: FilePath -> [SaleRecord] -> IO FilePath
generateReport fp sales = do
    let prod = byProduct sales
        month = byMonth sales
        (rev, qty, count) = analyze sales
        outFile = fp ++ "_report.txt"
    
    withFile outFile WriteMode $ \h -> do
        hPutStrLn h $ "Total Revenue: " ++ show rev
        hPutStrLn h $ "Total Quantity: " ++ show qty
        hPutStrLn h $ "Number of Sales Records: " ++ show count
        hPutStrLn h "\nRevenue by Product:"
        mapM_ (\(k,v) -> hPutStrLn h $ k ++ ": " ++ show v) (Map.toList prod)
        hPutStrLn h "\nRevenue by Month:"
        mapM_ (\(k,v) -> hPutStrLn h $ k ++ ": " ++ show v) (Map.toList month)
    
    return outFile
