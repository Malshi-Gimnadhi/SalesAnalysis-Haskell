{-# LANGUAGE OverloadedStrings #-}

module Report
  ( generateReport
  , writeReport
  ) where

import Types
import Analysis
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName)
import Control.Exception (try, SomeException)

-- Generate a report CSV file (legacy function, not currently used)
generateReport :: FilePath -> [SaleRecord] -> IO FilePath
generateReport fp _ = do
    let outFile = fp ++ "_report.txt"
    -- This function is currently unused; stub implementation
    withFile outFile WriteMode $ \h -> do
        hPutStrLn h "Report placeholder"
    return outFile

-- Write a sales analysis report to generated/ folder
-- Returns Either error message or the path to the written report
writeReport :: FilePath 
            -> (Double, Int, Int) 
            -> Map String Double 
            -> Map String Double 
            -> IO (Either String FilePath)
writeReport fp (rev, qty, count) prodMap monthMap = do
    result <- try $ do
        createDirectoryIfMissing True "generated"
        let outFile = "generated" </> takeBaseName fp ++ "_report.txt"
        withFile outFile WriteMode $ \h -> do
            hPutStrLn h $ "Total Revenue: " ++ show rev
            hPutStrLn h $ "Total Quantity: " ++ show qty
            hPutStrLn h $ "Number of Sales Records: " ++ show count
            hPutStrLn h "\nRevenue by Product:"
            mapM_ (\(k,v) -> hPutStrLn h $ k ++ ": " ++ show v) (Map.toList prodMap)
            hPutStrLn h "\nRevenue by Month:"
            mapM_ (\(k,v) -> hPutStrLn h $ k ++ ": " ++ show v) (Map.toList monthMap)
        return outFile
    case result of
        Left e -> return $ Left (show (e :: SomeException))
        Right path -> return $ Right path
