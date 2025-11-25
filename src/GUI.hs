{-# LANGUAGE OverloadedStrings #-}
module GUI
  ( startGUI
  ) where

import qualified Graphics.UI.Threepenny as T
import Graphics.UI.Threepenny.Core hiding (startGUI)
import Parser
import Analysis
import Report
import Types
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import System.Process (callCommand)
import System.FilePath (takeFileName)
import System.Info (os)

-- Start GUI: the handler type is Window -> UI ()
startGUI :: T.Window -> UI ()
startGUI window = do
  return window # set T.title "Sales Analysis (Haskell)"
  -- Build UI elements
  inputPath <- T.input # set (attr "placeholder") "Enter path to CSV (e.g. data/sample.csv)"
  btnAnalyze <- T.button #+ [string "Load & Analyze"]
  btnOpenFolder <- T.button #+ [string "Open generated folder"]
  resultArea <- T.pre # set text "No analysis yet."
  linkReport <- T.span # set text ""

  getBody window #+ [ column
    [ T.h1 # set text "Sales Analysis (Haskell)"
    , element inputPath
    , element btnAnalyze
    , element btnOpenFolder
    , T.hr
    , element resultArea
    , T.hr
    , element linkReport
    ]
    ]

  -- Button actions
  on T.click btnAnalyze $ \_ -> do
    fp <- get value inputPath
    liftIO $ putStrLn $ "Analyzing: " ++ fp
    res <- liftIO $ parseSalesFile fp
    case res of
      Left err -> element resultArea # set text ("Error parsing CSV: " ++ err)
      Right (sales, failedCount) -> do
        let (rev, qty, rc) = analyzeSales sales
            prodMap = salesByProduct sales
            monthMap = salesByMonth sales
            rowsInfo = "Rows parsed: " ++ show rc ++ ", failed: " ++ show failedCount
            summaryStr = unlines
              [ rowsInfo
              , ""
              , "Overview:"
              , " Total Revenue: " ++ show rev
              , " Total Quantity: " ++ show qty
              , " Records: " ++ show rc
              ]
            topProds = take 10 $ reverse $ Map.toList prodMap
            prodLines = map (\(p,v) -> "  " ++ p ++ " : " ++ show v) topProds
            monthLines = map (\(m,v) -> "  " ++ m ++ " : " ++ show v) (reverse $ Map.toList monthMap)
            displayText = summaryStr ++ "\nTop Products:\n" ++ unlines prodLines ++ "\nSales by Month:\n" ++ unlines monthLines
        -- write report
        reportResult <- liftIO $ writeReport fp (rev, qty, rc) prodMap monthMap
        case reportResult of
          Left writeErr -> do
            element resultArea # set text (displayText ++ "\n\nError writing report: " ++ writeErr)
            element linkReport # set text ""
          Right reportPath -> do
            element resultArea # set text displayText
            element linkReport # set text ("Report written: " ++ reportPath)

  -- Open generated folder button
  on T.click btnOpenFolder $ \_ -> liftIO $ do
    -- simple OS detection
    let cmd =
          if os == "mingw32" then "start generated"
          else if os == "darwin" then "open generated"
          else "xdg-open generated || true"
    putStrLn $ "Opening generated folder with command: " ++ cmd
    -- try run; ignore failures
    _ <- callCommand cmd
    return ()