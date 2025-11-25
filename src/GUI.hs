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
import Control.Monad (void)
import System.Process (callCommand)
import System.Info (os)

-- Start GUI
startGUI :: T.Window -> UI ()
startGUI window = do
    -- Set window title
    void $ return window # set T.title "Sales Analysis (Haskell)" 

    -- Apply styles to body to center content
    getBody window # set style 
      [ ("background-color", "#f4f4f9")
      , ("font-family", "Arial, sans-serif")
      , ("display", "flex")
      , ("justify-content", "center")
      , ("align-items", "center")
      , ("height", "100vh")
      ]

    -- Container div for GUI elements
    container <- T.div # set style 
      [ ("background-color", "#ffffff")
      , ("padding", "30px")
      , ("border-radius", "10px")
      , ("box-shadow", "0 4px 10px rgba(0,0,0,0.1)")
      , ("width", "600px")
      , ("text-align", "center")
      ]

    -- UI Elements
    inputPath <- T.input 
        # set (attr "placeholder") "Enter path to CSV (e.g. data/sample.csv)"
        # set style [("padding", "8px"), ("width", "70%"), ("border-radius", "5px"), ("border", "1px solid #ccc")]

    btnAnalyze <- T.button #+ [string "Load & Analyze"]
        # set style [("background-color", "#4CAF50"), ("color", "white"), ("padding", "10px 20px"),
                     ("margin-left", "10px"), ("border", "none"), ("border-radius", "5px"), ("cursor", "pointer")]

    btnOpenFolder <- T.button #+ [string "Open generated folder"]
        # set style [("background-color", "#2196F3"), ("color", "white"), ("padding", "10px 20px"),
                     ("margin-left", "10px"), ("border", "none"), ("border-radius", "5px"), ("cursor", "pointer")]

    resultArea <- T.pre # set text "No analysis yet."
        # set style [("background-color", "#f9f9f9"), ("padding", "10px"), ("border", "1px solid #ddd"),
                     ("border-radius", "5px"), ("max-height", "250px"), ("overflow-y", "auto"), ("margin-top", "10px"), ("text-align", "left")]

    linkReport <- T.span # set text ""
        # set style [("color", "blue"), ("font-weight", "bold"), ("margin-top", "10px"), ("display", "block")]

    -- Add elements to container
    element container #+ [column
        [ T.h1 # set text "📊 Sales Analysis Dashboard"
               # set style [("color", "#333")]
        , row [element inputPath, element btnAnalyze, element btnOpenFolder]
        , element resultArea
        , element linkReport
        ]]

    -- Attach container to body
    getBody window #+ [element container]

    -- Analyze button action
    on T.click btnAnalyze $ \_ -> do
        fp <- get value inputPath
        liftIO $ putStrLn $ "Analyzing: " ++ fp
        res <- liftIO $ parseSalesFile fp
        case res of
          Left err -> element resultArea # set text ("❌ Error parsing CSV: " ++ err)
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
                element resultArea # set text (displayText ++ "\n\n❌ Error writing report: " ++ writeErr)
                element linkReport # set text ""
              Right reportPath -> do
                element resultArea # set text displayText
                element linkReport # set text ("Report written: " ++ reportPath)

    -- Open folder button action
    on T.click btnOpenFolder $ \_ -> liftIO $ do
        let cmd =
              if os == "mingw32" then "start generated"
              else if os == "darwin" then "open generated"
              else "xdg-open generated || true"
        putStrLn $ "Opening generated folder with command: " ++ cmd
        _ <- callCommand cmd
        return ()
