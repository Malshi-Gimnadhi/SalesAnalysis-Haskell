module Main where

import qualified Graphics.UI.Threepenny as T
import Graphics.UI.Threepenny.Core (startGUI, defaultConfig, tpPort, tpCustomHTML)
import GUI (startGUI)

main :: IO ()
main = do
  let config = defaultConfig { T.tpPort = 8023 }
  putStrLn "Starting Sales Analysis GUI on http://localhost:8023"
  startGUI config startGUI
