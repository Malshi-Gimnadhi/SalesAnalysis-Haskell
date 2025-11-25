module Main where

import qualified Graphics.UI.Threepenny as T
import GUI (startGUI)

main :: IO ()
main = do
  let config = T.defaultConfig { T.jsPort = Just 8023 }
  putStrLn "Starting Sales Analysis GUI on http://localhost:8023"
  -- call the library startGUI (qualified) with the GUI.startGUI handler
  T.startGUI config GUI.startGUI