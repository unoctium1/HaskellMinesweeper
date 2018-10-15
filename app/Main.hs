module Main where

import Minesweeper
import Graphics.UI.Gtk  

main :: IO ()
main = do
  initGUI          -- (1)
  window <- windowNew   -- (2)
                        -- (3)
  widgetShowAll window  -- (4)
  mainGUI  
