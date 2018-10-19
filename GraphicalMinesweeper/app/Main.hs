module Main where

import Minesweeper
import Graphics.UI.Gtk  
import Data.Maybe
import Text.Read
import Control.Monad.IO.Class
import Data.IORef

data GUI = GUI {
      mainWin :: Window,
      minesLbl :: Label,
      movesLbl :: Label,
      mineTbl :: Fixed}

main :: IO ()
main = 
    do
      initGUI
      gui <- loadGlade "src/Minesweeper1.glade"
      connectGui gui
      widgetShowAll (mainWin gui)
      mainGUI  
      
loadGlade gladepath =
    do
       builder <- builderNew                     
       builderAddFromFile builder gladepath 

       -- Load main window
       mw <- builderGetObject builder castToWindow "Minesweeper"
       minel <- builderGetObject builder castToLabel "MinesRemaining"
       movel <- builderGetObject builder castToLabel "MovesRemaining"
       table <- builderGetObject builder castToFixed "MineTable"
       -- Load all buttons

       return $ GUI mw minel movel table

connectGui gui =
    do -- When the close button is clicked, terminate GUI loop
       -- by calling GTK mainQuit function
       onDestroy (mainWin gui) mainQuit
       
       -- Main window buttons

              
type TournammentState = (Int,Int)   -- wins, losses

mkMine :: String -> IO Button
mkMine label = 
    do
      btn <- buttonNew
      set btn [ buttonLabel := label ]
      return btn
 {--    
play :: Game -> Result -> TournammentState -> IO TournammentState
play game start tournament_state =
  let (wins, losses) = tournament_state in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses")
      putStrLn "Start? 0=yes, 1=no"
      line <- getLine
      if line == "0"
        then
            person_play game start tournament_state
        else if line == "1"
            then return tournament_state
        else play game start tournament_state

person_play :: Game -> Result -> TournammentState -> IO TournammentState
person_play game (EndOfGame 1 start_state) (wins,losses) =
   do
      putStrLn "You win!"
      play game (ContinueGame start_state) (wins+1,losses)
person_play game (EndOfGame 0 start_state) (wins,losses) =
   do
      putStrLn "You lose"
      play game (ContinueGame start_state) (wins,losses+1)
person_play game (ContinueGame state) tournament_state =
   do
      let State internal = state
      putStrLn ("State: "++show state++" choose a (x, y) coordinate and either LeftClick or RightClick")
      line <- getLine
      let action = (readMaybe line :: Maybe UserAction)
      if (action == Nothing)
        then  -- error; redo
           person_play game (ContinueGame state) tournament_state
        else
           person_play game (game (fromJust action) state) tournament_state
           --}