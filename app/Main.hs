module Main where

import Minesweeper
import Graphics.UI.Gtk  

main :: IO ()
main = 
	do
	
	  initGUI          -- (1)
	  window <- windowNew   -- (2)
							-- (3)
	  widgetShowAll window  -- (4)
	  mainGUI  

type TournammentState = (Int,Int)   -- wins, losses
	  
play :: Game -> Result -> TournammentState -> IO TournammentState
play game start tournament_state =
  let (wins, losses,ties) = tournament_state in
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
person_play game (EndOfGame 1 start_state) (wins,losses,ties) =
   do
      putStrLn "You win!"
      play game (ContinueGame start_state) (wins+1,losses)
person_play game (EndOfGame 0 start_state) (wins,losses,ties) =
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