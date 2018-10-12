module Minesweeper where

-- To run it, try:
-- ghci
-- :load Minesweeper

data State = State InternalState
         deriving (Ord, Eq)

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq)

type Game = Action -> State -> Result

type Player = State -> Action

-----------------Minesweeper Game --------------------------------------

newtype Action = Action (Int, Int, Int)
	deriving (Eq, Ord)
type InternalState = [[Int]]
	
-- Game board Enumeration
-- 0 - empty
-- 1 - empty, flagged
-- 2 - empty, cleared
-- 3 - bomb
-- 4 - bomb, flagged

-- the game is over when all the B's are replaced with F's and there are
-- no more 1's

empty = 0
emptyFlagged = 1
emptyCleared = 2
bomb = 3
bombFLagged = 4

small_grid = [[0,0,0], [0,3,0],[0,0,0]]

-- note: find_replace uses 1 indexing and (1,1) is the top left corner of the grid

find_replace :: [[t]] -> Int -> Int -> t -> [[t]]
find_replace [] x y c = []
find_replace (first:rest) x y c
	|y == 1 = (find_replace_helper first x c) : rest
	|otherwise = first : find_replace rest x (y-1) c
	
find_replace_helper :: [t] -> Int -> t -> [t]
find_replace_helper [] x c = []
find_replace_helper (first:rest) x c
	|x == 1 = c : rest
	|otherwise = first : (find_replace_helper rest (x-1) c)
	
minesweeper_start = State small_grid

minesweeper :: Game
minesweeper (Action (x,y,c)) (State (grid))
	| win (find_replace grid x y c)	= EndOfGame 1 minesweeper_start
	|otherwise 			  			= ContinueGame (State(find_replace grid x y c))

win :: [[Int]] -> Bool
win [] = True
win (first:rest) 
	|elem 3 first = False
	|otherwise = True && win rest
