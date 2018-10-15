module Minesweeper
    ( someFunc
    ) where

import Graphics.UI.Gtk	
	
someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State = State InternalState        -- the state of the game is the 
         deriving (Ord, Eq)             -- internal state of the game

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq)

type Game = UserAction -> State -> Result

type Player = State -> UserAction

-----------------Minesweeper Game --------------------------------------
-- am action is a triple of an x value, a y value, and the value to be placed
-- at said x,y location
newtype UserAction = UserAction (Int, Int, Int)
    deriving (Eq, Ord) --Do we need this?
    
-- an internal state is the minesweeper grid as described below
type InternalState = [[Int]]

-- this ensures users do not have access to the internal state --i'm still on the fence about overriding show, might not be necessary
instance Show State where
    show (State ins) = show (showhelper ins)

showhelper :: [[Int]] -> [[Int]] -- converts internal state to concealed external state
showhelper [] = []
showhelper (first:rest) = (map 
    (\ a -> if a == 0 || a == 3 then 0  -- not clicked
        else if a == 1 || a == 4 then 1 -- flagged
        else 2)                         -- clicked
    first):showhelper rest
    
-- Game board Enumeration
-- 0 - empty
-- 1 - empty, flagged
-- 2 - empty, cleared
-- 3 - bomb
-- 4 - bomb, flagged
-- 5 - bomb, cleared. -- use for identifying loss conditions

-- the game is over when all the 3's are replaced with 4's and there are
-- no more 1's

empty = 0               -- empty space, uncleared
emptyFlagged = 1        -- empty space, flagged incorrectly
emptyCleared = 2        -- empty space, cleared over the course of the game
mine = 3                -- mine, undiscovered
bombFLagged = 4         -- mine, flagged

-- a small grid for testing purposes, feel free to design your own

small_grid = [[0,0,0], [0,3,0],[0,0,0]]

-- note: find_replace uses 1 indexing and (1,1) is the top left corner of the grid
-- if 1 indexing proves really inconvenient we can reformat

-- find_replace takes a grid, an x,y coordinate, and the number to replace
-- the number located at said coordinate. It returns the updated grid

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
    
minesweeper_start = State small_grid        -- initializes the game with the test grid

minesweeper :: Game
minesweeper (UserAction (x,y,c)) (State (grid))
    | loss (find_replace grid x y c) = EndOfGame 0 minesweeper_start
    | win (find_replace grid x y c)  = EndOfGame 1 minesweeper_start                     -- did we win?
    | otherwise                      = ContinueGame (State(find_replace grid x y c))     -- if not the game goes on

--TODO: Flesh out the win and loss conditions for the game, for example
-- we should loose if we click on a mine or run out of moves/time

--TODO: Create method for generating action triples from user input,
-- either keystrokes or mouse clicks

--TODO: Create a graphical representation of the mine grid

--TODO: random grid setup

-- we win if all the 3's are gone, and there are no ones
-- note that this win condition assumes we cannot incorrectly remove 3's

win :: [[Int]] -> Bool
win [] = True
win (first:rest) 
    |elem 3 first && elem 1 first = False
    |otherwise = True && win rest

--loss - if any space is a clicked mine, the game is a loss
loss :: [[Int]] -> Bool
loss [] = False;
loss (first:rest)
    |elem 5 first = True
    |otherwise = loss rest
    
-- countbombs :: [[int]] -> (int, int) -> int
-- in order to display the grid, we need some function to count the bombs nearby a given space
