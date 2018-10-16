module Minesweeper
    ( someFunc,
      getgrid,
      getgridext,
      minesweeper
    ) where

import System.Random
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State = State InternalState        -- the state of the game is the
         deriving (Ord, Eq)--, Show)             -- internal state of the game

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = UserAction -> State -> Result

type Player = State -> UserAction

-----------------Minesweeper Game --------------------------------------
data Click = LeftClick  --click
            |RightClick --flag?
        deriving (Eq)

-- am action is a triple of an x value, a y value, and the value to be placed
-- at said x,y location
newtype UserAction = UserAction (Int, Int, Click)
    deriving (Eq) --Do we need this?

-- an internal state is the minesweeper grid as described below
type InternalState = [[Int]]

-- this ensures users do not have access to the internal state --i'm still on the fence about overriding show, might not be necessary
instance Show State where
    show (State ins) = show (showhelper ins)

showhelper :: [[Int]] -> [[Char]] -- converts internal state to concealed external state
showhelper [] = []
showhelper (first:rest) = (map
    (\ a -> if a == 0 || a == 1 then  'B'  -- not clicked
        else if a == 2 || a == 4 then 'F' -- flagged
        else '2')                         -- clicked (replace with count bombs once implemented)
    first):showhelper rest

-- the game is over when all the 1's are replaced with 4's and there are
-- no more 2's
empty = 0               -- empty space, uncleared
mine = 1                -- mine, undiscovered
emptyFlagged = 2        -- empty space, flagged incorrectly
emptyCleared = 3        -- empty space, cleared over the course of the game
bombFlagged = 4         -- mine, flagged
bombCleared = 5         -- mine, cleared (loss condition)

-- a small grid for testing purposes, feel free to design your own

small_grid = [[0,0,0], [0,1,0],[0,0,0]]

-- generates an state random state grid
getgridext :: Int -> IO State
getgridext 0 =
    do return (State [])
getgridext x =
    do
        rg <- newStdGen
        return (State (gridmap x (take (x^2) (randomRs (0,1) rg))))

-- generates an internal state random state grid (I'm not sure which of these we'll want to you, but definitely not both)
getgrid :: Int -> IO InternalState
getgrid 0 =
    do return []
getgrid x =
    do
        rg <- newStdGen
        return (gridmap x (take (x^2) (randomRs (0,1) rg)))
        

-- makeGrid takes a grid size and a number of mines and returns an internal state with randomly distributed mines    
makeGrid :: Int -> Int -> IO InternalState
makeGrid gridSize numMines = populateGrid (makeGridHelper gridSize gridSize) gridSize numMines

makeGridHelper :: Int -> Int -> InternalState
makeGridHelper 0 x = []
makeGridHelper y x = (makeRow x) : (makeGridHelper (y-1) x)

makeRow :: Int -> [Int]
makeRow 0 = []
makeRow x = 0 : makeRow (x-1)


populateGrid :: InternalState -> Int -> Int -> IO InternalState
populateGrid grid gridSize 0 = do
	return grid
populateGrid grid gridSize numMines = 
	do 
		rg <- newStdGen
		let randomX = head(randomRs (1,gridSize) rg)
		rg2 <- newStdGen
		let randomY = head(randomRs (1,gridSize) rg2)
		if (hasBomb grid randomX randomY)
			then populateGrid grid gridSize numMines
			else populateGrid (find_replace grid randomX randomY 1) gridSize (numMines - 1)
			
hasBomb :: InternalState -> Int -> Int -> Bool
hasBomb grid x 0 = False
hasBomb (first:rest) x y
	|y == 1 = hasBombHelper first x
	|otherwise = hasBomb rest x (y-1)

hasBombHelper :: [Int] -> Int -> Bool
hasBombHelper grid 0 = False
hasBombHelper (first:rest) x
	|x == 1 = if (first == 1) then True else False
	|otherwise = hasBombHelper rest (x-1)
		
--test case
test_grid = makeGrid 5 10




-- Helper for getgrid - given a specified grid size, and a list of numbers
gridmap :: Int -> [Int] -> InternalState
gridmap x lst = gridmaphelper x x lst

--splits lst into y chunks of x size
gridmaphelper :: Int -> Int -> [Int] -> InternalState
gridmaphelper x 0 _ = []
gridmaphelper x y lst =
    let
        (fr,rst) = splitAt x lst
    in
        fr:(gridmaphelper x (y-1) rst)

-- note: find_replace uses 1 indexing and (1,1) is the top left corner of the grid
-- if 1 indexing proves really inconvenient we can reformat

-- find_replace takes a grid, an x,y coordinate, and the number to replace
-- the number located at said coordinate. It returns the updated grid

find_replace :: InternalState -> Int -> Int -> Int -> InternalState
find_replace [] x y c = []
find_replace (first:rest) x y c
    |y == 1 = (find_replace_helper first x c) : rest
    |otherwise = first : find_replace rest x (y-1) c

find_replace_helper :: [Int] -> Int -> Int -> [Int]
find_replace_helper [] x c = []
find_replace_helper (first:rest) x c
    |x == 1 = c : rest
    |otherwise = first : (find_replace_helper rest (x-1) c)
    
find :: InternalState -> Int -> Int -> Int
find lst x y = (lst!!(y-1))!!(x-1)


minesweeper_start = State small_grid        -- initializes the game with the test grid

minesweeper :: Game
minesweeper (UserAction (x,y,c)) (State (grid))
    | to_replace == 5                           = EndOfGame 0 minesweeper_start
    | win new_grid                              = EndOfGame 1 minesweeper_start    -- did we win?
    | otherwise                                 = ContinueGame (State new_grid)
        where
            init = find grid x y
            to_replace = if (init == 0 && c == LeftClick) then 3
                         else if (init == 0) then 2
                         else if (init == 1 && c == LeftClick) then 5
                         else if (init == 1) then 4
                         else if (init == 4 && c == LeftClick) then 1
                         else if (init == 2 && c == LeftClick) then 0
                         else init
            new_grid = find_replace grid x y to_replace

--TODO: Flesh out the win and loss conditions for the game, for example
-- we should loose if we click on a mine or run out of moves/time

--TODO: Create method for generating action triples from user input,
-- either keystrokes or mouse clicks --still needs work, but kind of done

--TODO: Create a graphical representation of the mine grid

-- we win if all the 1's are gone, and there are no 2s

win :: [[Int]] -> Bool
win [] = True
win (first:rest)
    |elem 1 first || elem 2 first = False
    |otherwise = True && win rest

--loss - if any space is a clicked mine, the game is a loss -- not actually needed
loss :: [[Int]] -> Bool
loss [] = False;
loss (first:rest)
    |elem 5 first = True
    |otherwise = loss rest

countbombs :: [[Int]] -> (Int, Int) -> Int
-- in order to display the grid, we need some function to count the bombs nearby a given space
countbombs grid (x,y) = (countThree grid (x,(y-1))) + (countAtX grid ((x-1),y)) + (countThree grid (x,(y+1))) + (countAtX grid ((x+1),y))

-- countAtX returns 1 if there is a bomb at (x,y) otherwise returns 0
-- countAtX itself only operates on the grid, calling its helper on the correct row of the grid
countAtX :: [[Int]] -> (Int, Int) -> Int
countAtX [] (x, y)  = 0
countAtX (first:rest) (x, y)
	| (x <= 0) || (y <= 0) = 0
    |y == 1 = (countAtXHelper first x)
    |otherwise = 0 + (countAtX rest (x, (y-1)))

-- helper function operates on a particular row of the grid, looking to see if there is a bomb
-- at that x value
countAtXHelper :: [Int] -> Int -> Int
countAtXHelper [] x = 0
countAtXHelper (first:rest) x
    |(x == 1) && (first == 1) = 1
    |otherwise = 0 + (countAtXHelper rest (x-1))

-- to count the bombs above and below a particular x,y coordinate we need to examine 3 spaces in a row, 
-- countThree does this for us by calling CountAtX three times
countThree :: [[Int]] -> (Int, Int) -> Int    
countThree grid (x,y) = (countAtX grid ((x-1), y)) + (countAtX grid ((x), y)) + (countAtX grid ((x+1), y))
