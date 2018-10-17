module Minesweeper
    ( getgrid,
      getgridext,
      minesweeper
    ) where

import System.Random
import Data.Char

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
    show (State ins) = show (showhelper (showhelper2 ins))
    
-- the game is over when all the 1's are replaced with 4's and there are
-- no more 2's
empty = 0               -- empty space, uncleared
mine = 1                -- mine, undiscovered
emptyFlagged = 2        -- empty space, flagged incorrectly
emptyCleared = 3        -- empty space, cleared over the course of the game
bombFlagged = 4         -- mine, flagged
bombCleared = 5         -- mine, cleared (loss condition)

showhelper :: [[Int]] -> [[Char]] -- converts internal state to concealed external state
showhelper [] = []
showhelper (first:rest) = (map intToDigit first):showhelper rest
    
showhelper2 :: [[Int]] -> [[Int]]
showhelper2 [] = []
showhelper2 (first:rest) =
    let
        width = (length first)
        height = ((length rest)+1)
    in
        showhelper3 (first:rest) 1 1 width height

showhelper3 lst a b x y
    | a == x && b == y  = new_grid
    | a == x            = showhelper3 new_grid 1 (b+1) x y
    | otherwise         = showhelper3 new_grid (x+1) b x y
        where
            init = find lst a b
            to_replace = if (init == empty || init == mine) then 11
                         else if (init == emptyFlagged || init == bombFlagged) then 15
                         else countbombs lst (a, b)
            new_grid = find_replace lst x y to_replace


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

-- generates an internal state random state grid (I'm not sure which of these we'll want to use, but definitely not both)
getgrid :: Int -> IO InternalState
getgrid 0 =
    do return []
getgrid x =
    do
        rg <- newStdGen
        return (gridmap x (take (x^2) (randomRs (0,1) rg)))

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
    | to_replace == bombCleared                 = EndOfGame 0 minesweeper_start
    | win new_grid                              = EndOfGame 1 minesweeper_start    -- did we win?
    | otherwise                                 = ContinueGame (State new_grid)
        where
            init = find grid x y
            to_replace = if (init == empty && c == LeftClick) then emptyCleared
                         else if (init == empty) then emptyFlagged
                         else if (init == mine && c == LeftClick) then bombCleared
                         else if (init == mine) then bombFlagged
                         else if (init == bombFlagged && c == LeftClick) then mine
                         else if (init == emptyFlagged && c == LeftClick) then empty
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
    |elem mine first || elem bombFlagged first = False
    |otherwise = True && win rest

--loss - if any space is a clicked mine, the game is a loss -- not actually needed
loss :: [[Int]] -> Bool
loss [] = False;
loss (first:rest)
    |elem bombCleared first = True
    |otherwise = loss rest

-- I think this might be more efficient but I'm not 100% sure
countbombs :: [[Int]] -> (Int, Int) -> Int
countbombs [] _ = 0
countbombs (first:rest) (x, y) =
    let
        neighbors = [(x-1, y),(x+1, y),(x-1, y-1),(x, y-1),(x+1,y-1),(x-1, y+1),(x, y+1),(x+1,y+1)]
        neighborsfilter = filter (\ (a,b) -> (a > 0) && (a <= (length first)) && b > 0 && (b <= (1+(length rest)))) neighbors
        mappedneighbors = map (\ (a,b) -> find (first:rest) a b) neighborsfilter
            
    in
        (length (filter (\ a -> a >= bombFlagged || a == mine) mappedneighbors))

{-- This doesn't seem to compile
countbombs :: [[Int]] -> (Int, Int) -> Int
-- in order to display the grid, we need some function to count the bombs nearby a given space
countbombs grid (x,y) = (countThree grid (x,(y-1))) + (countAtX grid ((x-1),y)) + (countThree grid (x,(y+1))) + (countAtX grid ((+1),y))

-- countAtX returns 1 if there is a bomb at (x,y) otherwise returns 0
-- countAtX itself only operates on the grid, calling its helper on the correct row of the grid
countAtX :: [[Int]] -> (Int, Int) -> Int
countAtX [] (x, y)  = 0
countAtX (first:rest) (x, y)
    |((x <= 0) || (y <= 0)) = 0
    |y == 1 = (countAtXHelper first x)
    |otherwise = 0 + (find_replace rest (x, (y-1)))

-- helper function operates on a particular row of the grid, looking to see if there is a bomb
-- at that x value
countAtXHelper :: [Int] -> Int -> Int
countAtXHelper [] x = 0
countAtXHelper (first:rest) x
    |(x == 1) && (first == 1) = 1
    |otherwise = 0 + (countLeftHelper rest (x-1))

-- to count the bombs above and below a particular x,y coordinate we need to examine 3 spaces in a row, 
-- countThree does this for us by calling CountAtX three times
countThree :: [[Int]] -> (Int, Int) -> Int    
countThree grid (x,y) = (countAtX grid ((x-1), y)) + (countAtX grid ((x), y)) + (countAtX grid ((x+1), y))
--}