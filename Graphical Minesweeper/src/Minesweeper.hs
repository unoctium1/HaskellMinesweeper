
module Minesweeper where

import System.Random
import Data.Char
import Data.Maybe

data State = State InternalState        -- the state of the game is the
         deriving (Ord, Eq)--, Show)             -- internal state of the game

data Result = EndOfGame Double    -- end of game, value, starting state
            | ContinueGame InternalState        -- continue with new state
         deriving (Eq, Show)

type Game = UserAction -> State -> Result

type Player = State -> UserAction

-----------------Minesweeper Game --------------------------------------
data Click = LeftClick  --click
            |RightClick --flag?
        deriving (Eq, Read)

-- am action is a triple of an x value, a y value, and the value to be placed
-- at said x,y location
newtype UserAction = UserAction (Int, Int, Click)
    deriving (Eq, Read) --Do we need this?

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


minesweeper_start = play (makeGrid 5 10)       -- initializes the game with the test grid

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
            
play :: IO InternalState -> IO InternalState
play internalGrid = 
	do
	putStrLn "  +───────────────────────+"
	putStrLn "  | M I N E S W E E P E R |"
	putStrLn "  +───────────────────────+"
	internalGrid


--TODO: Flesh out the win and loss conditions for the game, for example
-- we should loose if we click on a mine or run out of moves/time

--TODO: Create method for generating action triples from user input,
-- either keystrokes or mouse clicks --still needs work, but kind of done

--TODO: Create a graphical representation of the mine grid

-- we win if all the 1's are gone, and there are no 2s

printBoard :: [[Int]] -> Bool -> IO ()
printBoard board gg = do
    putStrLn "    1  2  3  4  5  6  7  8"
    putStrLn "  +────────────────────────+"
    sequence (mapWithBoardAndRow printRow board board gg)
    putStrLn "  +────────────────────────+"
    putStrLn "    1  2  3  4  5  6  7  8"

printRow :: [Int] -> (Int, [[Int]], Bool) -> IO ()
printRow row (index, board, gg)
  | index == 0 = do
    putStr ([chr (ord 'a' + index)] ++ " |")
    sequence (mapWithBoardAndCell printCell row index board gg)
    putStrLn "|"
  | index == boardSize - 1 = do
    putStr ([chr (ord 'a' + index)] ++ " |")
    sequence (mapWithBoardAndCell printCell row index board gg)
    putStrLn "|"
  | otherwise = do
    putStr ([chr (ord 'a' + index)] ++ " │")
    sequence (mapWithBoardAndCell printCell row index board gg)
    putStrLn "│"

printCell :: Int -> (Int, Int, [[Int]], Bool) -> IO ()
printCell cell (col, row, board, gg)
  | gg && (cell == mine || cell == mineFlagged) = putStr " ✘ "
  | cell == emptyFlagged || cell == mineFlagged = putStr " ⚑ "
  | cell == empty || cell == mine = putStr " ■ "
  | cell == emptyCleared && (numAdjacentBombs row col board) == 0 = putStr "   "
  | otherwise = putStr (" " ++ show (numAdjacentBombs row col board) ++ " ")


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

countbombs :: [[Int]] -> (Int, Int) -> Int
countbombs [] _ = 0
countbombs (first:rest) (x, y) =
    let
        neighbors = [(x-1, y),(x+1, y),(x-1, y-1),(x, y-1),(x+1,y-1),(x-1, y+1),(x, y+1),(x+1,y+1)]
        neighborsfilter = filter (\ (a,b) -> (a > 0) && (a <= (length first)) && b > 0 && (b <= (1+(length rest)))) neighbors
        mappedneighbors = map (\ (a,b) -> find (first:rest) a b) neighborsfilter
            
    in
        (length (filter (\ a -> a >= bombFlagged || a == mine) mappedneighbors))