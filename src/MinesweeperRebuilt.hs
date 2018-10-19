import System.Random
import Data.Maybe
import Data.Char

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
        deriving (Eq)

-- an action is a triple of an x value, a y value, and the value to be placed
-- at said x,y location
newtype UserAction = UserAction (Int, Int, Click)
    deriving (Eq) --Do we need this?

-- an internal state is the minesweeper grid as described below
type InternalState = [[Int]]

-- the game is over when all the 1's are replaced with 4's and there are
-- no more 2's
empty = 0               -- empty space, uncleared
mine = 1                -- mine, undiscovered
emptyFlagged = 2        -- empty space, flagged incorrectly
emptyCleared = 3        -- empty space, cleared over the course of the game
bombFlagged = 4         -- mine, flagged
bombCleared = 5         -- mine, cleared (loss condition)

main = do
	putStrLn "  +───────────────────────+"
	putStrLn "  | M I N E S W E E P E R |"
	putStrLn "  +───────────────────────+"
	putStrLn "  What size would you like the board to be?"
	size <- getLine
	let s = read size
	putStrLn "  How many mines would you like?"
	numMines <- getLine
	let mines = read numMines
	grid <- makeGrid s mines
	play grid s mines

play grid size mines = do
	printGrid grid
	putStrLn "  Please choose a square by inputting an x coordinate, a y coordinate and click or flag. Eg. 12c, 45f"

printGrid :: InternalState -> IO ()
printGrid grid = do
	let size = length grid
	let topper = "  +──" ++ (getTopper (head grid)) ++ "─+"
	putStrLn topper
	let board = getBoard grid size size
	putStrLn board
	let bottom = "  +──" ++ (getTopper (head grid)) ++ "─+"
	putStrLn bottom

getTopper :: [Int] -> String
getTopper [] = []
getTopper (first:rest) = (getTopper rest) ++ (show (length (first:rest))) ++ "─"

getBoard :: InternalState -> Int -> Int -> String
getBoard [] index size = []
getBoard (first:rest) index size
	|index == size = do
		let row = "1 |  " ++ (getRow first) ++ " |\n" ++ (getBoard rest (index - 1) size)
		row
	|index == 1 = do
		let row = (show size) ++ " |  " ++ (getRow first) ++ " |"
		row
	|otherwise = do
		let row = (show (size - index + 1)) ++ " |  " ++ (getRow first) ++ " |\n" ++ (getBoard rest (index - 1) size)
		row

getRow :: [Int] -> String
getRow [] = []
getRow (first:rest) = (getSpace first) ++ " " ++ (getRow rest)

getSpace :: Int -> String
getSpace space
	| space == mine = "0"
	| space == emptyFlagged = "F"
	| space == emptyCleared = " "
	| space == bombFlagged = "F"
	| space == bombCleared = "B"
	| otherwise = "0"

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
