import System.Random
import Data.Maybe
import Data.Char
import Text.Read

data State = State InternalState        -- the state of the game is the
         deriving (Ord, Eq)--, Show)    -- internal state of the game

data Result = EndOfGame Double                  -- end of game, value
            | ContinueGame InternalState        -- continue with new state
         deriving (Eq, Show)

type Game = UserAction -> State -> Result

type Player = State -> UserAction

type TournamentState = (Int, Int) -- wins, losses

-- =====================================================================
-----------------Minesweeper Game --------------------------------------
-- =====================================================================

data Click = LeftClick  --click
            |RightClick --flag?
        deriving (Eq)

-- an action is a triple of an x value, a y value, and the value to be placed
-- at said x,y location
newtype UserAction = UserAction (Int, Int, Click)
    deriving (Eq) --Do we need this?

readMaybeUA :: String -> Maybe UserAction
readMaybeUA (a:b:c:[])
    |(x == Nothing || y == Nothing || (c /= 'c' && c /= 'f')) = Nothing
    | c == 'c' = Just (UserAction (fromJust x, fromJust y, LeftClick))
    | otherwise = Just (UserAction (fromJust x, fromJust y, LeftClick))
        where
            x = (readMaybe [a] :: Maybe Int)
            y = (readMaybe [b] :: Maybe Int)
readmaybeUA _ = Nothing

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

-- =====================================================================
-- TITLE CARD AND GAME INSTANTIATION
-- Queries user for grid size and number of mines, then starts game
-- =====================================================================

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

-- =====================================================================
-- MAIN GAME LOOP
-- Queries user for a move, then updates the game accordingly
-- TODO: functions for input and updating game
-- =====================================================================
play :: State -> Int -> Int -> TournamentState -> IO TournamentState
play (State grid) size mines tourn = do
    printGrid grid
    putStrLn ("  Mines left: " + mines)
    putStrLn ("  Please choose a square by inputting an x coordinate, a y coordinate and click or flag. Eg. 12c, 45f")
    (UserAction (x,y,c)) <- readUA getLine
    case c of 
        LeftClick -> putStrLn ("Checking for a mine at " + x + " and " + y)
        RightClick -> putStrLn ("Flagging space at " + x + " and " + y)
    let newMines = if c == LeftClick then (mines-1) else mines
    let res = minesweeper (UserAction (x,y,c)) (State grid)
    case res of
        EndOfGame val -> return (playAgain val tourn)
        ContinueGame st -> return (play st size newMines)
       
-- =====================================================================
-- Play Again
-- Queries user if they would like to play again
-- =====================================================================
playAgain :: Double -> TournamentState -> IO TournamentState
playAgain val size mines (wins,losses) = do
    case val of 
        1 -> putStrLn ("You win!")
        0 -> putStrLn ("You lose!")
    let newTourn = if val == 1 then (wins+1, losses) else (wins, losses+1)
    putStrLn("Play again? y/n")
    line <- getLine
    if (line == "y")
        then do
            putStrLn "  What size would you like the board to be?"
            size <- getLine
            let s = read size
            putStrLn "  How many mines would you like?"
            numMines <- getLine
            let mines = read numMines
            grid <- makeGrid s mines
            return (play grid s mines newTourn)
        else
            putStrLn "  Thank you for playing!"
            return newTourn
    
-- =====================================================================
-- General game logic
-- Updates game state with user action
-- =====================================================================
minesweeper :: Game
minesweeper (UserAction (x,y,c)) (State (grid))
    | to_replace == bombCleared                 = EndOfGame 0
    | win new_grid                              = EndOfGame 1    -- did we win?
    | otherwise                                 = ContinueGame (State new_grid)
        where
            init = find grid x y
            to_replace = if (init == empty && c == LeftClick) then emptyCleared
                         else if (init == empty) then emptyFlagged
                         else if (init == mine && c == LeftClick) then bombCleared
                         else if (init == mine) then bombFlagged
                         else if (init == emptyFlagged && c == LeftClick) then empty
                         else if (init == bombFlagged && c == LeftClick) then mine
                         else init
            new_grid = find_replace grid x y to_replace

-- =====================================================================
-- Read user action
-- Queries user for a move, then updates the game accordingly
-- =====================================================================
readUA :: IO String -> IO UserAction
readUA str =
    do
        line <- str
        let ua = readMaybeUA line
        if ua == Nothing
            then
                putStrLn ("Please enter a valid coordinate and command!")
                return readUA
            else
                return fromJust ua

-- =====================================================================
-- Win condition
-- Returns true if no spaces are emptyFlagged or mines
-- =====================================================================
win :: [[Int]] -> Bool
win [] = True
win (first:rest)
    |elem mine first || elem emptyFlagged first = False
    |otherwise = True && win rest               

-- =====================================================================
-- GRID DISPLAY FUNCTIONS
-- =====================================================================

-- prints the grid of the game, adding x and y axes and concealing the
-- locations of remaining mines while displaying flags and cleared areas
printGrid :: InternalState -> IO ()
printGrid grid = do
    let size = length grid
    let topper = "  +──" ++ (getTopper (head grid)) ++ "─+"
    putStrLn topper
    let board = getBoard grid size size
    putStrLn board
    let bottom = "  +──" ++ (getTopper (head grid)) ++ "─+"
    putStrLn bottom

-- builds an x axis for the board
getTopper :: [Int] -> String
getTopper [] = []
getTopper (first:rest) = (getTopper rest) ++ (show (length (first:rest))) ++ "─"

-- builds a string of the entire board and the y axis of the board
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

-- generates each row to be assembled by getBoard
getRow :: [Int] -> String
getRow [] = []
getRow (first:rest) = (getSpace first) ++ " " ++ (getRow rest)

-- generates each space, concealing hidden information from the player
-- which is then assembled by getRow
getSpace :: Int -> String
getSpace space
    | space == mine = "0"
    | space == emptyFlagged = "F"
    | space == emptyCleared = " "
    | space == bombFlagged = "F"
    | space == bombCleared = "B"
    | otherwise = "0"

-- =====================================================================
-- GRID GENERATION FUNCTIONS
-- =====================================================================

-- assembles an InternalState from a grid size and a number of mines
makeGrid :: Int -> Int -> IO State
makeGrid gridSize numMines = State (populateGrid (makeGridHelper gridSize gridSize) gridSize numMines)

-- helper function which assembles lists into a list of lists
makeGridHelper :: Int -> Int -> InternalState
makeGridHelper 0 x = []
makeGridHelper y x = (makeRow x) : (makeGridHelper (y-1) x)

-- helper function which builds lists of int
makeRow :: Int -> [Int]
makeRow 0 = []
makeRow x = 0 : makeRow (x-1)


-- function which adds a number of mines to the grid
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

-- returns true if there is a bomb at x,y
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


-- =====================================================================
-- HELPER FUNCTIONS
-- =====================================================================

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
