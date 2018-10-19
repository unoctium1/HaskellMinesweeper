import System.Random
import Data.Maybe
import Data.Char
import Text.Read

data State = State InternalState        -- the state of the game is the
         deriving (Ord, Eq)--, Show)    -- internal state of the game

data Result = EndOfGame Double                  -- end of game, value
            | ContinueGame State        -- continue with new state
         deriving (Eq)

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
    (s,mines) <- getGridIO
    grid <- makeGrid s mines
    play (State grid) s mines (0,0)

-- =====================================================================
-- MAIN GAME LOOP
-- Queries user for a move, then updates the game accordingly
-- TODO: functions for input and updating game
-- =====================================================================
play :: State -> Int -> Int -> TournamentState -> IO TournamentState
play (State grid) size mines tourn = do
    printGrid grid
    putStrLn ("  Mines left: " ++ show mines)
    putStrLn ("  Please choose a square by inputting an x coordinate, a y coordinate and click or flag. Eg. 12c, 45f")
    (UserAction (x,y,c)) <- readUA
    case c of 
        LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
        RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
    let newMines = if c == RightClick then (mines-1) else mines
    let res = minesweeper (UserAction (x,y,c)) (State grid)
    case res of
        EndOfGame val -> (playAgain (find_replace grid x y 5) val tourn)
        ContinueGame st -> (play st size newMines tourn)
       
-- =====================================================================
-- Play Again
-- Queries user if they would like to play again
-- =====================================================================
playAgain :: InternalState -> Double -> TournamentState -> IO TournamentState
playAgain grid val (wins,losses) = do
    printGrid grid
    case val of 
        1 -> putStrLn ("You win!")
        0 -> putStrLn ("You lose!")
    let newTourn = if val == 1 then (wins+1, losses) else (wins, losses+1)
<<<<<<< HEAD
    putStrLn ("You have won " ++ (show (fst newTourn)) ++ " games and lost " ++ (show (snd newTourn)) ++ " games")
    putStrLn("Play again? y/n")
=======
    putStrLn("Current tournament: "++show(newTourn)++" Play again? y/n")
>>>>>>> 7c23bc6a1e8ea63413cee496b3559aa56711b738
    line <- getLine
    if (line == "y")
        then do
            (s,mines) <- getGridIO
            grid <- makeGrid s mines
            play (State grid) s mines newTourn
        else do
            putStrLn "  Thank you for playing!"
            return newTourn
    
-- =====================================================================
-- General game logic
-- Updates game state with user action
-- =====================================================================
minesweeper :: Game
minesweeper (UserAction (x,y,c)) (State (grid))
    | to_replace == bombCleared                 = EndOfGame 0    -- did we loose?
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
readUA :: IO UserAction
readUA =
    do
        line <- getLine
        let ua = readMaybeUA line
        if ua == Nothing
            then do
                putStrLn ("Please enter a valid coordinate and command!")
                readUA
            else do
                return (fromJust ua)
                
getGridIO :: IO (Int, Int)
getGridIO = 
    do
        putStrLn "  What size would you like the board to be?"
        size <- getLine
        putStrLn "  How many mines would you like?"
        numMines <- getLine
        case ((readMaybe size :: Maybe Int),(readMaybe numMines :: Maybe Int)) of
            (Nothing, _) -> redo
            (_, Nothing) -> redo
            (Just size, Just mines) -> return (size,mines)
           where redo = do 
                            putStrLn("Please enter a valid size and number of mines!")
                            getGridIO

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
-- Given an (x,y) coordinate, returns the number of bombs at the space
-- Returns true if no spaces are emptyFlagged or mines
-- =====================================================================
countbombs :: [[Int]] -> (Int, Int) -> Int
countbombs [] _ = 0
countbombs (first:rest) (x, y) =
    let
        neighbors = [(x-1, y),(x+1, y),(x-1, y-1),(x, y-1),(x+1,y-1),(x-1, y+1),(x, y+1),(x+1,y+1)]
        neighborsfilter = filter (\ (a,b) -> (a > 0) && (a <= (length first)) && b > 0 && (b <= (1+(length rest)))) neighbors
        mappedneighbors = map (\ (a,b) -> find (first:rest) a b) neighborsfilter
            
    in
        (length (filter (\ a -> a >= bombFlagged || a == mine) mappedneighbors))    

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
    let board = getBoard grid size size grid
    putStrLn board
    let bottom = "  +──" ++ (getTopper (head grid)) ++ "─+"
    putStrLn bottom

-- builds an x axis for the board
getTopper :: [Int] -> String
getTopper [] = []
getTopper (first:rest) = (getTopper rest) ++ (show (length (first:rest))) ++ "─"

-- builds a string of the entire board and the y axis of the board
getBoard :: InternalState -> Int -> Int -> InternalState -> String
getBoard [] index size _ = []
getBoard (first:rest) index size ins
    |index == size = do
        let row = "1 |  " ++ (getRow first 1 1 ins) ++ " |\n" ++ (getBoard rest (index - 1) size ins)
        row
    |index == 1 = do
        let row = (show size) ++ " |  " ++ (getRow first 1 size ins) ++ " |"
        row
    |otherwise = do
        let row = (show (size - index + 1)) ++ " |  " ++ (getRow first 1 (size-(index-1)) ins) ++ " |\n" ++ (getBoard rest (index - 1) size ins)
        row

-- generates each row to be assembled by getBoard
getRow :: [Int] -> Int -> Int -> InternalState -> String
getRow [] _ _ _ = []
getRow (first:rest) x y ins = (getSpace first x y ins) ++ " " ++ (getRow rest (x+1) y ins)

-- generates each space, concealing hidden information from the player
-- which is then assembled by getRow
getSpace :: Int -> Int -> Int -> InternalState -> String
getSpace space x y ins
    | space == mine = "_"
    | space == emptyFlagged = "F"
    | space == emptyCleared = show (countbombs ins (x, y))
    | space == bombFlagged = "F"
    | space == bombCleared = "B"
    | otherwise = "_"

-- =====================================================================
-- GRID GENERATION FUNCTIONS
-- =====================================================================

-- assembles an InternalState from a grid size and a number of mines
makeGrid :: Int -> Int -> IO InternalState
makeGrid gridSize numMines = populateGrid (makeGridHelper gridSize gridSize) gridSize numMines

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

readMaybeUA :: String -> Maybe UserAction
readMaybeUA (a:b:c:[])
    |(x == Nothing || y == Nothing || (c /= 'c' && c /= 'f')) = Nothing
    | c == 'c' = Just (UserAction (fromJust x, fromJust y, LeftClick))
    | otherwise = Just (UserAction (fromJust x, fromJust y, RightClick))
        where
            x = (readMaybe [a] :: Maybe Int)
            y = (readMaybe [b] :: Maybe Int)
readMaybeUA _ = Nothing
