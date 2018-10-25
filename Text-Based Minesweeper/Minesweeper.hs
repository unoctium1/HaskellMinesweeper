module Minesweeper( State,
                    Result(EndOfGame, ContinueGame),
                    Game,
                    Player,
                    Click(LeftClick,RightClick),
                    UserAction(UserAction),
                    minesweeper,
                    readUA,
                    getGridPresetsIO,
                    makeGrid,
                    printGrid) where

import System.Random
import Data.Maybe
import Data.Char
import Text.Read

data State = State InternalState        -- the state of the game is the
         deriving (Ord, Eq)--, Show)    -- internal state of the game

data Result = EndOfGame Double State                 -- end of game, value, endstate
            | ContinueGame State        -- continue with new state
         deriving (Eq)

type Game = UserAction -> State -> Result

type Player = State -> UserAction

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

-- difficulty enumeration ratios
easy = 0.3
medium = 0.5
hard = 0.7
    
-- =====================================================================
-- General game logic
-- Updates game state with user action
-- =====================================================================
minesweeper :: Game
minesweeper (UserAction (x,y,c)) (State (grid))
    | to_replace == bombCleared                 = EndOfGame 0 (State new_grid)   -- did we lose?
    | win new_grid                              = EndOfGame 1 (State new_grid)   -- did we win?
    | otherwise                                 = if boolToExplode then ContinueGame (State (explode new_grid (x,y))) else ContinueGame (State new_grid)
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
            boolToExplode = to_replace == emptyCleared && (countbombs new_grid (x,y)) == 0

-- =====================================================================
-- Read user action
-- Queries user for a move, then updates the game accordingly
-- =====================================================================
readUA :: Int -> IO UserAction
readUA size =
    do
        putStrLn ("  Please choose a square by inputting an x coordinate, a y coordinate and click or flag. Eg. 1,2,c, 4,5,f, 10,15,f")
        line <- getLine
        let ua = readMaybeUA line
        case ua of
            Nothing -> redo
            Just (UserAction (x,y,c)) -> if (x > size) || (y > size) || (x < 0) || (y < 0) then redo else return (UserAction (x,y,c))
          where redo = do 
                        putStrLn ("  Please enter a valid coordinate and command!")
                        readUA size 
                
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
            (Just size, Just mines) -> if mines > (size*size) then redo else return (size,mines)
           where redo = do 
                            putStrLn("  Please enter a valid size and number of mines!")
                            getGridIO
                            
getGridPresetsIO :: IO (Int, Int)
getGridPresetsIO = 
    do
        putStrLn "  What size would you like the board to be?"
        size <- getLine
        putStrLn "  What difficulty would you like? (easy/medium/hard)"
        diff <- getLine
        case ((readMaybe size :: Maybe Int),diff) of
            (Nothing, _) -> redo
            (Just s, diff) -> case (gridDifficulty (head diff) s) of 
                Just x -> return x
                Nothing -> redo
           where redo = do 
                            putStrLn("  Please enter a valid size and difficulty!")
                            getGridPresetsIO
           
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
-- Given an (x,y) coordinate, recursively explodes for all cells where count bombs is 0
-- Returns true if no spaces are emptyFlagged or mines
-- =====================================================================
explode :: InternalState -> (Int, Int) -> InternalState
explode [] _ = []
explode (first:rest) (x, y) =
    let
        neighbors = [(x-1, y),(x+1, y),(x-1, y-1),(x, y-1),(x+1,y-1),(x-1, y+1),(x, y+1),(x+1,y+1)]
        neighborsfilter = filter (\ (a,b) -> (a > 0) && (a <= (length first)) && b > 0 && (b <= (1+(length rest)))) neighbors
        neighborsfilter2 = filter (\ (a,b) -> (find (first:rest) a b) == 0) neighborsfilter
    in
        explodehelper (first:rest) neighborsfilter2
        
explodehelper st [] = st
explodehelper st ((x,y):rst)
    | countbombs st (x,y) == 0      =   explodehelper (explode (find_replace st x y emptyCleared) (x,y)) rst
    | otherwise                     =   explodehelper (find_replace st x y emptyCleared) rst                

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
printGrid :: State -> IO ()
printGrid (State grid) = do
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
makeGrid :: Int -> Int -> IO State
makeGrid gridSize numMines = do 
    ins <- populateGrid (makeGridHelper gridSize gridSize) gridSize numMines
    return (State ins)

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
readMaybeUA [] = Nothing
readMaybeUA str = readMaybeUA1 (splitsep (==',') str)

readMaybeUA1 :: [String] -> Maybe UserAction
readMaybeUA1 (a:b:c:[])
    |(x == Nothing || y == Nothing || (c /= ['c'] && c /= ['f'])) = Nothing
    | c == ['c'] = Just (UserAction (fromJust x, fromJust y, LeftClick))
    | otherwise = Just (UserAction (fromJust x, fromJust y, RightClick))
        where
            x = (readMaybe a :: Maybe Int)
            y = (readMaybe b :: Maybe Int)
readMaybeUA1 _ = Nothing        

splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

getMines :: (RealFrac a, Integral b) => a -> a -> b         
getMines size ratio = round((size*size)*ratio)

gridDifficulty :: Char -> Int -> Maybe (Int, Int)
gridDifficulty 'e' s = Just (s, (getMines (fromIntegral s) easy))
gridDifficulty 'm' s = Just (s, (getMines (fromIntegral s) medium))
gridDifficulty 'h' s = Just (s, (getMines (fromIntegral s) hard))
gridDifficulty _ s = Nothing