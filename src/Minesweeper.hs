module Minesweeper( State,
                    Result(EndOfGame, ContinueGame),
                    Game,
                    Player,
                    Click(LeftClick,RightClick),
                    Difficulty(Easy, Medium, Hard),
                    UserAction(UserAction),
                    minesweeper,
                    getGrid,
                    gridDifficulty,
                    makeGrid,
                    showStateEnd,
                    showState,
                    populateGrid, --IO
                    populateGridUA) where --IO

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
data Difficulty = Easy
        | Medium
        | Hard
    deriving (Eq,Show)

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
minesweeper (UserAction (x,y,c)) (State grid)
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
                            
getGrid :: (Maybe Int) -> (Maybe Int) -> Maybe (Int, Int)
getGrid size numMines = case (size, numMines) of 
                             (Nothing, _) -> Nothing
                             (_, Nothing) -> Nothing
                             (Just s, Just m) -> if m > (s*s) then Nothing else Just (s,m)
                             
gridDifficulty :: (Maybe Difficulty) -> (Maybe Int) -> Maybe (Int, Int)
gridDifficulty Nothing _ = Nothing
gridDifficulty _ Nothing = Nothing
gridDifficulty (Just Easy) (Just s) = getGrid (Just s) (Just (getMines (fromIntegral s) easy))
gridDifficulty (Just Medium) (Just s) = getGrid (Just s) (Just (getMines (fromIntegral s) medium))
gridDifficulty (Just Hard) (Just s) = getGrid (Just s) (Just (getMines (fromIntegral s) hard))

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

-- Returns a state, with all bombs revealed
showStateEnd :: State -> State
showStateEnd (State grid) = State (showHelperEnd grid)

showHelperEnd :: InternalState -> InternalState
showHelperEnd lst = [(map (\ x -> if x==mine then bombCleared else x) f)|f <- lst]

-- Returns a char array of the board as printable characters
showState :: State -> [[Char]]
showState (State grid) = showHelper grid 1 grid

showHelper :: InternalState -> Int -> InternalState -> [[Char]]
showHelper [] _ _ = []
showHelper (f:rst) y ins = (showRow f 1 y ins):(showHelper rst (y+1) ins)

showRow :: [Int] -> Int -> Int -> InternalState -> [Char]
showRow [] _ _ _ = []
showRow (f:rst) x y ins = (showSpace f x y ins):(showRow rst (x+1) y ins)

showSpace :: Int -> Int -> Int -> InternalState -> Char
showSpace space x y ins
    | space == emptyFlagged = 'F'
    | space == emptyCleared = head (show (countbombs ins (x, y)))
    | space == bombFlagged = 'F'
    | space == bombCleared = 'B'
    | otherwise = '_'

-- =====================================================================
-- GRID GENERATION FUNCTIONS
-- =====================================================================
-- generates an empty grid of set size
makeGrid :: Int -> State
makeGrid size = State (makeGridHelper size size)

-- helper function which assembles lists into a list of lists
makeGridHelper :: Int -> Int -> InternalState
makeGridHelper 0 x = []
makeGridHelper y x = (makeRow x) : (makeGridHelper (y-1) x)

-- helper function which builds lists of int
makeRow :: Int -> [Int]
makeRow 0 = []
makeRow x = 0 : makeRow (x-1)


-- IO function which adds a number of mines to the grid
populateGrid :: State -> Int -> Int -> IO State
populateGrid (State grid) gridSize 0 = do
    return (State grid)
populateGrid (State grid) gridSize numMines =
    do
        rg <- newStdGen
        let randomX = head(randomRs (1,gridSize) rg)
        rg2 <- newStdGen
        let randomY = head(randomRs (1,gridSize) rg2)
        if (hasBomb grid randomX randomY)
            then populateGrid (State grid) gridSize numMines
            else populateGrid (State (find_replace grid randomX randomY 1)) gridSize (numMines - 1)

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
-- GRID GENERATION FUNCTIONS - Modified version uses a UserAction as input and ensure that that UA will result in a grid wherein that action is safe
-- =====================================================================
-- function which adds a number of mines to the grid
populateGridUA :: State -> Int -> Int -> UserAction -> IO State
populateGridUA st gridSize 0 _ = do
    return st
populateGridUA (State grid) gridSize numMines (UserAction (x,y,c)) =
    if((gridSize * gridSize) < ((length lst) + numMines)) 
        then populateGrid (State grid) gridSize numMines
        else do
                ins <- populateGridUAHelper grid gridSize numMines lst
                return (State ins)
  where lst = filter (\ (a,b) -> (a > 0) && (a <= gridSize ) && b > 0 && (b <= gridSize)) [(x,y),(x-1, y),(x+1, y),(x-1, y-1),(x, y-1),(x+1,y-1),(x-1, y+1),(x, y+1),(x+1,y+1)]   

populateGridUAHelper :: InternalState -> Int -> Int -> [(Int,Int)] -> IO InternalState
populateGridUAHelper grid _ 0 _ = do return grid
populateGridUAHelper grid gridSize numMines lst =
    do
        rg <- newStdGen
        let randomX = head(randomRs (1,gridSize) rg)
        rg2 <- newStdGen
        let randomY = head(randomRs (1,gridSize) rg2)
        if (hasBomb grid randomX randomY || (randomX,randomY) `elem` lst)
            then populateGridUAHelper grid gridSize numMines lst
            else populateGridUAHelper (find_replace grid randomX randomY 1) gridSize (numMines - 1) lst

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

getMines :: (RealFrac a, Integral b) => a -> a -> b
getMines size ratio = round((size*size)*ratio)