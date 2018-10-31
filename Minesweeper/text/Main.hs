module Main(main) where

import Minesweeper
import Text.Read

type TournamentState = (Int, Int) -- wins, losses

-- =====================================================================
-- TITLE CARD AND GAME INSTANTIATION
-- Queries user for grid size and number of mines, then starts game
-- =====================================================================

main = do
    putStrLn "  +───────────────────────+"
    putStrLn "  | M I N E S W E E P E R |"
    putStrLn "  +───────────────────────+"
    (s,mines) <- getGridPresetsIO
    playInit s mines (0,0)

-- =====================================================================
-- MAIN GAME LOOP
-- Queries user for a move, then updates the game accordingly
-- TODO: functions for input and updating game
-- =====================================================================
play :: State -> Int -> Int -> TournamentState -> IO TournamentState
play st size mines tourn = do
    printGrid st
    putStrLn ("  Mines left: " ++ show mines)
    UserAction (x,y,c) <- readUA size
    case c of
        LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
        RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
    let newMines = if c == RightClick then (mines-1) else mines
    let res = minesweeper (UserAction (x,y,c)) st
    case res of
        EndOfGame val st -> (playAgain st val tourn)
        ContinueGame st -> (play st size newMines tourn)

        
playInit size mines tourn = do
    putStrLn ("  Mines left: " ++ show mines)
    UserAction (x,y,c) <- readUA size
    grid <- makeGridUA size mines (UserAction (x,y,c))
    case c of
        LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
        RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
    let newMines = if c == RightClick then (mines-1) else mines
    let res = minesweeper (UserAction (x,y,c)) grid
    case res of
        EndOfGame val st -> (playAgain st val tourn)
        ContinueGame st -> (play st size newMines tourn)
-- =====================================================================
-- Play Again
-- Queries user if they would like to play again
-- =====================================================================
playAgain :: State -> Double -> TournamentState -> IO TournamentState
playAgain grid val (wins,losses) = do
    printGrid grid
    case val of
        1 -> putStrLn ("You win!")
        0 -> putStrLn ("You lose!")
    let newTourn = if val == 1 then (wins+1, losses) else (wins, losses+1)

    putStrLn ("You have won " ++ (show (fst newTourn)) ++ " games and lost " ++ (show (snd newTourn)) ++ " games")
    putStrLn("Play again? y/n")
    putStrLn("Current tournament: "++show(newTourn)++" Play again? y/n")
    line <- getLine
    if (line == "y")
        then do
            (s,mines) <- getGridPresetsIO
            playInit s mines newTourn
        else do
            putStrLn "  Thank you for playing!"
            return newTourn

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
getGridIO = do
    putStrLn "  What size would you like the board to be?"
    size <- getLine
    putStrLn "  How many mines would you like?"
    numMines <- getLine
    case getGrid (readMaybe size :: Maybe Int) (readMaybe numMines :: Maybe Int) of
        Nothing -> redo
        Just x -> return x
       where redo = do
                        putStrLn("  Please enter a valid size and number of mines!")
                        getGridIO
            
getGridPresetsIO :: IO (Int, Int)
getGridPresetsIO = do
    putStrLn "  What size would you like the board to be?"
    size <- getLine
    putStrLn "  What difficulty would you like? (easy/medium/hard)"
    diff <- getLine
    case gridDifficulty (readMaybeDiff diff) (readMaybe size :: Maybe Int) of
        Nothing -> redo
        Just x -> return x
       where redo = do
                        putStrLn("  Please enter a valid size and difficulty!")
                        getGridPresetsIO
                        
readMaybeDiff :: String -> (Maybe Difficulty)
readMaybeDiff ('e':rst) = Just Easy
readMaybeDiff ('m':rst) = Just Medium
readMaybeDiff ('h':rst) = Just Hard
readMaybeDiff _ = Nothing
                        
readMaybeUA :: String -> Maybe UserAction
readMaybeUA [] = Nothing
readMaybeUA str = readMaybeUA1 (splitsep (==',') str)

readMaybeUA1 :: [String] -> Maybe UserAction
readMaybeUA1 (a:b:c:[]) = case (x0,y0,c) of
    (Just x, Just y, ('c':[])) -> Just (UserAction(x,y,LeftClick))
    (Just x, Just y, ('f':[])) -> Just (UserAction(x,y,RightClick))
    (Nothing, _, _) -> Nothing
    (_, Nothing, _) -> Nothing
    (_, _, c) -> Nothing
   where
       x0 = (readMaybe a :: Maybe Int)
       y0 = (readMaybe b :: Maybe Int)
readMaybeUA1 _ = Nothing

splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t