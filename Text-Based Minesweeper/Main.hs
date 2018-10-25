module Main(main) where

import Minesweeper

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
    grid <- makeGrid s mines
    play grid s mines (0,0)

-- =====================================================================
-- MAIN GAME LOOP
-- Queries user for a move, then updates the game accordingly
-- TODO: functions for input and updating game
-- =====================================================================
play :: State -> Int -> Int -> TournamentState -> IO TournamentState
play st size mines tourn = do
    printGrid st
    putStrLn ("  Mines left: " ++ show mines)
    (UserAction (x,y,c)) <- readUA size
    case c of
        LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
        RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
    let newMines = if c == RightClick then (mines-1) else mines
    let res = minesweeper (UserAction (x,y,c)) st
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
            grid <- makeGrid s mines
            play grid s mines newTourn
        else do
            putStrLn "  Thank you for playing!"
            return newTourn

