module Main where

import Minesweeper
import Graphics.UI.Gtk
import Data.Maybe
import Text.Read
import Control.Monad.IO.Class
import Data.IORef

data GUI = GUI {
      mainWin :: Window,
      minesLbl :: Label,
      movesLbl :: Label,
      mineTblCont :: Box,
      lCLick :: RadioButton,
      rClick :: RadioButton,
      setUp :: Dialog,
      sIn :: Entry,
      easy :: RadioButton,
      medium :: RadioButton,
      hard :: RadioButton,
      diagConf :: Button,
      errorWin :: Dialog,
      acceptBtn :: Button,
      cancBtn :: Button
      }

main :: IO ()
main =
    do
      initGUI
      gui <- loadGlade "glade/Minesweeper2.glade"
      setupDiag gui
      mainGUI

loadGlade gladepath =
    do
       builder <- builderNew
       builderAddFromFile builder gladepath

       -- Load main window
       mw <- builderGetObject builder castToWindow "Minesweeper"
       --win <- builderGetObject builder castToTable "WindowTable"
       minel <- builderGetObject builder castToLabel "MinesRemaining"
       movel <- builderGetObject builder castToLabel "MovesRemaining"
       table <- builderGetObject builder castToBox "mineTbl"
       radButton1 <- builderGetObject builder castToRadioButton "LClickBtn"
       radButton2 <- builderGetObject builder castToRadioButton "RClickBtn"

       setup <- builderGetObject builder castToDialog "GameSetup"
       --setupcont <- builderGetObject builder castToBox "SetUpContainer"
       --optionscont <- builderGetObject builder castToBox "Options"

       --sizeBox <- builderGetObject builder castToBox "SizeCont"
       --sizelabel <- builderGetObject builder castToLabel "GridSizeLabel"
       sizeInput <- builderGetObject builder castToEntry "GridSize"

       --mineBox <- builderGetObject builder castToBox "MineCont"
       --minelabel <- builderGetObject builder castToLabel "MineNumberLabel"
       --mineInput <- builderGetObject builder castToEntry "MineNumber"
       easyButton <- builderGetObject builder castToRadioButton "easyButton"
       medButton <- builderGetObject builder castToRadioButton "medButton"
       hardButton <- builderGetObject builder castToRadioButton "hardButton"

       --buttonInput <- builderGetObject builder castToBox "actionArea"
       confirm <- builderGetObject builder castToButton "ConfirmButton"
       
       errorWin <- builderGetObject builder castToDialog "ErrorBoardMsg"
       errorConf <- builderGetObject builder castToButton "acceptBtn"
       errorCanc <- builderGetObject builder castToButton "cancelBtn"

       return $ GUI mw minel movel table radButton1 radButton2 setup sizeInput easyButton medButton hardButton confirm errorWin errorConf errorCanc

setupDiag gui =
    do
        onDestroy (setUp gui) mainQuit
        entrySetText (sIn gui) ""

        onClicked (diagConf gui) readProc
        
        windowPresent (setUp gui)
      where readProc = do
                        s <- (entryGetText (sIn gui) :: IO String)
                        diff <- getDifficulty gui
                        initGame gui (gridDifficulty (Just diff) (readMaybe s :: Maybe Int))

getDifficulty :: GUI -> IO Difficulty
getDifficulty gui = do
    easyBool <- toggleButtonGetActive (easy gui)
    medBool <- toggleButtonGetActive (medium gui)
    hardBool <- toggleButtonGetActive (hard gui)
    if easyBool 
        then return Easy 
        else if medBool 
            then return Medium 
            else return Hard
                        
initGame :: GUI -> (Maybe (Int, Int)) -> IO()
initGame gui Nothing = errorWinSetup gui
initGame gui (Just(0,_)) = errorWinSetup gui
initGame gui (Just (s,m)) = do
    widgetHide (setUp gui)
    onDestroy (mainWin gui) mainQuit
    let grid = makeGrid s
    buildGrid s gui grid
    windowPresent (mainWin gui)

errorWinSetup :: GUI -> IO()
errorWinSetup gui = do
    onClicked (cancBtn gui) mainQuit
    onClicked (acceptBtn gui) acceptProc
    onDestroy (errorWin gui) acceptProc
    windowPresent (errorWin gui)
   where acceptProc = 
           do
                widgetHide (errorWin gui)
                setupDiag gui

buildGrid size gui st = do
    let attach x y st grid btn = tableAttachDefaults grid btn (x-1) x (y-1) y
    let mkMineBtn x y st grid = mkMine st x y gui >>= attach x y st grid
    minTbl <- tableNew size size True
    attachHelper mkMineBtn size size size st minTbl
    boxPackStartDefaults (mineTblCont gui) minTbl
    
attachHelper fn 1 1 _ st table = do fn 1 1 st table
attachHelper fn 1 y x0 st table = do 
    fn 1 y st table
    attachHelper fn x0 (y-1) x0 st table
attachHelper fn x y x0 st table = do
    fn x y st table
    attachHelper fn (x-1) y x0 st table

type TournammentState = (Int,Int)   -- wins, losses

mkMine st x y gui =
    do
      btn <- buttonNew
      return btn
