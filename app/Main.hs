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
      mineTbl :: Fixed,
      setUp :: Dialog,
      sIn :: Entry,
      mIn :: Entry,
      diagConf :: Button
      }

main :: IO ()
main =
    do
      initGUI
      gui <- loadGlade "glade/Minesweeper1.glade"
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
       table <- builderGetObject builder castToFixed "MineTable"

       setup <- builderGetObject builder castToDialog "GameSetup"
       --setupcont <- builderGetObject builder castToBox "SetUpContainer"
       --optionscont <- builderGetObject builder castToBox "Options"

       --sizeBox <- builderGetObject builder castToBox "SizeCont"
       --sizelabel <- builderGetObject builder castToLabel "GridSizeLabel"
       sizeInput <- builderGetObject builder castToEntry "GridSize"

       --mineBox <- builderGetObject builder castToBox "MineCont"
       --minelabel <- builderGetObject builder castToLabel "MineNumberLabel"
       mineInput <- builderGetObject builder castToEntry "MineNumber"

       --buttonInput <- builderGetObject builder castToBox "actionArea"
       confirm <- builderGetObject builder castToButton "ConfirmButton"

       return $ GUI mw minel movel table setup sizeInput mineInput confirm

setupDiag gui =
    do
        onDestroy (setUp gui) mainQuit
        entrySetText (mIn gui) ""
        entrySetText (sIn gui) ""

        onClicked (diagConf gui) readProc
        
        windowPresent (setUp gui)
      where readProc = do
                        s <- (entryGetText (sIn gui) :: IO String)
                        m <- (entryGetText (mIn gui) :: IO String)
                        initGame gui (getGrid (readMaybe s :: Maybe Int) (readMaybe m :: Maybe Int))

initGame :: GUI -> (Maybe (Int, Int)) -> IO()
initGame gui Nothing = setupDiag gui
initGame gui (Just (s,m)) = do
    widgetHide (setUp gui)
    onDestroy (mainWin gui) mainQuit
    putStrLn("test success")
    windowPresent (mainWin gui)


type TournammentState = (Int,Int)   -- wins, losses

mkMine :: String -> IO Button
mkMine label =
    do
      btn <- buttonNew
      set btn [ buttonLabel := label ]
      return btn
