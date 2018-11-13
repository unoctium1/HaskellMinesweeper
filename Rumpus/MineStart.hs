module MineStart where
import Rumpus
import Minesweeper
import qualified Data.Vector as V

size = 10
diffRatio = 0.2

start :: Start
start = do
	
	let initState = makeGrid size
	st <- populateGrid initState size 20
	
	xKnob <- addKnob "x" (Linear 1 size) 1
	yKnob <- addKnob "y" (Linear 1 size) 1
	
	pose <- getPose
    buttonLeftClick <- spawnChild $ do
        myShape           ==> Cube
        myBody            ==> Animated
        mySize            ==> 0.1
        -- Set initial pose so there's not a spurious collision
        myPose            ==> pose !*! position (V3 0 0.5 0)
        myDragOverride    ==> True
        myDragBegan  ==> do
			x <- readKnob xKnob
			y <- readKnob yKnob
            hue <- randomRange (0,1)
            setColor $ colorHSL hue 0.8 0.4
			st <- updateGrid st UserAction(x, y, LeftClick)

    attachEntity buttonLeftClick (position $ V3 0 0.5 0)
	
	buttonRightClick <- spawnChild $ do
        myShape           ==> Cube
        myBody            ==> Animated
        mySize            ==> 0.1
        -- Set initial pose so there's not a spurious collision
        myPose            ==> pose !*! position (V3 0 1 0)
        myDragOverride    ==> True
        myDragBegan  ==> do
            hue <- randomRange (0,1)
			x <- readKnob xKnob
			y <- readKnob yKnob
            setColor $ colorHSL hue 0.8 0.4
			st <- updateGrid st UserAction(x, y, RightClick)

    attachEntity buttonRightClick (position $ V3 0 1 0)
	
createGrid st size 
	
updateGrid :: State -> UserAction -> IO State
updateGrid st ua = do
	let r = minesweeper st ua
	
	case r where 
		ContinueGame newSt -> return newSt
		EndOfGame val endSt -> endGame val

endGame :: Double -> IO State
endGame val = do
	let initState = makeGrid size
	st <- populateGrid initState size 20
	return st