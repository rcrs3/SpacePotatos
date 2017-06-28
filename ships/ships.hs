module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

data GameState = Level Int

type ShipsAction a = IOGame () () GameState () a
--type ShipsAction a = IOGame () () () () a
type ShipsObject = GameObject ()
 
width = 800
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble


magenta :: InvList
magenta = Just [(255,0,255)]


main :: IO ()
main = do
  space <- getDataFileName "ships/space.bmp"
  shipDown <- getDataFileName "ships/alienblaster.bmp"
  shipUp <- getDataFileName "ships/alienblaster3.bmp"
  shipLeft <- getDataFileName "ships/alienblaster2.bmp"
  shipRight <- getDataFileName "ships/alienblaster4.bmp"
  roid <- getDataFileName "ships/roid_big.bmp"
  hole <- getDataFileName "ships/hole.bmp"
  let 
    winConfig = ((100,80),(width,height),"In space...");
    bmplist = [(space,Nothing),(shipUp,magenta),(roid,magenta),(hole,magenta), (shipDown, magenta), (shipLeft, magenta), (shipRight, magenta)];
    --gameMap = colorMap 0.0 0.0 0.0 w ;
	gameMap = textureMap 0 w h w h;
	bar   = objectGroup "barGroup" [createBar];
    ball = objectGroup "ballGroup" [createBall,createBall2,createBall3,createBall4,createBlackHole];    

    input = [(Char 'q', Press, \_ _ -> funExit), (Char 'r', Press, restart), (SpecialKey KeyRight, StillDown, turnRight), (SpecialKey KeyLeft,  StillDown, turnLeft), (SpecialKey KeyUp, StillDown, turnUp), (SpecialKey KeyDown, StillDown, turnDown)]
  funInit winConfig gameMap [bar, ball] (Level 1) () input gameCycle (Timer 40) bmplist

restart :: Modifiers -> Position -> ShipsAction ()
restart _ _ = do bar <- findObject "bar" "barGroup"
                 setObjectCurrentPicture 1 bar
                 setObjectPosition (w/2,30) bar
                 blackHole <- findObject "blackHole" "ballGroup"
                 setObjectPosition (w/2, h-30) blackHole
                 ball2 <- findObject "ball2" "ballGroup"
                 ball3 <- findObject "ball3" "ballGroup"
                 ball4 <- findObject "ball4" "ballGroup"
                 setObjectAsleep True ball2
                 setObjectAsleep True ball3
                 setObjectAsleep True ball4
                 setGameState (Level 1)

createBar :: ShipsObject
createBar =
  let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)];
      barPic   = Tex (60,60) 1;
  in object "bar" barPic True (w/2,30) (0,0) ()

turnRight :: Modifiers -> Position -> ShipsAction ()
turnRight _ _ = do
  obj <- findObject "bar" "barGroup"
  setObjectCurrentPicture 6 obj
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + 20 + sX / 2 <= w)
     then (setObjectPosition ((pX + 20),pY) obj)
     else (setObjectPosition (w - (sX/2), pY) obj)

turnLeft :: Modifiers -> Position -> ShipsAction ()
turnLeft _ _ = do
  obj <- findObject "bar" "barGroup"
  setObjectCurrentPicture 5 obj
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - 20 - sX/2 >= 0)
     then (setObjectPosition ((pX - 20),pY) obj)
     else (setObjectPosition (sX/2, pY) obj)

turnUp :: Modifiers -> Position -> ShipsAction ()
turnUp _ _ = do
  obj <- findObject "bar" "barGroup"
  setObjectCurrentPicture 1 obj
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY +20 + sY/2 <= h)
     then (setObjectPosition (pX, (pY + 20)) obj)
     else (setObjectPosition (pX, h - (sY/2)) obj)

turnDown :: Modifiers -> Position -> ShipsAction ()
turnDown _ _ = do
  obj <- findObject "bar" "barGroup"
  setObjectCurrentPicture 4 obj
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY - 20 - sY / 2 >= 0)
     then (setObjectPosition (pX, (pY - 20)) obj)
     else (setObjectPosition (pX, sY/2) obj)

createBall :: ShipsObject
createBall =
  let ballPic = Tex (25,25) 2
  in object "ball1" ballPic True (w/2,h/2) (-40,0) ();
      
createBall2 :: ShipsObject
createBall2 =
  let ballPic = Tex (25,25) 2
 in object "ball2" ballPic True (w/2, 3*h/4) (-60, 0) ();

createBall3 :: ShipsObject
createBall3 = 
  let ballPic = Tex (25,25) 2
 in object "ball3" ballPic True (w/4, h/2) (0, -70) ();

createBall4 :: ShipsObject
createBall4 = 
  let ballPic = Tex (25,25) 2
 in object "ball4" ballPic True (3*w/4, h/2) (0, -80) ();

createBlackHole :: GameObject ()
createBlackHole =
--  let blackHolePic = Basic (Circle 20.0 0.5 0.0 0.7 Filled)
  let blackHolePic = Tex (70,70) 3
  in object "blackHole" blackHolePic True (w/2, h-30) (0,0) ();

gameCycle :: ShipsAction ()
gameCycle = do 
  (Level level) <- getGameState
  case level of
    0 -> do
      printOnScreen (show("Game over...")) TimesRoman24 ((w/2)-90,h-100) 1.0 1.0 1.0
      printOnScreen (show("Press 'q' to quit")) TimesRoman24 ((w/2)-100,h-300) 1.0 1.0 1.0
      printOnScreen (show("Press 'r' to play again")) TimesRoman24 ((w/2)-100,h-350) 1.0 1.0 1.0
      ball1 <- findObject "ball1" "ballGroup"
      ball2 <- findObject "ball2" "ballGroup"
      ball3 <- findObject "ball3" "ballGroup"
      ball4 <- findObject "ball4" "ballGroup"
      blackHole <- findObject "blackHole" "ballGroup"
      bar <- findObject "bar" "barGroup"
      setObjectAsleep True ball1
      setObjectAsleep True ball2
      setObjectAsleep True ball3
      setObjectAsleep True ball4
      setObjectAsleep True bar
    1 -> firstLevel
    2 -> secondLevel
    3 -> thirdLevel
    4 -> do
      printOnScreen (show("You won!!!")) TimesRoman24 ((w/2)-90,h-100) 1.0 1.0 1.0
      printOnScreen (show("Press 'q' to quit")) TimesRoman24 ((w/2)-100,h-300) 1.0 1.0 1.0
      printOnScreen (show("Press 'r' to play again")) TimesRoman24 ((w/2)-100,h-350) 1.0 1.0 1.0
      ball1 <- findObject "ball1" "ballGroup"
      ball2 <- findObject "ball2" "ballGroup"
      ball3 <- findObject "ball3" "ballGroup"
      ball4 <- findObject "ball4" "ballGroup"
      blackHole <- findObject "blackHole" "ballGroup"
      bar <- findObject "bar" "barGroup"
      setObjectAsleep True ball1
      setObjectAsleep True ball2
      setObjectAsleep True ball3
      setObjectAsleep True ball4
      setObjectAsleep True bar

firstLevel :: ShipsAction()
firstLevel = do
  ball1 <- findObject "ball1" "ballGroup"
  setObjectAsleep False ball1
  bar <- findObject "bar" "barGroup"
  setObjectAsleep False bar
  blackHole <- findObject "blackHole" "ballGroup"
  setObjectAsleep False blackHole
  ball1Left <- objectLeftMapCollision ball1
  when (ball1Left) (reverseXSpeed ball1)
  ball1Right <- objectRightMapCollision ball1
  when (ball1Right) (reverseXSpeed ball1)
  lossCol1 <- objectsCollision bar ball1
  when (lossCol1) (setGameState (Level 0))
  winCol <- objectsCollision bar blackHole
  when (winCol) (setGameState (Level 2))
  when (winCol) (setObjectCurrentPicture 4 bar)

secondLevel :: ShipsAction()
secondLevel = do
  bar <- findObject "bar" "barGroup"
  setObjectAsleep False bar
  ball1 <- findObject "ball1" "ballGroup"
  ball2 <- findObject "ball2" "ballGroup"
  setObjectAsleep False ball2
  blackHole <- findObject "blackHole" "ballGroup"
  setObjectAsleep False blackHole
  setObjectPosition (w-40, 50) blackHole
  ball1Left <- objectLeftMapCollision ball1
  ball2Left <- objectLeftMapCollision ball2
  when (ball1Left) (reverseXSpeed ball1)
  when (ball2Left) (reverseXSpeed ball2)
  ball1Right <- objectRightMapCollision ball1
  ball2Right <- objectRightMapCollision ball2
  when (ball1Right) (reverseXSpeed ball1)
  when (ball2Right) (reverseXSpeed ball2)
  lossCol1 <- objectsCollision bar ball1
  when (lossCol1) (setGameState (Level 0))
  lossCol2 <- objectsCollision bar ball2
  when (lossCol2) (setGameState (Level 0))
  winCol <- objectsCollision bar blackHole
  when (winCol) (setGameState (Level 3))
  when (winCol) (setObjectCurrentPicture 5 bar)
thirdLevel :: ShipsAction()
thirdLevel = do
  bar <- findObject "bar" "barGroup"
  setObjectAsleep False bar
  ball1 <- findObject "ball1" "ballGroup"
  setObjectAsleep False ball1
  --setObjectPosition (w/2, h/4) ball1
  ball2 <- findObject "ball2" "ballGroup"
  setObjectAsleep False ball2
  --setObjectPosition (w/2, 3*h/4) ball2
  ball3 <- findObject "ball3" "ballGroup"
  setObjectAsleep False ball3
  ball4 <- findObject "ball4" "ballGroup"
  setObjectAsleep False ball4
  blackHole <- findObject "blackHole" "ballGroup"
  setObjectAsleep False blackHole
  setObjectPosition (50, h-40) blackHole
  ball1Left <- objectLeftMapCollision ball1
  ball2Left <- objectLeftMapCollision ball2
  when (ball1Left) (reverseXSpeed ball1)
  when (ball2Left) (reverseXSpeed ball2)
  ball1Right <- objectRightMapCollision ball1
  ball2Right <- objectRightMapCollision ball2
  when (ball1Right) (reverseXSpeed ball1)
  when (ball2Right) (reverseXSpeed ball2)
  ball3Down <- objectBottomMapCollision ball3
  ball4Down <- objectBottomMapCollision ball4
  when (ball3Down) (reverseYSpeed ball3)
  when (ball4Down) (reverseYSpeed ball4)
  ball3Up <- objectTopMapCollision ball3
  ball4Up <- objectTopMapCollision ball4
  when (ball3Up) (reverseYSpeed ball3)
  when (ball4Up) (reverseYSpeed ball4)
  lossCol1 <- objectsCollision bar ball1
  when (lossCol1) (setGameState (Level 0))
  lossCol2 <- objectsCollision bar ball2
  when (lossCol2) (setGameState (Level 0))
  lossCol3 <- objectsCollision bar ball3
  when (lossCol3) (setGameState (Level 0))
  lossCol4 <- objectsCollision bar ball4
  when (lossCol4) (setGameState (Level 0))
  winCol <- objectsCollision bar blackHole
  when (winCol) (setGameState (Level 4))
    
{--}
