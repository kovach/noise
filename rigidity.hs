{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID

import Data.IORef

import Data.Maybe
import qualified Data.Map as M
import Data.List

{-
class Shape a where
  draw    :: a -> IO ()
  drawAlt :: a -> IO ()
  pointIn :: Float -> Float -> a -> Bool

data SObj = forall a. (Shape a) => SObj a

instance Shape RSquare where
  draw s =  color (Color3 1 (0.5::GLfloat) 0) >> drawSquare s
  drawAlt s = color (Color3 (1::GLfloat) 0 0) >> drawSquare s
  pointIn mx my (RSquare x y width) = 
    (mx > x) && (my > y) && (mx < x + width) && (my < y + width)

instance Shape RLine where
  draw (RLine x1 y1 x2 y2) = do 
    color (Color3 1 (1::GLfloat) 0)
    renderPrimitive Lines $ 
           mapM_ (\(x, y, z)->vertex $ Vertex3 x y z)
           $ map flatz [(x1,y1),(x2,y2)]
  drawAlt = draw
  pointIn mx my (RLine x1 y1 x2 y2) = 
    ((mx > x1) && (my > y1) && (mx < x2) && (mx < y2)) ||
    ((mx > x2) && (my > y2) && (mx < x1) && (mx < y1))
  

myShapes = [SObj (RSquare 0 0 1), SObj (RLine 0 0 1 1)]
-}

data RPoint = RPoint
  { sx :: Float
  , sy :: Float
  }

pointWidth = 0.1

data RLine = RLine
  { lx1 :: Float
  , ly1 :: Float
  , lx2 :: Float
  , ly2 :: Float
  }

flatz (x,y) = (x,y,0.0)

drawSquare (RPoint x y) (r, g, b) = do
  color (Color3 (r ::GLfloat) g b)
  let width = pointWidth in
   renderPrimitive Polygon $ 
   mapM_ (\(x, y, z)->vertex $ Vertex3 x y z)
   $ map flatz [(x,y),(x+width,y),(x+width,y+width),(x,y+width)]

drawLine (RLine x1 y1 x2 y2) (r, g, b)= do 
  color (Color3 (r::GLfloat) g b)
  renderPrimitive Lines $ 
   mapM_ (\(x, y, z)->vertex $ Vertex3 x y z)
   $ map flatz [(x1,y1),(x2,y2)]

pointOnPoint mx my (RPoint x y) width = 
  (mx > x) && (my > y) && (mx < x + width) && (my < y + width)

pointOnLine mx my (RLine x1 y1 x2 y2) = 
  ((mx > x1) && (my > y1) && (mx < x2) && (mx < y2)) ||
  ((mx > x2) && (my > y2) && (mx < x1) && (mx < y1))


type ID = Int

data GameState 
  = GameState
  { lbuttonState :: Bool
  , rbuttonState :: Bool
  , gspoints :: M.Map ID RPoint
  , gslines :: M.Map ID (ID, ID)
  , isBold :: [ID]
  , currentPoint :: Maybe ID
  , winsize :: Size
  } 

setLUp stateref = 
  modifyIORef stateref (\s -> s {lbuttonState = False})
setLDown stateref = 
  modifyIORef stateref (\s -> s {lbuttonState = True})
setRUp stateref = 
  modifyIORef stateref (\s -> s {rbuttonState = False})
setRDown stateref = 
  modifyIORef stateref (\s -> s {rbuttonState = True})

setCurrent stateref id = 
  modifyIORef stateref (\s -> s {currentPoint = Just id})
resetCurrent stateref = 
  modifyIORef stateref (\s -> s {currentPoint = Nothing})
updatePoint stateref id p = 
  modifyIORef stateref (\s -> s {gspoints = M.insert id p $ gspoints s})

updateSize stateref size = 
  modifyIORef stateref (\s -> s {winsize = size})


{-pushShape stateref = do
  state <- readIORef stateref
  case currentShape state of
    Just s -> writeIORef stateref $ state{currentShape = Nothing, shapes = (s,False) : shapes state}
    Nothing -> return ()
-}

main = do 
 -- audio
  withSC3 (\fd -> 
   do {
     reset fd
   ; let fun = defineFunny
   ; lol <- send fd $ g_queryTree [(0,True)]
   ; print lol
   })
  installSynth defineFunny
--  addSynth 22 400 0.5


 -- video
  stateref <- newIORef $ (GameState False False (M.fromList [(1, RPoint 0 0)]) M.empty 
                          [] Nothing (Size 0 0))

  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "noise"
  displayCallback $= display stateref
  reshapeCallback $= Just (reshape stateref)
  keyboardMouseCallback $= Just (mouse stateref)
  motionCallback $= Just (mousemove stateref)
  mainLoop

display gamestate = do 
  clear [ColorBuffer]
  loadIdentity
  gstate <- readIORef gamestate
  let bolds = isBold gstate
  mapM (\(id, p) -> if id `elem` bolds then 
                      drawSquare p (1,0,0) else
                      drawSquare p (1,0.5,0)) $ M.toList $ gspoints gstate
  swapBuffers
reshape gamestate s@(Size w h) = do
  updateSize gamestate s
  viewport $= (Position 0 0, s)
  postRedisplay Nothing
normalize' x' y' w' h' = 
  let x = fromIntegral x'
      y = fromIntegral y'
      w = fromIntegral w'
      h = fromIntegral h' in
  (2*x/w-1, -(2*y/h-1))

mouse gamestate key state modifiers (Position x' y') = do
  gstate <- readIORef gamestate
  let (Size w h) = winsize gstate
  let (x,y) = normalize' x' y' w h
  if state == Down then do
    print (x,y)                 
    case currentPoint gstate of
     Nothing -> -- no current point
      case key of
        MouseButton LeftButton -> 
          case find (\(id, p) -> pointOnPoint x y p pointWidth) (M.toList $ gspoints gstate) of
            Just (id, _) -> setCurrent gamestate id >> print "GOT IT"
            Nothing -> return ()
        MouseButton RightButton -> return ()
     Just id ->
      case key of
        MouseButton LeftButton -> return ()
        MouseButton RightButton -> return ()
        _ -> return ()
  else -- Up
    case key of
      MouseButton LeftButton -> 
        resetCurrent gamestate
      MouseButton RightButton -> return ()
      _ -> return ()

mousemove gamestate (Position x' y') = do
  gstate <- readIORef gamestate
  let (Size w h) = winsize gstate
  let (x,y) = normalize' x' y' w h
  case currentPoint gstate of
    Nothing -> return ()
    Just id -> do
      updatePoint gamestate id (RPoint x y)
      postRedisplay Nothing
      
      

      
  


-- audio

mono = audition . out 0 
s freq amp = (sinOsc AR freq 0) * amp
sw freq amp = (saw AR freq) * amp

funny freq amp = 
  let mult = freq / 400.0
      c1 = s (mult * 100) 0.6
      c2 = s (mult * 400) 0.25
      c3 = s (1 * 600) 0.10
      saw1 = (sw (mult * 1 * 800) 0.05)
      saw2 = (sw (mult * 1 * 100) 0.05)
  in
    (c1 + c2 + c3 + saw1 + saw2) * amp
defineFunny =
   let {f = control KR "freq" 440
       ;g = control KR "gate" 1
       ;a = control KR "amp" 1
       ;dur = control KR "dur" 1
       ;d = envASR 0.05 dur dur (EnvNum (-4))
       ;e = envGen KR g a 0 1 RemoveSynth d
       ;o = out 0 (funny f a * e)}
      in synthdef "sine" o
--withSC3 \fd -> send fd $ n_set 2222 [("freq", 900), ("amp", 0.5)]
--withSC3 $ \fd -> send fd $ n_run [(22, True)]

installSynth def = 
  withSC3 $ \fd -> async fd $ d_recv def

addSynth n f a =
 withSC3 $ \fd -> do
   send fd $ s_new "sine" n AddToTail 1 []
   send fd $ n_set n [("freq", f), ("amp", a)]   
setF n f = 
  withSC3 $ \fd -> 
    send fd $ n_set n [("freq", f)]
setA n a = 
  withSC3 $ \fd -> 
    send fd $ n_set n [("amp", a)]
freen n = withSC3 $ \fd -> send fd $ n_free [n]
resetm n = withSC3 $ \fd -> reset fd >> send fd (g_deepFree [1])

