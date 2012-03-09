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

import System.Random

import GHC.Float


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
 deriving (Show)

pointWidth = 0.1
baseLength = 1 :: Float
baseFreq = 500 :: Float

data RLine = RLine
  { lx1 :: Float
  , ly1 :: Float
  , lx2 :: Float
  , ly2 :: Float
  }
 deriving (Show)

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
  , lineSynths :: M.Map ID SynthParams
  , isBold :: [ID]
  , currentPoint :: Maybe ID
  , winsize :: Size
  , currentLine :: Maybe ID -- ID represents start POINT
  , mouse :: (Float, Float)
  } 
 deriving (Show)


setLUp stateref = 
  modifyIORef stateref (\s -> s {lbuttonState = False})
setLDown stateref = 
  modifyIORef stateref (\s -> s {lbuttonState = True})
setRUp stateref = 
  modifyIORef stateref (\s -> s {rbuttonState = False})
setRDown stateref = 
  modifyIORef stateref (\s -> s {rbuttonState = True})

setCurrentPoint stateref id = 
  modifyIORef stateref (\s -> s {currentPoint = Just id})
resetCurrentPoint stateref = 
  modifyIORef stateref (\s -> s {currentPoint = Nothing})
setCurrentLine stateref id = 
  modifyIORef stateref (\s -> s {currentLine = Just id})
resetCurrentLine stateref =
  modifyIORef stateref (\s -> s {currentLine = Nothing})

setMouse stateref x y = 
  modifyIORef stateref (\s -> s {mouse = (x, y)})

newPoint stateref x y = 
  modifyIORef stateref (\s ->
    let points = gspoints s 
        newID = (largestID s + 1) in
    s { gspoints = M.insert newID (RPoint x y) points
      , currentPoint = Just newID }
  )
newLine stateref id1 id2 = do
  gstate <- readIORef stateref
  let newID = largestID gstate +1
  modifyIORef stateref (\s ->
    let lines = gslines s in                        
    s { gslines = M.insert newID (id1, id2) lines
      , currentLine = Nothing }
   )
  addRandomFM stateref newID
  updateSounds stateref id2

newSynth stateref id sp = 
  modifyIORef stateref (\s -> 
   let synths = lineSynths s in
   s {lineSynths = M.insert id sp synths}
  )
  

maximum' [] = 0
maximum' x = maximum x

largestID s = 
  let pids = M.keys (gspoints s)
      lids = M.keys (gslines s)
  in maximum' $ pids ++ lids

updatePoint stateref id p = do
  modifyIORef stateref (\s -> s {gspoints = M.insert id p $ gspoints s})
  updateSounds stateref id
updateSize stateref size = 
  modifyIORef stateref (\s -> s {winsize = size})

-- responsible for handling sound updates
-- location of id changed
-- CHANGE

pairHas x (a,b) = a == x || b == x

updateSounds stateref id = do
  gstate <- readIORef stateref
  let affected = M.filter (pairHas id) (gslines gstate) in do
   modifyIORef stateref (\s -> M.foldrWithKey fixFreq s affected)
   sendParams stateref
  where
    fixFreq id (id1, id2) gstate = 
      let pts = gspoints gstate
          (RPoint x1 y1) = fromJust $ M.lookup id1 pts
          (RPoint x2 y2) = fromJust $ M.lookup id2 pts
          newfreq = (baseFreq * (sqrt $ (x1-x2)^2 + (y1-y2)^2) / baseLength)
          synths = lineSynths gstate
      in
        gstate { lineSynths = M.adjust (\sp -> sp {fbase = float2Double newfreq}) id synths }
  
sendParams stateref = do
  gstate <- readIORef stateref
  mapM_ (\(id, syp) -> setParamOSC id "fbase" (fbase syp)) $ M.toList $ lineSynths gstate


{-pushShape stateref = do
  state <- readIORef stateref
  case currentShape state of
    Just s -> writeIORef stateref $ state{currentShape = Nothing, shapes = (s,False) : shapes state}
    Nothing -> return ()
-}

main = do 
  -- audio
  resetSC3OSC
  installSynthOSC synthDefFM

  -- video
  stateref <- newIORef $ (GameState False False M.empty M.empty M.empty
                          [] Nothing (Size 0 0) Nothing (0,0))

  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "min/dsk"
  displayCallback $= display stateref
  reshapeCallback $= Just (reshape stateref)
  keyboardMouseCallback $= Just (mouseH stateref)
  motionCallback $= Just (mousemove stateref)
  mainLoop
  resetSC3OSC

display gamestate = do 
  clear [ColorBuffer]
  loadIdentity
  gstate <- readIORef gamestate
  let bolds = isBold gstate
  mapM (\(id, p) -> if id `elem` bolds then 
                      drawSquare p (1,1,1) else -- electric purple
                      drawSquare p (0,0,1)) $ M.toList $ gspoints gstate
  mapM (safeDrawLine gstate) $ M.toList $ gslines gstate
  case currentLine gstate of
    Just id -> case M.lookup id (gspoints gstate) of
                 Just (RPoint x1 y1) -> 
                   let (x2, y2) = mouse gstate in drawLine (RLine x1 y1 x2 y2) (1, 0.5, 0)
                 Nothing -> return ()
    Nothing -> return ()
  swapBuffers
 where
   safeDrawLine gstate (_,(id1,id2))
     | Just (RPoint x1 y1) <- M.lookup id1 $ gspoints gstate
     , Just (RPoint x2 y2) <- M.lookup id2 $ gspoints gstate
     = drawLine (RLine x1 y1 x2 y2) (0.75,0,1)
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

mouseH gamestate key state modifiers (Position x' y') = do
  gstate <- readIORef gamestate
  let (Size w h) = winsize gstate
  let (x,y) = normalize' x' y' w h
  if state == Down then do
    case currentPoint gstate of
     Nothing -> -- no current point
      case key of
        MouseButton LeftButton -> 
          case find (\(id, p) -> pointOnPoint x y p pointWidth) (M.toList $ gspoints gstate) of
            Just (id, _) -> setCurrentPoint gamestate id
            Nothing -> return ()
        MouseButton RightButton ->
          case find (\(id, p) -> pointOnPoint x y p pointWidth) (M.toList $ gspoints gstate) of
            Just (id, _) -> setCurrentLine gamestate id
            Nothing -> do newPoint gamestate x y -- adds new point, sets it to current
                          redraw
        _ -> return ()

     Just id -> -- do nothing
      case key of
        MouseButton LeftButton -> return ()
        MouseButton RightButton -> return ()
        _ -> return ()
  else -- Up
    case key of
      MouseButton LeftButton -> 
        resetCurrentPoint gamestate
      MouseButton RightButton -> 
        case currentLine gstate of
          Nothing -> resetCurrentPoint gamestate
          Just id -> 
            case findPoint gstate x y of
              Just (id', _) -> newLine gamestate id id' >> redraw
              Nothing -> resetCurrentLine gamestate -- don't add a new line

      _ -> return ()

findPoint gstate x y = 
  find (\(id, p) -> pointOnPoint x y p pointWidth) (M.toList $ gspoints gstate)
mousemove gamestate (Position x' y') = do
  gstate <- readIORef gamestate
  let (Size w h) = winsize gstate
  let (x,y) = normalize' x' y' w h
  case currentPoint gstate of
    Nothing -> return ()
    Just id -> do
      updatePoint gamestate id (RPoint x y)
  setMouse gamestate x y
  redraw
      
redraw = postRedisplay Nothing


-- audio

mono = audition . out 0 
s freq amp = (sinOsc AR freq 0) * amp
sw freq amp = (saw AR freq) * amp

synthDefFM = 
  let g = control KR "gate" 1
      a = control KR "amp" 1
      d = control KR "dur" 1
      fbase = control KR "fbase" 400
      fosc1  = control KR "fosc1" 10
      oamp1  = control KR "oamp1" 22
      fosc2  = control KR "fosc2" 0
      oamp2  = control KR "oamp2" 1
  in synthdef "fm" $ out 0 $ s (fbase + s fosc1 oamp1 * s fosc2 oamp2) a

-- see above
data SynthParams 
  = SynthParams
    { fbase :: Double
    , fosc1 :: Double
    , oamp1 :: Double
    , fosc2 :: Double
    , oamp2 :: Double
    }
 deriving (Show)                 

roll :: Int -> Int -> IO Int
roll lo hi = getStdRandom (randomR (lo,hi))

roll' = fmap fromIntegral . uncurry roll

addRandomFM stateref id = 
  let prs = 
        [ ("fbase", (200, 1000)) 
        , ("fosc1", (1, 50))
        , ("oamp1", (1, 50))
        , ("fosc2", (1, 50))
        , ("oamp2", (1, 50))
        ] 
  in
  do addSynthOSC "fm" id
     setParamOSC id "amp" 0.1
     fbase <- roll' $ snd (prs !! 0)
     fosc1 <- roll' $ snd (prs !! 1)
     oamp1 <- roll' $ snd (prs !! 2)
     fosc2 <- roll' $ snd (prs !! 3)
     oamp2 <- roll' $ snd (prs !! 4)
     mapM (\(name, val) -> setParamOSC id name val) 
            $ zip (map fst prs) 
                  [fbase, fosc1, oamp1, fosc2, oamp2]
     newSynth stateref id (SynthParams fbase fosc1 oamp1 fosc2 oamp2)     
--     mapM (\(name, (low, high)) -> do roll low high >>= setParam id name . fromIntegral) paramranges
     

addSynthOSC name n =
 withSC3 $ \fd -> do
   send fd $ s_new name n AddToTail 1 []
   send fd $ n_set n []

setParamOSC n name val =
  withSC3 $ \fd -> 
    send fd $ n_set n [(name, val)]

installSynthOSC def = 
  withSC3 $ \fd -> async fd $ d_recv def

setFOSC n f = 
  withSC3 $ \fd -> 
    send fd $ n_set n [("freq", f)]
setAOSC n a = 
  withSC3 $ \fd -> 
    send fd $ n_set n [("amp", a)]
freenOSC n = withSC3 $ \fd -> send fd $ n_free [n]
resetmOSC n = withSC3 $ \fd -> reset fd >> send fd (g_deepFree [1])

resetSC3OSC = withSC3 (\fd -> reset fd)