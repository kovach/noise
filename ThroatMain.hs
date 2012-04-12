{-# LANGUAGE NamedFieldPuns #-}

import Graphics.Rendering.OpenGL hiding (get, normalize)
import Graphics.UI.GLUT hiding (get, normalize)

import Data.IORef
import Control.Monad.State
import Control.Monad
import Control.Applicative

import Data.Maybe
import qualified Data.Map as M
import Data.List
import GHC.Float

import Util
import Graphics
import Audio
import Synths
--import TestPattern

-- stuff
setAmps freqs = 
  mapM (\n -> do
          setParamOSC (base+n) "amp" (freqs !! n))
       [0..(num-1)]


conv = float2Double . fromIntegral

addTones = do
  installSynthOSC toneSynth
  mapM (\n -> do
          addSynthOSC "tone" n
          setParamOSC n "amp" 0.1 
          setParamOSC n "freq" ((conv (n - base + 1)) * 100)
          return n) [base..(base+num-1)]

base = 22 :: Int
num = 8
normalizedubs xs = 
  map (/ (sum xs)) xs


-- end stuff


type ID = Int

data GlutState
  = GlutState
  { winsize :: Size
  , mousepos :: (Float, Float)
  , keystate :: M.Map Key KeyState
  } 
 deriving (Show)

data GameState
  = GameState
  { which :: Pair
  , lower :: (Double, Double)
  , upper :: (Double, Double) 

  , p1 :: (Double, Double)
  , p2 :: (Double, Double) 
  , p3 :: (Double, Double)
  , p4 :: (Double, Double) 
  }

data Pair = A | B | C | D

type Event = GlutState -> GameState -> IO GameState
--type Event' = GlutState -> GameState -> IO GameState

--type GlutMonad a = StateT (IORef (GlutState, GameState)) IO a
--type GlutMonad = StateT (IORef GlutState) IO
--type GameMonad = StateT (IORef GameState) GlutMonad

--changeKey :: Key -> Bool -> GlutMonad ()
changeKey ref k b = do
  modifyIORef ref (\gls -> gls {keystate = M.insert k b (keystate gls)})
changeSize ref size = do
  modifyIORef ref (\gls -> gls {winsize = size})
changeMouse ref pos = do
  modifyIORef ref (\gls -> gls {mousepos = pos})


main = do 
  -- init monad
  glutref <- newIORef $ GlutState (Size 0 0) (0, 0) M.empty
  gameref <- newIORef $ GameState A (45, 66) (80, 90) (10, 10) (10,10) (10,10) (10,10)

  addTones

  -- video
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "min/dsk"
  displayCallback $= display glutref gameref
  reshapeCallback $= Just (reshape glutref gameref)
  keyboardMouseCallback $= Just (keymouse glutref gameref)
  motionCallback $= Just (mousemove glutref gameref)
  mainLoop

display gl gr = do
  clear [ColorBuffer]
  loadIdentity
  swapBuffers

reshape gl gr s@(Size w h) = do
  changeSize gl s
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

keymouse gl gr key state modifiers (Position x' y') = do
  gl' <- readIORef gl
  gr' <- readIORef gr
  let (Size w h) = winsize gl'
  let (x,y) = normalize' x' y' w h
  changeMouse gl (x, y)
  changeKey gl key state
  runEvents gl' gr' >>= writeIORef (gr :: IORef GameState)
  

mousemove gl gr (Position x' y') = do
  gl' <- readIORef gl
  gr' <- readIORef gr
  let (Size w h) = winsize gl'
  let (x,y) = normalize' x' y' w h
  changeMouse gl (x, y)
  (runEvents gl' gr') >>= writeIORef gr
  redraw

events :: [Event]
events = [eventKey, eventMouseClick]
-- eventAB glstate grstate = 
--   let keys = keystate glstate in 
--   case M.lookup (Char 'a') keys of
--     Just Down -> return $ grstate {which = A}
--     _ -> case M.lookup (Char 'b') keys of
--            Just Down -> return $ grstate {which = B}
--            _ -> return $ grstate
eventKey glstate grstate = 
  let keys = keystate glstate in 
  case M.lookup (Char 'a') keys of
    Just Down -> return $ grstate {which = A}
    _ -> case M.lookup (Char 's') keys of
           Just Down -> return $ grstate {which = B}
           _ -> case M.lookup (Char 'd') keys of
                  Just Down -> return $ grstate {which = C}
                  _ -> case M.lookup (Char 'f') keys of
                         Just Down -> return $ grstate {which = D}
                         _ -> return $ grstate

eventMouseClick glstate grstate = 
  case M.lookup (MouseButton LeftButton) (keystate glstate) of
    Just Down ->
      let (x,y) = mousepos glstate in
      case which grstate of
        A -> 
          let f1 = ascale x
              f2 = ascale y in
             updateAmps $ grstate {p1 = (f1,f2)}
        B -> 
          let f3 = ascale x
              f4 = ascale y in
             updateAmps $ grstate {p2 = (f3,f4)}
        C -> 
          let f5 = ascale x
              f6 = ascale y in
             updateAmps $ grstate {p3 = (f5,f6)}
        D -> 
          let f7 = ascale x
              f8 = ascale y in
             updateAmps $ grstate {p4 = (f7,f8)}
    _ -> return $ grstate

gscale :: Float -> Double
gscale u = float2Double $ (u+1) * 50 + 22

ascale :: Float -> Double
ascale u = float2Double $ (u+1) * 30

updateAmps grstate = 
  let 
    (f1,f2) = p1 grstate
    (f3,f4) = p2 grstate
    (f5,f6) = p3 grstate
    (f7,f8) = p4 grstate
  in do
    setAmps (normalizedubs [f1,f2,f3,f4,f5,f6,f7,f8])
    return grstate

runEvents :: GlutState -> GameState -> IO GameState
runEvents glstate grstate = 
  foldM (flip id) grstate (map ($ glstate) events)
