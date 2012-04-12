{-# LANGUAGE NamedFieldPuns #-}

import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get)

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
--import Growl
import Synths

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
  }

data Pair = A | B

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
  -- audio 
  resetSC3OSC
  installSynthOSC growlSynth
  addSynthOSC "growl" 22

  -- init monad
  glutref <- newIORef $ GlutState (Size 0 0) (0, 0) M.empty
  gameref <- newIORef $ GameState A (45, 66) (80, 90)

  -- video
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "min"
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
events = [eventAB, eventMouseClick, eventPrint]

eventAB glstate grstate = 
  let keys = keystate glstate in 
  case M.lookup (Char 'a') keys of
    Just Down -> return $ grstate {which = A}
    _ -> case M.lookup (Char 'f') keys of
           Just Down -> return $ grstate {which = B}
           _ -> return $ grstate

eventPrint glstate grstate = 
  let keys = keystate glstate in 
  if keyDown 'p' keys then do
    putStrLn $ show (pairMap round (lower grstate)) ++ 
               show (pairMap round (upper grstate))
    return grstate
  else
    return grstate
    

keyDown char keys = 
  case M.lookup (Char char) keys of
    Just Down -> True
    _ -> False
    

eventMouseClick glstate grstate = 
  case M.lookup (MouseButton LeftButton) (keystate glstate) of
    Just Down ->
      let (x,y) = mousepos glstate in
      case which grstate of
        A -> 
          let f1 = gscale x
              f2 = gscale y in
          do setParamOSC 22 "f1" f1
             setParamOSC 22 "f2" f2
             return $ grstate {lower = (f1,f2)}
        B -> 
          let f3 = gscale x
              f4 = gscale y in
          do setParamOSC 22 "f3" f3
             setParamOSC 22 "f4" f4
             return $ grstate {upper = (f3,f4)}
    _ -> return grstate

gscale :: Float -> Double
gscale u = float2Double $ (u+1) * 75

runEvents :: GlutState -> GameState -> IO GameState
runEvents glstate grstate = 
  foldM (flip id) grstate (map ($ glstate) events)