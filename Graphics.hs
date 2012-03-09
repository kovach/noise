module Graphics where

import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get)

import Util

data RPoint = RPoint
  { sx :: Float
  , sy :: Float
  }
 deriving (Show)

pointWidth = 0.1

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

normalize' x' y' w' h' = 
  let x = fromIntegral x'
      y = fromIntegral y'
      w = fromIntegral w'
      h = fromIntegral h' in
  (2*x/w-1, -(2*y/h-1))

      
redraw = postRedisplay Nothing

