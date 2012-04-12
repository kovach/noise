module Util where
import System.Random
import Control.Applicative

import GHC.Float

roll :: Int -> Int -> IO Int
roll lo hi = getStdRandom (randomR (lo,hi))
rollf :: Float -> Float -> IO Float
rollf lo hi = getStdRandom (randomR (lo,hi))
roll' = fmap fromIntegral . uncurry roll

rollSeq lo hi n = sequence (replicate n (roll lo hi))

pairHas x (a,b) = a == x || b == x

pairMap f (a,b) = (f a, f b)

randDist :: [Float] -> IO Int
randDist = undefined

map1 f [] = []
map1 f ((a,b):xs) = (f a, b) : map1 f xs
map2 f [] = []
map2 f ((a,b):xs) = (a, f b) : map2 f xs

roundFloat = float2Double . fromIntegral . round

rot 0 list = list
rot n (x : []) = [x]
rot n' (x:xs) = 
  let n = mod n' (length xs + 1) in
  rot (n-1) (xs ++ [x])

takeRand n list | n <= 0 = return []
takeRand n list = do
  i <- roll 0 (length list - 1)
  rest <- takeRand (n-1) list
  return $ (list !! i) : rest


randMeanS m s = do
  b <- roll 0 1
  return $ if b == 0 then m + s else m - s



randNorm m s = do
  u <- rollf 0 1
  v <- rollf 0 1
  return $
         sqrt (-2 * log u) * cos (2 * pi * v)
