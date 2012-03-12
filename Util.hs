module Util where
import System.Random

import GHC.Float

roll :: Int -> Int -> IO Int
roll lo hi = getStdRandom (randomR (lo,hi))

roll' = fmap fromIntegral . uncurry roll

pairHas x (a,b) = a == x || b == x

pairMap f (a,b) = (f a, f b)

randDist :: [Float] -> IO Int
randDist = undefined

map1 f [] = []
map1 f ((a,b):xs) = (f a, b) : map1 f xs
map2 f [] = []
map2 f ((a,b):xs) = (a, f b) : map2 f xs

roundFloat = float2Double . fromIntegral . round