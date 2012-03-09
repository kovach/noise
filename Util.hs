module Util where
import System.Random

import GHC.Float

roll :: Int -> Int -> IO Int
roll lo hi = getStdRandom (randomR (lo,hi))

roll' = fmap fromIntegral . uncurry roll
pairHas x (a,b) = a == x || b == x

pairMap f (a,b) = (f a, f b)
