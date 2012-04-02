module Patterns where
import Sound.SC3.Lang.Pattern.ID

import Control.Monad
import Control.Applicative
import GHC.Float
import Data.Ratio

import Util
import Audio

type Dur = Double
type Freq = Double
type Melody = [(Dur, Rational)]

-- "adds a note" maybe randomly
type Expander = Melody -> IO Melody

generateN :: Int -> Expander -> IO Melody
generateN n exp = 
  foldM (flip id) [] (replicate n exp)

melody2Pat mel quarter_t =
  let (durs, ratios) = unzip mel
      times = map (* (quarter_t / 2)) durs
      freqs = map fromRational ratios
  in
    pbind $ [("dur", toP times), ("freq", toP freqs)]

raw2Param param freqs = [(param, toP freqs)]

raw2Pat thing = 
  let (durs, freqs) = unzip thing in
  pbind $ [("dur", toP durs)] ++ raw2Param "freq" freqs -- ("freq", toP (map fromRational freqs))]



-- tunings 
--ratio2Double (Ratio a b) = float2Double $ (fromIntegral a) / (fromIntegral b)
type Scale = [Rational] -- and each is between 1 and 2
--pythTuning :: Scale
--temp53 = map ((2.0 **) . (/ 53.0)) [1.0..53]
-- pythTuning = [ 1, 256/243, 9/8, 32/27, 81/64
--              , 4/3, 729/512, 3/2, 128/81
--              , 27/16, 16/9, 243/128, 2]
toRatio = flip approxRational 1e-6
pythTuning = map toRatio
             [ 1, 256/243, 9/8, 32/27, 81/64
             , 4/3, 729/512, 3/2, 128/81
             , 27/16, 16/9, 243/128]


-- instances
simpleGr :: Scale -> Rational -> Expander
simpleGr scale r m = do
  a <- roll 0 2
  b <- roll 0 (length scale - 1)
  return $ m ++ [(2 ^ a, r * scale !! b)]

--todo
--smallSkips


-- total dur, ratio of note lengths, number of notes, target freq
-- note durs in the sequence change geometrically
mkSquiggleNote ratio n duration target = 
  let init_dur = duration / ((1 - ratio ** (n+1))/(1 - ratio)) -- how long initial note should be
      final_dur = init_dur * (ratio ** (n-1)) -- for last note
      addNote s@(_ : (_,freq) : _) n = do
        next <- roll (min freq target) (max freq target)
        return $ (init_dur * (ratio ** n), next) : s
  in
    do
      radius <- roll (div target 2) (div target 5)
      let (a,b) = (target - radius, target + radius)
      res <- 
        if n <= 1 then return [(duration, target)] else -- duration = final_dur
          if n == 2 then return [(init_dur, a),(final_dur, target)] else
            -- else, compute a series and append the final (target) note
            (reverse . ((final_dur, target) :)) <$> foldM addNote [(init_dur * ratio, b),(init_dur, a)] [2..(n-2)]  
      return $ map2 fromIntegral res
      
  
squigglify ratio n pairs = 
  concat <$> mapM (uncurry $ mkSquiggleNote ratio n) pairs

-- some scale operations
addOctave x = x ++ (map (* 2) x)

shrinkOctave x = map (/ 2) x

addDetail x = shrinkOctave $ addOctave x