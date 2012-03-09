module Patterns where
import Sound.SC3.Lang.Pattern.ID

import Control.Monad

import Util
import Audio

type Dur = Double
type Freq = Double
type Melody = [(Dur, Freq)]

scaleDiv53 = map ((2.0 **) . (/ 53.0)) [1.0..53]

-- "adds a note" maybe randomly
type Expander = Melody -> IO Melody

simpleGr :: Expander
simpleGr m = do
  a <- roll 0 2
  b <- roll 0 (length scaleDiv53 - 1)
  return $ m ++ [(2 ^ a, 100 * scaleDiv53 !! b)]

generateN :: Int -> Expander -> IO Melody
generateN n exp = 
  foldM (flip id) [] (replicate n simpleGr)

toSCPattern mel quarter_t =
  let (durs, freqs) = unzip mel
      times = map (* (quarter_t / 2)) durs
  in
    pbind $ [("dur", toP times), ("freq", toP  freqs)]