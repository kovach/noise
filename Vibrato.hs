module Main where

import Patterns
import Audio
import Synths

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lang.Pattern.ID
import Sound.SC3.Lang.Control.Instrument

--import Data.Random.Distribution
import Control.Applicative

import GHC.Float


main = do
  resetSC3OSC
  --installSynthOSC sine3Synth
  installSynthOSC sawSynth
  installSynthOSC sineSynth
  -- installSynthOSC (by3Synth howl)
  let base = 200 :: Int
  let base' = constant base
  let fbase = fromIntegral base
  let [note1, note2, note3] = [1, 5/4, 3/2] :: [Double]
  line1 <- mkSquiggleNote 0.9 16 30.0 (round $ fbase * note1)
  line2 <- mkSquiggleNote 0.9 16 30   (round $ fbase * note2)
  line3 <- mkSquiggleNote 0.9 16 30   (round $ fbase * note3)
  p1 <- squigglify 0.999 50 line1
  p2 <- squigglify 0.999 50 line2
  p3 <- squigglify 0.999 50 line3
  let sawi = toP [InstrumentName "saw"]
  let sini = toP [InstrumentName "sine"]
  audition $ ppar [pinstr sawi (raw2Pat p1), pinstr sawi (raw2Pat p2), pinstr sawi (raw2Pat p3)]

  mono $ (sw (constant $ fbase * note1) 0.3 +sw (constant $ fbase * note2) 0.3 +sw (constant $ fbase * note3) 0.3) * 0.1





