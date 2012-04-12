module Main where

import Patterns
import Audio
import Synths
import Util

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lang.Pattern.ID
import Sound.SC3.Lang.Control.Instrument

import Data.Random.Distribution
import Control.Applicative

import GHC.Float


main = do
  resetSC3OSC
  --installSynthOSC sine3Synth
  installSynthOSC sawSynth
  installSynthOSC sineSynth
  installSynthOSC percSynth
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
  
  let base = 0.6
  delays <- rollSeq 0 9 100
  let gradient = concatMap (replicate 5) [1000,100,100,80,80,100,100,60,80,40,100]
  let durs = map ((+ base) . (uncurry (-))) $ pairify $ 
             zipWith (/) (map ((* base) .fromIntegral) delays) gradient
  let pat = (raw2Pat $ zip durs (repeat 440))
  audition $ ppar [pinstr_s (toP ["saw"]) pat]
--  audition $ ppar [pinstr sawi (raw2Pat p1), pinstr sawi (raw2Pat p2), pinstr sawi (raw2Pat p3)]
--  mono $ (sw (constant $ fbase * note1) 0.3 +sw (constant $ fbase * note2) 0.3 +sw (constant $ fbase * note3) 0.3) * 0.1



pairify [] = []
pairify (x : xs) = (0,x) : pairify' (x : xs)
pairify' [] = []
pairify' (x : []) = [(x,0)]
pairify' (x : y : xs) = (x,y) : pairify' (y : xs)

