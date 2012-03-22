module TestPattern where

import Patterns
import Audio
import Synths

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lang.Pattern.ID
import Sound.SC3.Lang.Control.Instrument

import Data.Random.Distribution
import Control.Applicative

import GHC.Float


main = do
  resetSC3OSC
  installSynthOSC sine3Synth
  installSynthOSC sawSynth
  installSynthOSC sineSynth
  installSynthOSC (by3Synth howl)
  mel <- generateN 22 (simpleGr pythTuning 100)
  short <- generateN 5 (simpleGr pythTuning 100)
  let mel' = zip (repeat 0.2) $ map (* 100) pythTuning 
  let base = 200 :: Int
  let base' = constant base
  let fbase = fromIntegral base
  let [note1, note2, note3] = [1, 5/4, 3/2] :: [Double]
  -- (durs, squig1) <- unzip <$> mkSquiggleNote 0.3 0.99 20 base
  -- (_, squig2) <- unzip <$> mkSquiggleNote 0.5 0.99 20 (round $ (fromIntegral base) * 2.5)
  -- (_, squig3) <- unzip <$> mkSquiggleNote 0.5 0.99 20 (base * 3)
  line1 <- mkSquiggleNote 0.9 16 30.0 (round $ fbase * note1)
  line2 <- mkSquiggleNote 0.9 16 30   (round $ fbase * note2)
  line3 <- mkSquiggleNote 0.9 16 30   (round $ fbase * note3)
  p1 <- squigglify 0.999 50 line1
  p2 <- squigglify 0.999 50 line2
  p3 <- squigglify 0.999 50 line3
  let (durs, squig1) = unzip $ p1
  let (_, squig2) = unzip $ p2
  let (_, squig3) = unzip $ p3
  print line1
  print mel
  print short
  let sawi = toP [InstrumentName "saw"]
  let sini = toP [InstrumentName "sine"]
--  audition (sineSynth, ppar [melody2Pat mel 0.25, melody2Pat (reverse mel) 0.25])
{-  audition (sine3Synth, pbind $ [("attack", 0.001)] ++ 
                      raw2Param "dur" durs ++ raw2Param "freq1" squig1 ++ 
                      raw2Param "freq2" squig2 ++ raw2Param "freq3" squig3) -}
  audition $ ppar [pinstr sawi (raw2Pat p1), pinstr sawi (raw2Pat p2), pinstr sawi (raw2Pat p3)]
  mono $ (sw (constant $ fbase * note1) 0.3 +sw (constant $ fbase * note2) 0.3 +sw (constant $ fbase * note3) 0.3) * 0.1
--  mono $ growl base' base' (base' * 2.5) (base' * 3) * 0.3

--withSC3 (\fd -> do {_ <- async fd (b_alloc 10 512 1) ;async fd (b_gen 10 "sine1" [440])})
--mono $ playBuf 10 AR 0 (bufRateScale KR 0) 1 0 NoLoop RemoveSynth
--mono $ growl 300 300 750 900 * 0.3

-- mono $ s 100 0.1
-- mono $ s 200 0.1
-- mono $ s 300 0.1
-- mono $ s 400 0.1
-- mono $ s 500 0.1
-- mono $ s 600 0.1
-- mono $ s 700 0.1
-- mono $ s 800 0.1


normalize xs' = 
  let xs = map fromIntegral xs' in
  map (/ (sum xs)) xs
fall = map fromIntegral [10, 8, 7, 7, 6, 5, 4, 3]
rot 0 list = list
rot n' (x:xs) = 
  let n = mod n' (length xs + 1) in
  rot (n-1) (xs ++ [x])

-- addTones 
-- setAmps (rot 0 $ normalize [10,6,15,10, 2,2,15,5, 5,5,5,4])
-- setAmps 0 [0.1, 0.15, 0.15, 0.2, 0.2, 0.1, 0.15, 0.15] 
-- setParamOSC 22 "amp" 0.1
-- setParamOSC 23 "amp" 0.15
-- setParamOSC 24 "amp" 0.15
-- setParamOSC 25 "amp" 0.20
-- setParamOSC 26 "amp" 0.20
-- setParamOSC 27 "amp" 0.1
-- setParamOSC 28 "amp" 0.15
-- setParamOSC 29 "amp" 0.05


