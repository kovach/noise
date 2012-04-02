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

data Direction = Up | Down

nudge f 0 = f
nudge f n | n > 0 = nudge (f * 1.01) (n-1)
nudge f n | n < 0 = nudge (f * 0.99) (n+1)

main = do
  resetSC3OSC
  --installSynthOSC sine3Synth
  installSynthOSC sawSynth
  installSynthOSC sineSynth
  installSynthOSC harmSynth
  let base = 130
      third = base * 5/4
      fifth = base * 3/2
      lapse = 20

--  mono $    (s 440 0.2) +   (s 550 0.2) +   (s 660 0.2)

  let 
    cresc1 = line KR 0.03 0.3 lapse DoNothing
    cresc2 = cresc1 * 0.7
    dcresc1 = line KR 0.3 0.03 lapse DoNothing
    dcresc2 = dcresc1 * 0.7
    dcresc3 = line KR 1 0.03 lapse DoNothing
    cresc3 = line KR 0.03 1 lapse DoNothing
    baseScale = map (* base) pythTuning
    tonic = baseScale !! 0
    minor3 = baseScale !! 3
    major3 = baseScale !! 4
    major5 = baseScale !! 7

    scale =  addDetail . addOctave $ addDetail $ baseScale
--    inst = (flip s 0.5)
    inst = harm
    numnotes = 4


  notes <- takeRand numnotes $ scale
  let notes' = rot 1 $ notes

  print notes
--  mono $ (harm (constant base) + harm (constant (3 * major3)) + harm (constant major5)) * 0.3

  mono $
{-    (inst (line KR (nudge tonic 3) tonic lapse DoNothing) * dcresc1) +
    (inst (line KR (nudge third 3) third lapse DoNothing) * dcresc2) +
    (inst (line KR (nudge fifth (-3)) fifth lapse DoNothing) * dcresc2)
-}

          mix $ mce $ map (\(p1,p2) ->
                 (inst (xLine KR p1 p2 lapse DoNothing) * (0.5/numnotes)))
           (zip (map constant notes') (map constant notes))

            
{-    (inst (line KR fifth base lapse DoNothing) dcresc1) +
    (inst (line KR base third lapse DoNothing) dcresc2) +
    (inst (line KR third fifth lapse DoNothing) dcresc2)
-}
  

