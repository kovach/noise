module Synths where

import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID

import Audio


sineSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       e = envGen KR g a 0 1 RemoveSynth (envPerc 0.01 dur)
   in synthdef "sine" $ out 0 $ (s f 1) * e

sine2Synth =
   let f1 = control KR "freq1" 100
       f2 = control KR "freq2" 200
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       e = envGen KR g a 0 1 RemoveSynth (envPerc 0.01 dur)
   in synthdef "sine2" $ out 0 $ (s f1 0.5 + s f2 0.5) * e