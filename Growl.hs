module Growl where
import Sound.SC3
import Sound.SC3.UGen.Dot
import Sound.SC3.ID

import Sound.SC3.Lang.Pattern.ID

import Audio

sineSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 1
       dur = control KR "dur" 1
---       d = envASR 0.05 dur dur (EnvNum (-4))
--       e = envGen KR 1 a 0 1 RemoveSynth d
   in synthdef "sine" $ out 0 $ (s f a)

growl a b c d = 
  let freqs = [a,b,c,d]
      amps  = [0.4, 0.2, 0.2, 0.2] 
  in
    mix $ s (mce freqs) (mce amps)

growlSynth = 
  let 
    f1 = control KR "f1" 45
    f2 = control KR "f2" 66
    f3 = control KR "f3" 80
    f4 = control KR "f4" 90
    a = control KR "amp" 0.5
  in synthdef "growl" $ out 0 $ growl f1 f2 f3 f4 * a

newGrowl n = 
 withSC3 $ \fd -> do
   send fd $ s_new "growl" n AddToTail 1 []
   send fd $ n_set n []


{-
interesting
45 66 80 90
74 50 199 199
-}