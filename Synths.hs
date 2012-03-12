module Synths where

import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID

import Audio

mono = audition . out 0 
s freq amp = (sinOsc AR freq 0) * amp
sw freq amp = (saw AR freq) * amp

sineSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "sine" $ out 0 $ (s f 1) * e

sawSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "saw" $ out 0 $ (sw f 1) * e

toneSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 0.1
   in synthdef "tone" $ out 0 $ s f a

sine2Synth =
   let f1 = control KR "freq1" 100
       f2 = control KR "freq2" 200
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "sine2" $ out 0 $ (s f1 0.5 + s f2 0.5) * e

sine3Synth =
   let f1 = control KR "freq1" 100
       f2 = control KR "freq2" 250
       f3 = control KR "freq3" 300
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "sine3" $ out 0 $ (s f1 0.33 + s f2 0.33 + s f3 0.33) * e

by3Synth sound = 
   let f1 = control KR "freq1" 100
       f2 = control KR "freq2" 250
       f3 = control KR "freq3" 300
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "by3" $ out 0 $ (sound f1 + sound f2 + sound f3) * 0.33 * e


growl a b c d = 
  let freqs = [a,b,c,d]
      amps  = [0.25, 0.25, 0.25, 0.25] 
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

howl f =
  rlpf (whiteNoise 'a' AR) f 0.1

--bLowShelf