module Synths where

import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID

import Audio


-- UGENS
mono = audition . out 0 
s freq amp = (sinOsc AR freq 0) * amp
sw freq amp = (saw AR freq) * amp

ph freq amp phase = phasor AR 0 (freq / sampleRate) 0 1 phase * amp

growl a b c d = 
  let freqs = [a,b,c,d]
      amps  = [0.25, 0.25, 0.25, 0.25] 
  in
    mix $ s (mce freqs) (mce amps)

harm f a = 
  let amps  = [5, 1, 0.6, 0, 0.5, 0.4, 0.2, 0.1,0,0.1,0,0.05,0,0.05]
  in
    mix $ s (mce (take (length amps) (map (* f) [1..]) )) (mce (map (/ sum amps) amps)) * a

howl f a =
  rlpf (whiteNoise 'a' AR) f 0.1 * a

-- bufnum, rate, offset(offset range 0 to 1)
loopPlay b rate phase  =
  let dur = bufDur KR b
      fr = bufFrames KR b
      pos = (lfSaw AR (rate * 0.5/dur) 0 + phase) * fr
  in
  bufRdC 1 AR b pos Loop


-- synths
sineSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "sine" $ out 0 $ (s f 1) * e
harmSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "sine" $ out 0 $ harm f e

percSynth =
   let f = control KR "freq" 100
       a = control KR "amp" 1
       g = control KR "gate" 1
       dur = control KR "dur" 1
       att = control KR "attack" 0.01
       e = envGen KR g a 0 1 RemoveSynth (envPerc att dur)
   in synthdef "perc" $ out 0 $ (pinkNoise 'a' AR) * e

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


growlSynth = 
  let 
    f1 = control KR "f1" 45
    f2 = control KR "f2" 66
    f3 = control KR "f3" 80
    f4 = control KR "f4" 90
    a = control KR "amp" 0.5
  in synthdef "growl" $ out 0 $ growl f1 f2 f3 f4 * a


loopSynth = 
--loopPlay b rate phase  =
  let 
    buf = control KR "b" 0
    rate = control KR "rate" 1
    phase = control KR "phase" 0
    a = control KR "amp" 0
    bus = control KR "bus" 0
    att = control KR "attack" 0.01
    g = control KR "gate" 1
    dur = control KR "dur" 1
    sus = control KR "sustain" 1
    e = envGen KR g a 0 1 DoNothing (envPerc att dur)
  in
    synthdef "play" $ out bus $ loopPlay buf rate phase * line AR 0 a 0.1 DoNothing
    
grainSynth = 
  let w = control KR "width" 1
      r = control KR "rate" 5
      p = control KR "pos" 0
      bus = control KR "bus" 0
      buf = control KR "b" 0
      a = control KR "amp" 1
      dur = control KR "dur" 1
  in
    synthdef "grain" $ out bus $ tGrains 2 (impulse AR r 0) buf 1 
                                   (p * bufDur KR buf) w 0 a 2
