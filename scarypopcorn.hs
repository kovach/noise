import Sound.SC3
import Sound.SC3.UGen.Dot
import Sound.SC3.ID

import Sound.SC3.Lang.Pattern.ID

-- examples
--mono $ s 697 0.4 + s 1209 0.4
--mono $ s 440 0.5
--mono whirr
main = do
 withSC3 (\fd -> send fd (g_new [(1, AddToTail, 0)]))
 let pad = definePad

 timerstart pad
 mono (whirr * 0.7 + popcorn)

--ready (defineSine)

mono = audition . out 0 
s freq amp = (sinOsc AR freq 0) * amp

whirr = 
  let input = (crackle AR 1.4 * 0.05) 
              + 
              s (mce [200,400,800]) ((mce [0.5,0.3,0.2]) * (s 1 0.02 + 0.2))
  in 
--    input
    ringz input 600 0.1

--mono $ whirr


padPattern coords = 
  let (rs, cs) = unzip coords in
  (("rowf", toP $ map (rlowfreqs !!) rs), ("colf", toP $ map (chighfreqs !!) cs))
 where
  rlowfreqs  = [697, 770, 852, 941]
  chighfreqs = [1209, 1336, 1477, 1633]


definePad = 
  let rowf = control KR "rowf" 222
      colf = control KR "colf" 444
      _ = control KR "freq" 444
      g = control KR "gate" 1
      a = control KR "amp" 0.3
      d = envASR 0.01 1 1 (EnvNum (-4))
      e = envGen KR g a 0 1 RemoveSynth d
      o = out 0 $ (s rowf 0.5 + s colf 0.5) * e
  in synthdef "pad" o

defineSine =
   let {f = control KR "freq" 440
          ;g = control KR "gate" 1
          ;a = control KR "amp" 1
          ;d = envASR 0.01 1 1 (EnvNum (-4))
          ;e = envGen KR g a 0 1 RemoveSynth d
          ;o = out 0 (sinOsc AR f 0 * e)}
      in synthdef "sine" o

timerstart pad = do
 let (rs, cs) = padPattern [(1,3),(1,2),(2,0),(3,3)]
 audition (pad, pbind $ ("dur", 0.4) : [rs,cs])

ready synth = 
  audition (synth, pbind $ [("dur", toP [0.1,1.0]), ("freq", toP [700,900]), ("amp", toP [0.3, 0.7])])


popcorn = 
 let n = pinkNoise 'a' AR
     cracks = crackle AR 1.95
     dt = 22
       -- envelope for random hit stuff
     plat1 = envCoord [(0,0),(0.5,2),(1.5,22*2),(2.5,2)] dt 0.5 EnvLin
     plat2 = envCoord [(0,0),(0.5,1),(1.5,40),(2.5,50)] dt 0.5 EnvLin

     slope' = impulse AR (platEnvelope plat2) 0.25
     platEnvelope plat = envGen KR 1 1 0 1 RemoveSynth $ plat
     popdust = decay (dust 'a' AR (platEnvelope plat1))   0.5
     ramp    = decay (impulse  AR (platEnvelope plat2) 0) 0.5
     
   in cracks * ramp * popdust

--mono $ crackle AR 1.95

--     ; slope = impulse AR (xLine KR 1 50 (2*dt) RemoveSynth) 0.25
