import Audio
import Synths
import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl as O

initf = 
  let files = ["/home/scott/noise/duet.wav"
              , "/home/scott/noise/stabat.wav"
              , "/home/scott/noise/wisdomclipp1.wav"
              , "/home/scott/noise/wisdomclipp2.wav"
              , "/home/scott/noise/wisdomclipp3.wav"
              ]
      fn0 = "/home/scott/noise/duet.wav"
      fn1 = "/home/scott/noise/stabat.wav"
  in withSC3 (\fd -> mapM (\(i,fn) ->
                             async fd (b_allocRead i fn 0 0))
                         $ zip [0..] files
             )

f n = n * 44100

  
--mono $ ph 500 0.2 (mouseY KR 0 3 Linear 0.1) + ph 500 0.2 0
--mono $ phasor AR 0 (400 / sampleRate) 0 1 0
--mono $ saw AR 400 
--mono $( lfSaw AR 400 0 * 0 + phasor AR 0 400 0 1 (mouseY KR 0 3 Linear 0.1) ) * 0.2

main = do
  resetSC3OSC
  installSynthOSC loopSynth
  let snum = 22
  mapM (\i -> addSynthOSC "play" i) [0..snum]
  let b = 3
      sc = bufRateScale KR b
      fr = bufFrames KR b
      dur = bufDur KR b
      s1 = fr/dur
      pos rate = (lfSaw AR (rate * 0.5/dur) 0 * fr) -- + f (mouseY KR (-5) 5 Linear 0.2)
      my a b = mouseY KR a b Linear 0.1
      mx a b = mouseX KR a b Linear 0.1
      lol = 0.05
      frames = [1,1.001..1.01]
      speedup = [line KR lol 1 (bufDur KR b / (0.1 * lol)) DoNothing] -- [1,1.001..1.01]

--  setParamOSC snum "b" b
--  setParamOSC snum "amp" 1
  let n = 48
  let pat = 
        pinstr_s (return "play") $ 
           pbind $ [ ("b",toP [3])
                   , ("amp",toP . take n $ itrconcat (* 1.05) [3,0.5,0.2,0.1])
--                   , ("amp",toP . take n $ itrconcat (* 1.05) [0.7,0.5,0.4,0.3])
                   , ("dur", toP . take n $ [1])
                   , ("sustain", toP [1]) -- lol
                   , ("bus", toP . take n $ [0,1,0,1,1,0,1,0])
                   , ("rate", toP [0.01,0.05,0.1,0.2]) -- ,0.8,1.0])
--                   , ("rate", toP . take n $ itrconcat (* 1.5) [1..4]) -- ,0.8,1.0])
                   ]
  
  audition pat
--  print . (!! 4) $ renderEvents pat
--  writeNRT "/home/scott/noise/quiet1.osc" (Bundle (O.UTCr 0) [g_new [(1, AddToTail, 0)]] : Bundle (O.UTCr 0) [d_recv loopSynth] : renderEvents pat)
  print 22

--      frames = [line KR lol lol (bufDur KR b / (0.1 * lol)) DoNothing] -- [1,1.001..1.01]
--  mono $ mix $ 2 * loopPlay b (mce speedup) 0 / (constant $ length frames)
---  mono $ mix $ loopPlay b (my 0.1 1) (mce [0,0.05..1.0])


--  mono $ mix $ loopPlay b 1 0
--  mono $ mix $ loopPlay 3 1 (my 0 1)
--  mono $ mix $ loopPlayS 0 1 0 -- + loopPlay 3 1 (my 0 2)
--  audition $ out 0 $ mix $ bufRdC 1 AR b (pos $ -1) Loop

-- basic:
--  audition (out 0 $ mix $ (playBuf 2 AR b (-sc * 0.5) 1 (fr - s1 * 0.1 * (0/0.22 + mce [1..22])) NoLoop DoNothing))

--  audition (out 0 $ mix $ (bufRdC 1 AR b (pos + f 1 * (mce [0..2])) Loop))
--  audition (out 0 $ mix $ (s (s 3.1 0.5 + 1.5) 0.1 + 0.9) * (playBuf 1 AR b (-sc * (s 0.1 0.8 + 0.3)) 1 (fr - s1 * (0 + mce [1..11])) NoLoop DoNothing))





itrconcat f xs = concat $ iterate (map f) xs