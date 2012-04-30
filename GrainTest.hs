import Audio
import Synths
import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID

initf = 
  let files = 
        [ "/home/scott/noise/intervallo1.wav"
        , "/home/scott/noise/wisdomclipp2mono.wav"
        , "/home/scott/noise/striated-1-mo.wav"
        , "/home/scott/noise/gran-laser-bells-mo.wav"
        , "/home/scott/noise/blip-mo.wav"
        ]
  in withSC3 (\fd -> mapM (\(i,fn) ->
                             async fd (b_allocRead i fn 0 0))
                         $ zip [0..] files
             )
f n = n * 44100
rate = 5
pat b = 
  pinstr_s (return "grain") $ 
           pbind $ [ ("b",toP [b])
                   , ("amp", toP [1])
                   , ("width", toP [5.5/rate])
                   , ("rate", toP [rate])
                   , ("pos", toP [0.4])--) ,0.1..1])
                   , ("amp", toP [0.4])
                   , ("dur", toP [1])
                   ]

addSynths b = do
  print $ fun i1
  print $ fun i2
  mapM_ go [i1..i2]
  where
    adjust k = k - i1 + 22
    i1 = 0
    i2 = 10
    w = 0.5
    fun k = exp (fromIntegral k * 0.00002)
    go k = do
      addSynthOSC "grain" (adjust k)
      setParamsOSC (adjust k) (params k)

    params k = 
     [ ("b", b)
     , ("amp", 0.22)
     , ("width", w/rate)
     , ("rate", fun k)
     , ("phase", 0)
     , ("bus", fromIntegral $ mod k 2)
--     , ("attack", 0.01)
     , ("gate", 1)
--     , ("dur", 1)
--     , ("sustain", 1)
     ]

main = do
  resetSC3OSC
  installSynthOSC loopSynth
  installSynthOSC grainSynth
  let snum = 22
--  addSynthOSC "play" snum
  let buf = 4
      sc = bufRateScale KR buf
      fr = bufFrames KR buf
      dur = bufDur KR buf
      s1 = fr/dur
--      pos rate = (lfSaw AR (rate * 0.5/dur) 0 * fr) -- + f (mouseY KR (-5) 5 Linear 0.2)
      my a b = mouseY KR a b Linear 0.1
      mx a b = mouseX KR a b Linear 0.1
      mx' a b = mouseX' KR a b Linear 0.1

--  setParamOSC snum "b" b
--  setParamOSC snum "amp" 1

  let w = mx 0 6
      rmax = 30
      rate = my rmax 0
      rate' = mx 0 22
      pos = 0.11 -- ((mx 0 1 + 0 * mce [0.1,0.2..0.5]) * bufDur KR b)
--  audition $ pat 2
--  mono $ loopPlay b 1 0
-- delayN LinLin rlpf

  let z = (decay (dust 'a' AR 1) 0.3 * whiteNoise 'b' AR)
      dl r x = x + delayN x 0.2 r
      r = my 0 30
      d1 = dust 'a' AR r
      d2 = impulse AR r 0
  
      tonic = 261.62
      rn = mce [0.5,1]
      rn' = mce [0.5,2]

      c = 1
      cs = 1.0594
      f = 1.3348
      fs = 1.4142
      g = 1.4983
      bf = 1.7817
      b = 1.887
      notes = [g/2, bf/2, cs, fs, bf, b]
      notes' = [g/2, bf/2, c, f, bf, b]
      
--  mono $ mix $ rlpf (whiteNoise 'a' AR) (mce notes * tonic) 0.1 * 0.3
--  mono $ mix $ s (mce notes' * tonic) 0.05
--  mono $ s (200 * mce [1.1224,1.414,1.4983]) (mce [0.2,0.1,0.1])
--  mono $ mix $ loopPlay b (mx 0 10) (0.1 * mce [0,0.1,0.2,0.3]) * (flip decay 0.5 $ d2) * 0.1

--  mono $ rlpf (loopPlay buf (mx 0 1) 0 * (flip decay 0.5 $ d1) * 0.5) (tonic * 1.5) 0.5
--  mono $ rlpf (loopPlay buf (mx 40 0) 0 * (flip decay 0.5 $ d1) * 0.5) tonic 0.3
  mono $ (loopPlay buf (mx 4 40) 0 * (flip decay 0.5 $ d1) * 0.5) 
  mono $ (loopPlay buf (mx 4 40) 0 * (flip decay 0.5 $ d1) * 0.5) 
  mono $ (loopPlay buf (mx 40 0) 0 * (flip decay 0.5 $ d1) * 0.5) 
  mono $ (loopPlay buf (mx 40 0) 0 * (flip decay 0.5 $ d1) * 0.5) 
  mono $ (loopPlay buf (mx 40 0) 0 * (flip decay 0.5 $ d1) * 0.5) 

  -- mono $ loopPlay b (mx 0 80) 0 * (flip decay 0.5 $ d1) * 0.2
      
--  mono $ mix $ (tGrains 2 (impulse AR rate 0) b (mce [(rmax-rate)/i | i <- [20..21]]) pos  (w / rate) 0 0.5 2)
  print 22

itrconcat f xs = concat $ iterate (map f) xs


