import Audio
import Synths
import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID

initf = 
  let fn0 = "/home/scott/noise/duet.wav"
      fn1 = "/home/scott/noise/stabat.wav"
      fn2 = "/home/scott/noise/wisdom.wav"
  in withSC3 (\fd -> do async fd (b_allocRead 0 fn0 0 0)
                        async fd (b_allocRead 1 fn1 0 0)
                        async fd (b_allocRead 2 fn2 0 0)
             )
f n = n * 48000

main = do
  resetSC3OSC
  let b = 0
      sc = bufRateScale KR b
      fr = bufFrames AR b
      dur = bufDur AR b
      s1 = fr/dur
  audition (out 0 $ mix $ (playBuf 2 AR b (-sc * 0.5) 1 (fr - s1 * 0.1 * (0/0.22 + mce [1..22])) NoLoop DoNothing))
