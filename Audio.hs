module Audio where
import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID


addSynthOSC name n =
 withSC3 $ \fd -> do
   send fd $ s_new name n AddToTail 1 []
   send fd $ n_set n []

setParamOSC n name val =
  withSC3 $ \fd -> 
    send fd $ n_set n [(name, val)]

installSynthOSC def = 
  withSC3 $ \fd -> async fd $ d_recv def

setFOSC n f = 
  withSC3 $ \fd -> 
    send fd $ n_set n [("freq", f)]
setAOSC n a = 
  withSC3 $ \fd -> 
    send fd $ n_set n [("amp", a)]
freenOSC n = withSC3 $ \fd -> send fd $ n_free [n]
resetmOSC n = withSC3 $ \fd -> reset fd >> send fd (g_deepFree [1])

resetSC3OSC = withSC3 (\fd -> reset fd)


