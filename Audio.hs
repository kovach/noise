module Audio where
import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl as O
import Sound.SC3.Lang.Control.Event -- for to_osc
import Debug.Trace

addSynthOSC name n =
 withSC3 $ \fd -> do
   send fd $ s_new name n AddToTail 1 []
   send fd $ n_set n []

setParamOSC n name val =
  withSC3 $ \fd -> 
    send fd $ n_set n [(name, val)]
setParamsOSC n pairs = 
  withSC3 $ \fd -> 
    send fd $ n_set n pairs

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


renderEvents t0 events = 
  case foldr process (0,O.UTCr t0 ,[]) (unP events) of
    (_,_,ms) -> ms
   where
     process event s@(i,time,es) = 
       case to_sc3_osc (as_utcr time) i event of
         Just (start, end@(O.Bundle t' _)) -> 
           (i+1,t',es ++ [start,end])
         _ -> s
  

bundles_to_ntp bs = 
  map (\(Bundle t osc) -> Bundle (pi_pr t) osc) bs

pi_pr (NTPi ntpi) = UTCr (ntpi_utcr ntpi)
pi_pr s = s