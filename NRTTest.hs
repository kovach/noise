import Audio
import Synths
import Sound.SC3
import Sound.SC3.ID hiding (ID)
import Sound.SC3.Lang.Pattern.ID
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl as O

main = do
  let
    thing = 
        [ Bundle (O.UTCr 0) [g_new [(1, AddToTail, 0)]] 
        , Bundle (O.UTCr 0) [d_recv sineSynth]
        ] ++
      (renderEvents 1 $ 
       pinstr_s (return "sine") $
                pbind $ [ ("freq", toP [400])
                        , ("dur", toP [2])])
      
  print thing
  writeNRT "test.osc" (bundles_to_ntp thing)