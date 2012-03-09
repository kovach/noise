import Patterns
import Audio
import Synths

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lang.Pattern.ID

import Data.Random.Distribution


main = do
  resetSC3OSC
  mel <- generateN 22 simpleGr
  audition (sineSynth, toSCPattern mel 0.5)

