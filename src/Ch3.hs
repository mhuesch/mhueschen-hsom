module Ch3 where


import Euterpea.Music
import Euterpea.IO.MIDI.Play

import Util

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p is = line (map (\ap -> note qn (trans ap p)) aps)
  where
    aps = scanl (+) 0 is

data MyMode = Ionian
            | Dorian
            | Phrygian
            | Lydian
            | Mixolydian
            | Aeolian
            | Locrian
  deriving (Show, Enum)

genScale :: MyMode -> Pitch -> Music Pitch
genScale mode p = mkScale p (modeIntervals mode)

ionianIntervals = [2,2,1,2,2,2,1]

modeIntervals :: MyMode -> [Int]
modeIntervals mode = rotate (fromEnum mode) ionianIntervals
