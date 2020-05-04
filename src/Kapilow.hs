{-# LANGUAGE RankNTypes #-}
module Kapilow where


import Control.Arrow
import Data.Monoid
import Euterpea.Music
import Euterpea.IO.MIDI.Play

import Util



go :: Music Pitch
go = chord [ (
