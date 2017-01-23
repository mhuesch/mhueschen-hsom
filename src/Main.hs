module Main where

import Euterpea.Music
import Euterpea.IO.MIDI.Play

import Util

main :: IO ()
main = do
--  putStrLn "Is t251 == (twoFiveOne (D, 4) wn) ?"
--  putStrLn $ answer (t251 == (twoFiveOne (D, 4) wn))
--  play t251
  play toBuildAHomeIntro


toBuildAHomeIntro :: Music Pitch
toBuildAHomeIntro =
  let dur = wn
      dMajor  = mkChord (D, 4) dur [0,12+4,7]
      aMajor  = mkChord (A, 3) dur [0,12+4,7]
      eSecond = mkChord (E, 3) (dur/8) [0,12+2,7]
      eMajor  = mkChord (E, 3) dur [0,12+4,7]
      bMinor  = mkChord (B, 2) dur [0,12+3,7]
      dSecond = mkChord (D, 3) dur [0,12+2,7]
  in  line [ dMajor, aMajor, eSecond, eMajor
           , rest (dur/4)
           , bMinor, dSecond, aMajor, eMajor
           ]

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in  dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d =
  let chord_ii = mkChord p d [0,3,7]
      chord_V  = mkChord (trans 5 p) d [0,4,7]
      chord_I  = mkChord (trans (-2) p) (2*d) [0,4,7]
  in  chord_ii :+: chord_V :+: chord_I

mkChord :: Pitch -> Dur -> [AbsPitch] -> Music Pitch
mkChord p d aps = chord (map (\ap -> note d (trans ap p)) aps)
