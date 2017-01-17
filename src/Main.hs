module Main where

import Euterpea.Music
import Euterpea.IO.MIDI.Play

main :: IO ()
main = do
--  putStrLn "Is t251 == (twoFiveOne (D, 4) wn) ?"
--  putStrLn $ answer (t251 == (twoFiveOne (D, 4) wn))
--  play t251
  play toBuildAHomeIntro


toBuildAHomeIntro :: Music Pitch
toBuildAHomeIntro =
  let dur = wn
      dMajor = majorTriadMidUp (D, 4) dur
      aMajor = majorTriadMidUp (A, 3) dur
      eSecond = secondTriadMidUp (E, 3) (dur/8)
      eMajor = majorTriadMidUp (E, 3) dur
  in  line [dMajor, aMajor, eSecond, eMajor]

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in  dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d =
  let chord_ii = minorTriad p d
      chord_V  = majorTriad (trans 5 p) d
      chord_I  = majorTriad (trans (-2) p) (2*d)
  in  chord_ii :+: chord_V :+: chord_I

secondTriadMidUp :: Pitch -> Dur -> Music Pitch
secondTriadMidUp p d = note d p
                   :=: note d (trans (12+2) p)
                   :=: note d (trans 7 p)

majorTriadMidUp :: Pitch -> Dur -> Music Pitch
majorTriadMidUp p d = note d p
                  :=: note d (trans (12+4) p)
                  :=: note d (trans 7 p)

majorTriad :: Pitch -> Dur -> Music Pitch
majorTriad p d = note d p
             :=: note d (trans 4 p)
             :=: note d (trans 7 p)

minorTriad :: Pitch -> Dur -> Music Pitch
minorTriad p d = note d p
             :=: note d (trans 3 p)
             :=: note d (trans 7 p)

answer :: Bool -> String
answer True = "Yes"
answer False = "No"
