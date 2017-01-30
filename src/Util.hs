module Util where

import Euterpea

answer :: Bool -> String
answer True = "Yes"
answer False = "No"

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

mkChord :: Pitch -> Dur -> [AbsPitch] -> Music Pitch
mkChord p d aps = chord (map (\ap -> note d (trans ap p)) aps)
