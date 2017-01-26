{-# LANGUAGE RankNTypes #-}
module Ch3 where


import Control.Arrow
import Data.Monoid
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

-- 3.6
length' :: [a] -> Int
length' = foldl (\ acc _ -> acc+1) 0

-- 3.7a
doubleEach :: Num a => [a] -> [a]
doubleEach = map (*2)

-- 3.7b
pairAndOne :: Num a => [a] -> [(a,a)]
pairAndOne = map (\x -> (x, x+1))

-- 3.7c
addEachPair :: Num a => [(a,a)] -> [a]
addEachPair = map (\(x,y) -> x+y)

-- 3.7d
-- Yes, this is totally overengineered, but it gives the most general
-- function and that is obviously quite important because we will use
-- this function A LOT. A LOT! I'M SERIOUS!!!
addPairsPointwise :: forall a b. (Num a, Num b) => [(a,b)] -> (a,b)
addPairsPointwise = extract . foldl f (xInit, yInit) . map inject
  where
    f (a,b) (c,d) = (a `mappend` c, b `mappend` d)
    extract = getSum *** getSum
    inject  = Sum *** Sum
    xInit = mempty :: Num a => Sum a
    yInit = mempty :: Num b => Sum b


-- 3.8
maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch = foldl1 (!!!)

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch = foldl1 (!$!)

p1 !!! p2 = if p1 > p2 then p1 else p2
p1 !$! p2 = if p1 < p2 then p1 else p2


-- 3.9
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 =
  let aps = case p1 `compare` p2 of
              GT -> let ap1 = absPitch p1
                    in  enumFromThenTo ap1 (ap1-1) (absPitch p2)
              _  -> enumFromTo (absPitch p1) (absPitch p2)
  in  line (map (note qn . pitch) aps)

-- 3.10
-- 3.11
-- 3.12
-- 3.13
