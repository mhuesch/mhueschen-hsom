{-# LANGUAGE RankNTypes #-}
module Ch3 where


import Control.Arrow
import Data.Monoid
import Euterpea.Music
import Euterpea.IO.MIDI.Play

import Util

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
mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p is = line (map (\ap -> note qn (trans ap p)) aps)
  where
    aps = scanl (+) 0 is

-- 3.11
data MyMode = Ionian
            | Dorian
            | Phrygian
            | Lydian
            | Mixolydian
            | Aeolian
            | Locrian
  deriving (Show, Enum)

ionianIntervals = [2,2,1,2,2,2,1]

modeIntervals :: MyMode -> [Int]
modeIntervals mode = rotate (fromEnum mode) ionianIntervals

genScale :: MyMode -> Pitch -> Music Pitch
genScale mode p = mkScale p (modeIntervals mode)

-- 3.12
-- Frere Jacques
melody :: Pitch -> Music Pitch
melody p = zoork $ concat [p0, p0, p1, p1, p2, p2, p3, p3]
  where
    ap = absPitch p
    apRelNote d rel = note d (pitch (ap + rel))
    zoork = line . map (uncurry apRelNote)
    p0 = [ (qn, 0)
         , (qn, 2)
         , (qn, 4)
         , (qn, 0)
         ]
    p1 = [ (qn, 4)
         , (qn, 5)
         , (hn, 7)
         ]
    p2 = [ (den, 7)
         , (sn, 9)
         , (en, 7)
         , (en, 5)
         , (qn, 4)
         , (qn, 0)
         ]
    p3 = [ (qn, 0)
         , (qn, (-5))
         , (hn, 0)
         ]

-- play with `play $ frereJacqueRound (C, 4)`
frereJacqueRound :: Pitch -> Music Pitch
frereJacqueRound p = chord (zipWith modder instruments [0,2..])
  where
    modder iName delayBeats =
      Modify (Instrument iName)
        (rest delayBeats :+: melody p)
    instruments = [ AcousticGrandPiano
                  , RockOrgan
                  , Violin
                  , SlapBass1
                  ] :: [InstrumentName]


-- 3.13
encrypt :: (Enum a) => [a] -> [a]
encrypt = map (toEnum . addWrap . fromEnum)
  where
    addWrap x = mod (x+1) 256

decrypt :: (Enum a) => [a] -> [a]
decrypt = map (toEnum . subWrap . fromEnum)
  where
    subWrap x = mod (x-1) 256

testEncDec :: Bool
testEncDec = decrypt (encrypt str) == str
  where
    str = map toEnum [0..255] :: String
