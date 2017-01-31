module Ch4 where

import Euterpea

import Util

-- Eighth note is 69 bpm, but Euterpea default is 120 for quarter note
summer :: Music Pitch
summer = tempo ((en/qn) * (69/120))
               (l0 :=: r0)


r0 :: Music Pitch
r0 = line [ -- enr
          --  , mkChord (D,5) en [0,5,8]
          --  , mkChord (D,5) en [0,4,7]
          --  , enr
          --  , mkChord (D,4) en [0,5,8]
          --  , mkChord (D,4) en [  4,7]
          --  , enr
          --  , mkChord (Fs,4) sn [0,3,6]
          --  , mkChord (Fs,4) sn [  4,8]
          --  , mkChord (Fs,4) en [0,6,9]
          --  , enr
          --  , mkChord (D,4) sn [0,4,7]
          --  , mkChord (D,4) sn [  5,8]
          --  , mkChord (D,4) en [0,7,10]
          --  , mkChord (D,5) qn [0,5,8]
          --    :+: enr
          --  , scaleRange en [33,32,31]
          --  , (a 5 qn :=: c 6 qn)
          --    :+: enr
          --  , scaleRange en [34,33,32]
          --  , (bf 5 qn :=: d 6 qn)
          --    :+: enr
          --  -- measure 10
          --  , scaleRange en [35,34,33]

          -- measure 11
            c 4 dqn :=: fs 4 dqn
          , scaleRange en [39,38,37,36,35,34]
          , daScale (2 * dqn) 33
          , scaleRange en [40,39,38,37,36,35]
          -- measures 18-20
          , (daScale ((2 * dqn) + qn) 34)
            :+: enr
          ]

l0 :: Music Pitch
l0 = line [ -- [ enr, g 3 en, d 3 en ]
          --, [ enr, g 2 en, d 2 en ]
          --, times 2 [ enr, d 3 en, d 2 en ]
          --  g 2 qn :+: enr
          --, times 2 ((d 3 qn :=: fs 3 qn) :+: enr)
          --, times 2 ((g 2 qn :=: g 3 qn) :+: enr)
          ---- measure 10
          --, (c 3 qn :=: g 3 qn) :+: a 3 en

          -- measure 11
            d 3 dqn :=: a 3 dqn
          -- measures 12-13
          , dqnr
          , dqnr
          -- 14-17
          , sevenNoteScale 29
          , daScale (dqn * 2) 22
          -- 18-20
          , sevenNoteScale 25
          , daScale qn 18
          , enr
          ]

sevenNoteScale top = line
  -- 14
  [ scaleRange en [top,top-1,top-2]
  -- 15
  , scaleRange sn [top-3,top-4]
  , scaleRange en [top-5,top-6]
  ]

type Note = Octave -> Dur -> Music Pitch

scaleRange :: Dur -> [Int] -> Music Pitch
scaleRange dur = line . map (daScale dur)

-- daScale qn 28 == (c 4 qn :=: ef 4 qn)
daScale :: Dur -> Int -> Music Pitch
daScale dur i = f1 octave dur :=: f2 octave dur
  where
    (octave,idx) = i `quotRem` (length scale)
    scale :: [(Note,Note)]
    scale = [(c,ef),(d,fs),(ef,g),(fs,a),(g,bf),(a,\o -> c (o+1)),(bf,\o -> d (o+1))]
    (f1,f2) = scale !! idx

