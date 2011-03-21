module Monster where

import Geometry
import Random
import Movable

-- TODO: move more monster data here from Movable.hs, Grammar.hs, etc.

-- | Monster frequencies (TODO: should of course vary much more
-- on local circumstances).
monsterFrequency :: Frequency MovableType
monsterFrequency =
  Frequency
  [
    (2, Nose),
    (6, Eye),
    (1, FastEye)
  ]

-- | Generate monster.
newMonster :: Loc -> Frequency MovableType -> Rnd Movable
newMonster loc ftp =
    do
      tp <- frequency ftp
      hp <- hps tp
      let s = speed tp
      return (template tp hp loc s)
  where
    -- setting the time of new monsters to 0 makes them able to
    -- move immediately after generation; this does not seem like
    -- a bad idea, but it would certainly be "more correct" to set
    -- the time to the creation time instead
    template tp hp loc s = Movable tp hp hp Nothing TCursor loc [] 'a' s 0

    hps Eye      = randomR (1,12)  -- falls in 1--4 unarmed rounds
    hps FastEye  = randomR (1,6)   -- 1--2
    hps Nose     = randomR (6,13)  -- 2--5 and in 1 round of the strongest sword

    speed Eye      = 10
    speed FastEye  = 4
    speed Nose     = 11
