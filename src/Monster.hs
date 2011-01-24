module Monster where

import Data.Binary
import Control.Monad

import Geometry
import Random
import Display

-- TODO: move _all_ monster data here from Grammar.hs, etc.

data MovableType =
    Hero Char String
  | Eye
  | FastEye
  | Nose
  deriving (Show, Eq)

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
newMonster :: (MovableType -> Int -> Loc -> Int -> a) ->
              Loc -> Frequency MovableType -> Rnd a
newMonster template loc ftp =
    do
      tp <- frequency ftp
      hp <- hps tp
      let s = speed tp
      return (template tp hp loc s)
  where
    hps Eye      = randomR (1,12)  -- falls in 1--4 unarmed rounds
    hps FastEye  = randomR (1,6)   -- 1--2
    hps Nose     = randomR (6,13)  -- 2--5 and in 1 round of the strongest sword

    speed Eye      = 10
    speed FastEye  = 4
    speed Nose     = 11

-- Heroes are white, monsters are colorful.
viewMovable :: MovableType -> (Char, AttrColor)
viewMovable (Hero sym _) = (sym, white)
viewMovable Eye          = ('e', red)
viewMovable FastEye      = ('e', blue)
viewMovable Nose         = ('n', green)

instance Binary MovableType where
  put (Hero symbol name) = putWord8 0 >> put symbol >> put name
  put Eye                = putWord8 1
  put FastEye            = putWord8 2
  put Nose               = putWord8 3
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM2 Hero get get
            1 -> return Eye
            2 -> return FastEye
            3 -> return Nose
            _ -> fail "no parse (MovableType)"
