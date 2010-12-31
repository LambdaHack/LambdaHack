module Monster where

import Data.Char
import Data.Binary
import Control.Monad

import Geometry
import Display
import Item
import Random

-- | Hit points of the player. TODO: Should not be hardcoded.
playerHP :: Int
playerHP = 50

-- | Time the player can be traced by monsters. TODO: Make configurable.
smellTimeout :: Time
smellTimeout = 1000

-- | Initial player.
defaultPlayer :: Loc -> Player
defaultPlayer ploc =
  Monster Player playerHP Nothing ploc [] 'a' 10 0

type Player = Monster

data Monster = Monster
                { mtype   :: !MonsterType,
                  mhp     :: !Int,
                  mdir    :: Maybe Dir,  -- for monsters: the dir the monster last moved;
                                         -- for the player: the dir the player is running
                  mloc    :: !Loc,
                  mitems  :: [Item],     -- inventory
                  mletter :: !Char,      -- next inventory letter
                  mspeed  :: !Time,      -- speed (i.e., delay before next action)
                  mtime   :: !Time }     -- time of next action
  deriving Show

instance Binary Monster where
  put (Monster mt mhp md ml minv mletter mspeed mtime) =
    do
      put mt
      put mhp
      put md
      put ml
      put minv
      put mletter
      put mspeed
      put mtime
  get = do
          mt      <- get
          mhp     <- get
          md      <- get
          ml      <- get
          minv    <- get
          mletter <- get
          mspeed  <- get
          mtime   <- get
          return (Monster mt mhp md ml minv mletter mspeed mtime)

data MonsterType =
    Player
  | Eye
  | FastEye
  | Nose
  deriving (Show, Eq)

instance Binary MonsterType where
  put Player  = putWord8 0
  put Eye     = putWord8 1
  put FastEye = putWord8 2
  put Nose    = putWord8 3
  get = do
          tag <- getWord8
          case tag of
            0 -> return Player
            1 -> return Eye
            2 -> return FastEye
            3 -> return Nose
            _ -> fail "no parse (MonsterType)"

-- | Monster frequencies (TODO: should of course vary much more
-- on local circumstances).
monsterFrequency :: Frequency MonsterType
monsterFrequency =
  Frequency
  [
    (2, Nose),
    (6, Eye),
    (1, FastEye)
  ]

-- | Generate monster.
newMonster :: Loc -> Frequency MonsterType -> Rnd Monster
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
    template tp hp loc s = Monster tp hp Nothing loc [] 'a' s 0

    hps Eye      = randomR (2,8)
    hps FastEye  = randomR (1,5)
    hps Nose     = randomR (5,8)

    speed Eye      = 10
    speed FastEye  = 4
    speed Nose     = 11

-- | Insert a monster in an mtime-sorted list of monsters.
-- Returns the position of the inserted monster and the new list.
insertMonster :: Monster -> [Monster] -> (Int, [Monster])
insertMonster = insertMonster' 0
  where
    insertMonster' n m []      = (n, [m])
    insertMonster' n m (m':ms)
      | mtime m <= mtime m'    = (n, m : m' : ms)
      | otherwise              = let (n', ms') = insertMonster' (n + 1) m ms
                                 in  (n', m' : ms')

-- * Grammar

-- | How to refer to a monster in object position of a sentence.
objectMonster :: MonsterType -> String
objectMonster Player  = "you"
objectMonster Eye     = "the reducible eye"
objectMonster FastEye = "the super-fast eye"
objectMonster Nose    = "the point-free nose"

-- | How to refer to a monster in subject position of a sentence.
subjectMonster :: MonsterType -> String
subjectMonster x = let (s:r) = objectMonster x in toUpper s : r

verbMonster :: MonsterType -> String -> String
verbMonster Player v = v
verbMonster _      v = v ++ "s"

compoundVerbMonster :: MonsterType -> String -> String -> String
compoundVerbMonster Player v p = v ++ " " ++ p
compoundVerbMonster _      v p = v ++ "s " ++ p

viewMonster :: MonsterType -> (Char, Attr -> Attr)
viewMonster Player  = ('@', setBG white . setFG black)
viewMonster Eye     = ('e', setFG red)
viewMonster FastEye = ('e', setFG blue)
viewMonster Nose    = ('n', setFG green)
