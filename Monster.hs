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
playerHP = 20

-- | Time the player can be traced by monsters. TODO: Make configurable.
smellTimeout :: Time
smellTimeout = 100

-- | Initial player.
defaultPlayer :: Loc -> Player
defaultPlayer ploc =
  Monster Player playerHP Nothing ploc [] 0

type Player = Monster

data Monster = Monster
                { mtype   :: MonsterType,
                  mhp     :: Int,
                  mdir    :: Maybe Dir, -- for monsters: the dir the monster last moved;
                                        -- for the player: the dir the player is running
                  mloc    :: Loc,
                  mitems  :: [Item],    -- inventory
                  mtime   :: Time }     -- time of next action
  deriving Show

instance Binary Monster where
  put (Monster mt mhp md ml minv mtime) =
    do
      put mt
      put mhp
      put md
      put ml
      put minv
      put mtime
  get = do
          mt     <- get
          mhp    <- get
          md     <- get
          ml     <- get
          minv   <- get
          mtime  <- get
          return (Monster mt mhp md ml minv mtime)

data MonsterType =
    Player
  | Eye
  | Nose
  deriving (Show, Eq)

instance Binary MonsterType where
  put Player = putWord8 0 
  put Eye    = putWord8 1
  put Nose   = putWord8 2
  get = do
          tag <- getWord8
          case tag of
            0 -> return Player 
            1 -> return Eye
            2 -> return Nose
            _ -> fail "no parse (MonsterType)" 

monsterFrequency :: Frequency MonsterType
monsterFrequency =
  [ 
    (1, Nose),
    (3, Eye)
  ]

-- | Generate monster.
newMonster :: Loc -> Frequency MonsterType -> Rnd Monster
newMonster loc ftp =
    do
      tp <- frequency ftp
      hp <- hps tp
      return (template tp hp loc)
  where
    -- setting the time of new monsters to 0 makes them able to
    -- move immediately after generation; this does not seem like
    -- a bad idea, but it would certainly be "more correct" to set
    -- the time to the creation time instead
    template tp hp loc = Monster tp hp Nothing loc [] 0
    
    hps Eye  = randomR (1,3)
    hps Nose = randomR (2,3)

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


objectMonster :: MonsterType -> String
objectMonster Player = "you"
objectMonster Eye    = "the reducible eye"
objectMonster Nose   = "the point-free nose"

subjectMonster :: MonsterType -> String
subjectMonster x = let (s:r) = objectMonster x in toUpper s : r

verbMonster :: MonsterType -> String -> String
verbMonster Player v = v
verbMonster _      v = v ++ "s"

compoundVerbMonster :: MonsterType -> String -> String -> String
compoundVerbMonster Player v p = v ++ " " ++ p
compoundVerbMonster _      v p = v ++ "s " ++ p

viewMonster :: MonsterType -> (Char, Attr -> Attr)
viewMonster Player = ('@', setBG white . setFG black)
viewMonster Eye    = ('e', setFG red)
viewMonster Nose   = ('n', setFG green)
