module MonsterState where

import Prelude hiding (floor)
import qualified Data.IntMap as IM
import Data.List as L
import Data.Map as M
import Data.Ratio

import Geometry
import State
import Level
import Dungeon
import Movable
import MovableState
import Monster
import Random

-- setting the time of new monsters to 0 makes them able to
-- move immediately after generation; this does not seem like
-- a bad idea, but it would certainly be "more correct" to set
-- the time to the creation time instead
templateMonster :: MovableType -> Loc -> Rnd Movable
templateMonster mt loc = do
  hp <- randomR (nhpMin mt, nhpMax mt)
  return $ Movable mt hp Nothing TCursor loc [] 'a' 0

newMonsterIndex :: State -> Int
newMonsterIndex (State { slevel = lvl, sdungeon = Dungeon m }) =
  let f lvl = let mms = lmonsters lvl
              in  if IM.null mms then -1 else fst (IM.findMax mms)
      maxes = L.map f (lvl : M.elems m)
  in  1 + L.maximum maxes

-- | Chance that a new monster is generated. Currently depends on the
-- number of monsters already present, and on the level. In the future,
-- the strength of the character and the strength of the monsters present
-- could further influence the chance, and the chance could also affect
-- which monster is generated.
monsterGenChance :: LevelName -> [Movable] -> Rnd Bool
monsterGenChance (LambdaCave n) ms =
  chance $ 1%(fromIntegral (250 + 200 * (L.length ms - n)) `max` 50)
monsterGenChance _ _ = return False

-- | Create a new monster in the level, at a random position.
addMonster :: State -> Rnd Level
addMonster state@(State { slevel = lvl }) = do
  let hs = levelHeroList state
      ms = levelMonsterList state
  rc <- monsterGenChance (lname lvl) ms
  if rc
    then
      do
        let ni = newMonsterIndex state
        -- TODO: new monsters should always be generated in a place that isn't
        -- visible by the player (if possible -- not possible for bigrooms)
        -- levels with few rooms are dangerous, because monsters may spawn
        -- in adjacent and unexpected places
        loc <- findLocTry 1000 lvl
               (\ l t -> open t
                         && not (l `L.elem` L.map mloc (hs ++ ms)))
               (\ l t -> floor t
                         && L.all (\ pl -> distance (mloc pl, l) > 400) hs)
        let fmt = Frequency $ L.zip (L.map nfreq roamingMts) roamingMts
        mt <- frequency fmt
        m  <- templateMonster mt loc
        return (updateMonsters (IM.insert ni m) lvl)
    else return lvl
