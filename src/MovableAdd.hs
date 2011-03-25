module MovableAdd where

import Prelude hiding (floor)
import qualified Data.IntMap as IM
import Data.List as L
import Data.Map as M
import Data.Ratio
import Data.Maybe
import qualified Data.Char as Char

import Geometry
import State
import Level
import Dungeon
import Movable
import MovableState
import MovableKind
import Random
import qualified Config

-- Generic functions

-- setting the time of new monsters to 0 makes them able to
-- move immediately after generation; this does not seem like
-- a bad idea, but it would certainly be "more correct" to set
-- the time to the creation time instead
template :: MovableKind -> Int -> Loc -> Movable
template mk hp loc = Movable mk hp Nothing TCursor loc [] 'a' 0

nearbyFreeLoc :: Loc -> State -> Loc
nearbyFreeLoc origin state@(State { slevel = Level { lmap = map } }) =
  let hs = levelHeroList state
      ms = levelMonsterList state
      places = origin : L.nub (concatMap surroundings places)
      good loc = open (map `at` loc) && not (loc `L.elem` L.map mloc (hs ++ ms))
  in  fromMaybe (error "no nearby free location found") $ L.find good places

newMovableIndex :: (Level -> Party) -> State -> Int
newMovableIndex projection (State { slevel = lvl, sdungeon = Dungeon m }) =
  let f lvl = let mms = projection lvl
              in  if IM.null mms then -1 else fst (IM.findMax mms)
      maxes = L.map f (lvl : M.elems m)
  in  1 + L.maximum maxes

-- Adding heroes

findHeroName :: Config.CP -> Int -> String
findHeroName config n =
  let heroName = Config.getOption config "heroes" ("HeroName_" ++ show n)
  in  fromMaybe ("hero number " ++ show n) heroName

-- | Create a new hero on the current level, close to the given location.
addHero :: Loc -> State -> State
addHero ploc state =
  let config = sconfig state
      n = newMovableIndex lheroes state
      bHp = Config.get config "heroes" "baseHp"
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      name = findHeroName config n
      mk = hero {nhpMin = bHp, nhpMax = bHp, nsymbol = symbol, nname = name }
      loc = nearbyFreeLoc ploc state
      startHp = bHp `div` (min 10 (n + 1))
      m = template mk startHp loc
  in  updateLevel (updateHeroes (IM.insert n m)) state

-- | Create a set of initial heroes on the current level, at location ploc.
initialHeroes :: Loc -> State -> State
initialHeroes ploc state =
  let k = 1 + Config.get (sconfig state) "heroes" "extraHeroes"
  in  iterate (addHero ploc) state !! k

-- Adding monsters

-- | Chance that a new monster is generated. Currently depends on the
-- number of monsters already present, and on the level. In the future,
-- the strength of the character and the strength of the monsters present
-- could further influence the chance, and the chance could also affect
-- which monster is generated.
monsterGenChance :: LevelName -> Int -> Rnd Bool
monsterGenChance (LambdaCave depth) numMonsters =
  chance $ 1%(fromIntegral (250 + 200 * (numMonsters - depth)) `max` 50)
monsterGenChance _ _ = return False

-- | Create a new monster in the level, at a random position.
addMonster :: State -> Rnd Level
addMonster state@(State { slevel = lvl }) = do
  let hs = levelHeroList state
      ms = levelMonsterList state
  rc <- monsterGenChance (lname lvl) (L.length ms)
  if not rc
    then return lvl
    else do
      let ni = newMovableIndex lmonsters state
      -- TODO: new monsters should always be generated in a place that isn't
      -- visible by the player (if possible -- not possible for bigrooms)
      -- levels with few rooms are dangerous, because monsters may spawn
      -- in adjacent and unexpected places
      loc <- findLocTry 1000 lvl
             (\ l t -> open t
                       && not (l `L.elem` L.map mloc (hs ++ ms)))
             (\ l t -> floor t
                       && L.all (\ pl -> distance (mloc pl, l) > 400) hs)
      let fmk = Frequency $ L.zip (L.map nfreq roamingMts) roamingMts
      mk <- frequency fmk
      hp <- randomR (nhpMin mk, nhpMax mk)
      let m = template mk hp loc
      return (updateMonsters (IM.insert ni m) lvl)
