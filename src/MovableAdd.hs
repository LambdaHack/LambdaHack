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

-- setting the time of new monsters to 0 makes them able to
-- move immediately after generation; this does not seem like
-- a bad idea, but it would certainly be "more correct" to set
-- the time to the creation time instead
template :: MovableKind -> Int -> Loc -> Movable
template mk hp loc = Movable mk hp Nothing TCursor loc [] 'a' 0

-- | Create a new hero on the current level, close to the given location.
addHero :: Loc -> Int -> String -> State -> Int -> State
addHero ploc hp name state@(State { slevel = Level { lmap = map } }) n =
  let hs = levelHeroList state
      ms = levelMonsterList state
      places = ploc : L.nub (concatMap surroundings places)
      good l = open (map `at` l) && not (l `L.elem` L.map mloc (hs ++ ms))
      loc = fromMaybe (error "no place for a hero") $ L.find good places
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      mk = hero {nhpMin = hp, nhpMax = hp, nsymbol = symbol, nname = name }
      m = template mk hp loc
  in  updateLevel (updateHeroes (IM.insert n m)) state

-- | Create a set of new heroes on the current level, at location ploc.
addHeroes :: Loc -> State -> State
addHeroes ploc state =
  let config = sconfig state
      findHeroName n =
        let heroName = Config.getOption config "heroes" ("HeroName_" ++ show n)
        in  fromMaybe ("hero number " ++ show n) heroName
      k = Config.get config "heroes" "extraHeroes"
      b = Config.get config "heroes" "baseHp"
      hp = k + b `div` (k + 1)
      addNamedHero state n = addHero ploc hp (findHeroName n) state n
  in  foldl' addNamedHero state [0..k]

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
        let fmk = Frequency $ L.zip (L.map nfreq roamingMts) roamingMts
        mk <- frequency fmk
        hp <- randomR (nhpMin mk, nhpMax mk)
        let m = template mk hp loc
        return (updateMonsters (IM.insert ni m) lvl)
    else return lvl
