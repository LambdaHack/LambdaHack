module HeroState where

import qualified Data.Char as Char
import Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe

import Geometry
import qualified Config
import Movable
import MovableState
import MovableKind
import Level
import State

templateHero :: Char -> String -> Loc -> Int -> Movable
templateHero symbol name ploc hp =
  let mk = hero {nhpMin = hp, nhpMax = hp, nsymbol = symbol, nname = name }
  in  Movable mk hp Nothing TCursor ploc [] 'a' 0

-- | Create a new hero on the current level, close to the given location.
addHero :: Loc -> Int -> String -> State -> Int -> State
addHero ploc hp name state@(State { slevel = Level { lmap = map } }) n =
  let hs = levelHeroList state
      ms = levelMonsterList state
      places = ploc : L.nub (concatMap surroundings places)
      good l = open (map `at` l) && not (l `L.elem` L.map mloc (hs ++ ms))
      place = fromMaybe (error "no place for a hero") $ L.find good places
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      hero = templateHero symbol name place hp
  in  updateLevel (updateHeroes (IM.insert n hero)) state

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
