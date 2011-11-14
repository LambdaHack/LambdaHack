module Game.LambdaHack.ActorAdd where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Ratio
import Data.Maybe
import qualified Data.Char as Char

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Loc
import Game.LambdaHack.State
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.WorldLoc
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind

-- Generic functions

-- Setting the time of new monsters to 0 makes them able to
-- move immediately after generation. This does not seem like
-- a bad idea, but it would certainly be "more correct" to set
-- the time to the creation time instead.
template :: Kind.Id ActorKind -> Maybe String -> Maybe Char -> Int -> Loc
            -> Actor
template mk ms mc hp loc = Actor mk ms mc hp Nothing TCursor loc 'a' 0

nearbyFreeLoc :: Loc -> State -> Loc
nearbyFreeLoc origin state =
  let lvl@Level{lxsize, lysize} = slevel state
      hs = levelHeroList state
      ms = levelMonsterList state
      cops = scops state
      places = origin : L.nub (concatMap (surroundings lxsize lysize) places)
      good loc = Tile.isWalkable cops (lvl `at` loc)
                 && loc `L.notElem` L.map bloc (hs ++ ms)
  in  fromMaybe (assert `failure` "too crowded map") $ L.find good places

-- Adding heroes

findHeroName :: Config.CP -> Int -> String
findHeroName config n =
  let heroName = Config.getOption config "heroes" ("HeroName_" ++ show n)
  in  fromMaybe ("hero number " ++ show n) heroName

-- | Create a new hero on the current level, close to the given location.
addHero :: Loc -> State -> State
addHero ploc state =
  let cops = scops state
      config = sconfig state
      bHP = Config.get config "heroes" "baseHP"
      loc = nearbyFreeLoc ploc state
      n = fst (scounter state)
      symbol = if n < 1 || n > 9 then Nothing else Just $ Char.intToDigit n
      name = findHeroName config n
      startHP = bHP `div` min 10 (n + 1)
      m = template (heroKindId cops) (Just name) symbol startHP loc
      state' = state { scounter = (n + 1, snd (scounter state))
                     , sparty = IS.insert n (sparty state) }
  in  updateLevel (updateHeroes (IM.insert n m)) state'

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
monsterGenChance :: LevelId -> Int -> Rnd Bool
monsterGenChance (LambdaCave d) numMonsters =
  chance $ 1%(fromIntegral (250 + 200 * (numMonsters - d)) `max` 50)

-- | Create a new monster in the level, at a random position.
addMonster :: Kind.Id ActorKind -> Int -> Loc -> State -> State
addMonster mk hp ploc state = do
  let loc = nearbyFreeLoc ploc state
      n = snd (scounter state)
      m = template mk Nothing Nothing hp loc
      state' = state { scounter = (fst (scounter state), n + 1) }
  updateLevel (updateMonsters (IM.insert n m)) state'

-- | Create a new monster in the level, at a random position.
rollMonster :: State -> Rnd State
rollMonster state = do
  let lvl = slevel state
      hs = levelHeroList state
      ms = levelMonsterList state
      cops@Kind.COps{coactor=Kind.Ops{ofrequency}} = scops state
      isLit = Tile.isLit cops
  rc <- monsterGenChance (slid state) (L.length ms)
  if not rc
    then return state
    else do
      -- TODO: new monsters should always be generated in a place that isn't
      -- visible by the player (if possible -- not possible for bigrooms)
      -- levels with few rooms are dangerous, because monsters may spawn
      -- in adjacent and unexpected places
      loc <- findLocTry 2000 (lmap lvl)
             (\ l t -> Tile.isWalkable cops t
                       && l `L.notElem` L.map bloc (hs ++ ms))
             (\ l t -> not (isLit t)  -- try a dark, distant place first
                       && L.all (\ pl -> distance
                                           (lxsize lvl)
                                           (bloc pl) l > 30) hs)
      (mk, k) <- frequency ofrequency
      hp <- rollDice $ ahp k
      return $ addMonster mk hp loc state
