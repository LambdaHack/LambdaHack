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
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.WorldLoc
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Feature as F

-- Generic functions

-- Setting the time of new monsters to 0 makes them able to
-- move immediately after generation. This does not seem like
-- a bad idea, but it would certainly be "more correct" to set
-- the time to the creation time instead.
template :: Kind.Id ActorKind -> Maybe String -> Maybe Char -> Int -> Loc
         -> Actor
template mk ms mc hp loc = Actor mk ms mc hp Nothing TCursor loc 'a' 0

nearbyFreeLoc :: Kind.Ops TileKind -> Loc -> State -> Loc
nearbyFreeLoc cotile origin state =
  let lvl@Level{lxsize, lysize} = slevel state
      hs = levelHeroList state
      ms = levelMonsterList state
      places = origin : L.nub (concatMap (surroundings lxsize lysize) places)
      good loc = Tile.hasFeature cotile F.Walkable (lvl `at` loc)
                 && loc `L.notElem` L.map bloc (hs ++ ms)
  in fromMaybe (assert `failure` "too crowded map") $ L.find good places

-- Adding heroes

findHeroName :: Config.CP -> Int -> String
findHeroName config n =
  let heroName = Config.getOption config "heroes" ("HeroName_" ++ show n)
  in fromMaybe ("hero number " ++ show n) heroName

-- | Create a new hero on the current level, close to the given location.
addHero :: Kind.COps -> Loc -> State -> State
addHero Kind.COps{coactor, cotile} ploc state =
  let config = sconfig state
      bHP = Config.get config "heroes" "baseHP"
      loc = nearbyFreeLoc cotile ploc state
      n = fst (scounter state)
      symbol = if n < 1 || n > 9 then Nothing else Just $ Char.intToDigit n
      name = findHeroName config n
      startHP = bHP `div` min 10 (n + 1)
      m = template (heroKindId coactor) (Just name) symbol startHP loc
      state' = state { scounter = (n + 1, snd (scounter state))
                     , sparty = IS.insert n (sparty state) }
  in updateLevel (updateHeroes (IM.insert n m)) state'

-- | Create a set of initial heroes on the current level, at location ploc.
initialHeroes :: Kind.COps -> Loc -> State -> State
initialHeroes cops ploc state =
  let k = 1 + Config.get (sconfig state) "heroes" "extraHeroes"
  in iterate (addHero cops ploc) state !! k

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
addMonster :: Kind.Ops TileKind -> Kind.Id ActorKind -> Int -> Loc -> State
           -> State
addMonster cotile mk hp ploc state@State{scounter = (heroC, monsterC)} = do
  let loc = nearbyFreeLoc cotile ploc state
      m = template mk Nothing Nothing hp loc
      state' = state { scounter = (heroC, monsterC + 1) }
  updateLevel (updateMonsters (IM.insert monsterC m)) state'

-- | Create a new monster in the level, at a random position.
rollMonster :: Kind.COps -> State -> Rnd State
rollMonster Kind.COps{cotile, coactor=Kind.Ops{opick, okind}} state = do
  let lvl = slevel state
      hs = levelHeroList state
      ms = levelMonsterList state
      isLit = Tile.isLit cotile
  rc <- monsterGenChance (slid state) (L.length ms)
  if not rc
    then return state
    else do
      -- TODO: new monsters should be generated in a place that isn't
      -- visible by the player (if possible -- not possible for bigrooms)
      -- levels with few rooms are dangerous, because monsters may spawn
      -- in adjacent and unexpected places
      loc <- findLocTry 2000 (lmap lvl)
             (\ l t -> Tile.hasFeature cotile F.Walkable t
                       && l `L.notElem` L.map bloc (hs ++ ms))
             (\ l t -> not (isLit t)  -- try a dark, distant place first
                       && L.all (\ pl ->
                                  distance (lxsize lvl) (bloc pl) l > 20) hs)
      mk <- opick (const True)
      hp <- rollDice $ ahp $ okind mk
      return $ addMonster cotile mk hp loc state
