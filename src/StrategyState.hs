module StrategyState where

import Data.List as L
import Data.Map as M
import Data.Set as S
import qualified Data.IntMap as IM
import Data.Maybe

import Geometry
import Level
import Movable
import MovableState
import MovableKind
import Random
import Perception
import Strategy
import State

strategy :: Actor -> State -> Perceptions -> Strategy Dir
strategy actor
         state@(State { splayer = pl,
                        stime   = time,
                        slevel  = Level { lsmell = nsmap,
                                          lmap = lmap } })
         per =
    if nsmell mk then nose else openEye  -- TODO: unify the 2 kinds using nsight
  where
    -- TODO: set monster targets and then prefer targets to other heroes
    Movable { mkind = mk, mloc = me, mdir = mdir } = getActor actor state
    delState = deleteActor actor state
    hs = L.map (\ (i, m) -> (AHero i, mloc m)) $
         IM.assocs $ lheroes $ slevel delState
    ms = L.map (\ (i, m) -> (AMonster i, mloc m)) $
         IM.assocs $ lmonsters $ slevel delState
    -- If no heroes on the level, monsters go at each other. TODO: let them
    -- earn XP by killing each other to make this dangerous to the player.
    lmh = if L.null hs then ms else hs
    lmhVisible = L.filter (\ (a, l) -> actorSeesActor a actor l me per pl) lmh
    lmhDist = L.map (\ (_, l) -> (distance (me, l), l)) lmhVisible
    -- Below, "player" is the hero (or a monster, if no heroes on this level)
    -- chased by the monster ("ploc" is his location, etc.).
    -- As soon as the monster hits, the hero becomes really the currently
    -- player-controlled hero.
    ploc = case lmhVisible of
             [] -> Nothing
             _  -> Just $ snd $ L.minimum lmhDist
    playerVisible      =  isJust ploc  -- monster sees any hero
    playerAdjacent     =  maybe False (adjacent me) ploc
    towardsPlayer      =  maybe (0, 0) (\ ploc -> towards (me, ploc)) ploc
    onlyTowardsPlayer  =  only (\ x -> distance (towardsPlayer, x) <= 1)
    lootPresent        =  (\ x -> not $ L.null $ titems $ lmap `at` x)
    onlyLootPresent    =  onlyMoves lootPresent me
    onlyPreservesDir   =  only (\ x -> maybe True (\ d -> distance (neg d, x) > 1) mdir)
    onlyUnoccupied     =  onlyMoves (unoccupied (levelMonsterList delState)) me
    onlyAccessible     =  onlyMoves (accessible lmap me) me
    -- Monsters don't see doors more secret than that. Enforced when actually
    -- opening doors, too, so that monsters don't cheat.
    onlyOpenable       =  onlyMoves (openable (niq mk) lmap) me
    smells             =  L.map fst $
                          L.sortBy (\ (_,s1) (_,s2) -> compare s2 s1) $
                          L.filter (\ (_,s) -> s > 0) $
                          L.map (\ x -> (x, nsmap ! (me `shift` x) - time `max` 0)) moves

    eye                =  onlyUnoccupied $
                            playerVisible .=> onlyTowardsPlayer moveRandomly
                            .| lootPresent me .=> wait
                            .| onlyLootPresent moveRandomly
                            .| onlyPreservesDir moveRandomly

    openEye            =  playerAdjacent .=> return towardsPlayer
                          .| not playerVisible .=> onlyOpenable eye
                          .| onlyAccessible eye

    nose               =  playerAdjacent .=> return towardsPlayer
                          .| (onlyAccessible $
                              lootPresent me .=> wait
                              .| foldr (.|) reject (L.map return smells)
                              .| onlyLootPresent moveRandomly
                              .| moveRandomly)

onlyMoves :: (Dir -> Bool) -> Loc -> Strategy Dir -> Strategy Dir
onlyMoves p l = only (\ x -> p (l `shift` x))

moveRandomly :: Strategy Dir
moveRandomly = liftFrequency $ uniform moves

wait :: Strategy Dir
wait = return (0,0)
