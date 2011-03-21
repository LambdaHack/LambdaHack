module StrategyState where

import Data.List as L
import Data.Map as M
import Data.Set as S
import qualified Data.IntMap as IM

import Geometry
import Level
import Movable
import Random
import Perception
import Strategy
import State
import Actor

strategy :: Actor -> State -> Perception -> Strategy Dir
strategy actor
         state@(State { stime   = time,
                        slevel  = Level { lsmell = nsmap,
                                          lmap = lmap } })
         per =
    case mt of
      Eye     -> slowEye
      FastEye -> fastEye
      Nose    -> nose
      _       -> onlyAccessible moveRandomly
  where
    -- TODO: we check if the monster is visible by the hero rather than
    -- if the hero is visible by the monster -- this is more efficient, but
    -- is not correct with the Shadow FOV (the other FOVs are symmetrical)
    -- TODO: set monster targets and then prefer targets to other heroes
    Movable { mtype = mt, mloc = me, mdir = mdir } = getActor state actor
    delState = deleteActor actor state
    hs = levelHeroList delState
    ms = levelMonsterList delState
    -- If no heroes on the level, monsters go at each other. TODO: let them
    -- earn XP by killing each other to make this dangerous to the player.
    enemyLocs = L.map mloc $ if L.null hs then ms else hs
    enemyDist = L.sort $ L.map (\ l -> (distance (me, l), l)) enemyLocs
    -- Below, "player" is the hero (or a monster, if no heroes on this level)
    -- chased by the monster ("ploc" is his location, etc.).
    -- As soon as the monster hits, this hero becomes really the currently
    -- selected hero.
    -- We have to sort the list to avoid bias towards the currently selected
    -- hero; instead monsters will prefer heroes with smaller locations.
    ploc  = case enemyDist of
              [] -> Nothing
              (_, ploc) : _ -> Just ploc
    -- TODO: currently even invisible heroes are targeted if _any_ hero
    -- is visible; each hero should carry his own perception to check
    -- if he's visible by a given monster
    playerVisible      =  me `S.member` pvisible per  -- monster sees any hero
    playerAdjacent     =  maybe False (adjacent me) ploc
    towardsPlayer      =  maybe (0, 0) (\ ploc -> towards (me, ploc)) ploc
    onlyTowardsPlayer  =  only (\ x -> distance (towardsPlayer, x) <= 1)
    lootPresent        =  (\ x -> not $ L.null $ titems $ lmap `at` x)
    onlyLootPresent    =  onlyMoves lootPresent me
    onlyPreservesDir   =  only (\ x -> maybe True (\ d -> distance (neg d, x) > 1) mdir)
    onlyUnoccupied     =  onlyMoves (unoccupied ms) me
    onlyAccessible     =  onlyMoves (accessible lmap me) me
    -- Monsters don't see doors more secret than that. Enforced when actually
    -- opening doors, too, so that monsters don't cheat.
    -- TODO: vary the parameter per monster intelligence level.
    onlyOpenable       =  onlyMoves (openable 10 lmap) me
    smells             =  L.map fst $
                          L.sortBy (\ (_,s1) (_,s2) -> compare s2 s1) $
                          L.filter (\ (_,s) -> s > 0) $
                          L.map (\ x -> (x, nsmap ! (me `shift` x) - time `max` 0)) moves

    eye                =  onlyUnoccupied $
                            playerVisible .=> onlyTowardsPlayer moveRandomly
                            .| lootPresent me .=> wait
                            .| onlyLootPresent moveRandomly
                            .| onlyPreservesDir moveRandomly

    slowEye            =  playerAdjacent .=> return towardsPlayer
                          .| not playerVisible .=> onlyOpenable eye
                          .| onlyAccessible eye

    fastEye            =  playerAdjacent .=> return towardsPlayer
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
