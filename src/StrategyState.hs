module StrategyState where

import Data.List as L
import Data.Map as M
import Data.Set as S
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad
import Control.Monad.State hiding (State)
import Control.Exception (assert)

import Geometry
import Level
import Movable
import MovableState
import MovableKind
import Random
import Perception
import Strategy
import State
import Action
import Actions
import ItemAction

strategy :: Actor -> State -> Perceptions -> Strategy (Action ())
strategy actor
         oldState@(State { scursor = cursor,
                           splayer = pl,
                           stime   = time,
                           slevel  = Level { lname = ln,
                                             lsmell = nsmap,
                                             lmap = lmap } })
         per =
    strategy
  where
    -- TODO: set monster targets and then prefer targets to other heroes
    Movable { mkind = mk, mloc = me, mdir = mdir, mtarget = tgt } =
      getActor actor oldState
    delState = deleteActor actor oldState
    -- If the player is a monster, monsters spot and attack him when adjacent.
    ploc = if isAHero pl || creturnLn cursor /= ln
           then Nothing
           else Just $ mloc $ getPlayerBody delState
    onlyTraitor = onlyMoves (maybe (const False) (==) ploc) me
    -- If no heroes on the level, monsters go at each other. TODO: let them
    -- earn XP by killing each other to make this dangerous to the player.
    hs = L.map (\ (i, m) -> (AHero i, mloc m)) $
         IM.assocs $ lheroes $ slevel delState
    ms = L.map (\ (i, m) -> (AMonster i, mloc m)) $
         IM.assocs $ lmonsters $ slevel delState
    -- Below, "foe" is the hero (or a monster, or loc), followed by the actor.
    (newTgt, floc) =
      case tgt of
        TEnemy a ll | focusedMonster ->
          case findActorAnyLevel a delState of
            Just (_, m) ->
              let l = mloc m
              in  -- We assume monster sight is infravision.
                  if actorReachesActor a actor l me per pl
                  then (TEnemy a l, Just l)
                  else if isJust (snd closest) || me == ll
                       then closest         -- prefer visible enemies
                       else (tgt, Just ll)  -- last known location of enemy
            Nothing -> closest  -- enemy dead
        TLoc loc -> if me == loc
                    then closest
                    else (tgt, Just loc)  -- ignore everything and go to loc
        _  -> closest
    closest =
      let foes = if L.null hs then ms else hs
          -- We assume monster sight is infravision, so light has no effect.
          foeVisible =
            L.filter (\ (a, l) -> actorReachesActor a actor l me per pl) foes
          foeDist = L.map (\ (a, l) -> (distance (me, l), l, a)) foeVisible
      -- Below, "foe" is the hero (or a monster) at floc, attacked by the actor.
      in  case foeDist of
            [] -> (TCursor, Nothing)
            _  -> let (_, l, a) = L.minimum foeDist
                  in  (TEnemy a l, Just l)
    onlyFoe        = onlyMoves (maybe (const False) (==) floc) me
    towardsFoe     = case floc of
                       Nothing -> const mzero
                       Just loc ->
                         let foeDir = towards (me, loc)
                         in  only (\ x -> distance (foeDir, x) <= 1)
    lootHere       = (\ x -> not $ L.null $ titems $ lmap `at` x)
    onlyLoot       = onlyMoves lootHere me
    onlyKeepsDir k = only (\ x -> maybe True (\ d -> distance (d, x) <= k) mdir)
    onlyUnoccupied = onlyMoves (unoccupied (levelMonsterList delState)) me
    -- Monsters don't see doors more secret than that. Enforced when actually
    -- opening doors, too, so that monsters don't cheat.
    openableHere   = openable (niq mk) lmap
    onlyOpenable   = onlyMoves openableHere me
    accessibleHere = accessible lmap me
    onlySensible   = onlyMoves (\ l -> accessibleHere l || openableHere l) me
    greedyMonster  = niq mk < 5
    focusedMonster = niq mk > 10
    pushyMonster   = not $ nsight mk
    smells         =
      L.map fst $
      L.sortBy (\ (_, s1) (_, s2) -> compare s2 s1) $
      L.filter (\ (_, s) -> s > 0) $
      L.map (\ x -> (x, nsmap ! (me `shift` x) - time `max` 0)) moves
    fromDir d = dirToAction actor newTgt `liftM` onlySensible d

    strategy =
      fromDir (onlyTraitor moveFreely)  -- traitor has priority
      .| fromDir (onlyFoe moveFreely)
      .| (greedyMonster && lootHere me) .=> actionPickup
      .| fromDir moveTowards
      .| lootHere me .=> actionPickup
      .| fromDir moveAround
    actionPickup = return $ actorPickupItem actor
    moveTowards =
      (if pushyMonster then id else onlyUnoccupied) $
        nsight mk .=> towardsFoe moveFreely
    moveAround =
      (if pushyMonster then id else onlyUnoccupied) $
        nsmell mk .=> foldr (.|) reject (L.map return smells)
        .| onlyOpenable moveFreely
        .| moveFreely
    moveFreely = onlyLoot moveRandomly
                 .| niq mk > 15 .=> onlyKeepsDir 0 moveRandomly
                 .| niq mk > 10 .=> onlyKeepsDir 1 moveRandomly
                 .| niq mk > 5  .=> onlyKeepsDir 2 moveRandomly
                 .| moveRandomly

dirToAction :: Actor -> Target -> Dir -> Action ()
dirToAction actor tgt dir =
  assert (dir /= (0,0)) $ do
  -- set new direction
  updateAnyActor actor $ \ m -> m { mdir = Just dir, mtarget = tgt }
  -- perform action
  tryWith (advanceTime actor) $
    -- if the following action aborts, we just advance the time and continue
    moveOrAttack True True actor dir

onlyMoves :: (Dir -> Bool) -> Loc -> Strategy Dir -> Strategy Dir
onlyMoves p l = only (\ x -> p (l `shift` x))

moveRandomly :: Strategy Dir
moveRandomly = liftFrequency $ uniform moves

wait :: Strategy (Action ())
wait = return $ return ()
