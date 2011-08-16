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
import Actor
import ActorState
import ActorKind
import Random
import Perception
import Strategy
import State
import Action
import Actions
import ItemAction
import qualified ItemKind
import Item
import qualified Effect

-- import Debug.Trace

strategy :: ActorId -> State -> Perceptions -> Strategy (Action ())
strategy actor
         oldState@(State { scursor = cursor,
                           splayer = pl,
                           stime   = time,
                           slevel  = Level { lname = ln,
                                             lsmell = nsmap,
                                             lmap = lmap } })
         per =
--  trace (show time ++ ": " ++ show actor) $
    strategy
  where
    Actor { akind = mk, aloc = me, adir = adir,
            atarget = tgt, aitems = items } =
      getActor actor oldState
    delState = deleteActor actor oldState
    enemyVisible a l =
      -- We assume monster sight is infravision, so light has no significance.
      bsight mk && actorReachesActor a actor l me per Nothing ||
      -- Any enemy is visible if adjacent (e. g., a monster player).
      memActor a delState && adjacent me l
    -- If no heroes on the level, monsters go at each other. TODO: let them
    -- earn XP by killing each other to make this dangerous to the player.
    hs = L.map (\ (i, m) -> (AHero i, aloc m)) $
         IM.assocs $ lheroes $ slevel delState
    ms = L.map (\ (i, m) -> (AMonster i, aloc m)) $
         IM.assocs $ lmonsters $ slevel delState
    -- Below, "foe" is the hero (or a monster, or loc) chased by the actor.
    (newTgt, floc) =
      case tgt of
        TEnemy a ll | focusedMonster ->
          case findActorAnyLevel a delState of
            Just (_, m) ->
              let l = aloc m
              in  if enemyVisible a l
                  then (TEnemy a l, Just l)
                  else if isJust (snd closest) || me == ll
                       then closest         -- prefer visible foes
                       else (tgt, Just ll)  -- last known location of enemy
            Nothing -> closest  -- enemy dead, monsters can feel it
        TLoc loc -> if me == loc
                    then closest
                    else (tgt, Just loc)  -- ignore everything and go to loc
        _  -> closest
    closest =
      let hsAndTraitor = if isAMonster pl
                         then (pl, aloc $ getPlayerBody delState) : hs
                         else hs
          foes = if L.null hsAndTraitor then ms else hsAndTraitor
          -- We assume monster sight is infravision, so light has no effect.
          foeVisible = L.filter (\ (a, l) -> enemyVisible a l) foes
          foeDist = L.map (\ (a, l) -> (distance (me, l), l, a)) foeVisible
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
    exitHere       = (\ x -> let t = lmap `at` x in open t && reflects t)
    onlyExit       = onlyMoves exitHere me
    onlyKeepsDir k = only (\ x -> maybe True (\ d -> distance (d, x) <= k) adir)
    onlyKeepsDir_9 = only (\ x -> maybe True (\ d -> neg x /= d) adir)
    onlyNoMs       = onlyMoves (unoccupied (levelMonsterList delState)) me
    -- Monsters don't see doors more secret than that. Enforced when actually
    -- opening doors, too, so that monsters don't cheat. TODO: remove the code
    -- duplication, though.
    openPower      = case strongestItem items "ring" of
                       Just i  -> biq mk + ipower i
                       Nothing -> biq mk
    openableHere   = openable openPower lmap
    onlyOpenable   = onlyMoves openableHere me
    accessibleHere = accessible lmap me
    onlySensible   = onlyMoves (\ l -> accessibleHere l || openableHere l) me
    focusedMonster = biq mk > 10
    smells         =
      L.map fst $
      L.sortBy (\ (_, s1) (_, s2) -> compare s2 s1) $
      L.filter (\ (_, s) -> s > 0) $
      L.map (\ x -> (x, nsmap ! (me `shift` x) - time `max` 0)) moves
    fromDir allowAttacks d = dirToAction actor newTgt allowAttacks `liftM` d

    strategy =
      fromDir True (onlyFoe moveFreely)
      .| isJust floc .=> liftFrequency (msum freqs)
      .| lootHere me .=> actionPickup
      .| fromDir True moveAround
    actionPickup = return $ actorPickupItem actor
    tis = titems $ lmap `at` me
    freqs = [applyFreq items 1, applyFreq tis 2,
             throwFreq items 2, throwFreq tis 5, towardsFreq]
    applyFreq is multi = Frequency
      [ (benefit * multi, actionApply (ItemKind.jname ik) i)
      | i <- is,
        let ik = ItemKind.getIK (ikind i),
        let benefit =
              (1 + ipower i) * Effect.effectToBenefit (ItemKind.jeffect ik),
        benefit > 0,
        bsight mk || not (ItemKind.jname ik == "scroll")]
    actionApply groupName item =
      applyGroupItem actor (applyToVerb groupName) item
    throwFreq is multi = if not $ bsight mk then mzero else Frequency
      [ (benefit * multi, actionThrow (ItemKind.jname ik) i)
      | i <- is,
        let ik = ItemKind.getIK (ikind i),
        let benefit =
              - (1 + ipower i) * Effect.effectToBenefit (ItemKind.jeffect ik),
        benefit > 0,
        -- Wasting swords would be too cruel to the player.
        not (ItemKind.jname ik == "sword")]
    actionThrow groupName item =
      zapGroupItem actor (fromJust floc) (zapToVerb groupName) item
    towardsFreq =
      let freqs = runStrategy $ fromDir False moveTowards
      in  if bsight mk && not (L.null freqs)
          then scale 30 $ head freqs
          else mzero
    moveTowards = onlySensible $ onlyNoMs (towardsFoe moveFreely)
    moveAround =
      onlySensible $
        (if bsight mk then onlyNoMs else id) $
          bsmell mk .=> L.foldr (.|) reject (L.map return smells)
          .| onlyOpenable moveFreely
          .| moveFreely
    moveFreely = onlyLoot moveRandomly
                 .| onlyExit (onlyKeepsDir 2 moveRandomly)
                 .| biq mk > 15 .=> onlyKeepsDir 0 moveRandomly
                 .| biq mk > 10 .=> onlyKeepsDir 1 moveRandomly
                 .| biq mk > 5  .=> onlyKeepsDir 2 moveRandomly
                 .| onlyKeepsDir_9 moveRandomly
                 .| moveRandomly

dirToAction :: ActorId -> Target -> Bool -> Dir -> Action ()
dirToAction actor tgt allowAttacks dir =
  assert (dir /= (0,0)) $ do
  -- set new direction
  updateAnyActor actor $ \ m -> m { adir = Just dir, atarget = tgt }
  -- perform action
  tryWith (advanceTime actor) $
    -- if the following action aborts, we just advance the time and continue
    -- TODO: ensure time is taken for other aborted actions in this file
    moveOrAttack allowAttacks True actor dir

onlyMoves :: (Dir -> Bool) -> Loc -> Strategy Dir -> Strategy Dir
onlyMoves p l = only (\ x -> p (l `shift` x))

moveRandomly :: Strategy Dir
moveRandomly = liftFrequency $ uniform moves

wait :: ActorId -> Strategy (Action ())
wait actor = return $ advanceTime actor
