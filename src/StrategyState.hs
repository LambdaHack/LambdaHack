module StrategyState where

import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad
import Control.Arrow

import Utils.Assert
import Geometry
import Level
import Actor
import ActorState
import Content.ActorKind
import Frequency
import Perception
import Strategy
import State
import Action
import Actions
import ItemAction
import Content.ItemKind
import Item
import qualified Effect
import qualified Tile
import qualified Kind

-- import Debug.Trace

{-
Monster movement
----------------

Not all monsters use the same algorithm to find the hero.
Some implemented and unimplemented methods are listed below:

* Random
The simplest way to have a monster move is at random.

* Sight
If a monster can see the hero (as an approximation,
we assume it is the case when the hero can see the monster),
the monster should move toward the hero.

* Smell
The hero leaves a trail when moving toward the dungeon.
For a certain timespan (100--200 moves), it is possible
for certain monsters to detect that a hero has been at a certain field.
Once a monster is following a trail, it should move to the
neighboring field where the hero has most recently visited.

* Noise
The hero makes noise. If the distance between the hero
and the monster is small enough, the monster can hear the hero
and moves into the approximate direction of the hero.
-}

strategy :: ActorId -> State -> Perceptions -> Strategy (Action ())
strategy actor
         oldState@(State{ splayer = pl
                        , stime   = time
                        , slevel  = lvl@Level{ lsmell = nsmap
                                             , lsecret = le
                                             , lxsize } })
         per =
--  trace (show time ++ ": " ++ show actor) $
    strat
  where
    Actor { akind = ak, aloc = me, adir = ad,
            atarget = tgt, aitems = items } =
      getActor actor oldState
    mk = Kind.getKind ak
    delState = deleteActor actor oldState
    enemyVisible a l =
      -- We assume monster sight is infravision, so light has no significance.
      bsight mk && actorReachesActor a actor l me per Nothing ||
      -- Any enemy is visible if adjacent (e. g., a monster player).
      memActor a delState && adjacent lxsize me l
    -- If no heroes on the level, monsters go at each other. TODO: let them
    -- earn XP by killing each other to make this dangerous to the player.
    hs = L.map (AHero *** aloc) $
         IM.assocs $ lheroes $ slevel delState
    ms = L.map (AMonster *** aloc) $
         IM.assocs $ lmonsters $ slevel delState
    -- Below, "foe" is the hero (or a monster, or loc) chased by the actor.
    (newTgt, floc) =
      case tgt of
        TEnemy a ll | focusedMonster ->
          if memActor a delState
          then let l = aloc $ getActor a delState
               in if enemyVisible a l
                  then (TEnemy a l, Just l)
                  else if isJust (snd closest) || me == ll
                       then closest         -- prefer visible foes
                       else (tgt, Just ll)  -- last known location of enemy
          else closest  -- enemy not on the level, temporarily chase others
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
          foeVisible = L.filter (uncurry enemyVisible) foes
          foeDist = L.map (\ (a, l) -> (distance lxsize me l, l, a)) foeVisible
      in  case foeDist of
            [] -> (TCursor, Nothing)
            _  -> let (_, l, a) = L.minimum foeDist
                  in  (TEnemy a l, Just l)
    onlyFoe        = onlyMoves (maybe (const False) (==) floc) me
    towardsFoe     = case floc of
                       Nothing -> const mzero
                       Just loc ->
                         let foeDir = towards lxsize me loc
                         in  only (\ x -> dirDistSq foeDir x <= 1)
    lootHere x     = not $ L.null $ lvl `iat` x
    onlyLoot       = onlyMoves lootHere me
    exitHere x     = let t = lvl `at` x in Tile.isExit t
    onlyExit       = onlyMoves exitHere me
    onlyKeepsDir k = only (\ x -> maybe True (\ d -> dirDistSq d x <= k) ad)
    onlyKeepsDir_9 = only (\ x -> maybe True (\ d -> neg x /= d) ad)
    onlyNoMs       = onlyMoves (unoccupied (levelMonsterList delState)) me
    -- Monsters don't see doors more secret than that. Enforced when actually
    -- opening doors, too, so that monsters don't cheat. TODO: remove the code
    -- duplication, though.
    openPower      = case strongestItem items "ring" of
                       Just i  -> biq mk + ipower i
                       Nothing -> biq mk
    openableHere   = openable openPower lvl le
    onlyOpenable   = onlyMoves openableHere me
    accessibleHere = accessible lvl me
    onlySensible   = onlyMoves (\ l -> accessibleHere l || openableHere l) me
    focusedMonster = biq mk > 10
    smells         =
      L.map fst $
      L.sortBy (\ (_, s1) (_, s2) -> compare s2 s1) $
      L.filter (\ (_, s) -> s > 0) $
      L.map (\ x -> let sm = smelltime $ IM.findWithDefault
                                           (SmellTime 0) ((me `shift` lxsize) x) nsmap
                    in  (x, (sm - time) `max` 0)) moves
    fromDir allowAttacks d = dirToAction actor newTgt allowAttacks `liftM` d

    strat =
      fromDir True (onlyFoe moveFreely)
      .| isJust floc .=> liftFrequency (msum freqs)
      .| lootHere me .=> actionPickup
      .| fromDir True moveAround
    actionPickup = return $ actorPickupItem actor
    tis = lvl `iat` me
    freqs = [applyFreq items 1, applyFreq tis 2,
             throwFreq items 2, throwFreq tis 5, towardsFreq]
    applyFreq is multi = Frequency
      [ (benefit * multi, actionApply (jname ik) i)
      | i <- is,
        let ik = Kind.getKind (ikind i),
        let benefit =
              (1 + ipower i) * Effect.effectToBenefit (jeffect ik),
        benefit > 0,
        bsight mk || jname ik /= "scroll"]
    actionApply groupName = applyGroupItem actor (applyToVerb groupName)
    throwFreq is multi = if not $ bsight mk then mzero else Frequency
      [ (benefit * multi, actionThrow (jname ik) i)
      | i <- is,
        let ik = Kind.getKind (ikind i),
        let benefit =
              - (1 + ipower i) * Effect.effectToBenefit (jeffect ik),
        benefit > 0,
        -- Wasting swords would be too cruel to the player.
        jname ik /= "sword"]
    actionThrow groupName =
      zapGroupItem actor (fromJust floc) (zapToVerb groupName)
    towardsFreq =
      let freqs2 = runStrategy $ fromDir False moveTowards
      in  if bsight mk && not (L.null freqs2)
          then scale 30 $ head freqs2
          else mzero
    moveTowards = onlySensible $ onlyNoMs (towardsFoe moveFreely)
    moveAround =
      onlySensible $
        (if bsight mk then onlyNoMs else id) $
          bsmell mk .=> L.foldr ((.|) . return) reject smells
          .| onlyOpenable moveFreely
          .| moveFreely
    moveFreely = onlyLoot moveRandomly
                 .| onlyExit (onlyKeepsDir 2 moveRandomly)
                 .| biq mk > 15 .=> onlyKeepsDir 0 moveRandomly
                 .| biq mk > 10 .=> onlyKeepsDir 1 moveRandomly
                 .| biq mk > 5  .=> onlyKeepsDir 2 moveRandomly
                 .| onlyKeepsDir_9 moveRandomly
                 .| moveRandomly
    onlyMoves :: (Loc -> Bool) -> Loc -> Strategy Dir -> Strategy Dir
    onlyMoves p l = only (\ x -> p ((l `shift` lxsize) x))

dirToAction :: ActorId -> Target -> Bool -> Dir -> Action ()
dirToAction actor tgt allowAttacks dir =
  assert (dir /= neg dir `blame` dir) $ do
  -- set new direction
  updateAnyActor actor $ \ m -> m { adir = Just dir, atarget = tgt }
  -- perform action
  tryWith (advanceTime actor) $
    -- if the following action aborts, we just advance the time and continue
    -- TODO: ensure time is taken for other aborted actions in this file
    moveOrAttack allowAttacks True actor dir

moveRandomly :: Strategy Dir
moveRandomly = liftFrequency $ uniform moves

wait :: ActorId -> Strategy (Action ())
wait actor = return $ advanceTime actor
