module Game.LambdaHack.StrategyState where

import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad
import Control.Arrow

import Game.LambdaHack.Loc
import Game.LambdaHack.Dir
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Frequency
import Game.LambdaHack.Perception
import Game.LambdaHack.Strategy
import Game.LambdaHack.State
import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.ItemAction
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind

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

strategy :: Kind.COps -> ActorId -> State -> Perceptions -> Strategy (Action ())
strategy cops actor oldState@State{splayer = pl, stime = time} per =
  strat
 where
  Kind.COps{ cotile
           , coactor=Kind.Ops{okind}
           , coitem=coitem@Kind.Ops{okind=iokind} } = cops
  lvl@Level{lsmell = nsmap, lxsize} = slevel oldState
  Actor { bkind = ak, bloc = me, bdir = ad, btarget = tgt } =
    getActor actor oldState
  items = getActorItem actor oldState
  mk = okind ak
  delState = deleteActor actor oldState
  enemyVisible a l =
    -- We assume monster sight is infravision, so light has no significance.
    asight mk && actorReachesActor a actor l me per Nothing ||
    -- Any enemy is visible if adjacent (e. g., a monster player).
    memActor a delState && adjacent lxsize me l
  -- If no heroes on the level, monsters go at each other. TODO: let them
  -- earn XP by killing each other to make this dangerous to the player.
  hs = L.map (AHero *** bloc) $
         IM.assocs $ lheroes $ slevel delState
  ms = L.map (AMonster *** bloc) $
         IM.assocs $ lmonsters $ slevel delState
  -- Below, "foe" is the hero (or a monster, or loc) chased by the actor.
  (newTgt, floc) =
    case tgt of
      TEnemy a ll | focusedMonster ->
        if memActor a delState
        then let l = bloc $ getActor a delState
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
                       then (pl, bloc $ getPlayerBody delState) : hs
                       else hs
        foes = if L.null hsAndTraitor then ms else hsAndTraitor
        -- We assume monster sight is infravision, so light has no effect.
        foeVisible = L.filter (uncurry enemyVisible) foes
        foeDist = L.map (\ (a, l) -> (distance lxsize me l, l, a)) foeVisible
    in case foeDist of
         [] -> (TCursor, Nothing)
         _  -> let (_, l, a) = L.minimum foeDist
               in (TEnemy a l, Just l)
  onlyFoe        = onlyMoves (maybe (const False) (==) floc) me
  towardsFoe     = case floc of
                     Nothing -> const mzero
                     Just loc ->
                       let foeDir = towards lxsize me loc
                       in only (\ x -> dirDistSq lxsize foeDir x <= 1)
  lootHere x     = not $ L.null $ lvl `atI` x
  onlyLoot       = onlyMoves lootHere me
  exitHere x     = let t = lvl `at` x in Tile.isExit cotile t
  onlyExit       = onlyMoves exitHere me
  onlyKeepsDir k =
    only (\ x -> maybe True (\ (d, _) -> dirDistSq lxsize d x <= k) ad)
  onlyKeepsDir_9 = only (\ x -> maybe True (\ (d, _) -> neg x /= d) ad)
  onlyNoMs       = onlyMoves (unoccupied (levelMonsterList delState)) me
  -- Monsters don't see doors more secret than that. Enforced when actually
  -- opening doors, too, so that monsters don't cheat. TODO: remove the code
  -- duplication, though.
  openPower      = Tile.SecretStrength $
                   case strongestItem coitem items "ring" of
                     Just i  -> aiq mk + jpower i
                     Nothing -> aiq mk
  openableHere   = openable cotile lvl openPower
  onlyOpenable   = onlyMoves openableHere me
  accessibleHere = accessible cops lvl me
  onlySensible   = onlyMoves (\ l -> accessibleHere l || openableHere l) me
  focusedMonster = aiq mk > 10
  smells         =
    L.map fst $
    L.sortBy (\ (_, s1) (_, s2) -> compare s2 s1) $
    L.filter (\ (_, s) -> s > 0) $
    L.map (\ x -> let sm = smelltime $ IM.findWithDefault
                                         (SmellTime 0) (me `shift` x) nsmap
                  in (x, (sm - time) `max` 0)) (moves lxsize)
  fromDir allowAttacks d = dirToAction actor newTgt allowAttacks `liftM` d

  strat =
    fromDir True (onlyFoe moveFreely)
    .| isJust floc .=> liftFrequency (msum freqs)
    .| lootHere me .=> actionPickup
    .| fromDir True moveAround
  actionPickup = return $ actorPickupItem actor
  tis = lvl `atI` me
  freqs = [applyFreq items 1, applyFreq tis 2,
           throwFreq items 2, throwFreq tis 5] ++ towardsFreq
  applyFreq is multi = Frequency
    [ (benefit * multi, actionApply (iname ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit =
            (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0,
      asight mk || iname ik /= "scroll"]
  actionApply groupName = applyGroupItem actor (applyToVerb groupName)
  throwFreq is multi = if not $ asight mk then mzero else Frequency
    [ (benefit * multi, actionThrow (iname ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit =
            - (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0,
      -- Wasting swords would be too cruel to the player.
      iname ik /= "sword"]
  actionThrow groupName =
    zapGroupItem actor (fromJust floc) (zapToVerb groupName)
  towardsFreq =
    let freqs2 = runStrategy $ fromDir False moveTowards
    in if asight mk
       then map (scale 30) freqs2
       else [mzero]
  moveTowards = onlySensible $ onlyNoMs (towardsFoe moveFreely)
  moveAround =
    onlySensible $
      (if asight mk then onlyNoMs else id) $
        asmell mk .=> L.foldr ((.|) . return) reject smells
        .| onlyOpenable moveFreely
        .| moveFreely
  moveIQ = aiq mk > 15 .=> onlyKeepsDir 0 moveRandomly
           .| aiq mk > 10 .=> onlyKeepsDir 1 moveRandomly
           .| aiq mk > 5  .=> onlyKeepsDir 2 moveRandomly
  exitFreq =  -- don't detour towards an exit if already on an exit
    if exitHere me
    then []
    else map (scale 3) (runStrategy $ onlyExit (onlyKeepsDir 2 moveRandomly))
  exitIQFreq =
    exitFreq ++ runStrategy moveIQ
  moveFreely = onlyLoot moveRandomly
               .| liftFrequency (msum exitIQFreq)
               .| onlyKeepsDir_9 moveRandomly
               .| moveRandomly
  onlyMoves :: (Loc -> Bool) -> Loc -> Strategy Dir -> Strategy Dir
  onlyMoves p l = only (\ x -> p (l `shift` x))
  moveRandomly :: Strategy Dir
  moveRandomly = liftFrequency $ uniform (moves lxsize)

dirToAction :: ActorId -> Target -> Bool -> Dir -> Action ()
dirToAction actor tgt allowAttacks dir = do
  -- set new direction
  updateAnyActor actor $ \ m -> m { bdir = Just (dir, 0), btarget = tgt }
  -- perform action
  tryWith (advanceTime actor) $
    -- if the following action aborts, we just advance the time and continue
    -- TODO: ensure time is taken for other aborted actions in this file
    moveOrAttack allowAttacks True actor dir

wait :: ActorId -> Strategy (Action ())
wait actor = return $ advanceTime actor
