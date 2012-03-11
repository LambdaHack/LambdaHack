-- | AI strategy operations implemented with the 'Action' monad.
module Game.LambdaHack.StrategyAction
  ( strategy, wait
  ) where

import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad
import Control.Monad.State hiding (State, state)
import Control.Arrow

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Point
import Game.LambdaHack.Vector
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Utils.Frequency
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
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Time

{-
Monster movement
----------------

Not all monsters use the same algorithm to find the hero.
Some implemented and unimplemented methods are listed below:

* Random
The simplest way to have a monster move is at random.

* Sight
If a monster can see the hero (as an approximation,
we assume it is the case when the hero can see the monster,
unless either of the locations is dark),
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

-- TODO: improve, split up, etc.
-- | Monster AI strategy based on monster sight, smell, intelligence, etc.
strategy :: Kind.COps -> ActorId -> State -> Perception -> Strategy (Action ())
strategy cops actor oldState@State{splayer = pl} per =
  strat
 where
  Kind.COps{ cotile
           , coactor=coactor@Kind.Ops{okind}
           , coitem=coitem@Kind.Ops{okind=iokind}
           } = cops
  lvl@Level{lsmell = nsmap, lxsize, lysize, ltime} = slevel oldState
  actorBody@Actor { bkind = ak, bloc = me, bdir = ad, btarget, bhp } =
    getActor actor oldState
  bitems = getActorItem actor oldState
  mk = okind ak
  delState = deleteActor actor oldState
  enemyVisible a l =
    asight mk &&
    isAHero delState a &&
    monsterSeesHero cotile per lvl actor a me l
    -- Enemy can be felt if adjacent (e. g., a player-controlled monster).
    -- TODO: can this be replaced by setting 'lights' to [me]?
    || (asmell mk || asight mk)
       && adjacent lxsize me l
  -- Below, "foe" is the hero (or a monster, or loc) chased by the actor.
  chase tgt =
    case tgt of
      TEnemy a ll | focusedMonster && memActor a delState ->
        let l = bloc $ getActor a delState
        in if enemyVisible a l
           then (TEnemy a l, Just l, True)
           else if isJust (case closest of (_, m, _) -> m) || me == ll
                then closest                -- prefer visible foes
                else (tgt, Just ll, False)  -- last known loc of enemy
      TLoc loc | me == loc -> closest
      TLoc loc -> (tgt, Just loc, False)  -- ignore all and go to loc
      TPath [] -> (tgt, Nothing, False)  -- awaiting further instructions
      TPath (loc : ls) | me == loc -> chase $ TPath ls
      TPath (loc : _) -> (tgt, Just loc, True {- attack to free @loc@ -})
      _  -> closest
  (newTgt, floc, foeVisible) = chase btarget
  closest =
    let hs = L.map (second bloc) $ heroAssocs $ slevel delState
        foes = if not (isAHero delState pl) && memActor pl delState
               then (pl, bloc $ getPlayerBody delState) : hs
               else hs
        visible = L.filter (uncurry enemyVisible) foes
        foeDist = L.map (\ (a, l) -> (chessDist lxsize me l, l, a)) visible
    in case foeDist of
         [] -> (TCursor, Nothing, False)
         _  -> let (_, l, a) = L.minimum foeDist
               in (TEnemy a l, Just l, True)
  onlyFoe        = onlyMoves (maybe (const False) (==) floc) me
  towardsFoe     = case floc of
                     Nothing -> const mzero
                     Just loc ->
                       let foeDir = towards lxsize me loc
                       in only (\ x -> euclidDistSq lxsize foeDir x <= 1)
  lootHere x     = not $ L.null $ lvl `atI` x
  onlyLoot       = onlyMoves lootHere me
  interestHere x = let t = lvl `at` x
                       ts = map (lvl `at`) $ vicinity lxsize lysize x
                   in Tile.hasFeature cotile F.Exit t ||
                      -- Lit indirectly. E.g., a room entrance.
                      (not (Tile.hasFeature cotile F.Lit t) &&
                       L.any (Tile.hasFeature cotile F.Lit) ts)
  onlyInterest   = onlyMoves interestHere me
  onlyKeepsDir k =
    only (\ x -> maybe True (\ (d, _) -> euclidDistSq lxsize d x <= k) ad)
  onlyKeepsDir_9 = only (\ x -> maybe True (\ (d, _) -> neg x /= d) ad)
  onlyNoMs       = onlyMoves (unoccupied (levelMonsterList delState)) me
  -- Monsters don't see doors more secret than that. Enforced when actually
  -- opening doors, too, so that monsters don't cheat. TODO: remove the code
  -- duplication, though.
  openPower      = timeScale timeStep $
                   case strongestSearch coitem bitems of
                     Just i  -> aiq mk + jpower i
                     Nothing -> aiq mk
  openableHere   = openable cotile lvl openPower
  onlyOpenable   = onlyMoves openableHere me
  accessibleHere = accessible cops lvl me
  onlySensible   = onlyMoves (\ l -> accessibleHere l || openableHere l) me
  focusedMonster = actorSpeed coactor actorBody <= speedNormal
  movesNotBack   = maybe id (\ (d, _) -> L.filter (/= neg d)) ad $ moves lxsize
  smells         =
    L.map fst $
    L.sortBy (\ (_, s1) (_, s2) -> compare s2 s1) $
    L.filter (\ (_, s) -> s > timeZero) $
    L.map (\ x -> let sm = IM.findWithDefault timeZero (me `shift` x) nsmap
                  in (x, (sm `timeAdd` timeNegate ltime) `max` timeZero))
      movesNotBack
  attackDir d = dirToAction actor newTgt True  `liftM` d
  moveDir d   = dirToAction actor newTgt False `liftM` d

  strat = case newTgt of
    TPath [] -> dieOrSleep
    TPath _ | foeAdjacent && not foeAccessible -> dieOrSleep
    _ -> foeVisible .=> attackDir (onlyFoe moveFreely)
         .| foeVisible .=> liftFrequency (msum seenFreqs)
         .| lootHere me .=> actionPickup
         .| moveDir moveTowards  -- go to last known foe location
         .| attackDir moveAround
  dieOrSleep | bhp <= 0  = dieNow actor
             | otherwise = wait
  foeAccessible =
    maybe False (Tile.hasFeature cotile F.Walkable . (lvl `at`)) floc
  actionPickup = return $ actorPickupItem actor
  tis = lvl `atI` me
  seenFreqs = [applyFreq bitems 1, applyFreq tis 2,
               throwFreq bitems 3, throwFreq tis 6] ++ towardsFreq
  applyFreq is multi = toFreq "applyFreq"
    [ (benefit * multi, applyGroupItem actor (iverbApply ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit = (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0,
      asight mk || isymbol ik == '!']
  foeAdjacent = maybe False (adjacent lxsize me) floc
  throwFreq is multi = if foeAdjacent || not (asight mk)
                       then mzero
                       else toFreq "throwFreq"
    [ (benefit * multi,
       projectGroupItem actor (fromJust floc) (iverbProject ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit =
            - (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0,
      -- Wasting swords would be too cruel to the player.
      isymbol ik /= ')']
  towardsFreq = map (scaleFreq 30) $ runStrategy $ moveDir moveTowards
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
        .| onlyKeepsDir_9 moveRandomly
  interestFreq =  -- don't detour towards an interest if already on one
    if interestHere me
    then []
    else map (scaleFreq 3)
           (runStrategy $ onlyInterest (onlyKeepsDir 2 moveRandomly))
  interestIQFreq = interestFreq ++ runStrategy moveIQ
  moveFreely = onlyLoot moveRandomly
               .| liftFrequency (msum interestIQFreq)
               .| moveRandomly
  onlyMoves :: (Point -> Bool) -> Point -> Strategy Vector -> Strategy Vector
  onlyMoves p l = only (\ x -> p (l `shift` x))
  moveRandomly :: Strategy Vector
  moveRandomly = liftFrequency $ uniformFreq "moveRandomly" (moves lxsize)

dirToAction :: ActorId -> Target -> Bool -> Vector -> Action ()
dirToAction actor tgt allowAttacks dir = do
  -- set new direction
  updateAnyActor actor $ \ m -> m { bdir = Just (dir, 0), btarget = tgt }
  -- perform action
  tryWith (\ msg -> if null msg
                    then return ()
                    else assert `failure` (msg, "in AI")) $ do
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: ensure time is taken for other aborted actions in this file
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
    moveOrAttack allowAttacks actor dir

-- | A strategy to always just wait.
wait :: Strategy (Action ())
wait = return $ return ()

-- | A strategy to always just die.
dieNow :: ActorId -> Strategy (Action ())
dieNow actor = return $ do  -- TODO: explode if a potion
  bitems <- gets (getActorItem actor)
  Actor{bloc} <- gets (getActor actor)
  modify (updateLevel (dropItemsAt bitems bloc))
  modify (deleteActor actor)
