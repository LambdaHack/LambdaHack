-- | AI strategy operations implemented with the 'Action' monad.
module Game.LambdaHack.StrategyAction
  ( pickTarget, strategy
  ) where

import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad
import Control.Monad.State hiding (State, state)
import Control.Arrow

import Game.LambdaHack.Utils.Assert
import qualified Game.LambdaHack.Ability as Ability
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
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Actions
import Game.LambdaHack.ItemAction
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Time
import qualified Game.LambdaHack.Color as Color

-- | AI proposes possible targets for the actor.
pickTarget :: Kind.COps -> ActorId -> State -> Perception -> Strategy Target
pickTarget cops actor oldState@State{splayer = pl, sfaction} per =
  case acanDo mk of
    [Ability.Track] -> return btarget  -- TODO: a hack
    _ -> chase btarget
 where
  Kind.COps{ cotile
           , coactor=coactor@Kind.Ops{okind}
           } = cops
  lvl@Level{lxsize} = slevel oldState
  actorBody@Actor{ bkind = ak, bloc = me, btarget } = getActor actor oldState
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
  focusedMonster = actorSpeed coactor actorBody <= speedNormal
  chase :: Target -> Strategy Target
  chase tgt =
    case tgt of
      TEnemy a ll | focusedMonster && memActor a delState ->
        let l = bloc $ getActor a delState
        in if enemyVisible a l         -- prefer visible foes
           then return $ TEnemy a l
           else if null visibleFoes    -- prefer visible foes
                   && me /= ll         -- not yet reached the last enemy loc
                then return $ TLoc ll  -- chase the last known loc
                else closest
      TLoc loc | me == loc -> closest  -- already reached the loc
      TLoc _ | null visibleFoes -> return tgt  -- nothing visible, go to loc
      TLoc _ -> closest                -- prefer visible foes
      _  -> closest
  hs = heroAssocs sfaction $ slevel delState
  foes = if not (isAHero delState pl) && memActor pl delState
         then (pl, getPlayerBody delState) : hs
         else hs
  visibleFoes = L.filter (uncurry enemyVisible) (L.map (second bloc) foes)
  closest :: Strategy Target
  closest =
    let foeDist = L.map (\ (_, l) -> chessDist lxsize me l) visibleFoes
        minDist = L.minimum foeDist
        minFoes =
          L.filter (\ (_, l) -> chessDist lxsize me l == minDist) visibleFoes
        minTargets = map (\ (a, l) -> TEnemy a l) minFoes
        minTgtS = liftFrequency $ uniformFreq "closest" minTargets
    in minTgtS .| noFoes .| return btarget
  noFoes :: Strategy Target
  noFoes = liftM (TLoc . (me `shift`)) $ moveAround cops actor oldState

-- | AI finds interesting moves in the absense of visible foes.
-- This strategy can be null (e.g., if the actor is blocked by friends).
moveAround :: Kind.COps -> ActorId -> State -> Strategy Vector
moveAround cops actor oldState =
  onlySensible $
  (if asight mk then onlyNoMs else id) $
    asmell mk .=> L.foldr ((.|) . return) reject smells
    .| onlyOpenable moveFreely
    .| moveFreely
 where
  Kind.COps{ cotile
           , coactor=Kind.Ops{okind}
           , coitem
           } = cops
  lvl@Level{lsmell, lxsize, lysize, ltime} = slevel oldState
  Actor{ bkind = ak, bloc = me, bdir = ad } = getActor actor oldState
  bitems = getActorItem actor oldState
  mk = okind ak
  delState = deleteActor actor oldState
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
  onlyNoMs       = onlyMoves (unoccupied (dangerousList delState)) me
  -- Monsters don't see doors more secret than that. Enforced when actually
  -- opening doors, too, so that monsters don't cheat. TODO: remove the code
  -- duplication, though.
  openPower      = timeScale timeTurn $
                   case strongestSearch coitem bitems of
                     Just i  -> aiq mk + jpower i
                     Nothing -> aiq mk
  openableHere   = openable cotile lvl openPower
  onlyOpenable   = onlyMoves openableHere me
  accessibleHere = accessible cops lvl me
  onlySensible   = onlyMoves (\ l -> accessibleHere l || openableHere l) me
  movesNotBack   = maybe id (\ (d, _) -> L.filter (/= neg d)) ad $ moves lxsize
  smells         =
    L.map fst $
    L.sortBy (\ (_, s1) (_, s2) -> compare s2 s1) $
    L.filter (\ (_, s) -> s > timeZero) $
    L.map (\ x -> let sm = IM.findWithDefault timeZero (me `shift` x) lsmell
                  in (x, max timeZero (sm `timeAdd` timeNegate ltime)))
      movesNotBack
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

-- | Monster AI strategy based on monster sight, smell, intelligence, etc.
strategy :: Kind.COps -> ActorId -> State -> Perception
         -> Strategy (Action ())
strategy cops actor oldState@State{splayer = pl, sfaction} per =
  case acanDo mk of  -- TODO: a hack
    [Ability.Track] -> trackNow cops actor oldState per
    _ -> strat
 where
  Kind.COps{ coactor=Kind.Ops{okind}
           , coitem=Kind.Ops{okind=iokind}
           , corule
           } = cops
  lvl@Level{lxsize, lysize} = slevel oldState
  Actor{ bkind = ak, bloc = me, btarget } = getActor actor oldState
  bitems = getActorItem actor oldState
  mk = okind ak
  delState = deleteActor actor oldState
  (floc, foeVisible) = case btarget of
     TEnemy _ l -> (l, True)
     TLoc l     -> (l, False)
     _          -> (me, False)  -- a very dummy actor, no sensible target
  strat =
    floc == me .=> waitNow  -- a very dummy actor, no sensible target
    .| meleeNow actor oldState
    .| foeVisible .=> liftFrequency (msum seenFreqs)
    .| pickupNow actor oldState
    .| wanderNow cops actor oldState True
    .| waitNow  -- wait until friends move out of the way
  tis = lvl `atI` me
  seenFreqs = [applyFreq bitems 1, applyFreq tis 2,
               throwFreq bitems 3, throwFreq tis 6]
              ++ towardsFreq
  applyFreq is multi = toFreq "applyFreq"
    [ (benefit * multi, applyGroupItem actor (iverbApply ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit = (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0,
      asight mk || isymbol ik == '!']
  hs = heroAssocs sfaction $ slevel delState
  foes = if not (isAHero delState pl) && memActor pl delState
         then (pl, getPlayerBody delState) : hs
         else hs
  foesAdj = foesAdjacent lxsize lysize me (map snd foes)
  -- TODO: also don't throw if any loc on path is visibly not accessible
  -- from previous (and tweak eps in bla to make it accessible).
  -- Also don't throw if target not in range.
  eps = 0
  bl = bla lxsize lysize eps me floc
  loc1 = case bl of
    Nothing -> me
    Just [] -> me
    Just (lbl:_) -> lbl
  throwFreq is multi = if foesAdj
                          || not (asight mk)
                          || not (accessible cops lvl me loc1)
                          || isJust (locToActor loc1 oldState)
                       then mzero
                       else toFreq "throwFreq"
    [ (benefit * multi,
       projectGroupItem actor floc (iverbProject ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit =
            - (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0,
      -- Wasting weapons and armour would be too cruel to the player.
      -- TODO: specify in content
      isymbol ik `elem` (ritemProject $ Kind.stdRuleset corule)]
  towardsFreq = map (scaleFreq 30) $ runStrategy $
                  wanderNow cops actor oldState False

dirToAction :: ActorId -> Target -> Bool -> Vector -> Action ()
dirToAction actor btarget allowAttacks dir = do
  -- set new direction
  updateAnyActor actor $ \ m -> m { bdir = Just (dir, 0), btarget }
  -- perform action
  tryWith (\ msg -> if null msg
                    then return ()
                    else assert `failure` (msg, "in AI")) $ do
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: ensure time is taken for other aborted actions in this file
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
    moveOrAttack allowAttacks actor dir

-- | A strategy to always just wait.
waitNow :: Strategy (Action ())
waitNow = return $ return ()

-- | A strategy to always just die.
dieNow :: ActorId -> Strategy (Action ())
dieNow actor = return $ do  -- TODO: explode if a potion
  bitems <- gets (getActorItem actor)
  Actor{bloc} <- gets (getActor actor)
  modify (updateLevel (dropItemsAt bitems bloc))
  modify (deleteActor actor)

-- | Strategy for dumb missiles.
trackNow :: Kind.COps -> ActorId -> State -> Perception
                   -> Strategy (Action ())
trackNow cops actor oldState _per =
  assert (not (nullStrat strat)) $ strat
 where
  lvl = slevel oldState
  Actor{ bloc = me, btarget } = getActor actor oldState
  darkenActor = updateAnyActor actor $ \ m -> m {bcolor = Just Color.BrBlack}
  strat = case btarget of
    TPath [] -> dieNow actor
    TPath (d : _) | not $ accessible cops lvl me (shift me d) -> dieNow actor
    -- TODO: perhaps colour differently the whole second turn of movement?
    TPath [d] -> return $ darkenActor >> dirToAction actor (TPath []) True d
    TPath (d : lv) -> return $ dirToAction actor (TPath lv) True d
    _ -> assert `failure` ("trackNow: no path", actor, btarget)

meleeNow :: ActorId -> State -> Strategy (Action ())
meleeNow actor oldState =
  (foeVisible && foeAdjacent) .=> (return $ dirToAction actor btarget True dir)
 where
  Level{lxsize} = slevel oldState
  Actor{ bloc = me, btarget } = getActor actor oldState
  (floc, foeVisible) = case btarget of
   TEnemy _ l -> (l, True)
   TLoc l     -> (l, False)
   _          -> (me, False)  -- a very dummy actor, no sensible target
  foeAdjacent = adjacent lxsize me floc
  dir = displacement me floc

pickupNow :: ActorId -> State -> Strategy (Action ())
pickupNow actor oldState =
  lootHere me .=> actionPickup
 where
  lvl = slevel oldState
  me = bloc $ getActor actor oldState
  lootHere x = not $ L.null $ lvl `atI` x
  actionPickup = return $ actorPickupItem actor

wanderNow :: Kind.COps -> ActorId -> State -> Bool -> Strategy (Action ())
wanderNow cops actor oldState allowAttacks =
  dirToAction actor btarget allowAttacks  `liftM` moveTowards
 where
  Kind.COps{ cotile
           , coactor=Kind.Ops{okind}
           , coitem
           } = cops
  lvl@Level{lxsize, lysize} = slevel oldState
  Actor{ bkind = ak, bloc = me, bdir = ad, btarget } = getActor actor oldState
  (floc, _foeVisible) = case btarget of
   TEnemy _ l -> (l, True)
   TLoc l     -> (l, False)
   _          -> (me, False)  -- a very dummy actor, no sensible target
  bitems = getActorItem actor oldState
  mk = okind ak
  delState = deleteActor actor oldState
  towardsFoe     = let foeDir = towards lxsize me floc
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
  onlyNoMs       = onlyMoves (unoccupied (dangerousList delState)) me
  -- Monsters don't see doors more secret than that. Enforced when actually
  -- opening doors, too, so that monsters don't cheat. TODO: remove the code
  -- duplication, though.
  openPower      = timeScale timeTurn $
                   case strongestSearch coitem bitems of
                     Just i  -> aiq mk + jpower i
                     Nothing -> aiq mk
  openableHere   = openable cotile lvl openPower
  accessibleHere = accessible cops lvl me
  onlySensible   = onlyMoves (\ l -> accessibleHere l || openableHere l) me
  moveTowards = onlySensible $ onlyNoMs (towardsFoe moveFreely)
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
