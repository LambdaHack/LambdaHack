-- | AI strategy operations implemented with the 'Action' monad.
module Game.LambdaHack.StrategyAction
  ( targetStrategy, strategy
  ) where

import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Function
import Control.Monad
import Control.Monad.State hiding (State, state)
import Control.Arrow

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Ability (Ability)
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

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: Kind.COps -> ActorId -> State -> Perception
               -> Strategy Target
targetStrategy cops actor oldState@State{splayer = pl, sfaction} per =
  retarget btarget
 where
  Kind.COps{ cotile
           , coactor=coactor@Kind.Ops{okind}
           } = cops
  lvl@Level{lxsize} = slevel oldState
  actorBody@Actor{ bkind, bloc = me, btarget } = getActor actor oldState
  mk = okind bkind
  delState = deleteActor actor oldState
  enemyVisible a l =
    asight mk
    && isAHero delState a
    && monsterSeesHero cotile per lvl actor a me l
    -- Enemy can be felt if adjacent (e. g., a player-controlled monster).
    -- TODO: can this be replaced by setting 'lights' to [me]?
    || adjacent lxsize me l
       && (asmell mk || asight mk)
  focusedMonster = actorSpeed coactor actorBody <= speedNormal
  retarget :: Target -> Strategy Target
  retarget tgt =
    case tgt of
      TPath _ -> return tgt            -- don't animate missiles
      TEnemy a ll | focusedMonster && memActor a delState ->
        let l = bloc $ getActor a delState
        in if enemyVisible a l         -- prefer visible foes
           then return $ TEnemy a l
           else if null visibleFoes    -- prefer visible foes
                   && me /= ll         -- not yet reached the last enemy loc
                then return $ TLoc ll  -- chase the last known loc
                else closest
      TEnemy _ _ -> closest            -- foe is gone and we forget
      TLoc loc | me == loc -> closest  -- already reached the loc
      TLoc _ | null visibleFoes -> return tgt  -- nothing visible, go to loc
      TLoc _ -> closest                -- prefer visible foes
      TCursor  -> closest
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
    in minTgtS .| noFoes .| return TCursor  -- never empty
  -- TODO: set distant targets so that monsters behave as if they have
  -- a plan. We need pathfinding for that.
  noFoes :: Strategy Target
  noFoes = liftM (TLoc . (me `shift`)) $ moveStrategy cops actor oldState False

-- | Monster AI strategy based on monster sight, smell, intelligence, etc.
-- Never empty.
strategy :: Kind.COps -> ActorId -> State -> [Ability] -> Strategy (Action ())
strategy cops actor oldState factionAbilities =
  sumS prefix .| combineDistant distant .| sumS suffix
  .| waitNow  -- wait until friends move out of the way, ensures never empty
 where
  Kind.COps{coactor=Kind.Ops{okind}} = cops
  Actor{ bkind, bloc, btarget } = getActor actor oldState
  (floc, foeVisible) = case btarget of
     TEnemy _ l -> (l, True)
     TLoc l     -> (l, False)
     TPath _    -> (bloc, False)  -- a missile
     TCursor    -> (bloc, False)  -- an actor blocked by friends
  combineDistant as = foeVisible .=> liftFrequency (sumF as)
  aFrequency :: Ability -> Frequency (Action ())
  aFrequency Ability.Ranged = rangedFreq cops actor oldState floc
  aFrequency Ability.Tools  = toolsFreq cops actor oldState
  aFrequency Ability.Chase  = chaseFreq
  aFrequency _              = assert `failure` distant
  chaseFreq = case runStrategy $ chase cops actor oldState of
    [] -> mzero
    f : _ -> scaleFreq 30 f
  aStrategy :: Ability -> Strategy (Action ())
  aStrategy Ability.Track  = track cops actor oldState
  aStrategy Ability.Heal   = mzero  -- TODO
  aStrategy Ability.Flee   = mzero  -- TODO
  aStrategy Ability.Melee  = foeVisible .=> melee actor oldState floc
  aStrategy Ability.Pickup = pickup actor oldState
  aStrategy Ability.Wander = wander cops actor oldState
  aStrategy _              = assert `failure` actorAbilities
  actorAbilities = acanDo (okind bkind) `L.intersect` factionAbilities
  isDistant = (`elem` [Ability.Ranged, Ability.Tools, Ability.Chase])
  (prefix, rest)    = L.break isDistant actorAbilities
  (distant, suffix) = L.partition isDistant rest
  sumS = msum . map aStrategy
  sumF = msum . map aFrequency

dirToAction :: ActorId -> Bool -> Vector -> Action ()
dirToAction actor allowAttacks dir = do
  -- set new direction
  updateAnyActor actor $ \ m -> m { bdir = Just (dir, 0) }
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
track :: Kind.COps -> ActorId -> State -> Strategy (Action ())
track cops actor oldState =
  strat
 where
  lvl = slevel oldState
  Actor{ bloc, btarget } = getActor actor oldState
  darkenActor = updateAnyActor actor $ \ m -> m {bcolor = Just Color.BrBlack}
  strat = case btarget of
    TPath [] -> dieNow actor
    TPath (d : _) | not $ accessible cops lvl bloc (shift bloc d) ->
      dieNow actor
    -- TODO: perhaps colour differently the whole second turn of movement?
    TPath [d] -> return $ do
      darkenActor
      updateAnyActor actor $ \ m -> m { btarget = TPath [] }
      dirToAction actor True d
    TPath (d : lv) -> return $ do
      updateAnyActor actor $ \ m -> m { btarget = TPath lv }
      dirToAction actor True d
    _ -> reject

melee :: ActorId -> State -> Point -> Strategy (Action ())
melee actor oldState floc =
  foeAdjacent .=> (return $ dirToAction actor True dir)
 where
  Level{lxsize} = slevel oldState
  Actor{bloc} = getActor actor oldState
  foeAdjacent = adjacent lxsize bloc floc
  dir = displacement bloc floc

rangedFreq :: Kind.COps -> ActorId -> State -> Point -> Frequency (Action ())
rangedFreq cops actor oldState@State{splayer = pl, sfaction} floc =
  toFreq "throwFreq" $
    if not foesAdj
       && asight mk
       && accessible cops lvl bloc loc1           -- first accessible
       && isNothing (locToActor loc1 oldState)  -- no friends on first
    then throwFreq bitems 3 ++ throwFreq tis 6
    else []
 where
  Kind.COps{ coactor=Kind.Ops{okind}
           , coitem=Kind.Ops{okind=iokind}
           , corule
           } = cops
  lvl@Level{lxsize, lysize} = slevel oldState
  Actor{ bkind, bloc } = getActor actor oldState
  bitems = getActorItem actor oldState
  mk = okind bkind
  delState = deleteActor actor oldState
  tis = lvl `atI` bloc
  hs = heroAssocs sfaction $ slevel delState
  foes = if not (isAHero delState pl) && memActor pl delState
         then (pl, getPlayerBody delState) : hs
         else hs
  foesAdj = foesAdjacent lxsize lysize bloc (map snd foes)
  -- TODO: also don't throw if any loc on path is visibly not accessible
  -- from previous (and tweak eps in bla to make it accessible).
  -- Also don't throw if target not in range.
  eps = 0
  bl = bla lxsize lysize eps bloc floc  -- TODO:make an arg of projectGroupItem
  loc1 = case bl of
    Nothing -> bloc  -- XXX
    Just [] -> bloc  -- XXX
    Just (lbl:_) -> lbl
  throwFreq is multi =
    [ (benefit * multi, projectGroupItem actor floc (iverbProject ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit = - (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0,
      -- Wasting weapons and armour would be too cruel to the player.
      isymbol ik `elem` (ritemProject $ Kind.stdRuleset corule)]

toolsFreq :: Kind.COps -> ActorId -> State -> Frequency (Action ())
toolsFreq cops actor oldState =
  toFreq "quaffFreq" $ quaffFreq bitems 1 ++ quaffFreq tis 2
 where
  Kind.COps{coitem=Kind.Ops{okind=iokind}} = cops
  lvl = slevel oldState
  Actor{bloc} = getActor actor oldState
  bitems = getActorItem actor oldState
  tis = lvl `atI` bloc
  quaffFreq is multi =
    [ (benefit * multi, applyGroupItem actor (iverbApply ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit = (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0, isymbol ik == '!']

-- | AI finds interesting moves in the absense of visible foes.
-- This strategy can be null (e.g., if the actor is blocked by friends).
moveStrategy :: Kind.COps -> ActorId -> State -> Bool -> Strategy Vector
moveStrategy cops actor oldState newTargetSet =
  if newTargetSet
  then
    let towardsFoe = let foeDir = towards lxsize bloc floc
                     in only (\ x -> euclidDistSq lxsize foeDir x <= 1)
        (floc, foeVisible) = case btarget of
          TEnemy _ l -> (l, True)
          TLoc l     -> (l, False)
          _          -> (bloc, False)  -- we won't move
    in if floc == bloc
       then reject
       else towardsFoe
            $ if foeVisible
              then moveClear -- enemies in sight, don't waste time for doors
                   .| moveOpenable
              else moveOpenable  -- no enemy in sight, explore doors
                   .| moveClear
  else
   let movesNotBack =
         maybe id (\ (d, _) -> L.filter (/= neg d)) bdir $ sensible
       smells =
         map (map fst)
         $ L.groupBy ((==) `on` snd)
         $ L.sortBy (flip compare `on` snd)
         $ L.filter (\ (_, s) -> s > timeZero)
         $ L.map (\ x ->
                   let sm = IM.findWithDefault timeZero (bloc `shift` x) lsmell
                   in (x, sm `timeAdd` timeNegate ltime))
             movesNotBack
   in asmell mk .=> L.foldr ((.|) . liftFrequency
                             . uniformFreq "smell k") reject smells
      .| moveOpenable  -- no enemy in sight, explore doors
      .| moveClear
 where
  Kind.COps{ cotile
           , coactor=Kind.Ops{okind}
           , coitem
           } = cops
  lvl@Level{lsmell, lxsize, lysize, ltime} = slevel oldState
  Actor{ bkind, bloc, bdir, btarget } = getActor actor oldState
  bitems = getActorItem actor oldState
  mk = okind bkind
  delState = deleteActor actor oldState
  lootHere x     = not $ L.null $ lvl `atI` x
  onlyLoot       = onlyMoves lootHere bloc
  interestHere x = let t = lvl `at` x
                       ts = map (lvl `at`) $ vicinity lxsize lysize x
                   in Tile.hasFeature cotile F.Exit t
                      -- Lit indirectly. E.g., a room entrance.
                      || (not (Tile.hasFeature cotile F.Lit t)
                          && L.any (Tile.hasFeature cotile F.Lit) ts)
  onlyInterest   = onlyMoves interestHere bloc
  onlyKeepsDir k =
    only (\ x -> maybe True (\ (d, _) -> euclidDistSq lxsize d x <= k) bdir)
  onlyKeepsDir_9 = only (\ x -> maybe True (\ (d, _) -> neg x /= d) bdir)
  moveIQ = aiq mk > 15 .=> onlyKeepsDir 0 moveRandomly
        .| aiq mk > 10 .=> onlyKeepsDir 1 moveRandomly
        .| aiq mk > 5  .=> onlyKeepsDir 2 moveRandomly
        .| onlyKeepsDir_9 moveRandomly
  interestFreq | interestHere bloc =
    []  -- don't detour towards an interest if already on one
               | otherwise =
    map (scaleFreq 7)
      (runStrategy $ onlyInterest (onlyKeepsDir 2 moveRandomly))
  interestIQFreq = interestFreq ++ runStrategy moveIQ
  moveClear    = onlyMoves (not . openableHere) bloc moveFreely
  moveOpenable = onlyMoves openableHere bloc moveFreely
  moveFreely = onlyLoot moveRandomly
               .| liftFrequency (msum interestIQFreq)
               .| moveRandomly
  onlyMoves :: (Point -> Bool) -> Point -> Strategy Vector -> Strategy Vector
  onlyMoves p l = only (\ x -> p (l `shift` x))
  moveRandomly :: Strategy Vector
  moveRandomly = liftFrequency $ uniformFreq "moveRandomly" sensible
  -- Monsters don't see doors more secret than that. Enforced when actually
  -- opening doors, too, so that monsters don't cheat. TODO: remove the code
  -- duplication, though.
  openPower      = timeScale timeTurn $
                   case strongestSearch coitem bitems of
                     Just i  -> aiq mk + jpower i
                     Nothing -> aiq mk
  openableHere   = openable cotile lvl openPower
  accessibleHere = accessible cops lvl bloc
  noFriends | asight mk = unoccupied (dangerousList delState)
            | otherwise = const True
  isSensible l = noFriends l && (accessibleHere l || openableHere l)
  sensible = filter (isSensible . (bloc `shift`)) (moves lxsize)

chase :: Kind.COps -> ActorId -> State -> Strategy (Action ())
chase cops actor oldState =
  dirToAction actor False `liftM` moveStrategy cops actor oldState True

pickup :: ActorId -> State -> Strategy (Action ())
pickup actor oldState =
  lootHere bloc .=> actionPickup
 where
  lvl = slevel oldState
  Actor{bloc} = getActor actor oldState
  lootHere x = not $ L.null $ lvl `atI` x
  actionPickup = return $ actorPickupItem actor

wander :: Kind.COps -> ActorId -> State -> Strategy (Action ())
wander cops actor oldState =
  dirToAction actor True `liftM` moveStrategy cops actor oldState True
