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
targetStrategy cops actor state@State{splayer = pl} per =
  retarget btarget
 where
  Kind.COps{ cotile
           , coactor=coactor@Kind.Ops{okind}
           } = cops
  lvl@Level{lxsize} = slevel state
  actorBody@Actor{ bkind, bloc = me, btarget, bfaction } =
    getActor actor state
  mk = okind bkind
  enemyVisible a l =
    asight mk
    && actorSeesActor cotile per lvl actor a me l pl
    -- Enemy can be felt if adjacent (e. g., a player-controlled monster).
    -- TODO: can this be replaced by setting 'lights' to [me]?
    || adjacent lxsize me l
       && (asmell mk || asight mk)
  focused = actorSpeed coactor actorBody <= speedNormal
  retarget :: Target -> Strategy Target
  retarget tgt =
    case tgt of
      TPath _ -> return tgt            -- don't animate missiles
      TEnemy a ll | focused
                    && memActor a state  -- present on this level
                    -- Don't hit a new player-controlled monster.
                    && not (isAHero state actor && a == pl) ->
        let l = bloc $ getActor a state
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
  hs = hostileAssocs bfaction lvl
  foes = if isAHero state actor
         then L.filter ((pl /=) . fst) hs  -- ignore player-controlled
         else if not (isAHero state pl) && memActor pl state
              then (pl, getPlayerBody state) : hs
              else hs  -- no player-controlled monster to add
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
  noFoes =
    (TLoc . (me `shift`)) `liftM` moveStrategy cops actor state Nothing

-- | AI strategy based on actor's sight, smell, intelligence, etc. Never empty.
strategy :: Kind.COps -> ActorId -> State -> [Ability] -> Strategy (Action ())
strategy cops actor state factionAbilities =
  sumS prefix .| combineDistant distant .| sumS suffix
  .| waitBlockNow actor  -- wait until friends sidestep, ensures never empty
 where
  Kind.COps{coactor=Kind.Ops{okind}} = cops
  Actor{ bkind, bloc, btarget } = getActor actor state
  (floc, foeVisible) = case btarget of
     TEnemy _ l -> (l, True)
     TLoc l     -> (l, False)
     TPath _    -> (bloc, False)  -- a missile
     TCursor    -> (bloc, False)  -- an actor blocked by friends
  combineDistant = liftFrequency . sumF
  aFrequency :: Ability -> Frequency (Action ())
  aFrequency Ability.Ranged = if foeVisible
                              then rangedFreq cops actor state floc
                              else mzero
  aFrequency Ability.Tools  = if foeVisible
                              then toolsFreq cops actor state
                              else mzero
  aFrequency Ability.Chase  = if (floc /= bloc)
                              then chaseFreq
                              else mzero
  aFrequency _              = assert `failure` distant
  chaseFreq =
    scaleFreq 30 $ bestVariant $ chase cops actor state (floc, foeVisible)
  aStrategy :: Ability -> Strategy (Action ())
  aStrategy Ability.Track  = track cops actor state
  aStrategy Ability.Heal   = mzero  -- TODO
  aStrategy Ability.Flee   = mzero  -- TODO
  aStrategy Ability.Melee  = foeVisible .=> melee actor state floc
  aStrategy Ability.Pickup = not foeVisible .=> pickup actor state
  aStrategy Ability.Wander = wander cops actor state
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
waitBlockNow :: ActorId -> Strategy (Action ())
waitBlockNow actor = return $ setWaitBlock actor

-- | A strategy to always just die.
dieNow :: ActorId -> Strategy (Action ())
dieNow actor = return $ do  -- TODO: explode if a potion
  bitems <- gets (getActorItem actor)
  Actor{bloc} <- gets (getActor actor)
  modify (updateLevel (dropItemsAt bitems bloc))
  modify (deleteActor actor)

-- | Strategy for dumb missiles.
track :: Kind.COps -> ActorId -> State -> Strategy (Action ())
track cops actor state =
  strat
 where
  lvl = slevel state
  Actor{ bloc, btarget, bhp } = getActor actor state
  darkenActor = updateAnyActor actor $ \ m -> m {bcolor = Just Color.BrBlack}
  dieOrReset | bhp <= 0  = dieNow actor
             | otherwise =
                 return $ updateAnyActor actor $ \ m -> m {btarget = TCursor}
  strat = case btarget of
    TPath [] -> dieOrReset
    TPath (d : _) | not $ accessible cops lvl bloc (shift bloc d) -> dieOrReset
    -- TODO: perhaps colour differently the whole second turn of movement?
    TPath [d] -> return $ do
      darkenActor
      updateAnyActor actor $ \ m -> m { btarget = TPath [] }
      dirToAction actor True d
    TPath (d : lv) -> return $ do
      updateAnyActor actor $ \ m -> m { btarget = TPath lv }
      dirToAction actor True d
    _ -> reject

pickup :: ActorId -> State -> Strategy (Action ())
pickup actor state =
  lootHere bloc .=> actionPickup
 where
  lvl = slevel state
  Actor{bloc} = getActor actor state
  lootHere x = not $ L.null $ lvl `atI` x
  actionPickup = return $ actorPickupItem actor

melee :: ActorId -> State -> Point -> Strategy (Action ())
melee actor state floc =
  foeAdjacent .=> (return $ dirToAction actor True dir)
 where
  Level{lxsize} = slevel state
  Actor{bloc} = getActor actor state
  foeAdjacent = adjacent lxsize bloc floc
  dir = displacement bloc floc

rangedFreq :: Kind.COps -> ActorId -> State -> Point -> Frequency (Action ())
rangedFreq cops actor state@State{splayer = pl} floc =
  toFreq "throwFreq" $
    if not foesAdj
       && asight mk
       && accessible cops lvl bloc loc1      -- first accessible
       && isNothing (locToActor loc1 state)  -- no friends on first
    then throwFreq bitems 3 ++ throwFreq tis 6
    else []
 where
  Kind.COps{ coactor=Kind.Ops{okind}
           , coitem=Kind.Ops{okind=iokind}
           , corule
           } = cops
  lvl@Level{lxsize, lysize} = slevel state
  Actor{ bkind, bloc, bfaction } = getActor actor state
  bitems = getActorItem actor state
  mk = okind bkind
  tis = lvl `atI` bloc
  hs = hostileAssocs bfaction lvl
  foes = if isAHero state actor
         then L.filter ((pl /=) . fst) hs  -- ignore player-controlled
         else if not (isAHero state pl) && memActor pl state
              then (pl, getPlayerBody state) : hs
              else hs  -- no player-controlled monster to add
  foesAdj = foesAdjacent lxsize lysize bloc (map snd foes)
  -- TODO: also don't throw if any loc on path is visibly not accessible
  -- from previous (and tweak eps in bla to make it accessible).
  -- Also don't throw if target not in range.
  eps = 0
  bl = bla lxsize lysize eps bloc floc  -- TODO:make an arg of projectGroupItem
  loc1 = case bl of
    Nothing -> bloc  -- TODO
    Just [] -> bloc  -- TODO
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
toolsFreq cops actor state =
  toFreq "quaffFreq" $ quaffFreq bitems 1 ++ quaffFreq tis 2
 where
  Kind.COps{coitem=Kind.Ops{okind=iokind}} = cops
  lvl = slevel state
  Actor{bloc} = getActor actor state
  bitems = getActorItem actor state
  tis = lvl `atI` bloc
  quaffFreq is multi =
    [ (benefit * multi, applyGroupItem actor (iverbApply ik) i)
    | i <- is,
      let ik = iokind (jkind i),
      let benefit = (1 + jpower i) * Effect.effectToBenefit (ieffect ik),
      benefit > 0, isymbol ik == '!']

-- | AI finds interesting moves in the absense of visible foes.
-- This strategy can be null (e.g., if the actor is blocked by friends).
moveStrategy :: Kind.COps -> ActorId -> State -> Maybe (Point, Bool)
             -> Strategy Vector
moveStrategy cops actor state mFoe =
  case mFoe of
    -- Target set and we chase the foe or his last position or another target.
    Just (floc, foeVisible) ->
      let towardsFoe =
            let foeDir = towards lxsize bloc floc
                tolerance | isUnit lxsize foeDir = 0
                          | otherwise = 1
            in only (\ x -> euclidDistSq lxsize foeDir x <= tolerance)
      in if floc == bloc
         then reject
         else towardsFoe
              $ if foeVisible
                then moveClear  -- enemies in sight, don't waste time for doors
                     .| moveOpenable
                else moveOpenable  -- no enemy in sight, explore doors
                     .| moveClear
    Nothing ->
      let smells =
            map (map fst)
            $ L.groupBy ((==) `on` snd)
            $ L.sortBy (flip compare `on` snd)
            $ L.filter (\ (_, s) -> s > timeZero)
            $ L.map (\ x ->
                      let sml = IM.findWithDefault
                                  timeZero (bloc `shift` x) lsmell
                      in (x, sml `timeAdd` timeNegate ltime))
                sensible
      in asmell mk .=> L.foldr ((.|)
                                . liftFrequency
                                . uniformFreq "smell k") reject smells
         .| moveOpenable  -- no enemy in sight, explore doors
         .| moveClear
 where
  Kind.COps{ cotile
           , coactor=Kind.Ops{okind}
           , coitem
           } = cops
  lvl@Level{lsmell, lxsize, lysize, ltime} = slevel state
  Actor{ bkind, bloc, bdir, bfaction } = getActor actor state
  bitems = getActorItem actor state
  mk = okind bkind
  lootHere x = not $ L.null $ lvl `atI` x
  onlyLoot   = onlyMoves lootHere bloc
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
    -- Don't detour towards an interest if already on one.
    mzero
               | otherwise =
    -- Prefer interests, but don't exclude other focused moves.
    scaleFreq 5 $ bestVariant $ onlyInterest $ onlyKeepsDir 2 moveRandomly
  interestIQFreq = interestFreq `mplus` bestVariant moveIQ
  moveClear    = onlyMoves (not . openableHere) bloc moveFreely
  moveOpenable = onlyMoves openableHere bloc moveFreely
  moveFreely = onlyLoot moveRandomly
               .| liftFrequency interestIQFreq
               .| moveIQ  -- sometimes interestIQFreq is excluded later on
               .| moveRandomly
  onlyMoves :: (Point -> Bool) -> Point -> Strategy Vector -> Strategy Vector
  onlyMoves p l = only (\ x -> p (l `shift` x))
  moveRandomly :: Strategy Vector
  moveRandomly = liftFrequency $ uniformFreq "moveRandomly" sensible
  -- Monsters don't see doors more secret than that. Enforced when actually
  -- opening doors, too, so that monsters don't cheat. TODO: remove the code
  -- duplication, though. TODO: make symmetric for playable monster faction?
  openPower      = timeScale timeTurn $
                   case strongestSearch coitem bitems of
                     Just i  -> aiq mk + jpower i
                     Nothing -> aiq mk
  openableHere   = openable cotile lvl openPower
  accessibleHere = accessible cops lvl bloc
  noFriends | asight mk = unoccupied (factionList [bfaction] state)
            | otherwise = const True
  isSensible l = noFriends l && (accessibleHere l || openableHere l)
  sensible = filter (isSensible . (bloc `shift`)) (moves lxsize)

chase :: Kind.COps -> ActorId -> State -> (Point, Bool) -> Strategy (Action ())
chase cops actor state foe@(_, foeVisible) =
  -- Target set and we chase the foe or offer null strategy if we can't.
  -- The foe is visible, or we remember his last position.
  let mFoe = Just foe
      fight = not foeVisible  -- don't pick fights if the real foe is close
  in dirToAction actor fight `liftM` moveStrategy cops actor state mFoe

wander :: Kind.COps -> ActorId -> State -> Strategy (Action ())
wander cops actor state =
  -- Target set, but we don't chase the foe, e.g., because we are blocked
  -- or we cannot chase at all.
  let mFoe = Nothing
  in dirToAction actor True `liftM` moveStrategy cops actor state mFoe
