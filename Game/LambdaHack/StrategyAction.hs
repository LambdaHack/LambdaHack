{-# LANGUAGE OverloadedStrings #-}
-- | AI strategy operations implemented with the 'Action' monad.
module Game.LambdaHack.StrategyAction
  ( targetStrategy, actionStrategy
  ) where

import Control.Arrow
import Control.Monad
import Data.Function
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

import Game.LambdaHack.Ability (Ability)
import qualified Game.LambdaHack.Ability as Ability
import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.StrategyKind
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import Game.LambdaHack.Strategy
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency
import Game.LambdaHack.Vector

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: MonadClientRO m => ActorId -> m (Strategy (Maybe Target))
targetStrategy actor = do
  cops@Kind.COps{costrat=Kind.Ops{okind}} <- getsLocal scops
  per <- askPerception
  loc <- getLocal
  cli <- getClient
  let Actor{bfaction} = getActor actor loc
      factionAI = gAiIdle $ sfaction loc IM.! bfaction
      factionAbilities = sabilities (okind factionAI)
  return $! reacquireTgt cops actor cli loc per factionAbilities

reacquireTgt :: Kind.COps -> ActorId -> StateClient -> State
               -> Perception -> [Ability]
               -> Strategy (Maybe Target)
reacquireTgt cops actor cli loc per factionAbilities =
  reacquire btarget
 where
  Kind.COps{coactor=coactor@Kind.Ops{okind}} = cops
  lvl@Level{lxsize} = getArena loc
  actorBody@Actor{ bkind, bpos = me, bfaction, bpath } =
    getActor actor loc
  btarget = IM.lookup actor . starget $ cli
  mk = okind bkind
  enemyVisible l =
    asight mk
    && actorSeesLoc per actor l
    -- Enemy can be felt if adjacent, even if invisible or disguise.
    -- TODO: can this be replaced by setting 'lights' to [me]?
    || adjacent lxsize me l
       && (asmell mk || asight mk)
  actorAbilities = acanDo (okind bkind) `L.intersect` factionAbilities
  focused = actorSpeed coactor actorBody <= speedNormal
            -- Don't focus on a distant enemy, when you can't chase him.
            -- TODO: or only if another enemy adjacent? consider Flee?
            && Ability.Chase `elem` actorAbilities
  reacquire :: Maybe Target -> Strategy (Maybe Target)
  reacquire tgt | isJust bpath = returN "TPath" tgt  -- don't animate missiles
  reacquire tgt =
    case tgt of
      Just (TEnemy a ll) | focused
                    && memActor a loc ->  -- present on this level
        let l = bpos $ getActor a loc
        in if enemyVisible l           -- prefer visible foes
           then returN "TEnemy" $ Just $ TEnemy a l
           else if null visibleFoes    -- prefer visible foes
                   && me /= ll         -- not yet reached the last enemy loc
                then returN "last known" $ Just $ TPos ll
                                       -- chase the last known loc
                else closest
      Just TEnemy{} -> closest            -- foe is gone and we forget
      Just (TPos pos) | me == pos -> closest  -- already reached the loc
      Just TPos{} | null visibleFoes -> returN "TPos" tgt
                                       -- nothing visible, go to loc
      Just TPos{} -> closest                -- prefer visible foes
      Nothing -> closest
  foes = hostileAssocs bfaction lvl
  visibleFoes = L.filter (enemyVisible . snd) (L.map (second bpos) foes)
  closest :: Strategy (Maybe Target)
  closest =
    let foeDist = L.map (\ (_, l) -> chessDist lxsize me l) visibleFoes
        minDist = L.minimum foeDist
        minFoes =
          L.filter (\ (_, l) -> chessDist lxsize me l == minDist) visibleFoes
        minTargets = map (\ (a, l) -> Just $ TEnemy a l) minFoes
        minTgtS = liftFrequency $ uniformFreq "closest" minTargets
    in minTgtS .| noFoes .| returN "TCursor" Nothing  -- never empty
  -- TODO: set distant targets so that monsters behave as if they have
  -- a plan. We need pathfinding for that.
  noFoes :: Strategy (Maybe Target)
  noFoes =
    (Just . TPos . (me `shift`)) `liftM` moveStrategy cops actor loc Nothing

-- | AI strategy based on actor's sight, smell, intelligence, etc. Never empty.
actionStrategy :: (MonadClientRO n, MonadAction m)
               => ActorId
               -> n (Strategy (m ()))
actionStrategy actor = do
  cops@Kind.COps{costrat=Kind.Ops{okind}} <- getsLocal scops
  loc <- getLocal
  cli <- getClient
  let Actor{bfaction} = getActor actor loc
      factionAI = gAiIdle $ sfaction loc IM.! bfaction
      factionAbilities = sabilities (okind factionAI)
  return $! proposeAction cops actor cli loc factionAbilities

proposeAction :: forall m. MonadAction m => Kind.COps -> ActorId
              -> StateClient -> State -> [Ability]
              -> Strategy (m ())
proposeAction cops actor cli loc factionAbilities =
  sumS prefix .| combineDistant distant .| sumS suffix
  .| waitBlockNow actor  -- wait until friends sidestep, ensures never empty
 where
  Kind.COps{coactor=Kind.Ops{okind}} = cops
  Actor{ bkind, bpos, bpath } = getActor actor loc
  btarget = IM.lookup actor . starget $ cli
  (fpos, foeVisible) | isJust bpath = (bpos, False)  -- a missile
                     | otherwise =
    case btarget of
      Just (TEnemy _ l) -> (l, True)
      Just (TPos l) -> (l, False)
      Nothing -> (bpos, False)  -- an actor blocked by friends
  combineDistant = liftFrequency . sumF
  aFrequency :: Ability -> Frequency (m ())
  aFrequency Ability.Ranged = if foeVisible
                              then rangedFreq cops actor loc fpos
                              else mzero
  aFrequency Ability.Tools  = if foeVisible
                              then toolsFreq cops actor loc
                              else mzero
  aFrequency Ability.Chase  = if (fpos /= bpos)
                              then chaseFreq
                              else mzero
  aFrequency _              = assert `failure` distant
  chaseFreq =
    scaleFreq 30 $ bestVariant $ chase cops actor loc (fpos, foeVisible)
  aStrategy :: Ability -> Strategy (m ())
  aStrategy Ability.Track  = track cops actor loc
  aStrategy Ability.Heal   = mzero  -- TODO
  aStrategy Ability.Flee   = mzero  -- TODO
  aStrategy Ability.Melee  = foeVisible .=> melee actor loc fpos
  aStrategy Ability.Pickup = not foeVisible .=> pickup actor loc
  aStrategy Ability.Wander = wander cops actor loc
  aStrategy _              = assert `failure` actorAbilities
  actorAbilities = acanDo (okind bkind) `L.intersect` factionAbilities
  isDistant = (`elem` [Ability.Ranged, Ability.Tools, Ability.Chase])
  (prefix, rest)    = L.break isDistant actorAbilities
  (distant, suffix) = L.partition isDistant rest
  sumS = msum . map aStrategy
  sumF = msum . map aFrequency

dirToAction :: MonadAction m => ActorId -> Bool -> Vector -> m ()
dirToAction actor allowAttacks dir = do
  -- set new direction
  updateAnyActor actor $ \ m -> m { bdirAI = Just (dir, 0) }
  -- perform action
  tryWith (\ msg -> if T.null msg
                    then return ()
                    else assert `failure` msg <> "in AI") $ do
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: ensure time is taken for other aborted actions in this file
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
    moveOrAttack allowAttacks actor dir

-- | A strategy to always just wait.
waitBlockNow :: MonadAction m => ActorId -> Strategy (m ())
waitBlockNow actor = returN "wait" $ setWaitBlock actor

-- | A strategy to always just die.
dieNow :: MonadAction m => ActorId -> Strategy (m ())
dieNow actor = returN "die" $ do  -- TODO: explode if a potion
  bitems <- getsGlobal (getActorItem actor)
  Actor{bpos} <- getsGlobal (getActor actor)
  modifyGlobal (updateArena (dropItemsAt bitems bpos))
  modifyGlobal (deleteActor actor)

-- TODO: move to server; the client and his AI does not have a say in that
-- | Strategy for dumb missiles.
track :: MonadAction m => Kind.COps -> ActorId -> State -> Strategy (m ())
track cops actor loc =
  strat
 where
  lvl = getArena loc
  Actor{ bpos, bpath, bhp } = getActor actor loc
  darkenActor = updateAnyActor actor $ \ m -> m {bcolor = Just Color.BrBlack}
  dieOrReset | bhp <= 0  = dieNow actor
             | otherwise =
                 returN "reset TPath" $ updateAnyActor actor
                 $ \ m -> m {bpath = Nothing}
  strat = case bpath of
    Just [] -> dieOrReset
    Just (d : _) | not $ accessible cops lvl bpos (shift bpos d) -> dieOrReset
    -- TODO: perhaps colour differently the whole second turn of movement?
    Just [d] -> returN "last TPath" $ do
      darkenActor
      updateAnyActor actor $ \ m -> m { bpath = Just [] }
      dirToAction actor True d
    Just (d : lv) -> returN "follow TPath" $ do
      updateAnyActor actor $ \ m -> m { bpath = Just lv }
      dirToAction actor True d
    Nothing -> reject

pickup :: MonadAction m => ActorId -> State -> Strategy (m ())
pickup actor loc =
  lootHere bpos .=> actionPickup
 where
  lvl = getArena loc
  Actor{bpos} = getActor actor loc
  lootHere x = not $ L.null $ lvl `atI` x
  actionPickup = returN "pickup" $ actorPickupItem actor

melee :: MonadAction m => ActorId -> State -> Point -> Strategy (m ())
melee actor loc fpos =
  foeAdjacent .=> (returN "melee" $ dirToAction actor True dir)
 where
  Level{lxsize} = getArena loc
  Actor{bpos} = getActor actor loc
  foeAdjacent = adjacent lxsize bpos fpos
  dir = displacement bpos fpos

rangedFreq :: MonadAction m => Kind.COps -> ActorId -> State -> Point -> Frequency (m ())
rangedFreq cops actor loc fpos =
  toFreq "throwFreq" $
    if not foesAdj
       && asight mk
       && accessible cops lvl bpos pos1      -- first accessible
       && isNothing (posToActor pos1 loc)  -- no friends on first
    then throwFreq bitems 3 ++ throwFreq tis 6
    else []
 where
  Kind.COps{ coactor=Kind.Ops{okind}
           , coitem=Kind.Ops{okind=iokind}
           , corule
           } = cops
  lvl@Level{lxsize, lysize} = getArena loc
  Actor{ bkind, bpos, bfaction } = getActor actor loc
  bitems = getActorItem actor loc
  mk = okind bkind
  tis = lvl `atI` bpos
  foes = hostileAssocs bfaction lvl
  foesAdj = foesAdjacent lxsize lysize bpos (map snd foes)
  -- TODO: also don't throw if any loc on path is visibly not accessible
  -- from previous (and tweak eps in bla to make it accessible).
  -- Also don't throw if target not in range.
  eps = 0
  bl = bla lxsize lysize eps bpos fpos  -- TODO:make an arg of projectGroupItem
  pos1 = case bl of
    Nothing -> bpos  -- TODO
    Just [] -> bpos  -- TODO
    Just (lbl:_) -> lbl
  throwFreq is multi =
    [ (benefit * multi,
       projectGroupItem actor fpos (iverbProject ik) i)
    | i <- is,
      let (ik, benefit) =
            case jkind (sdisco loc) i of
              Nothing -> (undefined, 0)
              Just ki ->
                let kik = iokind ki
                in (kik,
                    - (1 + jpower i) * Effect.effectToBenefit (ieffect kik)),
      benefit > 0,
      -- Wasting weapons and armour would be too cruel to the player.
      isymbol ik `elem` (ritemProject $ Kind.stdRuleset corule)]

toolsFreq :: MonadAction m => Kind.COps -> ActorId -> State -> Frequency (m ())
toolsFreq cops actor loc =
  toFreq "quaffFreq" $ quaffFreq bitems 1 ++ quaffFreq tis 2
 where
  Kind.COps{coitem=Kind.Ops{okind=iokind}} = cops
  lvl = getArena loc
  Actor{bpos} = getActor actor loc
  bitems = getActorItem actor loc
  tis = lvl `atI` bpos
  quaffFreq is multi =
    [ (benefit * multi, applyGroupItem actor (iverbApply ik) i)
    | i <- is,
      let (ik, benefit) =
            case jkind (sdisco loc) i of
              Nothing -> (undefined, 0)
              Just ki ->
                let kik = iokind ki
                in (kik,
                    - (1 + jpower i) * Effect.effectToBenefit (ieffect kik)),
      benefit > 0, isymbol ik == '!']

-- | AI finds interesting moves in the absense of visible foes.
-- This strategy can be null (e.g., if the actor is blocked by friends).
moveStrategy :: Kind.COps -> ActorId -> State -> Maybe (Point, Bool)
             -> Strategy Vector
moveStrategy cops actor loc mFoe =
  case mFoe of
    -- Target set and we chase the foe or his last position or another target.
    Just (fpos, foeVisible) ->
      let towardsFoe =
            let foeDir = towards lxsize bpos fpos
                tolerance | isUnit lxsize foeDir = 0
                          | otherwise = 1
            in only (\ x -> euclidDistSq lxsize foeDir x <= tolerance)
      in if fpos == bpos
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
                                  timeZero (bpos `shift` x) lsmell
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
           } = cops
  lvl@Level{lsmell, lxsize, lysize, ltime} = getArena loc
  Actor{ bkind, bpos, bdirAI, bfaction } = getActor actor loc
  mk = okind bkind
  lootHere x = not $ L.null $ lvl `atI` x
  onlyLoot   = onlyMoves lootHere bpos
  interestHere x = let t = lvl `at` x
                       ts = map (lvl `at`) $ vicinity lxsize lysize x
                   in Tile.hasFeature cotile F.Exit t
                      -- Lit indirectly. E.g., a room entrance.
                      || (not (Tile.hasFeature cotile F.Lit t)
                          && L.any (Tile.hasFeature cotile F.Lit) ts)
  onlyInterest   = onlyMoves interestHere bpos
  onlyKeepsDir k =
    only (\ x -> maybe True (\ (d, _) -> euclidDistSq lxsize d x <= k) bdirAI)
  onlyKeepsDir_9 = only (\ x -> maybe True (\ (d, _) -> neg x /= d) bdirAI)
  moveIQ = aiq mk > 15 .=> onlyKeepsDir 0 moveRandomly
        .| aiq mk > 10 .=> onlyKeepsDir 1 moveRandomly
        .| aiq mk > 5  .=> onlyKeepsDir 2 moveRandomly
        .| onlyKeepsDir_9 moveRandomly
  interestFreq | interestHere bpos =
    -- Don't detour towards an interest if already on one.
    mzero
               | otherwise =
    -- Prefer interests, but don't exclude other focused moves.
    scaleFreq 5 $ bestVariant $ onlyInterest $ onlyKeepsDir 2 moveRandomly
  interestIQFreq = interestFreq `mplus` bestVariant moveIQ
  moveClear    = onlyMoves (not . openableHere) bpos moveFreely
  moveOpenable = onlyMoves openableHere bpos moveFreely
  moveFreely = onlyLoot moveRandomly
               .| liftFrequency interestIQFreq
               .| moveIQ  -- sometimes interestIQFreq is excluded later on
               .| moveRandomly
  onlyMoves :: (Point -> Bool) -> Point -> Strategy Vector -> Strategy Vector
  onlyMoves p l = only (\ x -> p (l `shift` x))
  moveRandomly :: Strategy Vector
  moveRandomly = liftFrequency $ uniformFreq "moveRandomly" sensible
  openableHere   = openable cotile lvl
  accessibleHere = accessible cops lvl bpos
  noFriends | asight mk = unoccupied (factionList [bfaction] loc)
            | otherwise = const True
  isSensible l = noFriends l && (accessibleHere l || openableHere l)
  sensible = filter (isSensible . (bpos `shift`)) (moves lxsize)

chase :: MonadAction m => Kind.COps -> ActorId -> State -> (Point, Bool) -> Strategy (m ())
chase cops actor loc foe@(_, foeVisible) =
  -- Target set and we chase the foe or offer null strategy if we can't.
  -- The foe is visible, or we remember his last position.
  let mFoe = Just foe
      fight = not foeVisible  -- don't pick fights if the real foe is close
  in dirToAction actor fight `liftM` moveStrategy cops actor loc mFoe

wander :: MonadAction m => Kind.COps -> ActorId -> State -> Strategy (m ())
wander cops actor loc =
  -- Target set, but we don't chase the foe, e.g., because we are blocked
  -- or we cannot chase at all.
  let mFoe = Nothing
  in dirToAction actor True `liftM` moveStrategy cops actor loc mFoe
