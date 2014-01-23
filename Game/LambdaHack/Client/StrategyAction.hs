-- | AI strategy operations implemented with the 'Action' monad.
module Game.LambdaHack.Client.StrategyAction
  ( targetStrategy, actionStrategy
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Traversable as Traversable

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Common.Ability (Ability)
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Common.Random as Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Utils.Frequency

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: MonadClient m
               => ActorId -> [Ability] -> m (Strategy (Maybe Target))
targetStrategy aid factionAbilities = do
  btarget <- getsClient $ getTarget aid
  bfs <- getCacheBfs aid
  reacquireTgt aid factionAbilities btarget bfs

aimableFoes :: MonadActionRO m
            => ActorId -> PointArray.Array BfsDistance
            -> m [(ActorId, Actor)]
aimableFoes aid bfs = do
  b <- getsState $ getActorBody aid
  assert (not $ bproj b) skip  -- would work, but is probably a bug
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  foes <- getsState $ actorNotProjAssocs (isAtWar fact) (blid b)
  return $! filter (posAimsPos bfs (bpos b) . bpos . snd) foes

reacquireTgt :: MonadActionRO m
             => ActorId -> [Ability] -> Maybe Target
             -> PointArray.Array BfsDistance
             -> m (Strategy (Maybe Target))
reacquireTgt aid factionAbilities btarget bfs = do
  cops@Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  assert (not $ bproj b) skip  -- would work, but is probably a bug
  lvl <- getsState $ \s -> sdungeon s EM.! blid b
  visFoes <- aimableFoes aid bfs
  actorD <- getsState sactorD
  -- TODO: set distant targets so that monsters behave as if they have
  -- a plan. We need pathfinding for that.
  noFoes :: Strategy (Maybe Target) <- do
    s <- getState
    str <- moveStrategy cops aid s Nothing
    return $! (Just . TPoint (blid b) . (bpos b `shift`)) `liftM` str
  let mk = okind $ bkind b
      actorAbilities = acanDo mk `intersect` factionAbilities
      focused = bspeed b <= speedNormal
                -- Don't focus on a distant enemy, when you can't chase him.
                -- TODO: or only if another enemy adjacent? consider Flee?
                && Ability.Chase `elem` actorAbilities
      closest :: Strategy (Maybe Target)
      closest =
        let distB = chessDist (bpos b)
            foeDist = map (\(_, body) -> distB (bpos body)) visFoes
            minDist | null foeDist = maxBound
                    | otherwise = minimum foeDist
            minFoes =
              filter (\(_, body) -> distB (bpos body) == minDist) visFoes
            minTargets = map (\(a, _) -> Just $ TEnemy a False) minFoes
            minTgtS = liftFrequency $ uniformFreq "closest" minTargets
        in minTgtS .| noFoes .| returN "TCursor" Nothing  -- never empty
      reacquire :: Maybe Target -> Strategy (Maybe Target)
      reacquire tgt =
        case tgt of
          Just (TEnemy a _) ->
            case EM.lookup a actorD of
              Just body ->
                if posAimsPos bfs (bpos b) (bpos body) && blid b == blid body
                then returN "TEnemy" tgt
                else closest  -- prefer aimable foes on the same level
              Nothing -> assert `failure` tgt
          Just (TEnemyPos _ lid ll _) | focused && lid == blid b ->
            -- Chase even if enemy dead, to loot.
            if null visFoes               -- prefer visible foes to positions
               && bpos b /= ll            -- not yet reached the last pos
            then returN "last known" tgt  -- chase the last known pos
            else closest
          Just TEnemyPos{} -> closest
          Just (TPoint lid _) | lid /= blid b -> closest  -- wrong level
          Just (TPoint _ pos) | pos == bpos b -> closest  -- already reached
          Just (TPoint _ pos)
            | not (bumpableHere cops lvl False (asight mk) pos) ->
            closest  -- no longer bumpable, even assuming no foes
          Just TPoint{} | null visFoes -> returN "TPoint" tgt
                                          -- nothing visible, go to pos
          Just TPoint{} -> closest  -- prefer visible foes
          Just TVector{} -> closest
          Nothing -> closest
  return $! reacquire btarget

-- | AI strategy based on actor's sight, smell, intelligence, etc.
-- Never empty.
actionStrategy :: MonadClient m
               => ActorId -> [Ability] -> m (Strategy CmdTakeTimeSer)
actionStrategy aid factionAbilities = do
  disco <- getsClient sdisco
  btarget <- getsClient $ getTarget aid
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  Actor{bkind, bpos, blid} <- getsState $ getActorBody aid
  lvl <- getLevel blid
  mfpos <- aidTgtToPos aid blid btarget
  let mk = okind bkind
      fpos = maybe bpos fst mfpos
      mfAid =
        case btarget of
          Just (TEnemy foeAid _) -> Just foeAid
          _ -> Nothing
      foeVisible = isJust mfAid
      lootHere x = not $ EM.null $ lvl `atI` x
      actorAbilities = acanDo mk `intersect` factionAbilities
      isDistant = (`elem` [ Ability.Trigger
                          , Ability.Ranged
                          , Ability.Tools
                          , Ability.Chase ])
      -- TODO: this is too fragile --- depends on order of abilities
      (prefix, rest)    = break isDistant actorAbilities
      (distant, suffix) = partition isDistant rest
      -- TODO: Ranged and Tools should only be triggered in some situations.
      aFrequency :: MonadActionRO m => Ability -> m (Frequency CmdTakeTimeSer)
      aFrequency Ability.Trigger = if foeVisible then return mzero
                                   else triggerFreq aid
      aFrequency Ability.Ranged = if not foeVisible then return mzero
                                  else rangedFreq disco aid fpos
      aFrequency Ability.Tools  = if not foeVisible then return mzero
                                  else toolsFreq disco aid
      aFrequency Ability.Chase  = if fpos == bpos then return mzero
                                  else chaseFreq
      aFrequency ab             = assert `failure` "unexpected ability"
                                          `twith` (ab, distant, actorAbilities)
      chaseFreq :: MonadActionRO m => m (Frequency CmdTakeTimeSer)
      chaseFreq = do
        st <- chase aid (fpos, foeVisible)
        return $! scaleFreq 30 $ bestVariant st
      aStrategy :: MonadActionRO m => Ability -> m (Strategy CmdTakeTimeSer)
      aStrategy Ability.Track  = track aid
      aStrategy Ability.Heal   = return mzero  -- TODO
      aStrategy Ability.Flee   = return mzero  -- TODO
      aStrategy Ability.Melee | Just foeAid <- mfAid = melee aid fpos foeAid
      aStrategy Ability.Melee  = return mzero
      aStrategy Ability.Pickup | not foeVisible && lootHere bpos = pickup aid
      aStrategy Ability.Pickup = return mzero
      aStrategy Ability.Wander = wander aid
      aStrategy ab             = assert `failure` "unexpected ability"
                                        `twith`(ab, actorAbilities)
      sumS abis = do
        fs <- mapM aStrategy abis
        return $! msum fs
      sumF abis = do
        fs <- mapM aFrequency abis
        return $! msum fs
      combineDistant as = fmap liftFrequency $ sumF as
  sumPrefix <- sumS prefix
  comDistant <- combineDistant distant
  sumSuffix <- sumS suffix
  return $! sumPrefix .| comDistant .| sumSuffix
            -- Wait until friends sidestep; ensures strategy is never empty.
            -- TODO: try to switch leader away before that (we already
            -- switch him afterwards)
            .| waitBlockNow aid

-- | A strategy to always just wait.
waitBlockNow :: ActorId -> Strategy CmdTakeTimeSer
waitBlockNow aid = returN "wait" $ WaitSer aid

-- | Strategy for a dumb missile or a strongly hurled actor.
track :: MonadActionRO m => ActorId -> m (Strategy CmdTakeTimeSer)
track aid = do
  bpath <- getsState $ bpath . getActorBody aid
  return $! if isNothing bpath
            then reject
            else returN "SetPathSer" $ SetPathSer aid

-- TODO: (most?) animals don't pick up. Everybody else does.
pickup :: MonadActionRO m => ActorId -> m (Strategy CmdTakeTimeSer)
pickup aid = do
  body@Actor{bpos, blid} <- getsState $ getActorBody aid
  lvl <- getLevel blid
  actionPickup <- case EM.minViewWithKey $ lvl `atI` bpos of
    Nothing -> assert `failure` "pickup of empty pile" `twith` (aid, bpos, lvl)
    Just ((iid, k), _) -> do  -- pick up first item
      item <- getsState $ getItemBody iid
      let l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      return $! case assignLetter iid l body of
        Just _ -> returN "pickup" $ PickupSer aid iid k
        Nothing -> returN "pickup" $ WaitSer aid  -- TODO
  return $! actionPickup

-- Everybody melees in a pinch, even though some prefer ranged attacks.
melee :: MonadActionRO m
      => ActorId -> Point -> ActorId -> m (Strategy CmdTakeTimeSer)
melee aid fpos foeAid = do
  Actor{bpos} <- getsState $ getActorBody aid
  let foeAdjacent = adjacent bpos fpos  -- MeleeDistant
  return $! foeAdjacent .=> returN "melee" (MeleeSer aid foeAid)

-- Fast monsters don't pay enough attention to features.
triggerFreq :: MonadActionRO m
            => ActorId -> m (Frequency CmdTakeTimeSer)
triggerFreq aid = do
  cops@Kind.COps{cotile=Kind.Ops{okind}} <- getsState scops
  b@Actor{bpos, blid, bfid, boldpos} <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid
  lvl <- getLevel blid
  let spawn = isSpawnFact cops fact
      t = lvl `at` bpos
      feats = TileKind.tfeature $ okind t
      ben feat = case feat of
        F.Cause Effect.Escape | spawn -> 0  -- spawners lose if they escape
        F.Cause ef -> effectToBenefit cops b ef
        _ -> 0
      benFeat = zip (map ben feats) feats
      -- Probably recently switched levels or was pushed to another level.
      -- Do not repeatedly switch levels or push each other between levels.
      -- Consequently, AI won't dive many levels down with linked staircases.
      -- TODO: beware of stupid monsters that backtrack and so occupy stairs.
      recentlyAscended = bpos == boldpos
      -- Too fast to notice and use features.
      fast = bspeed b > speedNormal
  if recentlyAscended || fast then
    return mzero
  else
    return $! toFreq "triggerFreq" $ [ (benefit, TriggerSer aid (Just feat))
                                     | (benefit, feat) <- benFeat
                                     , benefit > 0 ]

-- Actors require sight to use ranged combat and intelligence to throw
-- or zap anything else than obvious physical missiles.
rangedFreq :: MonadActionRO m
           => Discovery -> ActorId -> Point -> m (Frequency CmdTakeTimeSer)
rangedFreq disco aid fpos = do
  cops@Kind.COps{ coactor=Kind.Ops{okind}
                , coitem=Kind.Ops{okind=iokind}
                , corule
                , cotile
                } <- getsState scops
  b@Actor{bkind, bpos, bfid, blid, bbag, binv} <- getsState $ getActorBody aid
  lvl@Level{lxsize, lysize} <- getLevel blid
  let mk = okind bkind
      tis = lvl `atI` bpos
  fact <- getsState $ \s -> sfactionD s EM.! bfid
  foes <- getsState $ actorNotProjList (isAtWar fact) blid
  let foesAdj = foesAdjacent lxsize lysize bpos foes
      posClear pos1 = Tile.hasFeature cotile F.Clear (lvl `at` pos1)
  -- TODO: also don't throw if any pos on path is visibly not accessible
  -- from previous (and tweak eps in bla to make it accessible).
  -- Also don't throw if target not in range.
  s <- getState
  let eps = 0
      bl = bla lxsize lysize eps bpos fpos  -- TODO:make an arg of projectGroupItem
      permitted = (if aiq mk >= 10 then ritemProject else ritemRanged)
                  $ Kind.stdRuleset corule
      throwFreq bag multi container =
        [ (- benefit * multi,
          ProjectSer aid fpos eps iid (container iid))
        | (iid, i) <- map (\iid -> (iid, getItemBody iid s))
                      $ EM.keys bag
        , let benefit =
                case jkind disco i of
                  Nothing -> -- TODO: (undefined, 0)   --- for now, cheating
                    effectToBenefit cops b (jeffect i)
                  Just _ki ->
                    let _kik = iokind _ki
                        _unneeded = isymbol _kik
                    in effectToBenefit cops b (jeffect i)
        , benefit < 0
        , jsymbol i `elem` permitted ]
  case bl of
    Just (pos1 : _) -> do
      mab <- getsState $ posToActor pos1 blid
      if not foesAdj  -- ProjectBlockFoes
         && asight mk
         && posClear pos1  -- ProjectBlockTerrain
         && maybe True (bproj . snd . fst) mab  -- ProjectBlockActor
      then return $! toFreq "throwFreq"
           $ throwFreq bbag 3 (actorContainer aid binv)
             ++ throwFreq tis 6 (const $ CFloor blid bpos)
      else return $! toFreq "throwFreq blocked" []
    _ -> return $! toFreq "throwFreq no bla" []  -- ProjectAimOnself

-- Tools use requires significant intelligence and sometimes literacy.
toolsFreq :: MonadActionRO m
          => Discovery -> ActorId -> m (Frequency CmdTakeTimeSer)
toolsFreq disco aid = do
  cops@Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b@Actor{bkind, bpos, blid, bbag, binv} <- getsState $ getActorBody aid
  lvl <- getLevel blid
  s <- getState
  let tis = lvl `atI` bpos
      mk = okind bkind
      mastered | aiq mk < 5 = ""
               | aiq mk < 10 = "!"
               | otherwise = "!?"  -- literacy required
      useFreq bag multi container =
        [ (benefit * multi, ApplySer aid iid (container iid))
        | (iid, i) <- map (\iid -> (iid, getItemBody iid s))
                      $ EM.keys bag
        , let benefit =
                case jkind disco i of
                  Nothing -> 30  -- experimenting is fun
                  Just _ki -> effectToBenefit cops b $ jeffect i
        , benefit > 0
        , jsymbol i `elem` mastered ]
  return $! toFreq "useFreq" $
    useFreq bbag 1 (actorContainer aid binv)
    ++ useFreq tis 2 (const $ CFloor blid bpos)

-- TODO: express fully in MonadActionRO
-- TODO: separate out bumping into solid tiles
-- TODO: also close doors; then stupid members of the party won't see them,
-- but it's assymetric warfare: rather harm humans than help party members
-- | AI finds interesting moves in the absense of visible foes.
-- This strategy can be null (e.g., if the actor is blocked by friends).
moveStrategy :: MonadActionRO m
             => Kind.COps -> ActorId -> State -> Maybe (Point, Bool)
             -> m (Strategy Vector)
moveStrategy cops aid s mFoe =
  return $! case mFoe of
    -- Target set and we chase the foe or his last position or another target.
    Just (fpos, _) ->
      let towardsFoe =
            let tolerance | adjacent bpos fpos = 0
                          | otherwise = 1
                foeDir = towards bpos fpos
            in only (\x -> euclidDistSq foeDir x <= tolerance)
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
            $ groupBy ((==) `on` snd)
            $ sortBy (flip compare `on` snd)
            $ filter (\(_, sm) -> sm > timeZero)
            $ map (\x ->
                      let sml = EM.findWithDefault
                                  timeZero (bpos `shift` x) lsmell
                      in (x, sml `timeAdd` timeNegate ltime))
                sensible
      in asmell mk .=> foldr ((.|)
                              . liftFrequency
                              . uniformFreq "smell k") reject smells
         .| moveOpenable  -- no enemy in sight, explore doors
         .| moveClear
 where
  Kind.COps{cotile, coactor=Kind.Ops{okind}} = cops
  lvl@Level{lsmell, lxsize, lysize, ltime} = sdungeon s EM.! blid
  Actor{bkind, bpos, boldpos, bfid, blid} = getActorBody aid s
  mk = okind bkind
  lootHere x = not $ EM.null $ lvl `atI` x
  onlyLoot = onlyMoves lootHere
  interestHere x = let t = lvl `at` x
                       ts = map (lvl `at`) $ vicinity lxsize lysize x
                   in -- Blind actors tend to reveal/forget repeatedly.
                      asight mk && Tile.hasFeature cotile F.Suspect t
                      || (x == bpos || accessible cops lvl x bpos)
                             -- E.g., a room entrance
                         && (Tile.hasFeature cotile F.Dark t
                             && any (not . Tile.hasFeature cotile F.Dark) ts
                             -- E.g., stairs.
                             || not (null (Tile.causeEffects cotile t)))
  onlyInterest = onlyMoves interestHere
  bdirAI | bpos == boldpos = Nothing
         | otherwise = Just $ towards boldpos bpos
  onlyKeepsDir k =
    only (\x -> maybe True (\d -> euclidDistSq d x <= k) bdirAI)
  onlyKeepsDir_9 = only (\x -> maybe True (\d -> neg x /= d) bdirAI)
  foeVisible = fmap snd mFoe == Just True
  -- TODO: aiq 16 leads to robotic, repetitious, looping movement;
  -- either base it off some new stat or wait until pathfinding,
  -- which will eliminate the loops
  moveIQ | foeVisible = onlyKeepsDir_9 moveRandomly  -- danger, be flexible
         | otherwise =
       aiq mk > 15 .=> onlyKeepsDir 0 moveRandomly
    .| aiq mk > 10 .=> onlyKeepsDir 1 moveRandomly
    .| aiq mk > 5  .=> onlyKeepsDir 2 moveRandomly
    .| onlyKeepsDir_9 moveRandomly
  interestFreq | interestHere bpos =
    -- Don't detour towards an interest if already on one.
    mzero
               | otherwise =
    -- Prefer interests, but don't exclude other focused moves.
    scaleFreq 10 $ bestVariant $ onlyInterest $ onlyKeepsDir 2 moveRandomly
  interestIQFreq = interestFreq `mplus` bestVariant moveIQ
  moveClear    =
    onlyMoves (not . bumpableHere cops lvl foeVisible (asight mk)) moveFreely
  moveOpenable =
    onlyMoves (bumpableHere cops lvl foeVisible (asight mk)) moveFreely
  -- Ignore previously ignored loot, to prevent repetition.
  moveNewLoot = onlyLoot (onlyKeepsDir 2 moveRandomly)
  moveFreely = moveNewLoot
               .| liftFrequency interestIQFreq
               .| moveIQ  -- @bestVariant moveIQ@ may be excluded elsewhere
               .| moveRandomly
  onlyMoves :: (Point -> Bool) -> Strategy Vector -> Strategy Vector
  onlyMoves p = only (\x -> p (bpos `shift` x))
  moveRandomly :: Strategy Vector
  moveRandomly = liftFrequency $ uniformFreq "moveRandomly" sensible
  accessibleHere = accessible cops lvl bpos
  fact = sfactionD s EM.! bfid
  friends = actorList (not . isAtWar fact) blid s
  noFriends | asight mk = unoccupied friends
            | otherwise = const True
  isSensible l = noFriends l
                 && (accessibleHere l
                     || bumpableHere cops lvl foeVisible (asight mk) l)
  sensible = filter (isSensible . (bpos `shift`)) moves

bumpableHere :: Kind.COps -> Level -> Bool -> Bool -> Point -> Bool
bumpableHere Kind.COps{cotile} lvl foeVisible asight pos =
  let t = lvl `at` pos  -- cannot hold items, so OK
  in Tile.openable cotile t
     || -- Try to find hidden doors only if no foe in sight and not blind.
        -- Blind actors forget their search results too quickly.
        asight && not foeVisible && Tile.hasFeature cotile F.Suspect t

chase :: MonadActionRO m
      => ActorId -> (Point, Bool) -> m (Strategy CmdTakeTimeSer)
chase aid foe@(_, foeVisible) = do
  cops <- getsState scops
  -- Target set and we chase the foe or offer null strategy if we can't.
  -- The foe is visible, or we remember his last position.
  let mFoe = Just foe
      fight = not foeVisible  -- don't pick fights if the real foe is close
  s <- getState
  str <- moveStrategy cops aid s mFoe
  if fight
    then Traversable.mapM (moveOrRunAid False aid) str
    else Traversable.mapM (moveOrRunAid True aid) str

wander :: MonadActionRO m
       => ActorId -> m (Strategy CmdTakeTimeSer)
wander aid = do
  cops <- getsState scops
  -- Target set, but we don't chase the foe, e.g., because we are blocked
  -- or we cannot chase at all.
  let mFoe = Nothing
  s <- getState
  str <- moveStrategy cops aid s mFoe
  Traversable.mapM (moveOrRunAid False aid) str

-- | Actor moves or searches or alters or attacks. Displaces if @run@.
moveOrRunAid :: MonadActionRO m
           => Bool -> ActorId -> Vector -> m CmdTakeTimeSer
moveOrRunAid run source dir = do
  cops@Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
  -- We start by checking actors at the the target position,
  -- which gives a partial information (actors can be invisible),
  -- as opposed to accessibility (and items) which are always accurate
  -- (tiles can't be invisible).
  tgts <- getsState $ posToActors tpos lid
  case tgts of
    [((target, _), _)] | run ->  -- can be a foe, as well as a friend
      if accessible cops lvl spos tpos then
        -- Displacing requires accessibility.
        return $! DisplaceSer source target
      else
        -- If cannot displace, hit. No DisplaceAccess.
        return $! MeleeSer source target
    ((target, _), _) : _ ->  -- can be a foe, as well as a friend
      -- Attacking does not require full access, adjacency is enough.
      return $! MeleeSer source target
    [] -> do  -- move or search or alter
      if accessible cops lvl spos tpos then
        -- Movement requires full access.
        return $! MoveSer source dir
        -- The potential invisible actor is hit.
      else if not $ EM.null $ lvl `atI` tpos then
        -- This is, e.g., inaccessible open door with an item in it.
        assert `failure` "AI causes AlterBlockItem" `twith` (run, source, dir)
      else if not (Tile.hasFeature cotile F.Walkable t)  -- not implied
              && (Tile.hasFeature cotile F.Suspect t
                  || Tile.openable cotile t
                  || Tile.closable cotile t
                  || Tile.changeable cotile t) then
        -- No access, so search and/or alter the tile.
        return $! AlterSer source tpos Nothing
      else
        -- Boring tile, no point bumping into it, do WaitSer if really idle.
        assert `failure` "AI causes MoveNothing or AlterNothing"
               `twith` (run, source, dir)

-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
effectToBenefit :: Kind.COps -> Actor -> Effect.Effect Int -> Int
effectToBenefit Kind.COps{coactor=Kind.Ops{okind}} b eff =
  let kind = okind $ bkind b
      deep k = signum k == signum (fromEnum $ blid b)
  in case eff of
    Effect.NoEffect -> 0
    (Effect.Heal p) -> 10 * min p (Random.maxDice (ahp kind) - bhp b)
    (Effect.Hurt _ p) -> -(p * 10)     -- TODO: dice ignored, not capped
    Effect.Mindprobe{} -> 0            -- AI can't benefit yet
    Effect.Dominate -> -100
    (Effect.CallFriend p) -> p * 100
    Effect.Summon{} -> 1               -- may or may not spawn a friendly
    (Effect.CreateItem p) -> p * 20
    Effect.ApplyPerfume -> 0
    Effect.Regeneration{} -> 0         -- bigger benefit from carrying around
    Effect.Searching{} -> 0
    (Effect.Ascend k) | deep k -> 500  -- AI likes to explore deep down
    Effect.Ascend{} -> 1
    Effect.Escape -> 1000              -- AI wants to win
