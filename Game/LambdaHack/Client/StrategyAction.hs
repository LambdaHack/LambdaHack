{-# LANGUAGE OverloadedStrings #-}
-- | AI strategy operations implemented with the 'Action' monad.
module Game.LambdaHack.Client.StrategyAction
  ( targetStrategy, actionStrategy, visibleFoes
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Function
import Data.List
import Data.Maybe

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
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

-- TODO: express many (all?) functions as MonadActionRO

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: MonadClient m
               => ActorId -> [Ability]
               -> m (Strategy (Maybe Target))
targetStrategy aid factionAbilities = do
  btarget <- getsClient $ getTarget aid
  fper <- getsClient sfper
  reacquireTgt fper aid btarget factionAbilities

visibleFoes :: MonadActionRO m
            => FactionPers -> ActorId -> m [(ActorId, Actor)]
visibleFoes fper aid = do
  b <- getsState $ getActorBody aid
  assert (not $ bproj b) skip  -- would work, but is probably a bug
  let per = fper EM.! blid b
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  foes <- getsState $ actorNotProjAssocs (isAtWar fact) (blid b)
  return $! filter (actorSeesLoc per aid . bpos . snd) foes

reacquireTgt :: MonadActionRO m
             => FactionPers -> ActorId -> Maybe Target -> [Ability]
             -> m (Strategy (Maybe Target))
reacquireTgt fper aid btarget factionAbilities = do
  cops@Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  assert (not $ bproj b) skip  -- would work, but is probably a bug
  lvl@Level{lxsize} <- getsState $ \s -> sdungeon s EM.! blid b
  visFoes <- visibleFoes fper aid
  actorD <- getsState sactorD
  -- TODO: set distant targets so that monsters behave as if they have
  -- a plan. We need pathfinding for that.
  noFoes :: Strategy (Maybe Target) <- getsState $ \s ->
    (Just . TPos . (bpos b `shift`)) `liftM` moveStrategy cops aid s Nothing
  let per = fper EM.! blid b
      mk = okind $ bkind b
      actorAbilities = acanDo mk `intersect` factionAbilities
      focused = actorSpeed coactor b <= speedNormal
                -- Don't focus on a distant enemy, when you can't chase him.
                -- TODO: or only if another enemy adjacent? consider Flee?
                && Ability.Chase `elem` actorAbilities
      closest :: Strategy (Maybe Target)
      closest =
        let distB = chessDist lxsize (bpos b)
            foeDist = map (\(_, body) -> distB (bpos body)) visFoes
            minDist | null foeDist = maxBound
                    | otherwise = minimum foeDist
            minFoes =
              filter (\(_, body) -> distB (bpos body) == minDist) visFoes
            minTargets = map (\(a, body) ->
                                Just $ TEnemy a $ bpos body) minFoes
            minTgtS = liftFrequency $ uniformFreq "closest" minTargets
        in minTgtS .| noFoes .| returN "TCursor" Nothing  -- never empty
      reacquire :: Maybe Target -> Strategy (Maybe Target)
      reacquire tgt =
        case tgt of
          Just (TEnemy a ll) | focused ->  -- chase even if enemy dead, to loot
            case fmap bpos $ EM.lookup a actorD of
              Just l | actorSeesLoc per aid l ->
                -- prefer visible (and alive) foes
                returN "TEnemy" $ Just $ TEnemy a l
              _ -> if null visFoes         -- prefer visible foes to positions
                      && bpos b /= ll      -- not yet reached the last pos
                   then returN "last known" $ Just $ TPos ll
                                           -- chase the last known pos
                   else closest
          Just TEnemy{} -> closest         -- just pick the closest foe
          Just (TPos pos) | bpos b == pos -> closest  -- already reached pos
          Just (TPos pos) | not $ bumpableHere cops lvl False pos ->
            closest  -- no longer bumpable, even assuming no foes
          Just TPos{} | null visFoes -> returN "TPos" tgt
                                           -- nothing visible, go to pos
          Just TPos{} -> closest           -- prefer visible foes
          Nothing -> closest
  return $! reacquire btarget

-- | AI strategy based on actor's sight, smell, intelligence, etc. Never empty.
actionStrategy :: MonadClient m
               => ActorId -> [Ability]
               -> m (Strategy CmdSerTakeTime)
actionStrategy aid factionAbilities = do
  cops <- getsState scops
  s <- getState
  btarget <- getsClient $ getTarget aid
  disco <- getsClient sdisco
  return $! proposeAction cops aid btarget disco s factionAbilities

proposeAction :: Kind.COps -> ActorId
              -> Maybe Target -> Discovery -> State -> [Ability]
              -> Strategy CmdSerTakeTime
proposeAction cops aid btarget disco s factionAbilities =
  sumS prefix .| combineDistant distant .| sumS suffix
  .| waitBlockNow aid  -- wait until friends sidestep, ensures never empty
 where
  Kind.COps{coactor=Kind.Ops{okind}} = cops
  Actor{bkind, bpos} = getActorBody aid s
  (fpos, mfAid) =
    case btarget of
      Just (TEnemy foeAid l) -> (l, Just foeAid)
      Just (TPos l) -> (l, Nothing)
      Nothing -> (bpos, Nothing)  -- an actor blocked by friends or a missile
  foeVisible = isJust mfAid
  combineDistant as = liftFrequency $ sumF as
  -- TODO: Ranged and Tools should only be triggered in some situations.
  aFrequency :: Ability -> Frequency CmdSerTakeTime
  aFrequency Ability.Ranged = if foeVisible
                              then rangedFreq cops aid disco s fpos
                              else mzero
  aFrequency Ability.Tools  = if foeVisible
                              then toolsFreq cops aid disco s
                              else mzero
  aFrequency Ability.Chase  = if fpos /= bpos
                              then chaseFreq
                              else mzero
  aFrequency _              = assert `failure` distant
  chaseFreq =
    scaleFreq 30 $ bestVariant $ chase cops aid s (fpos, foeVisible)
  aStrategy :: Ability -> Strategy CmdSerTakeTime
  aStrategy Ability.Track  = track cops aid s
  aStrategy Ability.Heal   = mzero  -- TODO
  aStrategy Ability.Flee   = mzero  -- TODO
  aStrategy Ability.Melee | Just foeAid <- mfAid = melee aid s fpos foeAid
  aStrategy Ability.Melee  = mzero
  aStrategy Ability.Pickup = not foeVisible .=> pickup aid s
  aStrategy Ability.Wander = wander cops aid s
  aStrategy _              = assert `failure` actorAbilities
  actorAbilities = acanDo (okind bkind) `intersect` factionAbilities
  isDistant = (`elem` [Ability.Ranged, Ability.Tools, Ability.Chase])
  (prefix, rest)    = break isDistant actorAbilities
  (distant, suffix) = partition isDistant rest
  sumS = msum . map aStrategy
  sumF = msum . map aFrequency

-- | A strategy to always just wait.
waitBlockNow :: ActorId -> Strategy CmdSerTakeTime
waitBlockNow aid = returN "wait" $ WaitSer aid

-- | Strategy for dumb missiles.
track :: Kind.COps -> ActorId -> State -> Strategy CmdSerTakeTime
track cops aid s =
  strat
 where
  lvl = sdungeon s EM.! blid
  b@Actor{bpos, bpath, blid} = getActorBody aid s
  clearPath = returN "ClearPathSer" $ SetPathSer aid []
  strat = case bpath of
    Nothing -> reject
    Just [] -> assert `failure` (aid, b, s)
    -- TODO: instead let server do this in MoveSer, abort and handle in loop:
    Just (d : _) | not $ accessibleDir cops lvl bpos d -> clearPath
    Just lv -> returN "SetPathSer" $ SetPathSer aid lv

pickup :: ActorId -> State -> Strategy CmdSerTakeTime
pickup aid s =
  lootHere bpos .=> actionPickup
 where
  lvl = sdungeon s EM.! blid
  body@Actor{bpos, blid} = getActorBody aid s
  lootHere x = not $ EM.null $ lvl `atI` x
  actionPickup = case EM.minViewWithKey $ lvl `atI` bpos of
    Nothing -> assert `failure` (aid, bpos, lvl)
    Just ((iid, k), _) ->  -- pick up first item
      let item = getItemBody iid s
          l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      in case assignLetter iid l body of
        Just l2 -> returN "pickup" $ PickupSer aid iid k l2
        Nothing -> returN "pickup" $ WaitSer aid  -- TODO

melee :: ActorId -> State -> Point -> ActorId -> Strategy CmdSerTakeTime
melee aid s fpos foeAid =
  foeAdjacent .=> returN "melee" (MeleeSer aid foeAid)
 where
  Level{lxsize} = sdungeon s EM.! blid
  Actor{bpos, blid} = getActorBody aid s
  foeAdjacent = adjacent lxsize bpos fpos

rangedFreq :: Kind.COps -> ActorId -> Discovery -> State -> Point
           -> Frequency CmdSerTakeTime
rangedFreq cops aid disco s fpos =
  toFreq "throwFreq" $
    case bl of
      Just (pos1 : _) ->
        if not foesAdj  -- ProjectBlockFoes
           && asight mk
           && posClear pos1  -- ProjectBlockTerrain
           && unoccupied as pos1  -- ProjectBlockActor
        then throwFreq bbag 3 (actorContainer aid binv)
             ++ throwFreq tis 6 (const $ CFloor blid bpos)
        else []
      _ -> []  -- ProjectAimOnself
 where
  Kind.COps{ coactor=Kind.Ops{okind}
           , coitem=Kind.Ops{okind=iokind}
           , corule
           , cotile
           } = cops
  lvl@Level{lxsize, lysize} = sdungeon s EM.! blid
  Actor{bkind, bpos, bfid, blid, bbag, binv} = getActorBody aid s
  mk = okind bkind
  tis = lvl `atI` bpos
  fact = sfactionD s EM.! bfid
  foes = actorNotProjList (isAtWar fact) blid s
  foesAdj = foesAdjacent lxsize lysize bpos foes
  posClear pos1 = Tile.hasFeature cotile F.Clear (lvl `at` pos1)
  as = actorList (const True) blid s
  -- TODO: also don't throw if any pos on path is visibly not accessible
  -- from previous (and tweak eps in bla to make it accessible).
  -- Also don't throw if target not in range.
  eps = 0
  bl = bla lxsize lysize eps bpos fpos  -- TODO:make an arg of projectGroupItem
  throwFreq bag multi container =
    [ (- benefit * multi,
      ProjectSer aid fpos eps iid (container iid))
    | (iid, i) <- map (\iid -> (iid, getItemBody iid s))
                  $ EM.keys bag
    , let benefit =
            case jkind disco i of
              Nothing -> -- TODO: (undefined, 0)   --- for now, cheating
                Effect.effectToBenefit (jeffect i)
              Just _ki ->
                let _kik = iokind _ki
                    _unneeded = isymbol _kik
                in Effect.effectToBenefit (jeffect i)
    , benefit < 0
      -- Wasting weapons and armour would be too cruel to the player.
    , jsymbol i `elem` ritemProject (Kind.stdRuleset corule) ]

toolsFreq :: Kind.COps -> ActorId -> Discovery -> State -> Frequency CmdSerTakeTime
toolsFreq cops aid disco s =
  toFreq "quaffFreq"
  $ quaffFreq bbag 1 (actorContainer aid binv)
  ++ quaffFreq tis 2 (const $ CFloor blid bpos)
 where
  Kind.COps{coitem=Kind.Ops{okind=_iokind}} = cops
  Actor{bpos, blid, bbag, binv} = getActorBody aid s
  lvl = sdungeon s EM.! blid
  tis = lvl `atI` bpos
  quaffFreq bag multi container =
    [ (benefit * multi, ApplySer aid iid (container iid))
    | (iid, i) <- map (\iid -> (iid, getItemBody iid s))
                  $ EM.keys bag
    , let benefit =
            case jkind disco i of
              Nothing -> 30  -- experimenting is fun
              Just _ki -> Effect.effectToBenefit $ jeffect i
    , benefit > 0
    , jsymbol i == '!' ]

-- TODO: separate out bumping into solid tiles
-- TODO: also close doors; then stupid members of the party won't see them,
-- but it's assymetric warfare: rather harm humans than help party members
-- | AI finds interesting moves in the absense of visible foes.
-- This strategy can be null (e.g., if the actor is blocked by friends).
moveStrategy :: Kind.COps -> ActorId -> State -> Maybe (Point, Bool)
             -> Strategy Vector
moveStrategy cops aid s mFoe =
  case mFoe of
    -- Target set and we chase the foe or his last position or another target.
    Just (fpos, _) ->
      let towardsFoe =
            let tolerance | adjacent lxsize bpos fpos = 0
                          | otherwise = 1
                foeDir = towards lxsize bpos fpos
            in only (\x -> euclidDistSq lxsize foeDir x <= tolerance)
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
                   in Tile.hasFeature cotile F.Exit t
                      -- Blind actors tend to reveal/forget repeatedly.
                      || asight mk && Tile.hasFeature cotile F.Suspect t
                      -- Lit indirectly. E.g., a room entrance.
                      || (not (Tile.hasFeature cotile F.Lit t)
                          && (x == bpos || accessible cops lvl x bpos)
                          && any (Tile.hasFeature cotile F.Lit) ts)
  onlyInterest = onlyMoves interestHere
  bdirAI | bpos == boldpos = Nothing
         | otherwise = Just $ towards lxsize boldpos bpos
  onlyKeepsDir k =
    only (\x -> maybe True (\d -> euclidDistSq lxsize d x <= k) bdirAI)
  onlyKeepsDir_9 = only (\x -> maybe True (\d -> neg x /= d) bdirAI)
  foeVisible = fmap snd mFoe == Just True
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
    scaleFreq 5 $ bestVariant $ onlyInterest $ onlyKeepsDir 2 moveRandomly
  interestIQFreq = interestFreq `mplus` bestVariant moveIQ
  moveClear    = onlyMoves (not . bumpableHere cops lvl foeVisible) moveFreely
  moveOpenable = onlyMoves (bumpableHere cops lvl foeVisible) moveFreely
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
  isSensible l = noFriends l && (accessibleHere l
                                 || bumpableHere cops lvl foeVisible l)
  sensible = filter (isSensible . (bpos `shift`)) (moves lxsize)

bumpableHere :: Kind.COps -> Level -> Bool ->  Point -> Bool
bumpableHere Kind.COps{cotile} lvl foeVisible pos  =
  let t = lvl `at` pos
  in Tile.openable cotile t
     || -- Try to find hidden doors only if no foe in sight.
        not foeVisible && Tile.hasFeature cotile F.Suspect t

chase :: Kind.COps -> ActorId -> State -> (Point, Bool)
      -> Strategy CmdSerTakeTime
chase cops aid s foe@(_, foeVisible) =
  -- Target set and we chase the foe or offer null strategy if we can't.
  -- The foe is visible, or we remember his last position.
  let mFoe = Just foe
      fight = not foeVisible  -- don't pick fights if the real foe is close
  in if fight
     then MoveSer {-TODO: ExploreSer-} aid `liftM` moveStrategy cops aid s mFoe
     else MoveSer {-TODO: RunSer-} aid `liftM` moveStrategy cops aid s mFoe

wander :: Kind.COps -> ActorId -> State -> Strategy CmdSerTakeTime
wander cops aid s =
  -- Target set, but we don't chase the foe, e.g., because we are blocked
  -- or we cannot chase at all.
  let mFoe = Nothing
  in MoveSer {-TODO: ExploreSer-} aid `liftM` moveStrategy cops aid s mFoe
