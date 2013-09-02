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
targetStrategy actor factionAbilities = do
  btarget <- getsClient $ getTarget actor
  fper <- getsClient sfper
  reacquireTgt fper actor btarget factionAbilities

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
  Level{lxsize} <- getsState $ \s -> sdungeon s EM.! blid b
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
        let distB p = chessDist lxsize (bpos b) p
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
          Just TPos{} | null visFoes -> returN "TPos" tgt
                                           -- nothing visible, go to pos
          Just TPos{} -> closest           -- prefer visible foes
          Nothing -> closest
  return $! reacquire btarget

-- | AI strategy based on actor's sight, smell, intelligence, etc. Never empty.
actionStrategy :: MonadClient m
               => ActorId -> [Ability]
               -> m (Strategy CmdSer)
actionStrategy actor factionAbilities = do
  cops <- getsState scops
  s <- getState
  btarget <- getsClient $ getTarget actor
  disco <- getsClient sdisco
  return $! proposeAction cops actor btarget disco s factionAbilities

proposeAction :: Kind.COps -> ActorId
              -> Maybe Target -> Discovery -> State -> [Ability]
              -> Strategy CmdSer
proposeAction cops actor btarget disco s factionAbilities =
  sumS prefix .| combineDistant distant .| sumS suffix
  .| waitBlockNow actor  -- wait until friends sidestep, ensures never empty
 where
  Kind.COps{coactor=Kind.Ops{okind}} = cops
  Actor{bkind, bpos} = getActorBody actor s
  (fpos, foeVisible) =
    case btarget of
      Just (TEnemy _ l) -> (l, True)
      Just (TPos l) -> (l, False)
      Nothing -> (bpos, False)  -- an actor blocked by friends or a missile
  combineDistant as = liftFrequency $ sumF as
  aFrequency :: Ability -> Frequency CmdSer
  aFrequency Ability.Ranged = if foeVisible
                              then rangedFreq cops actor disco s fpos
                              else mzero
  aFrequency Ability.Tools  = if foeVisible
                              then toolsFreq cops actor disco s
                              else mzero
  aFrequency Ability.Chase  = if fpos /= bpos
                              then chaseFreq
                              else mzero
  aFrequency _              = assert `failure` distant
  chaseFreq =
    scaleFreq 30 $ bestVariant $ chase cops actor s (fpos, foeVisible)
  aStrategy :: Ability -> Strategy CmdSer
  aStrategy Ability.Track  = track cops actor s
  aStrategy Ability.Heal   = mzero  -- TODO
  aStrategy Ability.Flee   = mzero  -- TODO
  aStrategy Ability.Melee  = foeVisible .=> melee actor s fpos
  aStrategy Ability.Pickup = not foeVisible .=> pickup actor s
  aStrategy Ability.Wander = wander cops actor s
  aStrategy _              = assert `failure` actorAbilities
  actorAbilities = acanDo (okind bkind) `intersect` factionAbilities
  isDistant = (`elem` [Ability.Ranged, Ability.Tools, Ability.Chase])
  (prefix, rest)    = break isDistant actorAbilities
  (distant, suffix) = partition isDistant rest
  sumS = msum . map aStrategy
  sumF = msum . map aFrequency

-- | A strategy to always just wait.
waitBlockNow :: ActorId -> Strategy CmdSer
waitBlockNow actor = returN "wait" $ WaitSer actor

-- | Strategy for dumb missiles.
track :: Kind.COps -> ActorId -> State -> Strategy CmdSer
track cops actor s =
  strat
 where
  lvl = sdungeon s EM.! blid
  b@Actor{bpos, bpath, blid} = getActorBody actor s
  clearPath = returN "ClearPathSer" $ SetPathSer actor []
  strat = case bpath of
    Just [] -> assert `failure` (actor, b, s)
    -- TODO: instead let server do this in MoveSer, abort and handle in loop:
    Just (d : _) | not $ accessibleDir cops lvl bpos d -> clearPath
    Just lv ->
      returN "SetPathSer; MoveSer" $ SetPathSer actor lv
    Nothing -> reject

pickup :: ActorId -> State -> Strategy CmdSer
pickup actor s =
  lootHere bpos .=> actionPickup
 where
  lvl = sdungeon s EM.! blid
  body@Actor{bpos, blid} = getActorBody actor s
  lootHere x = not $ EM.null $ lvl `atI` x
  actionPickup = case EM.minViewWithKey $ lvl `atI` bpos of
    Nothing -> assert `failure` (actor, bpos, lvl)
    Just ((iid, k), _) ->  -- pick up first item
      let item = getItemBody iid s
          l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      in case assignLetter iid l body of
        Just l2 -> returN "pickup" $ PickupSer actor iid k l2
        Nothing -> returN "pickup" $ WaitSer actor

melee :: ActorId -> State -> Point -> Strategy CmdSer
melee actor s fpos =
  foeAdjacent .=> (returN "melee" $ MoveSer actor dir)
 where
  Level{lxsize} = sdungeon s EM.! blid
  Actor{bpos, blid} = getActorBody actor s
  foeAdjacent = adjacent lxsize bpos fpos
  dir = displacement bpos fpos

rangedFreq :: Kind.COps -> ActorId -> Discovery -> State -> Point
           -> Frequency CmdSer
rangedFreq cops actor disco s fpos =
  toFreq "throwFreq" $
    case bl of
      Just (pos1 : _) ->
        if not foesAdj
           && asight mk
           && accessible cops lvl bpos pos1       -- first accessible
           && isNothing (posToActor pos1 blid s)  -- no friends on first
        then throwFreq bbag 3 (actorContainer actor binv)
             ++ throwFreq tis 6 (const $ CFloor blid bpos)
        else []
      _ -> []
 where
  Kind.COps{ coactor=Kind.Ops{okind}
           , coitem=Kind.Ops{okind=iokind}
           , corule
           } = cops
  lvl@Level{lxsize, lysize} = sdungeon s EM.! blid
  Actor{bkind, bpos, bfid, blid, bbag, binv} = getActorBody actor s
  mk = okind bkind
  tis = lvl `atI` bpos
  fact = sfactionD s EM.! bfid
  foes = actorNotProjAssocs (isAtWar fact) blid s
  foesAdj = foesAdjacent lxsize lysize bpos (map snd foes)
  -- TODO: also don't throw if any pos on path is visibly not accessible
  -- from previous (and tweak eps in bla to make it accessible).
  -- Also don't throw if target not in range.
  eps = 0
  bl = bla lxsize lysize eps bpos fpos  -- TODO:make an arg of projectGroupItem
  throwFreq bag multi container =
    [ (benefit * multi,
       ProjectSer actor fpos eps iid (container iid))
    | (iid, i) <- map (\iid -> (iid, getItemBody iid s))
                  $ EM.keys bag,
      let (ik, benefit) =
            case jkind disco i of
              Nothing -> (undefined, 0)
              Just ki ->
                let kik = iokind ki
                in (kik, Effect.effectToBenefit (jeffect i)),
      benefit > 0,
      -- Wasting weapons and armour would be too cruel to the player.
      isymbol ik `elem` (ritemProject $ Kind.stdRuleset corule)]

toolsFreq :: Kind.COps -> ActorId -> Discovery -> State -> Frequency CmdSer
toolsFreq cops actor disco s =
  toFreq "quaffFreq"
  $ quaffFreq bbag 1 (actorContainer actor binv)
  ++ quaffFreq tis 2 (const $ CFloor blid bpos)
 where
  Kind.COps{coitem=Kind.Ops{okind=iokind}} = cops
  Actor{bpos, blid, bbag, binv} = getActorBody actor s
  lvl = sdungeon s EM.! blid
  tis = lvl `atI` bpos
  quaffFreq bag multi container =
    [ (benefit * multi, ApplySer actor iid (container iid))
    | (iid, i) <- map (\iid -> (iid, getItemBody iid s))
                  $ EM.keys bag,
      let (ik, benefit) =
            case jkind disco i of
              Nothing -> (undefined, 0)
              Just ki ->
                let kik = iokind ki
                in (kik, Effect.effectToBenefit (jeffect i)),
      benefit > 0, isymbol ik == '!']

-- TODO: also close doors; then stupid members of the party won't see them,
-- but it's assymetric warfare: rather harm humans than help party members
-- | AI finds interesting moves in the absense of visible foes.
-- This strategy can be null (e.g., if the actor is blocked by friends).
moveStrategy :: Kind.COps -> ActorId -> State -> Maybe (Point, Bool)
             -> Strategy Vector
moveStrategy cops actor s mFoe =
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
            $ groupBy ((==) `on` snd)
            $ sortBy (flip compare `on` snd)
            $ filter (\ (_, sm) -> sm > timeZero)
            $ map (\ x ->
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
  Kind.COps{ cotile
           , coactor=Kind.Ops{okind}
           } = cops
  lvl@Level{lsmell, lxsize, lysize, ltime} = sdungeon s EM.! blid
  Actor{bkind, bpos, boldpos, bfid, blid} = getActorBody actor s
  mk = okind bkind
  lootHere x = not $ EM.null $ lvl `atI` x
  onlyLoot = onlyMoves lootHere bpos
  interestHere x = let t = lvl `at` x
                       ts = map (lvl `at`) $ vicinity lxsize lysize x
                   in Tile.hasFeature cotile F.Exit t
                      -- Lit indirectly. E.g., a room entrance.
                      || (not (Tile.hasFeature cotile F.Lit t)
                          && any (Tile.hasFeature cotile F.Lit) ts)
  onlyInterest = onlyMoves interestHere bpos
  bdirAI | bpos == boldpos = Nothing
         | otherwise = Just $ towards lxsize boldpos bpos
  onlyKeepsDir k =
    only (\ x -> maybe True (\d -> euclidDistSq lxsize d x <= k) bdirAI)
  onlyKeepsDir_9 = only (\ x -> maybe True (\d -> neg x /= d) bdirAI)
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
  openableHere pos = Tile.hasFeature cotile F.Openable $ lvl `at` pos
  accessibleHere = accessible cops lvl bpos
  fact = sfactionD s EM.! bfid
  friends = actorList (not . (isAtWar fact)) blid s
  noFriends | asight mk = unoccupied friends
            | otherwise = const True
  isSensible l = noFriends l && (accessibleHere l || openableHere l)
  sensible = filter (isSensible . (bpos `shift`)) (moves lxsize)

chase :: Kind.COps -> ActorId -> State -> (Point, Bool) -> Strategy CmdSer
chase cops actor s foe@(_, foeVisible) =
  -- Target set and we chase the foe or offer null strategy if we can't.
  -- The foe is visible, or we remember his last position.
  -- TODO: explore if a possible secret
  let mFoe = Just foe
      fight = not foeVisible  -- don't pick fights if the real foe is close
  in if fight
     then MoveSer actor `liftM` moveStrategy cops actor s mFoe
     else RunSer actor `liftM` moveStrategy cops actor s mFoe

wander :: Kind.COps -> ActorId -> State -> Strategy CmdSer
wander cops actor s =
  -- Target set, but we don't chase the foe, e.g., because we are blocked
  -- or we cannot chase at all.
  -- TODO: explore if a possible secret
  let mFoe = Nothing
  in MoveSer actor `liftM` moveStrategy cops actor s mFoe
