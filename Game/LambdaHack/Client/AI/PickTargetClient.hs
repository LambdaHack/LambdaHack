-- | Let AI pick the best target for an actor.
module Game.LambdaHack.Client.AI.PickTargetClient
  ( targetStrategy
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe

import Game.LambdaHack.Client.AI.ConditionClient
import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: forall m. MonadClient m
               => ActorId -> ActorId -> m (Strategy (Target, PathEtc))
targetStrategy oldLeader aid = do
  cops@Kind.COps{ cotile=cotile@Kind.Ops{ouniqGroup}
                , coactor=coactor@Kind.Ops{okind}
                , cofaction=Kind.Ops{okind=fokind} } <- getsState scops
  modifyClient $ \cli -> cli { sbfsD = EM.delete aid (sbfsD cli)
                             , seps = seps cli + 773 }  -- randomize paths
  b <- getsState $ getActorBody aid
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  oldTgtUpdatedPath <- case mtgtMPath of
    Just (tgt, Just path) -> do
      mvalidPos <- aidTgtToPos aid (blid b) (Just tgt)
      if isNothing mvalidPos then return Nothing  -- wrong level
      else return $! case path of
        (p : q : rest, (goal, len)) ->
          if bpos b == p
          then Just (tgt, path)  -- no move last turn
          else if bpos b == q
               then Just (tgt, (q : rest, (goal, len - 1)))  -- step along path
               else Nothing  -- veered off the path
        ([p], (goal, _)) -> do
          assert (p == goal `blame` (aid, b, mtgtMPath)) skip
          if bpos b == p then
            Just (tgt, path)  -- goal reached; stay there picking up items
          else
            Nothing  -- somebody pushed us off the goal; let's target again
        ([], _) -> assert `failure` (aid, b, mtgtMPath)
    Just (_, Nothing) -> return Nothing  -- path invalidated, e.g. SpotActorA
    Nothing -> return Nothing  -- no target assigned yet
  lvl <- getLevel $ blid b
  assert (not $ bproj b) skip  -- would work, but is probably a bug
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  dungeon <- getsState sdungeon
  itemD <- getsState sitemD
  disco <- getsClient sdisco
  -- TODO: we assume the actor eventually becomes a leader (or has the same
  -- set of abilities as the leader, anyway) and set his target accordingly.
  actorAbs <- actorAbilities aid (Just aid)
  condCanProject <- condCanProjectM aid
  condMeleeBad <- condMeleeBadM aid
  condHpTooLow <- condHpTooLowM aid
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  -- TODO: refine all this when some actors specialize in ranged attacks
  -- (then we have to target, but keep the distance, we can do similarly for
  -- wounded or alone actors, perhaps only until they are shot first time,
  -- and only if they can shoot at the moment)
  fightsSpawners <- fightsAgainstSpawners (bfid b)
  explored <- getsClient sexplored
  let meleeNearby | fightsSpawners = nearby `div` 2  -- not aggresive
                  | otherwise = nearby
      rangedNearby = 2 * meleeNearby
      targetableMelee body =
        chessDist (bpos body) (bpos b) < meleeNearby
        && not condMeleeBad
      targetableRangedOrSpecial body =
        chessDist (bpos body) (bpos b) < rangedNearby
        && (condCanProject
            || hpTooLow coactor body  -- easy prey
            || any (adjacent (bpos body) . bpos) friends)  -- attacks friends!
      targetableEnemy body =
        targetableMelee body || targetableRangedOrSpecial body
      nearbyFoes = filter (targetableEnemy . snd) allFoes
      unknownId = ouniqGroup "unknown space"
      itemUsefulness item =
        case jkind disco item of
          Nothing -> -- TODO: 30  -- experimenting is fun
             -- for now, cheating:
             effectToBenefit cops b (jeffect item)
          Just _ki -> effectToBenefit cops b $ jeffect item
      desirableItem item k | fightsSpawners = itemUsefulness item /= 0
                                              || itemPrice (item, k) > 0
                           | otherwise = itemUsefulness item /= 0
      desirableBag bag = any (\(iid, k) -> desirableItem (itemD EM.! iid) k)
                         $ EM.assocs bag
      desirable (_, (_, bag)) = desirableBag bag
      -- TODO: make more common when weak ranged foes preferred, etc.
      focused = bspeed b < speedNormal || condHpTooLow
      canSmell = asmell $ okind $ bkind b
      createPath :: Target -> m (Target, PathEtc)
      createPath tgt = do
        mpos <- aidTgtToPos aid (blid b) (Just tgt)
        let p = fromMaybe (assert `failure` (b, tgt)) mpos
        (bfs, mpath) <- getCacheBfsAndPath aid p
        return $! case mpath of
          Nothing -> assert `failure` "new target unreachable" `twith` (b, tgt)
          Just path -> (tgt, ( bpos b : path
                             , (p, fromMaybe (assert `failure` mpath)
                                   $ accessBfs bfs p) ))
      setPath :: Target -> m (Strategy (Target, PathEtc))
      setPath tgt = fmap (returN "pickNewTarget") $ createPath tgt
      pickNewTarget :: m (Strategy (Target, PathEtc))
      pickNewTarget = do
        -- TODO: for foes, items, etc. consider a few nearby, not just one
        cfoes <- closestFoes nearbyFoes aid
        case cfoes of
          (_, (aid2, _)) : _ -> setPath $ TEnemy aid2 False
          [] -> do
            -- Tracking enemies is more important than exploring,
            -- and smelling actors are usually blind, so bad at exploring.
            -- TODO: prefer closer items to older smells
            smpos <- if canSmell
                     then closestSmell aid
                     else return []
            case smpos of
              [] -> do
                citems <- if AbMoveItem `elem` actorAbs
                          then closestItems aid
                          else return []
                case filter desirable citems of
                  [] -> do
                    let lidExplored = ES.member (blid b) explored
                    upos <- if lidExplored
                            then return Nothing
                            else closestUnknown aid
                    case upos of
                      Nothing -> do
                        csuspect <- if lidExplored
                                    then return []
                                    else closestSuspect aid
                        case csuspect of
                          [] -> do
                            ctriggers <- if AbTrigger `elem` actorAbs
                                         then closestTriggers Nothing False aid
                                         else return []
                            case ctriggers of
                              [] -> do
                                getDistant <-
                                  rndToAction $ oneOf
                                  $ [fmap maybeToList . furthestKnown]
                                    ++ [ closestTriggers Nothing True
                                       | EM.size dungeon > 1 ]
                                kpos <- getDistant aid
                                case kpos of
                                  [] -> return reject
                                  p : _ -> setPath $ TPoint (blid b) p
                              p : _ -> setPath $ TPoint (blid b) p
                          p : _ -> setPath $ TPoint (blid b) p
                      Just p -> setPath $ TPoint (blid b) p
                  (_, (p, _)) : _ -> setPath $ TPoint (blid b) p
              (_, (p, _)) : _ -> setPath $ TPoint (blid b) p
      tellOthersNothingHere pos = do
        let f (tgt, _) = case tgt of
              TEnemyPos _ lid p _ -> p /= pos || lid /= blid b
              _ -> True
        modifyClient $ \cli -> cli {stargetD = EM.filter f (stargetD cli)}
        pickNewTarget
      updateTgt :: Target -> PathEtc
                -> m (Strategy (Target, PathEtc))
      updateTgt oldTgt updatedPath = case oldTgt of
        TEnemy a _ -> do
          body <- getsState $ getActorBody a
          if not focused  -- prefers closer foes
             && a `notElem` map fst nearbyFoes  -- old one not close enough
             || blid body /= blid b  -- wrong level
             || actorDying body  -- foe already dying
          then pickNewTarget
          else if bpos body == fst (snd updatedPath)
               then return $! returN "TEnemy" (oldTgt, updatedPath)
                      -- The enemy didn't move since the target acquired.
                      -- If any walls were added that make the enemy
                      -- unreachable, AI learns that the hard way,
                      -- as soon as it bumps into them.
               else do
                 let p = bpos body
                 (bfs, mpath) <- getCacheBfsAndPath aid p
                 case mpath of
                   Nothing -> pickNewTarget  -- enemy became unreachable
                   Just path ->
                      return $! returN "TEnemy"
                        (oldTgt, ( bpos b : path
                                 , (p, fromMaybe (assert `failure` mpath)
                                       $ accessBfs bfs p) ))
        TEnemyPos _ lid p _ ->
          -- Chase last position even if foe hides or dies,
          -- to find his companions, loot, etc.
          if lid /= blid b  -- wrong level
             || chessDist (bpos b) p >= nearby  -- too far and not visible
          then pickNewTarget
          else if p == bpos b
               then tellOthersNothingHere p
               else return $! returN "TEnemyPos" (oldTgt, updatedPath)
        _ | not $ null nearbyFoes ->
          pickNewTarget  -- prefer close foes to anything
        TPoint lid pos -> do
          let allExplored = ES.size explored == EM.size dungeon
              abilityLeader = fAbilityLeader $ fokind $ gkind fact
              abilityOther = fAbilityOther $ fokind $ gkind fact
          if lid /= blid b  -- wrong level
             -- Below we check the target could not be picked again in
             -- pickNewTarget, and only in this case it is invalidated.
             -- This ensures targets are eventually reached (unless a foe
             -- shows up) and not changed all the time mid-route
             -- to equally interesting, but perhaps a bit closer targets,
             -- most probably already targeted by other actors.
             || (AbMoveItem `notElem` actorAbs  -- closestItems
                 || not (desirableBag (lvl `atI` pos)))
                && (not canSmell  -- closestSmell
                    || pos == bpos b  -- in case server resends deleted smell
                    || let sml =
                             EM.findWithDefault timeZero pos (lsmell lvl)
                       in sml `timeDeltaToFrom` ltime lvl <= Delta timeZero)
                && let t = lvl `at` pos
                   in if ES.notMember lid explored
                      then  -- closestUnknown
                        t /= unknownId
                        && not (Tile.isSuspect cotile t)
                      else  -- closestTriggers
                        -- Try to kill that very last enemy for his loot before
                        -- leaving the level or dungeon.
                        not (null allFoes)
                        || -- If all explored, escape/block escapes.
                           (AbTrigger `notElem` actorAbs
                            || not (Tile.isEscape cotile t && allExplored))
                           -- The next case is stairs in closestTriggers.
                           -- We don't determine if the stairs are interesting
                           -- (this changes with time), but allow the actor
                           -- to reach them and then retarget.
                           && not (pos /= bpos b && Tile.isStair cotile t)
                           -- The remaining case is furthestKnown. This is
                           -- always an unimportant target, so we forget it
                           -- if the actor is stuck (could move, but waits).
                           && let isStuck =
                                    waitedLastTurn b
                                    && (oldLeader == aid
                                        || abilityLeader == abilityOther)
                              in not (pos /= bpos b
                                      && not isStuck
                                      && allExplored)
          then pickNewTarget
          else return $! returN "TPoint" (oldTgt, updatedPath)
        TVector{} -> pickNewTarget
  case oldTgtUpdatedPath of
    Just (oldTgt, updatedPath) -> updateTgt oldTgt updatedPath
    Nothing -> pickNewTarget
